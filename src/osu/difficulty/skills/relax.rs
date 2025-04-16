use std::f64::consts::FRAC_PI_2;
use std::{cmp, f64::consts::PI};

use crate::{
    any::difficulty::{
        object::{HasStartTime, IDifficultyObject},
        skills::{strain_decay, StrainSkill},
    },
    osu::difficulty::object::OsuDifficultyObject,
    util::{
        difficulty::{logistic, milliseconds_to_bpm, reverse_lerp, smootherstep, smoothstep},
        float_ext::FloatExt,
        strains_vec::StrainsVec,
    },
};

use super::strain::OsuStrainSkill;

define_skill! {
    #[derive(Clone)]
    pub struct Relax: StrainSkill => [OsuDifficultyObject<'a>][OsuDifficultyObject<'a>] {
        include_sliders: bool,
        current_strain: f64 = 0.0,
        hit_window: f64,
        slider_strains: Vec<f64> = Vec::with_capacity(64), // TODO: use `StrainsVec`?
    }
}

impl Relax {
    const SKILL_MULTIPLIER: f64 = 25.6;
    const STRAIN_DECAY_BASE: f64 = 0.15;

    fn calculate_initial_strain(
        &mut self,
        time: f64,
        curr: &OsuDifficultyObject<'_>,
        objects: &[OsuDifficultyObject<'_>],
    ) -> f64 {
        let prev_start_time = curr
            .previous(0, objects)
            .map_or(0.0, HasStartTime::start_time);

        self.current_strain * strain_decay(time - prev_start_time, Self::STRAIN_DECAY_BASE)
    }

    fn strain_value_at(
        &mut self,
        curr: &OsuDifficultyObject<'_>,
        objects: &[OsuDifficultyObject<'_>],
    ) -> f64 {
        self.current_strain *= strain_decay(curr.delta_time, Self::STRAIN_DECAY_BASE);
        self.current_strain += RelaxAimEvaluator::evaluate_diff_of(
            curr,
            objects,
            self.hit_window,
            self.include_sliders,
        ) * Self::SKILL_MULTIPLIER;

        if curr.base.is_slider() {
            self.slider_strains.push(self.current_strain);
        }

        self.current_strain
    }

    pub fn get_difficult_sliders(&self) -> f64 {
        if self.slider_strains.is_empty() {
            return 0.0;
        }

        let max_slider_strain = self.slider_strains.iter().copied().fold(0.0, f64::max);

        if FloatExt::eq(max_slider_strain, 0.0) {
            return 0.0;
        }

        self.slider_strains
            .iter()
            .copied()
            .map(|strain| 1.0 / (1.0 + f64::exp(-(strain / max_slider_strain * 12.0 - 6.0))))
            .sum()
    }

    // From `OsuStrainSkill`; native rather than trait function so that it has
    // priority over `StrainSkill::difficulty_value`
    fn difficulty_value(current_strain_peaks: StrainsVec) -> f64 {
        super::strain::difficulty_value(
            current_strain_peaks,
            Self::REDUCED_SECTION_COUNT,
            Self::REDUCED_STRAIN_BASELINE,
            Self::DECAY_WEIGHT,
        )
    }
}

impl OsuStrainSkill for Relax {}

struct RelaxAimEvaluator;

impl RelaxAimEvaluator {
    const WIDE_ANGLE_MULTIPLIER: f64 = 1.5;
    const ACUTE_ANGLE_MULTIPLIER: f64 = 2.6;
    const SLIDER_MULTIPLIER: f64 = 1.5;
    const VELOCITY_CHANGE_MULTIPLIER: f64 = 1.2;
    const WIGGLE_MULTIPLIER: f64 = 1.02;

    #[allow(clippy::too_many_lines)]
    fn evaluate_diff_of<'a>(
        curr: &'a OsuDifficultyObject<'a>,
        diff_objects: &'a [OsuDifficultyObject<'a>],
        hit_window: f64,
        with_slider_travel_dist: bool,
    ) -> f64 {
        let osu_curr_obj = curr;

        let Some((osu_last_last_obj, osu_last_obj)) = curr
            .previous(1, diff_objects)
            .zip(curr.previous(0, diff_objects))
            .filter(|(_, last)| !(curr.base.is_spinner() || last.base.is_spinner()))
        else {
            return 0.0;
        };

        #[allow(clippy::items_after_statements)]
        const RADIUS: i32 = OsuDifficultyObject::NORMALIZED_RADIUS;
        #[allow(clippy::items_after_statements)]
        const DIAMETER: i32 = OsuDifficultyObject::NORMALIZED_DIAMETER;

        // * Calculate the velocity to the current hitobject, which starts
        // * with a base distance / time assuming the last object is a hitcircle.
        let mut curr_vel = osu_curr_obj.lazy_jump_dist / osu_curr_obj.strain_time;

        // * But if the last object is a slider, then we extend the travel
        // * velocity through the slider into the current object.
        if osu_last_obj.base.is_slider() && with_slider_travel_dist {
            // * calculate the slider velocity from slider head to slider end.
            let travel_vel = osu_last_obj.travel_dist / osu_last_obj.travel_time;
            // * calculate the movement velocity from slider end to current object
            let movement_vel = osu_curr_obj.min_jump_dist / osu_curr_obj.min_jump_time;

            // * take the larger total combined velocity.
            curr_vel = curr_vel.max(movement_vel + travel_vel);
        }

        // * As above, do the same for the previous hitobject.
        let mut prev_vel = osu_last_obj.lazy_jump_dist / osu_last_obj.strain_time;

        if osu_last_last_obj.base.is_slider() && with_slider_travel_dist {
            let travel_vel = osu_last_last_obj.travel_dist / osu_last_last_obj.travel_time;
            let movement_vel = osu_last_obj.min_jump_dist / osu_last_obj.min_jump_time;

            prev_vel = prev_vel.max(movement_vel + travel_vel);
        }

        let mut wide_angle_bonus = 0.0;
        let mut acute_angle_bonus = 0.0;
        let mut slider_bonus = 0.0;
        let mut vel_change_bonus = 0.0;
        let mut wiggle_bonus = 0.0;

        // * Start strain with regular velocity.
        let mut aim_strain = curr_vel;

        // R* Penalize overall stream aim.
        // R* Fittings: [(100, 0.92), (300, 0.98)] linear function.
        let stream_nerf = 0.0006 * osu_curr_obj.lazy_jump_dist + 0.86;
        aim_strain *= stream_nerf.clamp(0.92, 0.98);

        // * If rhythms are the same.
        if osu_curr_obj.strain_time.max(osu_last_obj.strain_time)
            < 1.25 * osu_curr_obj.strain_time.min(osu_last_obj.strain_time)
        {
            if let Some((curr_angle, last_angle)) = osu_curr_obj.angle.zip(osu_last_obj.angle) {
                // * Rewarding angles, take the smaller velocity as base.
                let angle_bonus = curr_vel.min(prev_vel);

                wide_angle_bonus = Self::calc_wide_angle_bonus(curr_angle);
                acute_angle_bonus = Self::calc_acute_angle_bonus(curr_angle);

                // * Penalize angle repetition.
                wide_angle_bonus *= 1.0
                    - f64::min(
                        wide_angle_bonus,
                        f64::powf(Self::calc_wide_angle_bonus(last_angle), 3.0),
                    );
                acute_angle_bonus *= 0.08
                    + 0.92
                        * (1.0
                            - f64::min(
                                acute_angle_bonus,
                                f64::powf(Self::calc_acute_angle_bonus(last_angle), 3.0),
                            ));

                // * Apply full wide angle bonus for distance more than one diameter
                wide_angle_bonus *= angle_bonus
                    * smootherstep(osu_curr_obj.lazy_jump_dist, 0.0, f64::from(DIAMETER));

                // * Apply acute angle bonus for BPM above 300 1/2 and distance more than one diameter
                acute_angle_bonus *= angle_bonus
                    * smootherstep(
                        milliseconds_to_bpm(osu_curr_obj.strain_time, Some(2)),
                        300.0,
                        400.0,
                    )
                    * smootherstep(
                        osu_curr_obj.lazy_jump_dist,
                        f64::from(DIAMETER),
                        f64::from(DIAMETER * 2),
                    );

                // R* Penalize wide angles if their distances are quite small (consider as wide angle stream).
                // R* Only jump dist is considered here, not velocity.
                // R* Fittings: [(200, 0), (250, 0.5), (300, 1), (350, 1)] linear function.
                let wide_stream_nerf = osu_curr_obj.lazy_jump_dist * 0.007 - 1.3;
                wide_angle_bonus *= wide_stream_nerf.clamp(0.0, 1.0);

                // * Apply wiggle bonus for jumps that are [radius, 3*diameter] in distance, with < 110 angle
                // * https://www.desmos.com/calculator/dp0v0nvowc
                wiggle_bonus = angle_bonus
                    * smootherstep(
                        osu_curr_obj.lazy_jump_dist,
                        f64::from(RADIUS),
                        f64::from(DIAMETER),
                    )
                    * f64::powf(
                        reverse_lerp(
                            osu_curr_obj.lazy_jump_dist,
                            f64::from(DIAMETER * 3),
                            f64::from(DIAMETER),
                        ),
                        1.8,
                    )
                    * smootherstep(curr_angle, f64::to_radians(110.0), f64::to_radians(60.0))
                    * smootherstep(
                        osu_last_obj.lazy_jump_dist,
                        f64::from(RADIUS),
                        f64::from(DIAMETER),
                    )
                    * f64::powf(
                        reverse_lerp(
                            osu_last_obj.lazy_jump_dist,
                            f64::from(DIAMETER * 3),
                            f64::from(DIAMETER),
                        ),
                        1.8,
                    )
                    * smootherstep(last_angle, f64::to_radians(110.0), f64::to_radians(60.0));
            }
        }

        if prev_vel.max(curr_vel).not_eq(0.0) {
            // * We want to use the average velocity over the whole object when awarding
            // * differences, not the individual jump and slider path velocities.
            prev_vel = (osu_last_obj.lazy_jump_dist + osu_last_last_obj.travel_dist)
                / osu_last_obj.strain_time;
            curr_vel =
                (osu_curr_obj.lazy_jump_dist + osu_last_obj.travel_dist) / osu_curr_obj.strain_time;

            // * Scale with ratio of difference compared to 0.5 * max dist.
            let dist_ratio_base =
                (FRAC_PI_2 * (prev_vel - curr_vel).abs() / prev_vel.max(curr_vel)).sin();
            let dist_ratio = dist_ratio_base.powf(2.0);

            // * Reward for % distance up to 125 / strainTime for overlaps where velocity is still changing.
            let overlap_vel_buff = (f64::from(DIAMETER) * 1.25
                / osu_curr_obj.strain_time.min(osu_last_obj.strain_time))
            .min((prev_vel - curr_vel).abs());

            vel_change_bonus = overlap_vel_buff * dist_ratio;

            // * Penalize for rhythm changes.
            let bonus_base = (osu_curr_obj.strain_time).min(osu_last_obj.strain_time)
                / (osu_curr_obj.strain_time).max(osu_last_obj.strain_time);
            vel_change_bonus *= bonus_base.powf(2.0);
        }

        if osu_last_obj.base.is_slider() {
            // * Reward sliders based on velocity.
            slider_bonus = osu_last_obj.travel_dist / osu_last_obj.travel_time;
        }

        aim_strain += wiggle_bonus * Self::WIGGLE_MULTIPLIER;

        // * Add in acute angle bonus or wide angle bonus + velocity change bonus, whichever is larger.
        aim_strain += (acute_angle_bonus * Self::ACUTE_ANGLE_MULTIPLIER).max(
            wide_angle_bonus * Self::WIDE_ANGLE_MULTIPLIER
                + vel_change_bonus * Self::VELOCITY_CHANGE_MULTIPLIER,
        );

        // * Add in additional slider velocity bonus.
        if with_slider_travel_dist {
            aim_strain += slider_bonus * Self::SLIDER_MULTIPLIER;
        }

        // * If the distance is small enough, we want to buff the rhythm complexity.
        if osu_curr_obj.lazy_jump_dist < 350.0 {
            aim_strain *= RelaxRhythmEvaluator::evaluate_diff_of(curr, diff_objects, hit_window);
        }

        aim_strain
    }

    const fn calc_wide_angle_bonus(angle: f64) -> f64 {
        smoothstep(angle, f64::to_radians(40.0), f64::to_radians(140.0))
    }

    const fn calc_acute_angle_bonus(angle: f64) -> f64 {
        smoothstep(angle, f64::to_radians(140.0), f64::to_radians(40.0))
    }
}

struct RelaxRhythmEvaluator;

impl RelaxRhythmEvaluator {
    const HISTORY_TIME_MAX: u32 = 5 * 1000; // 5 seconds
    const HISTORY_OBJECTS_MAX: usize = 32;
    const RHYTHM_OVERALL_MULTIPLIER: f64 = 0.95;
    const RHYTHM_RATIO_MULTIPLIER: f64 = 12.0;

    #[allow(clippy::too_many_lines)]
    fn evaluate_diff_of<'a>(
        curr: &'a OsuDifficultyObject<'a>,
        diff_objects: &'a [OsuDifficultyObject<'a>],
        hit_window: f64,
    ) -> f64 {
        if curr.base.is_spinner() {
            return 0.0;
        }

        let mut rhythm_complexity_sum = 0.0;

        let delta_difference_eps = hit_window * 0.3;

        let mut island = RhythmIsland::new(delta_difference_eps);
        let mut prev_island = RhythmIsland::new(delta_difference_eps);

        // * we can't use dictionary here because we need to compare island with a tolerance
        // * which is impossible to pass into the hash comparer
        let mut island_counts = Vec::<IslandCount>::new();

        // * store the ratio of the current start of an island to buff for tighter rhythms
        let mut start_ratio = 0.0;

        let mut first_delta_switch = false;

        let historical_note_count = cmp::min(curr.idx, Self::HISTORY_OBJECTS_MAX);

        let mut rhythm_start = 0;

        while curr
            .previous(rhythm_start, diff_objects)
            .filter(|prev| {
                rhythm_start + 2 < historical_note_count
                    && curr.start_time - prev.start_time < f64::from(Self::HISTORY_TIME_MAX)
            })
            .is_some()
        {
            rhythm_start += 1;
        }

        if let Some((mut prev_obj, mut last_obj)) = curr
            .previous(rhythm_start, diff_objects)
            .zip(curr.previous(rhythm_start + 1, diff_objects))
        {
            // * we go from the furthest object back to the current one
            for i in (1..=rhythm_start).rev() {
                let Some(curr_obj) = curr.previous(i - 1, diff_objects) else {
                    break;
                };

                // * scales note 0 to 1 from history to now
                let time_decay = (f64::from(Self::HISTORY_TIME_MAX)
                    - (curr.start_time - curr_obj.start_time))
                    / f64::from(Self::HISTORY_TIME_MAX);
                let note_decay = (historical_note_count - i) as f64 / historical_note_count as f64;

                // * either we're limited by time or limited by object count.
                let curr_historical_decay = note_decay.min(time_decay);

                let curr_delta = curr_obj.strain_time;
                let prev_delta = prev_obj.strain_time;
                let last_delta = last_obj.strain_time;

                // * calculate how much current delta difference deserves a rhythm bonus
                // * this function is meant to reduce rhythm bonus for deltas that are multiples of each other (i.e 100 and 200)
                let delta_difference_ratio =
                    prev_delta.min(curr_delta) / prev_delta.max(curr_delta);
                let curr_ratio = 1.0
                    + Self::RHYTHM_RATIO_MULTIPLIER
                        * (PI / delta_difference_ratio).sin().powf(2.0).min(0.5);

                // reduce ratio bonus if delta difference is too big
                let fraction = (prev_delta / curr_delta).max(curr_delta / prev_delta);
                let fraction_multiplier = (2.0 - fraction / 8.0).clamp(0.0, 1.0);

                let window_penalty = (((prev_delta - curr_delta).abs() - delta_difference_eps)
                    .max(0.0)
                    / delta_difference_eps)
                    .min(1.0);

                let mut effective_ratio = window_penalty * curr_ratio * fraction_multiplier;

                if first_delta_switch {
                    // Keep in-sync with lazer
                    #[allow(clippy::if_not_else)]
                    if (prev_delta - curr_delta).abs() < delta_difference_eps {
                        // * island is still progressing
                        island.add_delta(curr_delta as i32);
                    } else {
                        // * bpm change is into slider, this is easy acc window
                        if curr_obj.base.is_slider() {
                            effective_ratio *= 0.125;
                        }

                        // * bpm change was from a slider, this is easier typically than circle -> circle
                        // * unintentional side effect is that bursts with kicksliders at the ends might have lower difficulty than bursts without sliders
                        if prev_obj.base.is_slider() {
                            effective_ratio *= 0.3;
                        }

                        // * repeated island polarity (2 -> 4, 3 -> 5)
                        if island.is_similar_polarity(&prev_island) {
                            effective_ratio *= 0.5;
                        }

                        // * previous increase happened a note ago, 1/1->1/2-1/4, dont want to buff this.
                        if last_delta > prev_delta + delta_difference_eps
                            && prev_delta > curr_delta + delta_difference_eps
                        {
                            effective_ratio *= 0.125;
                        }

                        // * repeated island size (ex: triplet -> triplet)
                        // * TODO: remove this nerf since its staying here only for balancing purposes because of the flawed ratio calculation
                        if prev_island.delta_count == island.delta_count {
                            effective_ratio *= 0.5;
                        }

                        if let Some(island_count) = island_counts
                            .iter_mut()
                            .find(|entry| entry.island == island)
                            .filter(|entry| !entry.island.is_default())
                        {
                            // * only add island to island counts if they're going one after another
                            if prev_island == island {
                                island_count.count += 1;
                            }

                            // * repeated island (ex: triplet -> triplet)
                            let power = logistic(f64::from(island.delta), 58.33, 0.24, Some(2.75));
                            effective_ratio *= (3.0 / island_count.count as f64)
                                .min((island_count.count as f64).recip().powf(power));
                        } else {
                            island_counts.push(IslandCount { island, count: 1 });
                        }

                        // * scale down the difficulty if the object is doubletappable
                        let doubletapness = prev_obj.get_doubletapness(Some(curr_obj), hit_window);
                        effective_ratio *= 1.0 - doubletapness * 0.75;

                        rhythm_complexity_sum +=
                            (effective_ratio * start_ratio).sqrt() * curr_historical_decay;

                        start_ratio = effective_ratio;

                        prev_island = island;

                        // * we're slowing down, stop counting
                        if prev_delta + delta_difference_eps < curr_delta {
                            // * if we're speeding up, this stays true and we keep counting island size.
                            first_delta_switch = false;
                        }

                        island =
                            RhythmIsland::new_with_delta(curr_delta as i32, delta_difference_eps);
                    }
                } else if prev_delta > curr_delta + delta_difference_eps {
                    // * we're speeding up.
                    // * Begin counting island until we change speed again.
                    first_delta_switch = true;

                    // * bpm change is into slider, this is easy acc window
                    if curr_obj.base.is_slider() {
                        effective_ratio *= 0.6;
                    }

                    // * bpm change was from a slider, this is easier typically than circle -> circle
                    // * unintentional side effect is that bursts with kicksliders at the ends might have lower difficulty than bursts without sliders
                    if prev_obj.base.is_slider() {
                        effective_ratio *= 0.6;
                    }

                    start_ratio = effective_ratio;

                    island = RhythmIsland::new_with_delta(curr_delta as i32, delta_difference_eps);
                }

                last_obj = prev_obj;
                prev_obj = curr_obj;
            }
        }

        // * produces multiplier that can be applied to strain. range [1, infinity) (not really though)
        (4.0 + rhythm_complexity_sum * Self::RHYTHM_OVERALL_MULTIPLIER).sqrt() / 2.0
    }
}

#[derive(Copy, Clone)]
struct RhythmIsland {
    delta_difference_eps: f64,
    delta: i32,
    delta_count: i32,
}

const MIN_DELTA_TIME: i32 = 25;

// Compile-time check in case `OsuDifficultyObject::MIN_DELTA_TIME` changes
// but we forget to update this value.
const _: [(); 0 - !{ MIN_DELTA_TIME - OsuDifficultyObject::MIN_DELTA_TIME as i32 == 0 } as usize] =
    [];

impl RhythmIsland {
    const fn new(delta_difference_eps: f64) -> Self {
        Self {
            delta_difference_eps,
            delta: 0,
            delta_count: 0,
        }
    }

    fn new_with_delta(delta: i32, delta_difference_eps: f64) -> Self {
        Self {
            delta_difference_eps,
            delta: delta.max(MIN_DELTA_TIME),
            delta_count: 1,
        }
    }

    fn add_delta(&mut self, delta: i32) {
        if self.delta == i32::MAX {
            self.delta = delta.max(MIN_DELTA_TIME);
        }

        self.delta_count += 1;
    }

    const fn is_similar_polarity(&self, other: &Self) -> bool {
        // * TODO: consider islands to be of similar polarity only if they're having the same average delta (we don't want to consider 3 singletaps similar to a triple)
        // *       naively adding delta check here breaks _a lot_ of maps because of the flawed ratio calculation
        self.delta_count % 2 == other.delta_count % 2
    }

    fn is_default(&self) -> bool {
        self.delta_difference_eps.abs() < f64::EPSILON
            && self.delta == i32::MAX
            && self.delta_count == 0
    }
}

impl PartialEq for RhythmIsland {
    fn eq(&self, other: &Self) -> bool {
        f64::from((self.delta - other.delta).abs()) < self.delta_difference_eps
            && self.delta_count == other.delta_count
    }
}

struct IslandCount {
    island: RhythmIsland,
    count: usize,
}
