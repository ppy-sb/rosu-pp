use std::{
    cmp,
    f64::consts::{FRAC_PI_2, PI, E},
};

use crate::{
    any::difficulty::{
        object::IDifficultyObject,
        skills::{strain_decay, ISkill, Skill},
    },
    osu::difficulty::object::OsuDifficultyObject,
    util::{float_ext::FloatExt, strains_vec::StrainsVec},
};

use super::strain::{DifficultyValue, OsuStrainSkill, UsedOsuStrainSkills};

const SKILL_MULTIPLIER: f64 = 23.55;
const STRAIN_DECAY_BASE: f64 = 0.15;

#[derive(Clone)]
pub struct Relax {
    curr_strain: f64,
    hit_window: f64,
    inner: OsuStrainSkill,
}

impl Relax {
    pub fn new(hit_window: f64) -> Self {
        Self {
            curr_strain: 0.0,
            hit_window,
            inner: OsuStrainSkill::default(),
        }
    }

    pub fn get_curr_strain_peaks(self) -> StrainsVec {
        self.inner.get_curr_strain_peaks().strains()
    }

    pub fn difficulty_value(self) -> UsedOsuStrainSkills<DifficultyValue> {
        Self::static_difficulty_value(self.inner)
    }

    /// Use [`difficulty_value`] instead whenever possible because
    /// [`as_difficulty_value`] clones internally.
    pub fn as_difficulty_value(&self) -> UsedOsuStrainSkills<DifficultyValue> {
        Self::static_difficulty_value(self.inner.clone())
    }

    fn static_difficulty_value(skill: OsuStrainSkill) -> UsedOsuStrainSkills<DifficultyValue> {
        skill.difficulty_value(
            OsuStrainSkill::REDUCED_SECTION_COUNT,
            OsuStrainSkill::REDUCED_STRAIN_BASELINE,
            OsuStrainSkill::DECAY_WEIGHT,
        )
    }
}

impl ISkill for Relax {
    type DifficultyObjects<'a> = [OsuDifficultyObject<'a>];
}

impl<'a> Skill<'a, Relax> {
    fn calculate_initial_strain(&mut self, time: f64, curr: &'a OsuDifficultyObject<'a>) -> f64 {
        let prev_start_time = curr
            .previous(0, self.diff_objects)
            .map_or(0.0, |prev| prev.start_time);

        self.inner.curr_strain * strain_decay(time - prev_start_time, STRAIN_DECAY_BASE)
    }

    fn curr_section_peak(&self) -> f64 {
        self.inner.inner.inner.curr_section_peak
    }

    fn curr_section_peak_mut(&mut self) -> &mut f64 {
        &mut self.inner.inner.inner.curr_section_peak
    }

    fn curr_section_end(&self) -> f64 {
        self.inner.inner.inner.curr_section_end
    }

    fn curr_section_end_mut(&mut self) -> &mut f64 {
        &mut self.inner.inner.inner.curr_section_end
    }

    pub fn process(&mut self, curr: &'a OsuDifficultyObject<'a>) {
        if curr.idx == 0 {
            *self.curr_section_end_mut() = (curr.start_time / OsuStrainSkill::SECTION_LEN).ceil()
                * OsuStrainSkill::SECTION_LEN;
        }

        while curr.start_time > self.curr_section_end() {
            self.inner.inner.save_curr_peak();
            let initial_strain = self.calculate_initial_strain(self.curr_section_end(), curr);
            self.inner.inner.start_new_section_from(initial_strain);
            *self.curr_section_end_mut() += OsuStrainSkill::SECTION_LEN;
        }

        let strain_value_at = self.strain_value_at(curr);
        *self.curr_section_peak_mut() = strain_value_at.max(self.curr_section_peak());
    }

    fn strain_value_at(&mut self, curr: &'a OsuDifficultyObject<'a>) -> f64 {
        self.inner.curr_strain *= strain_decay(curr.delta_time, STRAIN_DECAY_BASE);
        self.inner.curr_strain +=
            RelaxAimEvaluator::evaluate_diff_of(curr, self.diff_objects, self.inner.hit_window)
                * SKILL_MULTIPLIER;

        self.inner.curr_strain
    }
}

struct RelaxAimEvaluator;

impl RelaxAimEvaluator {
    const WIDE_ANGLE_MULTIPLIER: f64 = 1.5;
    const ACUTE_ANGLE_MULTIPLIER: f64 = 1.95;
    const SLIDER_MULTIPLIER: f64 = 1.5;
    const VELOCITY_CHANGE_MULTIPLIER: f64 = 1.2;

    fn evaluate_diff_of<'a>(
        curr: &'a OsuDifficultyObject<'a>,
        diff_objects: &'a [OsuDifficultyObject<'a>],
        hit_window: f64,
    ) -> f64 {
        let osu_curr_obj = curr;

        let Some((osu_last_last_obj, osu_last_obj)) = curr
            .previous(1, diff_objects)
            .zip(curr.previous(0, diff_objects))
            .filter(|(_, last)| !(curr.base.is_spinner() || last.base.is_spinner()))
        else {
            return 0.0;
        };

        // * Calculate the velocity to the current hitobject, which starts
        // * with a base distance / time assuming the last object is a hitcircle.
        let mut curr_vel = osu_curr_obj.lazy_jump_dist / osu_curr_obj.strain_time;

        // * But if the last object is a slider, then we extend the travel
        // * velocity through the slider into the current object.
        if osu_last_obj.base.is_slider() {
            // * calculate the slider velocity from slider head to slider end.
            let travel_vel = osu_last_obj.travel_dist / osu_last_obj.travel_time;
            // * calculate the movement velocity from slider end to current object
            let movement_vel = osu_curr_obj.min_jump_dist / osu_curr_obj.min_jump_time;

            // * take the larger total combined velocity.
            curr_vel = curr_vel.max(movement_vel + travel_vel);
        }

        // * As above, do the same for the previous hitobject.
        let mut prev_vel = osu_last_obj.lazy_jump_dist / osu_last_obj.strain_time;

        if osu_last_last_obj.base.is_slider() {
            let travel_vel = osu_last_last_obj.travel_dist / osu_last_last_obj.travel_time;
            let movement_vel = osu_last_obj.min_jump_dist / osu_last_obj.min_jump_time;

            prev_vel = prev_vel.max(movement_vel + travel_vel);
        }

        let mut wide_angle_bonus = 0.0;
        let mut acute_angle_bonus = 0.0;
        let mut slider_bonus = 0.0;
        let mut vel_change_bonus = 0.0;

        // * Start strain with regular velocity.
        let mut aim_strain = curr_vel;

        // * Penalize overall stream aim.
        // * Fittings: [(100, 0.92), (300, 0.98)] linear function.
        let stream_nerf = 0.0006 * osu_curr_obj.lazy_jump_dist + 0.86;
        aim_strain *= stream_nerf.clamp(0.92, 0.98);

        // * If rhythms are the same.
        if osu_curr_obj.strain_time.max(osu_last_obj.strain_time)
            < 1.25 * osu_curr_obj.strain_time.min(osu_last_obj.strain_time)
        {
            if let Some(((curr_angle, last_angle), last_last_angle)) = osu_curr_obj
                .angle
                .zip(osu_last_obj.angle)
                .zip(osu_last_last_obj.angle)
            {
                // * Rewarding angles, take the smaller velocity as base.
                let angle_bonus = curr_vel.min(prev_vel);

                wide_angle_bonus = Self::calc_wide_angle_bonus(curr_angle);
                acute_angle_bonus = Self::calc_acute_angle_bonus(curr_angle);

                // * Only buff deltaTime exceeding 300 bpm 1/2.
                if osu_curr_obj.strain_time > 100.0 {
                    acute_angle_bonus = 0.0;
                } else {
                    // * Penalize deltaTime since relax is eaiser to hit at that BPM.
                    let slower_strain_time = osu_curr_obj.strain_time * 1.2;

                    let base1 = (FRAC_PI_2 * ((120.0 - slower_strain_time) / 25.0).min(1.0)).sin();

                    let base2 = (FRAC_PI_2
                        * ((osu_curr_obj.lazy_jump_dist).clamp(50.0, 100.0) - 50.0)
                        / 50.0)
                        .sin();

                    // * Multiply by previous angle, we don't want to buff unless this is a wiggle type pattern.
                    acute_angle_bonus *= Self::calc_acute_angle_bonus(last_angle)
                    // * The maximum velocity we buff is equal to 125 / strainTime
                        * angle_bonus.min(125.0 / slower_strain_time)
                        // * scale buff from 150 bpm 1/4 to 200 bpm 1/4
                        * base1.powf(2.0)
                         // * Buff distance exceeding 50 (radius) up to 100 (diameter).
                        * base2.powf(2.0);
                }

                // * Penalize wide angles if they're repeated, reducing the penalty as the lastAngle gets more acute.
                wide_angle_bonus *= angle_bonus
                    * (1.0
                        - wide_angle_bonus.min(Self::calc_wide_angle_bonus(last_angle).powf(3.0)));

                // * Penalize wide angles if their distances are quite small (consider as wide angle stream).
                // * Only jump dist is considered here, not velocity.
                // * Fittings: [(200, 0), (250, 0.5), (300, 1), (350, 1)] linear function.
                let wide_stream_nerf = osu_curr_obj.lazy_jump_dist * 0.007 - 1.3;
                wide_angle_bonus *= wide_stream_nerf.clamp(0.0, 1.0);

                // * Penalize acute angles if they're repeated, reducing the penalty as the lastLastAngle gets more obtuse.
                acute_angle_bonus *= 0.5
                    + 0.5
                        * (1.0
                            - acute_angle_bonus
                                .min(Self::calc_acute_angle_bonus(last_last_angle).powf(3.0)));
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
            let overlap_vel_buff = (125.0 / osu_curr_obj.strain_time.min(osu_last_obj.strain_time))
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

        // * Add in acute angle bonus or wide angle bonus + velocity change bonus, whichever is larger.
        aim_strain += (acute_angle_bonus * Self::ACUTE_ANGLE_MULTIPLIER).max(
            wide_angle_bonus * Self::WIDE_ANGLE_MULTIPLIER
                + vel_change_bonus * Self::VELOCITY_CHANGE_MULTIPLIER,
        );

        aim_strain += slider_bonus * Self::SLIDER_MULTIPLIER;

        // * If the distance is small enough, we want to buff the rhythm complexity.
        if osu_curr_obj.lazy_jump_dist < 350.0 {
            aim_strain *= RelaxRhythmEvaluator::evaluate_diff_of(curr, diff_objects, hit_window);
        }

        aim_strain
    }

    fn calc_wide_angle_bonus(angle: f64) -> f64 {
        (3.0 / 4.0 * ((5.0 / 6.0 * PI).min(angle.max(PI / 6.0)) - PI / 6.0))
            .sin()
            .powf(2.0)
    }

    fn calc_acute_angle_bonus(angle: f64) -> f64 {
        1.0 - Self::calc_wide_angle_bonus(angle)
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
                            let power = logistic(f64::from(island.delta), 2.75, 0.24, 14.0);
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

fn logistic(x: f64, max_value: f64, multiplier: f64, offset: f64) -> f64 {
    max_value / (1.0 + E.powf(offset - (multiplier * x)))
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
