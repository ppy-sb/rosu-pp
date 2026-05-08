use crate::{model::beatmap::BeatmapAttributesExt, osurelax::performance::OsuRelaxPerformance};

/// The result of a difficulty calculation on an osu!standard map.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct OsuRelaxDifficultyAttributes {
    /// The difficulty of the aim skill.
    pub aim: f64,
    /// The number of sliders weighted by difficulty.
    pub aim_difficult_slider_count: f64,
    /// The difficulty of the speed skill.
    pub speed: f64,
    /// The difficulty of the flashlight skill.
    pub flashlight: f64,
    /// The ratio of the aim strain with and without considering sliders
    pub slider_factor: f64,
    /// Describes how much of aim's difficult strain count is contributed to by sliders
    pub aim_top_weighted_slider_factor: f64,
    /// Describes how much of speed's difficult strain count is contributed to by sliders
    pub speed_top_weighted_slider_factor: f64,
    /// The number of clickable objects weighted by difficulty.
    pub speed_note_count: f64,
    /// Weighted sum of aim strains.
    pub aim_difficult_strain_count: f64,
    /// Weighted sum of speed strains.
    pub speed_difficult_strain_count: f64,
    /// The amount of nested score per object.
    pub nested_score_per_object: f64,
    /// The legacy score base multiplier.
    pub legacy_score_base_multiplier: f64,
    /// The maximum legacy combo score.
    pub maximum_legacy_combo_score: f64,
    /// The approach rate.
    pub ar: f64,
    /// The great hit window.
    pub great_hit_window: f64,
    /// The ok hit window.
    pub ok_hit_window: f64,
    /// The meh hit window.
    pub meh_hit_window: f64,
    /// The health drain rate.
    pub hp: f64,
    /// The amount of circles.
    pub n_circles: u32,
    /// The amount of sliders.
    pub n_sliders: u32,
    /// The amount of "large ticks".
    ///
    /// The meaning depends on the kind of score:
    /// - if set on osu!stable, this value is irrelevant
    /// - if set on osu!lazer *with* slider accuracy, this value is the amount
    ///   of hit slider ticks and repeats
    /// - if set on osu!lazer *without* slider accuracy, this value is the
    ///   amount of hit slider heads, ticks, and repeats
    pub n_large_ticks: u32,
    /// The amount of spinners.
    pub n_spinners: u32,
    /// The final star rating
    pub stars: f64,
    /// The maximum combo.
    pub max_combo: u32,
}

impl OsuRelaxDifficultyAttributes {
    /// Return the maximum combo.
    pub const fn max_combo(&self) -> u32 {
        self.max_combo
    }

    /// Return the amount of hitobjects.
    pub const fn n_objects(&self) -> u32 {
        self.n_circles + self.n_sliders + self.n_spinners
    }

    /// The overall difficulty
    pub const fn od(&self) -> f64 {
        BeatmapAttributesExt::osu_great_hit_window_to_od(self.great_hit_window)
    }

    /// Returns a builder for performance calculation.
    pub fn performance<'a>(self) -> OsuRelaxPerformance<'a> {
        self.into()
    }
}

/// The result of a performance calculation on an osu!standard map.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct OsuRelaxPerformanceAttributes {
    /// The difficulty attributes that were used for the performance calculation
    pub difficulty: OsuRelaxDifficultyAttributes,
    /// The final performance points.
    pub pp: f64,
    /// The accuracy portion of the final pp.
    pub pp_acc: f64,
    /// The aim portion of the final pp.
    pub pp_aim: f64,
    /// The flashlight portion of the final pp.
    pub pp_flashlight: f64,
    /// The speed portion of the final pp.
    pub pp_speed: f64,
    /// Misses including an approximated amount of slider breaks
    pub effective_miss_count: f64,
    /// Approximated unstable-rate
    pub speed_deviation: Option<f64>,
    pub combo_based_estimated_miss_count: f64,
    pub score_based_estimated_miss_count: Option<f64>,
    pub aim_estimated_slider_breaks: f64,
    pub speed_estimated_slider_breaks: f64,
}

impl OsuRelaxPerformanceAttributes {
    /// Return the star value.
    pub const fn stars(&self) -> f64 {
        self.difficulty.stars
    }

    /// Return the performance point value.
    pub const fn pp(&self) -> f64 {
        self.pp
    }

    /// Return the maximum combo of the map.
    pub const fn max_combo(&self) -> u32 {
        self.difficulty.max_combo
    }
    /// Return the amount of hitobjects.
    pub const fn n_objects(&self) -> u32 {
        self.difficulty.n_objects()
    }

    /// Returns a builder for performance calculation.
    pub fn performance<'a>(self) -> OsuRelaxPerformance<'a> {
        self.difficulty.into()
    }
}

impl From<OsuRelaxPerformanceAttributes> for OsuRelaxDifficultyAttributes {
    fn from(attributes: OsuRelaxPerformanceAttributes) -> Self {
        attributes.difficulty
    }
}

impl From<OsuRelaxDifficultyAttributes> for crate::osu::OsuDifficultyAttributes {
    fn from(a: OsuRelaxDifficultyAttributes) -> Self {
        Self {
            aim: a.aim,
            aim_difficult_slider_count: a.aim_difficult_slider_count,
            speed: a.speed,
            flashlight: a.flashlight,
            slider_factor: a.slider_factor,
            aim_top_weighted_slider_factor: a.aim_top_weighted_slider_factor,
            speed_top_weighted_slider_factor: a.speed_top_weighted_slider_factor,
            speed_note_count: a.speed_note_count,
            aim_difficult_strain_count: a.aim_difficult_strain_count,
            speed_difficult_strain_count: a.speed_difficult_strain_count,
            nested_score_per_object: a.nested_score_per_object,
            legacy_score_base_multiplier: a.legacy_score_base_multiplier,
            maximum_legacy_combo_score: a.maximum_legacy_combo_score,
            ar: a.ar,
            great_hit_window: a.great_hit_window,
            ok_hit_window: a.ok_hit_window,
            meh_hit_window: a.meh_hit_window,
            hp: a.hp,
            n_circles: a.n_circles,
            n_sliders: a.n_sliders,
            n_large_ticks: a.n_large_ticks,
            n_spinners: a.n_spinners,
            stars: a.stars,
            max_combo: a.max_combo,
        }
    }
}

impl From<crate::osu::OsuDifficultyAttributes> for OsuRelaxDifficultyAttributes {
    fn from(a: crate::osu::OsuDifficultyAttributes) -> Self {
        Self {
            aim: a.aim,
            aim_difficult_slider_count: a.aim_difficult_slider_count,
            speed: a.speed,
            flashlight: a.flashlight,
            slider_factor: a.slider_factor,
            aim_top_weighted_slider_factor: a.aim_top_weighted_slider_factor,
            speed_top_weighted_slider_factor: a.speed_top_weighted_slider_factor,
            speed_note_count: a.speed_note_count,
            aim_difficult_strain_count: a.aim_difficult_strain_count,
            speed_difficult_strain_count: a.speed_difficult_strain_count,
            nested_score_per_object: a.nested_score_per_object,
            legacy_score_base_multiplier: a.legacy_score_base_multiplier,
            maximum_legacy_combo_score: a.maximum_legacy_combo_score,
            ar: a.ar,
            great_hit_window: a.great_hit_window,
            ok_hit_window: a.ok_hit_window,
            meh_hit_window: a.meh_hit_window,
            hp: a.hp,
            n_circles: a.n_circles,
            n_sliders: a.n_sliders,
            n_large_ticks: a.n_large_ticks,
            n_spinners: a.n_spinners,
            stars: a.stars,
            max_combo: a.max_combo,
        }
    }
}

impl From<OsuRelaxPerformanceAttributes> for crate::osu::OsuPerformanceAttributes {
    fn from(a: OsuRelaxPerformanceAttributes) -> Self {
        Self {
            difficulty: a.difficulty.into(),
            pp: a.pp,
            pp_acc: a.pp_acc,
            pp_aim: a.pp_aim,
            pp_flashlight: a.pp_flashlight,
            pp_speed: a.pp_speed,
            effective_miss_count: a.effective_miss_count,
            speed_deviation: a.speed_deviation,
            combo_based_estimated_miss_count: a.combo_based_estimated_miss_count,
            score_based_estimated_miss_count: a.score_based_estimated_miss_count,
            aim_estimated_slider_breaks: a.aim_estimated_slider_breaks,
            speed_estimated_slider_breaks: a.speed_estimated_slider_breaks,
        }
    }
}
