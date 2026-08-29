use crate::{
    GameMods,
    mania::{ManiaDifficultyAttributes, ManiaPerformanceAttributes, ManiaScoreState},
};

pub(super) struct ManiaPerformanceCalculator<'mods> {
    attrs: ManiaDifficultyAttributes,
    mods: &'mods GameMods,
    state: ManiaScoreState,
}

impl<'a> ManiaPerformanceCalculator<'a> {
    pub const fn new(
        attrs: ManiaDifficultyAttributes,
        mods: &'a GameMods,
        state: ManiaScoreState,
    ) -> Self {
        Self { attrs, mods, state }
    }
}

impl ManiaPerformanceCalculator<'_> {
    pub fn calculate(self) -> ManiaPerformanceAttributes {
        let mut multiplier = 1.0;

        if self.mods.nf() {
            multiplier *= 0.75;
        }

        if self.mods.ez() {
            multiplier *= 0.6;
        }

        let score_accuracy = self.calculate_custom_accuracy();

        let difficulty_value = self.compute_difficulty_value(score_accuracy);
        let variety_multiplier = variety_multiplier(self.attrs.variety);
        let acc_multiplier = acc_multiplier(score_accuracy, self.attrs.acc_scalar);
        let length_multiplier = length_multiplier(f64::from(self.attrs.n_objects), self.attrs.stars);

        let pp = difficulty_value
            * multiplier
            * variety_multiplier
            * acc_multiplier
            * length_multiplier;

        ManiaPerformanceAttributes {
            difficulty: self.attrs,
            pp,
            pp_difficulty: difficulty_value,
            variety_multiplier,
            acc_multiplier,
            length_multiplier,
        }
    }

    fn compute_difficulty_value(&self, score_accuracy: f64) -> f64 {
        // * The "proportion" of pp based on accuracy
        let proportion = performance_proportion(score_accuracy);

        // * Star rating to pp curve, scaled by the proportion
        9.8 * f64::powf(f64::max(self.attrs.stars - 0.15, 0.05), 2.2) * proportion
    }

    fn calculate_custom_accuracy(&self) -> f64 {
        let ManiaScoreState {
            n320,
            n300,
            n200,
            n100,
            n50,
            misses: _,
        } = &self.state;

        let total_hits = self.state.total_hits();

        if total_hits == 0 {
            return 0.0;
        }

        custom_accuracy(*n320, *n300, *n200, *n100, *n50, total_hits)
    }
}

pub(super) fn custom_accuracy(
    n320: u32,
    n300: u32,
    n200: u32,
    n100: u32,
    n50: u32,
    total_hits: u32,
) -> f64 {
    // Matches the reference implementation's 305-based weighting (perfect hits
    // are weighted with 305 instead of 320).
    let numerator = n320 * 305 + n300 * 300 + n200 * 200 + n100 * 100 + n50 * 50;
    let denominator = total_hits * 305;

    f64::from(numerator) / f64::from(denominator)
}

/// The "proportion" of pp that is awarded based on accuracy, i.e. how much
/// of the star rating is rewarded at the given accuracy.
fn performance_proportion(acc: f64) -> f64 {
    if acc > 0.80 {
        4.5 * (acc - 0.8) / f64::powf(100.0 * (1.0 - acc) + f64::powf(0.9, 20.0), 0.05)
    } else {
        0.0
    }
}

/// Multiplier based on the map's variety, in the range `[0.945, 1.055]`.
fn variety_multiplier(variety: f64) -> f64 {
    const FLOOR: f64 = 0.945;
    const CAP: f64 = 1.055;
    const V0: f64 = 3.25;
    const K: f64 = 3.0;

    FLOOR + (CAP - FLOOR) / (1.0 + (-K * (variety - V0)).exp())
}

/// Multiplier based on the play's accuracy and the map's accuracy scalar.
fn acc_multiplier(acc: f64, acc_scalar: f64) -> f64 {
    let sigmoid_scaler = 0.87 + 0.26 / (1.0 + (-20.0 * (acc_scalar - 1.0)).exp());

    sigmoid_scaler * (2.0 * acc.powi(20) - 1.0) + 2.0 - 2.0 * acc.powi(20)
}

/// Multiplier based on the amount of notes of the map.
fn length_multiplier(total_notes: f64, stars: f64) -> f64 {
    1.1 / (1.0 + (stars / (2.0 * total_notes)).sqrt())
}
