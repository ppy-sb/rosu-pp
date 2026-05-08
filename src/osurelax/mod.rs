use rosu_map::util::Pos;

use crate::{
    any::CalculateError,
    model::{
        beatmap::Beatmap,
        mode::{ConvertError, IGameMode},
    },
    Difficulty,
};

pub use self::{
    attributes::{OsuRelaxDifficultyAttributes, OsuRelaxPerformanceAttributes},
    difficulty::gradual::OsuRelaxGradualDifficulty,
    performance::{gradual::OsuRelaxGradualPerformance, OsuRelaxPerformance},
    score_state::{OsuRelaxScoreOrigin, OsuRelaxScoreState},
    strains::OsuRelaxStrains,
};

mod attributes;
mod convert;
pub(crate) mod difficulty;
mod object;
pub(crate) mod performance;
mod score_state;
mod strains;

const PLAYFIELD_BASE_SIZE: Pos = Pos::new(512.0, 384.0);

/// Marker type for osu!standard with Relax mod.
pub struct OsuRelax;

impl IGameMode for OsuRelax {
    type DifficultyAttributes = OsuRelaxDifficultyAttributes;
    type Strains = OsuRelaxStrains;
    type Performance<'map> = OsuRelaxPerformance<'map>;
    type HitResults = OsuRelaxScoreState;
    type GradualDifficulty = OsuRelaxGradualDifficulty;
    type GradualPerformance = OsuRelaxGradualPerformance;

    fn difficulty(
        difficulty: &Difficulty,
        map: &Beatmap,
    ) -> Result<Self::DifficultyAttributes, ConvertError> {
        difficulty::difficulty(difficulty, map)
    }

    fn checked_difficulty(
        difficulty: &Difficulty,
        map: &Beatmap,
    ) -> Result<Self::DifficultyAttributes, CalculateError> {
        difficulty::checked_difficulty(difficulty, map)
    }

    fn strains(difficulty: &Difficulty, map: &Beatmap) -> Result<Self::Strains, ConvertError> {
        strains::strains(difficulty, map)
    }

    fn performance(map: &Beatmap) -> Self::Performance<'_> {
        OsuRelaxPerformance::new(map)
    }

    fn gradual_difficulty(
        difficulty: Difficulty,
        map: &Beatmap,
    ) -> Result<Self::GradualDifficulty, ConvertError> {
        OsuRelaxGradualDifficulty::new(difficulty, map)
    }

    fn gradual_performance(
        difficulty: Difficulty,
        map: &Beatmap,
    ) -> Result<Self::GradualPerformance, ConvertError> {
        OsuRelaxGradualPerformance::new(difficulty, map)
    }
}
