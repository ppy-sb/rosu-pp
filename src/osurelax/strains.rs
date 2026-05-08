use rosu_map::section::general::GameMode;

use crate::{any::difficulty::skills::StrainSkill, model::mode::ConvertError, Beatmap, Difficulty};

use super::difficulty::{skills::OsuRelaxSkills, DifficultyValues};

/// The result of calculating the strains on a osu! map.
///
/// Suitable to plot the difficulty of a map over time.
#[derive(Clone, Debug, PartialEq)]
pub struct OsuRelaxStrains {
    /// Strain peaks of the aim skill.
    pub aim: Vec<f64>,
    /// Strain peaks of the relax skill.
    pub relax: Vec<f64>,
    /// Strain peaks of the aim skill without sliders.
    pub aim_no_sliders: Vec<f64>,
    /// Strain peaks of the speed skill.
    pub speed: Vec<f64>,
    /// Strain peaks of the flashlight skill.
    pub flashlight: Vec<f64>,
}

impl OsuRelaxStrains {
    /// Time between two strains in ms.
    pub const SECTION_LEN: f64 = 400.0;
}

pub fn strains(difficulty: &Difficulty, map: &Beatmap) -> Result<OsuRelaxStrains, ConvertError> {
    let map = map.convert_ref(GameMode::Osu, difficulty.get_mods())?;

    let DifficultyValues {
        skills:
            OsuRelaxSkills {
                aim,
                relax,
                aim_no_sliders,
                speed,
                flashlight,
            },
        attrs: _,
    } = DifficultyValues::calculate(difficulty, &map);

    Ok(OsuRelaxStrains {
        aim: aim.into_current_strain_peaks(),
        relax: relax.into_current_strain_peaks(),
        aim_no_sliders: aim_no_sliders.into_current_strain_peaks(),
        speed: speed.into_current_strain_peaks(),
        flashlight: flashlight.into_current_strain_peaks(),
    })
}
