use rosu_map::section::general::GameMode;

use crate::{
    any::difficulty::skills::StrainSkill, model::mode::ConvertError,
    taiko::difficulty::DifficultyValues, Beatmap, Difficulty,
};

/// The result of calculating the strains on a osu!taiko map.
///
/// Suitable to plot the difficulty of a map over time.
#[derive(Clone, Debug, PartialEq)]
pub struct TaikoStrains {
    /// Strain peaks of the color skill.
    pub color: Vec<f64>,
    /// Strain peaks of the rhythm skill.
    pub rhythm: Vec<f64>,
    /// Strain peaks of the stamina skill.
    pub stamina: Vec<f64>,
    /// Strain peaks of the single color stamina skill.
    pub single_color_stamina: Vec<f64>,
}

impl TaikoStrains {
    /// Time between two strains in ms.
    pub const SECTION_LEN: f64 = 400.0;
}

pub fn strains(difficulty: &Difficulty, map: &Beatmap) -> Result<TaikoStrains, ConvertError> {
    let map = map.convert_ref(GameMode::Taiko, difficulty.get_mods())?;

    let great_hit_window = map
        .attributes()
        .difficulty(difficulty)
        .hit_windows()
        .od_great;

    let values = DifficultyValues::calculate(difficulty, &map, great_hit_window);

    Ok(TaikoStrains {
        color: values.skills.color.into_current_strain_peaks(),
        rhythm: values.skills.rhythm.into_current_strain_peaks(),
        stamina: values.skills.stamina.into_current_strain_peaks(),
        single_color_stamina: values
            .skills
            .single_color_stamina
            .into_current_strain_peaks(),
    })
}
