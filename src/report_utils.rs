//! Shared utilities for report binaries.
//!
//! This module is only compiled when the `reports` feature is enabled.

use crate::model::beatmap::Beatmap;
use crate::mania::SunnyManiaDifficultyAttributes;
use crate::GameMods;
use rosu_mods::GameMod;

/// Parse a beatmap from a file path.
pub fn parse(path: &str) -> Option<Beatmap> {
    let bytes = std::fs::read(path).ok()?;
    Beatmap::from_bytes(&bytes).ok()
}

/// Create a wrapped GameMods with a single mod.
///
/// Example:
/// ```ignore
/// let hr_mods = single_mod(GameMod::HardRockMania(Default::default()));
/// ```
pub fn single_mod(gamemod: GameMod) -> GameMods {
    let mut mods = rosu_mods::GameMods::new();
    mods.insert(gamemod);
    GameMods::from(mods)
}

/// Calculate sunny difficulty attributes for a map.
///
/// Accepts the wrapped `rosu_pp::GameMods` and passes it directly to sunny::calculate.
pub fn calculate(
    map: &Beatmap,
    mods: &GameMods,
    clock_rate: f64,
    lazer: Option<bool>,
    passed_objects: Option<u32>,
) -> Option<SunnyManiaDifficultyAttributes> {
    crate::mania::sunny::calculate(map, mods, clock_rate, lazer, passed_objects)
}
