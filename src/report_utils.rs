//! Shared utilities for report binaries.
//!
//! This module is only compiled when the `reports` feature is enabled.

use crate::GameMods;
use crate::mania::SunnyManiaDifficultyAttributes;
use crate::model::beatmap::Beatmap;
use rayon::prelude::*;
use rosu_mods::{GameMod, GameMods as LazerMods};
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::Arc;

/// Group items by a derived key while retaining all items in each group.
pub fn group_by<I, K>(
    items: impl IntoIterator<Item = I>,
    key: impl Fn(&I) -> K,
) -> HashMap<K, Vec<I>>
where
    K: Eq + std::hash::Hash,
{
    let mut groups = HashMap::new();

    for item in items {
        groups.entry(key(&item)).or_insert_with(Vec::new).push(item);
    }

    groups
}

/// Run independent jobs concurrently and discard jobs that return `None`.
pub fn parallel<I, F, T>(
    jobs: I,
    calculate: F,
) -> rayon::iter::FilterMap<<I as IntoParallelIterator>::Iter, F>
where
    I: IntoParallelIterator,
    I::Item: Send,
    F: Fn(I::Item) -> Option<T> + Sync + Send,
    T: Send,
{
    jobs.into_par_iter().filter_map(calculate)
}

/// Supplies the map/mod-specific work required by [`batch`].
pub trait BatchCalculator<R>: Sync + Send {
    type Map: Send + Sync;
    type MapKey: Clone + Eq + Hash + Send + Sync;
    type ModKey: Clone + Eq + Hash + Send + Sync;
    type Attributes: Send + Sync;

    fn map_key(&self, row: &R) -> Self::MapKey;
    fn mod_key(&self, row: &R) -> Self::ModKey;
    fn load_map(&self, key: &Self::MapKey) -> Option<Self::Map>;
    fn calculate(&self, map: &Self::Map, mods: &Self::ModKey) -> Option<Self::Attributes>;
}

/// Group rows, cache maps and attributes, then process each calculated group.
pub fn batch<R, C, F, O>(
    rows: Vec<R>,
    calculator: C,
    consume: F,
) -> impl rayon::iter::ParallelIterator<Item = O>
where
    R: Clone + Send,
    C: BatchCalculator<R>,
    F: Fn((Arc<C::Map>, C::ModKey, Arc<C::Attributes>, Vec<R>)) -> Option<O> + Sync + Send,
    O: Send,
{
    let map_groups = group_by(rows.clone(), |row| calculator.map_key(row));
    let maps: HashMap<_, _> = parallel(map_groups, |(key, _rows)| {
        calculator.load_map(&key).map(|map| (key, Arc::new(map)))
    })
    .collect();

    let job_groups = group_by(rows, |row| {
        (calculator.map_key(row), calculator.mod_key(row))
    });
    let calculated = parallel(job_groups, move |((map_key, mods), rows)| {
        let map = maps.get(&map_key)?;
        let attrs = calculator.calculate(map, &mods)?;

        Some((Arc::clone(map), mods, Arc::new(attrs), rows))
    });

    parallel(calculated, consume)
}

/// Translate the compact mod names used by report TSV files.
pub fn mods_for(names: &str) -> (GameMods, f64) {
    let mut mods = LazerMods::new();
    if names.contains("V2") {
        mods.insert(GameMod::ScoreV2Mania(Default::default()));
    }
    if names.contains("EZ") {
        mods.insert(GameMod::EasyMania(Default::default()));
    }
    if names.contains("HR") {
        mods.insert(GameMod::HardRockMania(Default::default()));
    }
    if names.contains("NF") {
        mods.insert(GameMod::NoFailMania(Default::default()));
    }
    let clock_rate = if names.contains("DT") || names.contains("NC") {
        1.5
    } else if names.contains("HT") {
        0.75
    } else {
        1.0
    };
    (GameMods::from(mods), clock_rate)
}

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
