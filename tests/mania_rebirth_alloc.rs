//! Heap-allocation observations for mania's rebirth difficulty calculation.
//!
//! These are *observation* tests: they never fail. All measurements are
//! printed to stderr (use `--nocapture` to see them).
//!
//! Run with:
//!     cargo test --release --test mania_rebirth_alloc -- --nocapture --test-threads=1
//!
//! `--release` is required because `dhat` is meaningfully slow on debug
//! builds. `--test-threads=1` is *required*: `dhat` uses a global allocator
//! and panics if more than one `Profiler` runs concurrently. We also keep
//! these measurements in a separate file from `mania_rebirth_perf.rs` so
//! the `#[global_allocator]` (which adds overhead to every allocation) does
//! not distort the timing measurements over there.
//!
//! `dhat`'s global allocator tracks every heap allocation while the
//! `Profiler` is alive. `HeapStats::get()` reports:
//! - `total_bytes` / `total_blocks`: sum of all allocations made
//! - `max_bytes`   / `max_blocks`  : peak live heap (high water mark)
//! - `curr_bytes`  / `curr_blocks` : live heap at the moment of the query
//!
//! What to look for:
//! - `[COLD]`   : single-shot difficulty. mania should be in the same
//!                ballpark as the other rulesets.
//! - `[GRADUAL]`: total allocations across the whole gradual pass. mania
//!                rebuilds the entire `RebirthData` (notes, notes_by_column,
//!                long_notes, tails, all_corners, base_corners,
//!                awkwardness_corners, key_usage, anchor, ...) on *every*
//!                `next()`, so its total allocated bytes / block count
//!                should be ~O(N^2) while osu / taiko / catch stay ~O(N).
//!
//! NOTE: these numbers include allocations made by Rust's test harness
//! between `Profiler::build()` and the measured work (e.g. `Beatmap`'s own
//! heap during parse, if parsed inside the region). To minimize that, the
//! beatmap is parsed *before* the profiler is created, and only the
//! difficulty/gradual calculation itself is measured. Any leftover
//! allocation that the test harness does concurrently is still counted;
//! because we run `--test-threads=1` and only one test per process run is
//! triggered by the filter, this noise is negligible relative to the
//! tens-of-thousands of allocations we are measuring.

#![cfg(test)]

use rosu_pp::{Beatmap, Difficulty, catch::Catch, mania::Mania, osu::Osu, taiko::Taiko};

#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

const OSU_PATH: &str = "./resources/2785319.osu";
const TAIKO_PATH: &str = "./resources/1028484.osu";
const CATCH_PATH: &str = "./resources/2118524.osu";
const MANIA_PATH: &str = "./resources/1638954.osu";

#[derive(Clone, Copy)]
enum Mode {
    Osu,
    Taiko,
    Catch,
    Mania,
}

impl Mode {
    const fn label(self) -> &'static str {
        match self {
            Mode::Osu => "osu",
            Mode::Taiko => "taiko",
            Mode::Catch => "catch",
            Mode::Mania => "mania",
        }
    }

    const fn path(self) -> &'static str {
        match self {
            Mode::Osu => OSU_PATH,
            Mode::Taiko => TAIKO_PATH,
            Mode::Catch => CATCH_PATH,
            Mode::Mania => MANIA_PATH,
        }
    }
}

// Run one difficulty calculation, measuring the allocations done *inside*
// the closure. The Profiler is alive only for the duration of the work.
fn measure<F: FnOnce()>(work: F) -> dhat::HeapStats {
    let profiler = dhat::Profiler::builder().testing().build();
    work();
    let stats = dhat::HeapStats::get();
    drop(profiler);
    stats
}

fn run_cold(mode: Mode, map: &Beatmap) -> dhat::HeapStats {
    measure(|| {
        let _ = Difficulty::new().calculate_for_mode_dyn(mode, map);
    })
}

fn run_gradual(mode: Mode, map: &Beatmap) -> dhat::HeapStats {
    measure(|| {
        let mut iter = Difficulty::new().gradual_difficulty_for_mode_dyn(mode, map);
        while iter.next_dyn().is_some() {}
    })
}

// `IGameMode` is `pub(crate)`, so we dispatch through wrappers, mirroring
// the layout in `tests/mania_rebirth_perf.rs`.
trait DynDifficulty {
    fn calculate_for_mode_dyn(self, mode: Mode, map: &Beatmap) -> f64;
    fn gradual_difficulty_for_mode_dyn(self, mode: Mode, map: &Beatmap) -> DynIter;
}

impl DynDifficulty for Difficulty {
    fn calculate_for_mode_dyn(self, mode: Mode, map: &Beatmap) -> f64 {
        match mode {
            Mode::Osu => self.calculate_for_mode::<Osu>(map).unwrap().stars,
            Mode::Taiko => self.calculate_for_mode::<Taiko>(map).unwrap().stars,
            Mode::Catch => self.calculate_for_mode::<Catch>(map).unwrap().stars,
            Mode::Mania => self.calculate_for_mode::<Mania>(map).unwrap().stars,
        }
    }

    fn gradual_difficulty_for_mode_dyn(self, mode: Mode, map: &Beatmap) -> DynIter {
        match mode {
            Mode::Osu => DynIter::Osu(self.gradual_difficulty_for_mode::<Osu>(map).unwrap()),
            Mode::Taiko => DynIter::Taiko(self.gradual_difficulty_for_mode::<Taiko>(map).unwrap()),
            Mode::Catch => DynIter::Catch(self.gradual_difficulty_for_mode::<Catch>(map).unwrap()),
            Mode::Mania => DynIter::Mania(
                self.gradual_difficulty_for_mode::<Mania>(map).unwrap(),
            ),
        }
    }
}

enum DynIter {
    Osu(rosu_pp::osu::OsuGradualDifficulty),
    Taiko(rosu_pp::taiko::TaikoGradualDifficulty),
    Catch(rosu_pp::catch::CatchGradualDifficulty),
    Mania(rosu_pp::mania::ManiaGradualDifficulty),
}

impl DynIter {
    fn next_dyn(&mut self) -> Option<()> {
        match self {
            DynIter::Osu(it) => it.next().map(|_| ()),
            DynIter::Taiko(it) => it.next().map(|_| ()),
            DynIter::Catch(it) => it.next().map(|_| ()),
            DynIter::Mania(it) => it.next().map(|_| ()),
        }
    }
}

fn fmt_bytes(n: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = 1024 * KB;
    if n >= MB {
        format!("{:.2} MB", n as f64 / MB as f64)
    } else if n >= KB {
        format!("{:.2} KB", n as f64 / KB as f64)
    } else {
        format!("{n} B")
    }
}

fn fmt_blocks(n: u64) -> String {
    // Group with underscores for readability (e.g. 1_234_567).
    let s = n.to_string();
    let bytes = s.as_bytes();
    let mut out = Vec::with_capacity(bytes.len() + bytes.len() / 3);
    for (i, &b) in bytes.iter().enumerate() {
        if i > 0 && (bytes.len() - i) % 3 == 0 {
            out.push(b'_');
        }
        out.push(b);
    }
    // Safety: ASCII only.
    String::from_utf8(out).unwrap()
}

#[test]
fn alloc_cold_difficulty() {
    eprintln!("\n=== [COLD-ALLOC] single-shot difficulty ===");

    for mode in [Mode::Osu, Mode::Taiko, Mode::Catch, Mode::Mania] {
        // Parse the map OUTSIDE the profiler region so its allocations
        // are not counted; we only want the difficulty work measured.
        let map = Beatmap::from_path(mode.path()).unwrap();

        let stats = run_cold(mode, &map);

        eprintln!(
            "[COLD-ALLOC] {:<6}: total={} in {} blocks | peak={} in {} blocks | end={} in {} blocks",
            mode.label(),
            fmt_bytes(stats.total_bytes),
            fmt_blocks(stats.total_blocks),
            fmt_bytes(stats.max_bytes as u64),
            fmt_blocks(stats.max_blocks as u64),
            fmt_bytes(stats.curr_bytes as u64),
            fmt_blocks(stats.curr_blocks as u64),
        );
    }
}

#[test]
fn alloc_gradual_full() {
    eprintln!("\n=== [GRADUAL-ALLOC] iterate gradual_difficulty to completion ===");

    for mode in [Mode::Osu, Mode::Taiko, Mode::Catch, Mode::Mania] {
        let map = Beatmap::from_path(mode.path()).unwrap();

        let stats = run_gradual(mode, &map);

        eprintln!(
            "[GRADUAL-ALLOC] {:<6}: total={} in {} blocks | peak={} in {} blocks | end={} in {} blocks",
            mode.label(),
            fmt_bytes(stats.total_bytes),
            fmt_blocks(stats.total_blocks),
            fmt_bytes(stats.max_bytes as u64),
            fmt_blocks(stats.max_blocks as u64),
            fmt_bytes(stats.curr_bytes as u64),
            fmt_blocks(stats.curr_blocks as u64),
        );
    }
}
