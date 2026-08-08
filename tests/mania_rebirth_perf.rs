//! Performance observations for mania's rebirth-based *pp* calculation.
//!
//! These are *observation* tests: they never fail. All measurements are
//! printed to stderr (use `--nocapture` to see them) so one can compare
//! cold-start, repeated calculation on the same map, and the per-step cost
//! of `gradual_performance_for_mode` across all four rulesets.
//!
//! Run with:
//!     cargo test --release --test mania_rebirth_perf -- --nocapture --test-threads=1
//!
//! `--release` is required to measure realistic performance; under the
//! default test profile (debug) the numbers are dominated by lack of
//! optimization and are not representative.
//!
//! What to look for:
//! - `[COLD]`   : single-shot performance (pp) cost. mania should be in the
//!                same order of magnitude as the other rulesets.
//! - `[REUSED]` : repeated pp calculation on the same map. All rulesets
//!                should be roughly flat; no accumulation.
//! - `[GRADUAL]`: per-step cost of the gradual *performance* iterator.
//!                mania's underlying `ManiaGradualDifficulty::next` recomputes
//!                the entire rebirth pipeline (`calculate_stars_for_objects`
//!                -> `prepare_data` -> rebuild `RebirthData`) on every step,
//!                so its per-step time grows roughly linearly with the index
//!                and total cost is ~O(N^2 log N). osu / taiko / catch use
//!                incremental strain processing and stay flat.

use std::time::Instant;

use rosu_pp::{
    Beatmap, Difficulty,
    catch::{CatchGradualPerformance, CatchPerformance, CatchScoreState},
    mania::{ManiaGradualPerformance, ManiaPerformance, ManiaScoreState},
    osu::{OsuGradualPerformance, OsuPerformance, OsuScoreState},
    taiko::{TaikoGradualPerformance, TaikoPerformance, TaikoScoreState},
};

const OSU_PATH: &str = "./resources/2785319.osu";
const TAIKO_PATH: &str = "./resources/1028484.osu";
const CATCH_PATH: &str = "./resources/2118524.osu";
const MANIA_PATH: &str = "./resources/1638954.osu";

const REUSED_REPS: usize = 20;

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

// --- Non-gradual: full pp calculation ---------------------------------------

fn cold_pp(mode: Mode, map: &Beatmap) -> f64 {
    match mode {
        Mode::Osu => OsuPerformance::from(map).calculate().unwrap().pp(),
        Mode::Taiko => TaikoPerformance::from(map).calculate().unwrap().pp(),
        Mode::Catch => CatchPerformance::from(map).calculate().unwrap().pp(),
        Mode::Mania => ManiaPerformance::from(map).calculate().unwrap().pp(),
    }
}

// --- Gradual: iterate gradual_performance to completion ---------------------

enum DynGradualPerf {
    Osu(OsuGradualPerformance),
    Taiko(TaikoGradualPerformance),
    Catch(CatchGradualPerformance),
    Mania(ManiaGradualPerformance),
}

enum DynScoreState {
    Osu(OsuScoreState),
    Taiko(TaikoScoreState),
    Catch(CatchScoreState),
    Mania(ManiaScoreState),
}

impl DynScoreState {
    /// Advance the state by one object, used to feed the gradual iterator
    /// a plausible per-object score. We keep it intentionally simple: just
    /// increment the "best" hitresult for each mode so the state is non-empty
    /// and the performance calculator has something to chew on.
    fn advance(&mut self, mode: Mode) {
        match (self, mode) {
            (DynScoreState::Osu(s), Mode::Osu) => {
                s.max_combo += 1;
                s.hitresults.n300 += 1;
            }
            (DynScoreState::Taiko(s), Mode::Taiko) => {
                s.max_combo += 1;
                s.hitresults.n300 += 1;
            }
            (DynScoreState::Catch(s), Mode::Catch) => {
                s.max_combo += 1;
                s.hitresults.fruits += 1;
            }
            (DynScoreState::Mania(s), Mode::Mania) => s.n320 += 1,
            _ => unreachable!(),
        }
    }
}

fn new_gradual_perf(mode: Mode, map: &Beatmap) -> DynGradualPerf {
    let difficulty = Difficulty::new();
    match mode {
        Mode::Osu => DynGradualPerf::Osu(
            difficulty
                .gradual_performance_for_mode::<rosu_pp::osu::Osu>(map)
                .unwrap(),
        ),
        Mode::Taiko => DynGradualPerf::Taiko(
            difficulty
                .gradual_performance_for_mode::<rosu_pp::taiko::Taiko>(map)
                .unwrap(),
        ),
        Mode::Catch => DynGradualPerf::Catch(
            difficulty
                .gradual_performance_for_mode::<rosu_pp::catch::Catch>(map)
                .unwrap(),
        ),
        Mode::Mania => DynGradualPerf::Mania(
            difficulty
                .gradual_performance_for_mode::<rosu_pp::mania::Mania>(map)
                .unwrap(),
        ),
    }
}

fn new_score_state(mode: Mode) -> DynScoreState {
    match mode {
        Mode::Osu => DynScoreState::Osu(OsuScoreState::default()),
        Mode::Taiko => DynScoreState::Taiko(TaikoScoreState::default()),
        Mode::Catch => DynScoreState::Catch(CatchScoreState::default()),
        Mode::Mania => DynScoreState::Mania(ManiaScoreState::default()),
    }
}

fn gradual_pp_steps(mode: Mode, map: &Beatmap) -> Vec<u128> {
    let mut iter = new_gradual_perf(mode, map);
    let mut state = new_score_state(mode);
    let mut per_step = Vec::new();

    loop {
        // Advance the score state before each call so each step's pp is
        // computed over a growing, non-trivial score.
        state.advance(mode);

        let start = Instant::now();
        let next = match (&mut iter, &mut state) {
            (DynGradualPerf::Osu(it), DynScoreState::Osu(s)) => it.next(s.clone()).map(|a| a.pp()),
            (DynGradualPerf::Taiko(it), DynScoreState::Taiko(s)) => {
                it.next(s.clone()).map(|a| a.pp())
            }
            (DynGradualPerf::Catch(it), DynScoreState::Catch(s)) => {
                it.next(s.clone()).map(|a| a.pp())
            }
            (DynGradualPerf::Mania(it), DynScoreState::Mania(s)) => {
                it.next(s.clone()).map(|a| a.pp())
            }
            _ => unreachable!(),
        };
        let elapsed = start.elapsed().as_micros();

        match next {
            Some(_) => per_step.push(elapsed),
            None => break,
        }
    }

    per_step
}

// --- Stats ------------------------------------------------------------------

struct Stats {
    count: usize,
    total_us: u128,
    mean_us: f64,
    min_us: u128,
    max_us: u128,
    p99_us: u128,
}

impl Stats {
    fn from_micros(samples: &[u128]) -> Self {
        if samples.is_empty() {
            return Self {
                count: 0,
                total_us: 0,
                mean_us: 0.0,
                min_us: 0,
                max_us: 0,
                p99_us: 0,
            };
        }

        let total_us: u128 = samples.iter().sum();
        let mean_us = total_us as f64 / samples.len() as f64;

        let mut sorted: Vec<u128> = samples.to_vec();
        sorted.sort_unstable();

        let min_us = *sorted.first().unwrap();
        let max_us = *sorted.last().unwrap();

        let p99_idx = ((samples.len() as f64) * 0.99).floor() as usize;
        let p99_idx = p99_idx.min(samples.len() - 1);
        let p99_us = sorted[p99_idx];

        Self {
            count: samples.len(),
            total_us,
            mean_us,
            min_us,
            max_us,
            p99_us,
        }
    }
}

fn fmt_us(us: u128) -> String {
    if us >= 1000 {
        format!("{:.2} ms", us as f64 / 1000.0)
    } else {
        format!("{us} µs")
    }
}

fn fmt_us_f(us: f64) -> String {
    if us >= 1000.0 {
        format!("{:.2} ms", us / 1000.0)
    } else {
        format!("{:.1} µs", us)
    }
}

#[test]
fn cold_start_pp() {
    eprintln!("\n=== [COLD] single-shot performance (no warmup) ===");

    for mode in [Mode::Osu, Mode::Taiko, Mode::Catch, Mode::Mania] {
        let map = Beatmap::from_path(mode.path()).unwrap();

        let start = Instant::now();
        let pp = cold_pp(mode, &map);
        let elapsed = start.elapsed().as_micros();

        eprintln!(
            "[COLD] {:<6}: {:>9}  (pp={:.4})",
            mode.label(),
            fmt_us(elapsed),
            pp,
        );
    }
}

#[test]
fn reused_pp_repeated() {
    eprintln!(
        "\n=== [REUSED] performance x{REUSED_REPS} on the same map (after 1 warmup) ==="
    );

    for mode in [Mode::Osu, Mode::Taiko, Mode::Catch, Mode::Mania] {
        let map = Beatmap::from_path(mode.path()).unwrap();

        // Warmup: one unmeasured run to avoid first-call noise.
        let _ = cold_pp(mode, &map);

        let mut samples = Vec::with_capacity(REUSED_REPS);
        for _ in 0..REUSED_REPS {
            let start = Instant::now();
            let _ = cold_pp(mode, &map);
            samples.push(start.elapsed().as_micros());
        }

        let stats = Stats::from_micros(&samples);
        eprintln!(
            "[REUSED] {:<6}: mean={} min={} max={} p99={} total={}",
            mode.label(),
            fmt_us_f(stats.mean_us),
            fmt_us(stats.min_us),
            fmt_us(stats.max_us),
            fmt_us(stats.p99_us),
            fmt_us(stats.total_us),
        );
    }
}

#[test]
fn gradual_full_pass() {
    eprintln!("\n=== [GRADUAL] iterate gradual_performance to completion ===");

    for mode in [Mode::Osu, Mode::Taiko, Mode::Catch, Mode::Mania] {
        let map = Beatmap::from_path(mode.path()).unwrap();

        let per_step = gradual_pp_steps(mode, &map);
        let stats = Stats::from_micros(&per_step);

        let first = per_step.first().copied().unwrap_or(0);
        let last = per_step.last().copied().unwrap_or(0);

        eprintln!(
            "[GRADUAL] {:<6}: n={:<5} total={:<9} mean={:<9} min={:<7} max={:<9} p99={:<9} first={:<7} last={}",
            mode.label(),
            stats.count,
            fmt_us(stats.total_us),
            fmt_us_f(stats.mean_us),
            fmt_us(stats.min_us),
            fmt_us(stats.max_us),
            fmt_us(stats.p99_us),
            fmt_us(first),
            fmt_us(last),
        );
    }
}
