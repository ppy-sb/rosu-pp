//! Full judgement window set for osu!mania.
//!
//! [`crate::mania::sunny`] only ever computed the GREAT window, which is also all that
//! rosu-pp exposes for mania (`od_ok` and `od_meh` are always `None`). Modelling
//! how hard a given judgement is to hit needs every window, so they are built
//! here.
//!
//! # Sources
//!
//! Values follow osu!lazer's `ManiaHitWindows`. Three separate schemes exist:
//!
//! - **lazer** (`ScoreV2`, or lazer without `CL`): every window interpolates
//!   over OD via [`difficulty_range`].
//! - **classic, non-convert**: windows are `base + 3 * (10 - od)`, with a flat
//!   PERFECT window.
//! - **classic, convert**: a single threshold at `round(od) > 4`.
//!
//! # Mods
//!
//! `HR` and `EZ` in mania do *not* scale OD, unlike the other rulesets. Both
//! stamp a multiplier straight onto the windows: `HR` uses
//! `HIT_WINDOW_DIFFICULTY_MULTIPLIER = 1.4` and `EZ` uses `1 / 1.4`. Note that
//! this applies uniformly to *every* window including PERFECT, which is why
//! `EZ` grants far more leniency than its nominal "OD is halved" description
//! suggests.
//!
//! Rate-changing mods (`DT`/`HT`) are normalized away: the window is scaled by
//! the clock rate, floored to whole milliseconds, then divided back out. The
//! floor is not incidental — osu!'s input granularity is 1ms, so sub-ms
//! differences genuinely do not exist in gameplay. This is why `DT` barely
//! moves mania windows at all.

// Nothing consumes this module yet; it is the prerequisite for making the PP
// stage window-aware. Remove once the accuracy model is wired up.

use crate::model::beatmap::Beatmap;
use rosu_mods::{Acronym, GameMods};

/// A judgement in osu!mania, ordered from most to least precise.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ManiaJudgement {
    /// The rainbow 300 / MAX / 320.
    Perfect,
    /// The 300.
    Great,
    /// The 200.
    Good,
    /// The 100.
    Ok,
    /// The 50.
    Meh,
    /// Anything past the MEH window.
    Miss,
}

impl ManiaJudgement {
    /// All judgements, most precise first.
    pub const ALL: [Self; 6] = [
        Self::Perfect,
        Self::Great,
        Self::Good,
        Self::Ok,
        Self::Meh,
        Self::Miss,
    ];
}

/// The half-width of every judgement window, in milliseconds.
///
/// A hit counts as judgement `j` when its absolute hit error is at most
/// `window(j)`. Windows are nested, so `perfect <= great <= ... <= miss`.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct ManiaHitWindows {
    pub perfect: f64,
    pub great: f64,
    pub good: f64,
    pub ok: f64,
    pub meh: f64,
    pub miss: f64,
}

impl ManiaHitWindows {
    /// The window for a single judgement.
    pub fn get(&self, judgement: ManiaJudgement) -> f64 {
        match judgement {
            ManiaJudgement::Perfect => self.perfect,
            ManiaJudgement::Great => self.great,
            ManiaJudgement::Good => self.good,
            ManiaJudgement::Ok => self.ok,
            ManiaJudgement::Meh => self.meh,
            ManiaJudgement::Miss => self.miss,
        }
    }

    /// The wider of two window sets, taken judgement by judgement.
    ///
    /// Used to build a one-sided pricing reference: comparing a score against the wider
    /// of "a fixed reference OD" and "this map's own windows" means a map stricter than
    /// the reference is still rewarded for its strictness, while a more lenient map is
    /// simply priced against itself rather than charged for its leniency.
    ///
    /// Per judgement rather than picking one set wholesale, because the two schemes do not
    /// order uniformly: in the classic scheme PERFECT is a flat 16 ms at every OD while
    /// every other window scales, so a map can be stricter than the reference on GREAT and
    /// identical on PERFECT.
    pub fn widest_of(&self, other: &Self) -> Self {
        Self {
            perfect: self.perfect.max(other.perfect),
            great: self.great.max(other.great),
            good: self.good.max(other.good),
            ok: self.ok.max(other.ok),
            meh: self.meh.max(other.meh),
            miss: self.miss.max(other.miss),
        }
    }

    /// The judgement a hit error of `error` ms would receive.
    ///
    /// `error` is treated as an absolute value.
    pub fn judge(&self, error: f64) -> ManiaJudgement {
        let error = error.abs();

        ManiaJudgement::ALL
            .into_iter()
            .find(|&judgement| error <= self.get(judgement))
            .unwrap_or(ManiaJudgement::Miss)
    }

    /// The exclusive band a judgement occupies, i.e. the range of hit errors
    /// that produce it. `Miss` is unbounded above, reported as [`f64::INFINITY`].
    pub fn band(&self, judgement: ManiaJudgement) -> (f64, f64) {
        let upper = match judgement {
            ManiaJudgement::Miss => f64::INFINITY,
            _ => self.get(judgement),
        };

        let lower = match judgement {
            ManiaJudgement::Perfect => 0.0,
            ManiaJudgement::Great => self.perfect,
            ManiaJudgement::Good => self.great,
            ManiaJudgement::Ok => self.good,
            ManiaJudgement::Meh => self.ok,
            ManiaJudgement::Miss => self.meh,
        };

        (lower, upper)
    }
}

// ---------------------------------------------------------------------------
// lazer window ranges
// ---------------------------------------------------------------------------

/// A window range as lazer's `DifficultyRange(od0, od5, od10)`.
struct Range {
    od0: f64,
    od5: f64,
    od10: f64,
}

const LAZER_PERFECT: Range = Range {
    od0: 22.4,
    od5: 19.4,
    od10: 13.9,
};
const LAZER_GREAT: Range = Range {
    od0: 64.0,
    od5: 49.0,
    od10: 34.0,
};
const LAZER_GOOD: Range = Range {
    od0: 97.0,
    od5: 82.0,
    od10: 67.0,
};
const LAZER_OK: Range = Range {
    od0: 127.0,
    od5: 112.0,
    od10: 97.0,
};
const LAZER_MEH: Range = Range {
    od0: 151.0,
    od5: 136.0,
    od10: 121.0,
};
const LAZER_MISS: Range = Range {
    od0: 188.0,
    od5: 173.0,
    od10: 158.0,
};

/// Linear interpolation over OD, matching lazer's
/// `IBeatmapDifficultyInfo.DifficultyRange`: OD 0-5 interpolates between `od0`
/// and `od5`, OD 5-10 between `od5` and `od10`.
fn difficulty_range(od: f64, range: &Range) -> f64 {
    if od > 5.0 {
        range.od5 + (range.od10 - range.od5) * (od - 5.0) / 5.0
    } else {
        range.od0 + (range.od5 - range.od0) * od / 5.0
    }
}

// ---------------------------------------------------------------------------
// Construction
// ---------------------------------------------------------------------------

/// Build the full window set for a (converted) mania beatmap.
///
/// `classic` selects the osu!stable judgement scheme and should come from
/// [`crate::mania::sunny::is_classic`], so that the windows agree with the difficulty
/// calculation's notion of which scoring mode is in effect.
pub fn hit_windows(
    map: &Beatmap,
    mods: &GameMods,
    clock_rate: f64,
    classic: bool,
) -> ManiaHitWindows {
    let multiplier = difficulty_multiplier(mods);
    effective_windows_with_multiplier(
        f64::from(map.od),
        map.is_convert,
        multiplier,
        clock_rate,
        classic,
    )
}

/// Build effective Mania hit windows from difficulty state without requiring a beatmap.
pub fn effective_windows(
    od: f64,
    is_convert: bool,
    mods: &crate::GameMods,
    clock_rate: f64,
    classic: bool,
) -> ManiaHitWindows {
    let multiplier = difficulty_multiplier_wrapped(mods);
    effective_windows_with_multiplier(od, is_convert, multiplier, clock_rate, classic)
}

fn difficulty_multiplier_wrapped(mods: &crate::GameMods) -> f64 {
    match mods {
        crate::GameMods::Lazer(mods) => difficulty_multiplier(&mods.clone().into()),
        crate::GameMods::Intermode(mods) => difficulty_multiplier(
            &rosu_mods::GameMods::from_intermode(mods, rosu_mods::GameMode::Mania),
        ),
        crate::GameMods::Legacy(mods) => difficulty_multiplier(
            &rosu_mods::GameModsIntermode::from(mods.clone())
                .with_mode(rosu_mods::GameMode::Mania)
                .into(),
        ),
    }
}

fn effective_windows_with_multiplier(
    od: f64,
    is_convert: bool,
    multiplier: f64,
    clock_rate: f64,
    classic: bool,
) -> ManiaHitWindows {

    let raw = if classic {
        classic_windows(od, is_convert)
    } else {
        lazer_windows(od)
    };

    finalize(raw, multiplier, clock_rate)
}

/// The lazer scheme: every window interpolates over OD.
fn lazer_windows(od: f64) -> ManiaHitWindows {
    ManiaHitWindows {
        perfect: difficulty_range(od, &LAZER_PERFECT),
        great: difficulty_range(od, &LAZER_GREAT),
        good: difficulty_range(od, &LAZER_GOOD),
        ok: difficulty_range(od, &LAZER_OK),
        meh: difficulty_range(od, &LAZER_MEH),
        miss: difficulty_range(od, &LAZER_MISS),
    }
}

/// The osu!stable scheme.
///
/// Non-converts scale off the inverted OD; converts use a single threshold at
/// `round(od) > 4`. PERFECT is a flat 16ms in both cases and does *not* scale
/// with OD at all — the detail that makes `EZ`'s uniform widening so valuable,
/// since it is the only way the PERFECT window ever moves.
fn classic_windows(od: f64, is_convert: bool) -> ManiaHitWindows {
    if is_convert {
        if od.round_ties_even() > 4.0 {
            ManiaHitWindows {
                perfect: 16.0,
                great: 34.0,
                good: 67.0,
                ok: 97.0,
                meh: 121.0,
                miss: 158.0,
            }
        } else {
            ManiaHitWindows {
                perfect: 16.0,
                great: 47.0,
                good: 77.0,
                ok: 97.0,
                meh: 121.0,
                miss: 158.0,
            }
        }
    } else {
        let anti_od = (10.0 - od).clamp(0.0, 10.0);

        ManiaHitWindows {
            perfect: 16.0,
            great: 34.0 + 3.0 * anti_od,
            good: 67.0 + 3.0 * anti_od,
            ok: 97.0 + 3.0 * anti_od,
            meh: 121.0 + 3.0 * anti_od,
            miss: 158.0 + 3.0 * anti_od,
        }
    }
}

/// Reconstruct a window set from a GREAT window alone.
///
/// Needed on the round-trip path: attributes handed back from JS carry
/// `greatHitWindow` but not the full set, and the performance stage needs the set
/// to price mods. The classic non-convert scheme offsets every window from GREAT by
/// a fixed amount, so inverting it is exact for that scheme — and the offsets hold
/// under `EZ`/`HR` only approximately, since those scale rather than shift.
///
/// PERFECT is the one that cannot be recovered: it is a flat 16.5 regardless of OD,
/// so a widened GREAT under `EZ` leaves no trace in it. It is scaled by the same
/// ratio as GREAT here, which is right under `EZ`/`HR` and a no-op otherwise.
///
/// Prefer passing the real window set. This exists so a cached-attributes call
/// does not silently price mods differently from a from-beatmap one.
pub fn windows_from_great(great: f64) -> ManiaHitWindows {
    const REFERENCE_GREAT: f64 = 40.5;

    if great <= 0.0 {
        return ManiaHitWindows::default();
    }

    let ratio = great / REFERENCE_GREAT;

    ManiaHitWindows {
        perfect: 16.5 * ratio,
        great,
        good: great + 36.0,
        ok: great + 66.0,
        meh: great + 87.0,
        miss: great + 124.0,
    }
}

/// The `HR`/`EZ` window multiplier.
///
/// Mania is unusual here: neither mod touches OD. `HR` sets
/// `DifficultyMultiplier = 1.4` and `EZ` sets `1 / 1.4`, applied to every
/// window. Since lazer divides by this multiplier, `HR` narrows and `EZ` widens.
pub(crate) fn difficulty_multiplier(mods: &GameMods) -> f64 {
    if has_mod(mods, "HR") {
        1.4
    } else if has_mod(mods, "EZ") {
        1.0 / 1.4
    } else {
        1.0
    }
}

/// Apply the difficulty multiplier and normalize away the clock rate.
///
/// Mirrors lazer's `Math.Floor(range * totalMultiplier) + 0.5` with
/// `totalMultiplier = speed / difficulty`, then divides the clock rate back out
/// so the result is comparable across rates. The floor models osu!'s 1ms input
/// granularity.
fn finalize(raw: ManiaHitWindows, multiplier: f64, clock_rate: f64) -> ManiaHitWindows {
    let apply = |value: f64| {
        let scaled = value / multiplier * clock_rate;

        (scaled.floor() + 0.5) / clock_rate
    };

    ManiaHitWindows {
        perfect: apply(raw.perfect),
        great: apply(raw.great),
        good: apply(raw.good),
        ok: apply(raw.ok),
        meh: apply(raw.meh),
        miss: apply(raw.miss),
    }
}

/// Whether the mods contain the mod with the given acronym.
fn has_mod(mods: &GameMods, acronym: &str) -> bool {
    acronym
        .parse::<Acronym>()
        .is_ok_and(|acronym| mods.contains_acronym(acronym))
}

#[cfg(test)]
mod tests {
    use crate::model::mode::GameMode;
    use rosu_mods::{GameMod, GameMods as LazerMods};

    use super::*;

    fn map(od: f32, is_convert: bool) -> Beatmap {
        let mut map = Beatmap::default();
        map.mode = GameMode::Mania;
        map.od = od;
        map.is_convert = is_convert;

        map
    }

    fn mods(list: &[GameMod]) -> LazerMods {
        let mut mods = LazerMods::new();

        for gamemod in list {
            mods.insert(gamemod.clone());
        }

        mods
    }

    fn assert_close(actual: f64, expected: f64) {
        assert!(
            (actual - expected).abs() < 1e-9,
            "expected {expected}, got {actual}"
        );
    }

    #[test]
    fn classic_non_convert_od9() {
        let windows = hit_windows(&map(9.0, false), &mods(&[]), 1.0, true);

        // anti_od = 1, so great = 37, good = 70, ok = 100, meh = 124, miss = 161.
        assert_close(windows.perfect, 16.5);
        assert_close(windows.great, 37.5);
        assert_close(windows.good, 70.5);
        assert_close(windows.ok, 100.5);
        assert_close(windows.meh, 124.5);
        assert_close(windows.miss, 161.5);
    }

    #[test]
    fn classic_perfect_is_od_independent() {
        // 戌井's observation: stable's PERFECT window is flat regardless of OD.
        for od in [0.0, 4.0, 7.5, 10.0] {
            let windows = hit_windows(&map(od, false), &mods(&[]), 1.0, true);
            assert_close(windows.perfect, 16.5);
        }
    }

    #[test]
    fn classic_od_extremes_match_lazer_columns() {
        // At OD 0 and OD 10 the classic non-convert formula should coincide with
        // the lazer table's outer columns for every window except PERFECT.
        let low = hit_windows(&map(0.0, false), &mods(&[]), 1.0, true);
        assert_close(low.great, 64.5);
        assert_close(low.miss, 188.5);

        let high = hit_windows(&map(10.0, false), &mods(&[]), 1.0, true);
        assert_close(high.great, 34.5);
        assert_close(high.miss, 158.5);
    }

    #[test]
    fn hr_and_ez_scale_every_window_including_perfect() {
        let base = hit_windows(&map(9.0, false), &mods(&[]), 1.0, true);

        let hr = hit_windows(
            &map(9.0, false),
            &mods(&[GameMod::HardRockMania(Default::default())]),
            1.0,
            true,
        );

        let ez = hit_windows(
            &map(9.0, false),
            &mods(&[GameMod::EasyMania(Default::default())]),
            1.0,
            true,
        );

        // floor(16 / 1.4) + 0.5 = 11.5, floor(16 * 1.4) + 0.5 = 22.5
        assert_close(hr.perfect, 11.5);
        assert_close(ez.perfect, 22.5);

        // The PERFECT window is the crux of the EZ exploit: it is the only mod
        // that widens it, and it does so by ~36%.
        assert!(ez.perfect > base.perfect * 1.3);
        assert!(hr.perfect < base.perfect);

        // floor(37 / 1.4) + 0.5 = 26.5, floor(37 * 1.4) + 0.5 = 51.5
        assert_close(hr.great, 26.5);
        assert_close(ez.great, 51.5);
    }

    #[test]
    fn ez_widens_miss_window_beyond_equivalent_od() {
        // OD9+EZ reads as roughly OD4 if you back-derive from the GREAT window,
        // but its miss window is far wider than a real OD4 map's. This is the
        // asymmetry a single-window model cannot represent.
        let ez = hit_windows(
            &map(9.0, false),
            &mods(&[GameMod::EasyMania(Default::default())]),
            1.0,
            true,
        );

        let od4 = hit_windows(&map(4.0, false), &mods(&[]), 1.0, true);

        // GREAT windows land close together...
        assert!((ez.great - od4.great).abs() < 3.0);

        // ...but the miss window is over 20ms wider, and PERFECT is 6ms wider.
        assert!(ez.miss > od4.miss + 20.0);
        assert!(ez.perfect > od4.perfect + 5.0);
    }

    #[test]
    fn rate_change_barely_moves_windows() {
        // DT/HT are near no-ops for mania windows once 1ms input granularity is
        // accounted for.
        let normal = hit_windows(&map(9.0, false), &mods(&[]), 1.0, true);
        let dt = hit_windows(&map(9.0, false), &mods(&[]), 1.5, true);

        assert_close(normal.great, 37.5);
        assert_close(dt.great, 37.0); // floor(37 * 1.5) + 0.5 = 56.0, / 1.5
        assert!((normal.great - dt.great).abs() < 1.0);
    }

    #[test]
    fn lazer_windows_interpolate() {
        // OD 5 sits exactly on the middle column.
        let mid = hit_windows(&map(5.0, false), &mods(&[]), 1.0, false);
        assert_close(mid.perfect, 19.5); // floor(19.4) + 0.5
        assert_close(mid.great, 49.5);
        assert_close(mid.miss, 173.5);

        // Unlike classic, PERFECT now responds to OD.
        let high = hit_windows(&map(10.0, false), &mods(&[]), 1.0, false);
        assert_close(high.perfect, 13.5);
        assert!(high.perfect < mid.perfect);
    }

    #[test]
    fn classic_convert_threshold() {
        // round(od) > 4 picks the tighter set.
        let tight = hit_windows(&map(5.0, true), &mods(&[]), 1.0, true);
        assert_close(tight.great, 34.5);
        assert_close(tight.good, 67.5);

        let loose = hit_windows(&map(4.0, true), &mods(&[]), 1.0, true);
        assert_close(loose.great, 47.5);
        assert_close(loose.good, 77.5);

        // Converts keep the flat PERFECT window either way.
        assert_close(tight.perfect, 16.5);
        assert_close(loose.perfect, 16.5);
    }

    /// The JS round-trip drops the full window sets and carries only `greatHitWindow`,
    /// so the cached-attributes path has to rebuild the map's own windows by stripping the
    /// mod multiplier back out. Since that set is now the pricing reference, an inversion
    /// error there does not blur pricing — it reverses it. Dividing by the multiplier
    /// instead of multiplying gave `EZ` a reference *wider* than the windows it was played
    /// on, which prices the mod as a bonus.
    ///
    /// Tolerance is 1 ms because `finalize` floors to whole milliseconds before the 0.5
    /// offset, and that floor is not invertible.
    #[test]
    fn stripping_the_mod_multiplier_recovers_the_maps_own_window() {
        for od in [4.0, 8.0, 9.0] {
            let unmodded = hit_windows(&map(od, false), &mods(&[]), 1.0, true);

            for mod_list in [
                vec![GameMod::EasyMania(Default::default())],
                vec![GameMod::HardRockMania(Default::default())],
                vec![],
            ] {
                let m = mods(&mod_list);
                let played = hit_windows(&map(od, false), &m, 1.0, true);

                let recovered = played.great * difficulty_multiplier(&m);

                assert!(
                    (recovered - unmodded.great).abs() < 1.0,
                    "od {od} with {mod_list:?}: stripping {} from played {} gave {recovered}, \
                     want the map's own {}",
                    difficulty_multiplier(&m),
                    played.great,
                    unmodded.great
                );
            }
        }
    }

    #[test]
    fn windows_are_nested() {
        for &classic in &[true, false] {
            for od in [0.0, 4.0, 9.0, 10.0] {
                for mod_list in [
                    vec![],
                    vec![GameMod::EasyMania(Default::default())],
                    vec![GameMod::HardRockMania(Default::default())],
                ] {
                    let w = hit_windows(&map(od, false), &mods(&mod_list), 1.0, classic);

                    assert!(
                        w.perfect <= w.great
                            && w.great <= w.good
                            && w.good <= w.ok
                            && w.ok <= w.meh
                            && w.meh <= w.miss,
                        "windows not nested at od {od}, classic {classic}: {w:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn judge_maps_errors_to_judgements() {
        let w = hit_windows(&map(9.0, false), &mods(&[]), 1.0, true);

        assert_eq!(w.judge(0.0), ManiaJudgement::Perfect);
        assert_eq!(w.judge(16.5), ManiaJudgement::Perfect);
        assert_eq!(w.judge(16.6), ManiaJudgement::Great);
        assert_eq!(w.judge(37.5), ManiaJudgement::Great);
        assert_eq!(w.judge(70.0), ManiaJudgement::Good);
        assert_eq!(w.judge(100.0), ManiaJudgement::Ok);
        assert_eq!(w.judge(124.0), ManiaJudgement::Meh);
        assert_eq!(w.judge(200.0), ManiaJudgement::Miss);

        // Sign should not matter.
        assert_eq!(w.judge(-16.5), ManiaJudgement::Perfect);
        assert_eq!(w.judge(-200.0), ManiaJudgement::Miss);
    }

    #[test]
    fn bands_are_contiguous() {
        let w = hit_windows(&map(9.0, false), &mods(&[]), 1.0, true);

        let mut prev_upper = 0.0;

        for judgement in ManiaJudgement::ALL {
            let (lower, upper) = w.band(judgement);
            assert_close(lower, prev_upper);
            assert!(upper > lower);
            prev_upper = upper;
        }

        assert_eq!(prev_upper, f64::INFINITY);
    }

    #[test]
    fn great_matches_classic_formula() {
        for od in [0.0, 2.5, 4.0, 5.0, 7.0, 9.0, 10.0] {
            for &is_convert in &[false, true] {
                for mod_list in [
                    vec![],
                    vec![GameMod::HardRockMania(Default::default())],
                    vec![GameMod::EasyMania(Default::default())],
                ] {
                    for clock_rate in [1.0, 1.5, 0.75] {
                        let m = map(od, is_convert);

                        let test_mods = mods(&mod_list);
                        let expected = finalize(
                            classic_windows(f64::from(m.od), m.is_convert),
                            difficulty_multiplier(&test_mods),
                            clock_rate,
                        )
                        .great;

                        let actual = hit_windows(&m, &test_mods, clock_rate, true).great;

                        assert!(
                            (actual - expected).abs() < 1e-9,
                            "od {od}, convert {is_convert}, rate {clock_rate}: \
                             expected {expected}, got {actual}"
                        );
                    }
                }
            }
        }
    }
}
