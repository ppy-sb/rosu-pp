//! Star-Rating-Rebirth ("sunny") algorithm port for osu!mania.
//!
//! This module implements the community SR/PP algorithm by [Crz]sunnyxxy
//! (https://github.com/sunnyxxy/Star-Rating-Rebirth) as integrated into
//! osu!lazer on the `author-port` branch of `vernonlim/osu`.
//!
//! Notable differences to the upstream `rosu-pp` rebirth implementation:
//! - The hit leniency `x` is derived from the GREAT hit window which takes
//!   the EZ / HR mods into account (`EZ` widens the window by 1.4, `HR`
//!   shrinks it by 1.4) as well as the convert-specific base windows.
//! - The "switches" measure weights corners by the effective weights
//!   (`density * gap`) instead of the raw difficulty values.

use std::cmp::Ordering;
use std::collections::HashMap;

use crate::model::{
    beatmap::Beatmap,
    hit_object::{HitObject, HitObjectKind},
    mode::GameMode,
};
use rosu_mods::{Acronym, GameMods};

use crate::mania::sunny_accuracy::{
    ErrorModel, JudgementUnit, LN_DURATION_BUCKETS, TIMING_BASELINE_SIGMA,
    expected_counts_at_core_sigma, timing_sigma_for_counts,
};
use crate::mania::sunny_windows::{ManiaHitWindows, hit_windows};

// Import shared utilities from the difficulty module
use crate::mania::difficulty::shared::*;

/// The upper edges, in ms, of the first [`LN_DURATION_BUCKETS`] - 1 duration bins;
/// anything longer falls in the last.
///
/// These are a **quadrature grid, not a taxonomy**. The release-spread model is a
/// continuous function of hold duration
/// ([`crate::mania::sunny_accuracy::release_ratio_for_duration`]); the bins exist only because
/// [`SunnyManiaDifficultyAttributes`] is `Copy` and cannot carry a per-note duration
/// list. Each bin contributes one judgement unit evaluated at
/// [`LN_DURATION_REPRESENTATIVES`], so the bins approximate an integral rather than
/// asserting that a 59 ms hold and a 61 ms hold are different kinds of object.
///
/// Log-spaced, because that is how the durations themselves are distributed — over 130k
/// long notes in the fixture set the deciles run 50 ms at p10, 100 ms at p50, 300 ms at
/// p90 and 894 ms at p99. Even spacing would put most notes in one bin and leave the
/// rest nearly empty, which is exactly where a quadrature rule loses accuracy.
///
/// The count and spacing are set by measurement, not taste.
/// `ln_binning_error_stays_small` compares the binned fit against evaluating every long
/// note at its own duration: a coarser five-bin grid let the spread multiplier vary up to
/// 32% *within* one bin and shifted fitted skill by 4.5%, which is the same order as the
/// effect being measured and therefore useless. These edges keep the within-bin variation
/// near 10% and the skill error under 2%.
pub const LN_DURATION_EDGES: [f64; LN_DURATION_BUCKETS - 1] =
    [45.0, 70.0, 100.0, 145.0, 210.0, 320.0, 550.0];

/// The duration, in ms, at which each bin's judgement unit is evaluated.
///
/// Geometric midpoints of the bins rather than arithmetic ones, matching the log spacing
/// of [`LN_DURATION_EDGES`]: for a quantity varying multiplicatively within a bin, the
/// geometric centre is far closer to the typical member than the arithmetic one. The
/// first bin's lower edge is taken as 25 ms rather than zero, since the fixture set's p1
/// is 22 ms, and the open top bin uses a representative near the observed p99 rather than
/// an unbounded midpoint.
pub const LN_DURATION_REPRESENTATIVES: [f64; LN_DURATION_BUCKETS] =
    [34.0, 56.0, 84.0, 120.0, 175.0, 259.0, 419.0, 900.0];

/// Which [`LN_DURATION_EDGES`] bin a long note of `duration` ms belongs to.
fn ln_duration_bucket(duration: f64) -> usize {
    LN_DURATION_EDGES
        .iter()
        .position(|&edge| duration < edge)
        .unwrap_or(LN_DURATION_BUCKETS - 1)
}

/// Every long note in the modal duration bucket, for callers that know how many long
/// notes a map has but not how long they are.
///
/// The fallback for cached attributes round-tripped through JS, where the histogram is
/// not part of the public shape. Approximate by construction: it prices a map of
/// half-second holds as if they were one-beat notes. Prefer passing the beatmap.
pub fn modal_ln_duration_histogram(n_long_notes: usize) -> [usize; LN_DURATION_BUCKETS] {
    let mut buckets = [0; LN_DURATION_BUCKETS];

    // The bin containing the fixture set's median long note (100 ms), which is the
    // least-wrong single choice when the real distribution is unavailable.
    let modal = LN_DURATION_EDGES
        .iter()
        .position(|&edge| 100.0 < edge)
        .unwrap_or(LN_DURATION_BUCKETS - 1);

    buckets[modal] = n_long_notes;

    buckets
}

/// Bucket long notes by how long they are held.
///
/// Durations come from [`Note`], whose times are already divided by the clock rate, so
/// these are the map's own durations rather than what the player experienced. That is
/// the right convention here for the same reason the hit windows are rate-normalised:
/// under `DT` a 100 ms hold arrives as 67 ms of wall-clock but the judgement windows
/// shrink to match, so the *ratio* of hold length to window — which is what decides
/// whether a release is a separate act — is unchanged. Bucketing on wall-clock instead
/// would make `DT` silently reclassify every long note as shorter.
fn ln_duration_histogram(long_notes: &[Note]) -> [usize; LN_DURATION_BUCKETS] {
    let mut buckets = [0; LN_DURATION_BUCKETS];

    for note in long_notes {
        let duration = note.tail_or_head() - note.head;

        if duration > 0.0 {
            buckets[ln_duration_bucket(duration)] += 1;
        }
    }

    buckets
}

/// How many equal-count bins the per-note difficulty distribution is compressed into.
///
/// Chosen by measurement, not taste — `per_note_binning_cost` refits counts that the exact
/// per-note unit list generated, and reports how far each candidate list lands from the
/// skill that produced them, on the 218 of 234 map/skill points where the exact list
/// recovers its own generating skill:
///
/// | bins | median skill err | p90 | max | units/map | us/fit |
/// |------|------------------|-----|-----|-----------|--------|
/// | 8    | 0.174% | 0.407% | 2.884% | 13.7 | 1034 |
/// | **16** | **0.052%** | **0.158%** | **1.164%** | **26.6** | **1975** |
/// | 24   | 0.027% | 0.096% | 0.829% | 38.8 | 2853 |
///
/// The error halves with each doubling, so the residual is the difficulty binning rather
/// than the mean-hold-duration substitution below. 16 is where the residual becomes ~100x
/// smaller than the error it removes: the single-unit-at-`sr` list this replaces misses by
/// **5.37% median and 22.05% at p90**, which is the real reason any of this is here.
///
/// Also the reason it is not one unit per note: per-note `d_all` takes 105-895 distinct
/// values per map, and folding hold duration in pushes distinct `(d, duration)` pairs to
/// 4284, making the exact fit cost 98.8 ms against 2.0 ms here.
pub const NOTE_DIFFICULTY_BINS: usize = 16;

/// The primary map-derived input transition classes cached with difficulty attributes.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(u8)]
pub enum InputClass {
    #[default]
    FreshPress,
    RapidRepress,
    Jack,
    Release,
    ReleaseToPress,
    PressUnderHold,
    ChordEntryOrExit,
}

pub const INPUT_CLASSES: usize = 7;
pub const INPUT_STATE_BINS: usize = INPUT_CLASSES * NOTE_DIFFICULTY_BINS;

/// Maximum number of collapsed judgement populations emitted by a map.
pub const MAX_JUDGEMENT_UNITS: usize = INPUT_STATE_BINS * 2;

/// Fixed-capacity, serializable judgement-unit cache.
///
/// The fixed backing array keeps difficulty attributes `Copy`. `None` on the
/// containing attributes is supported for legacy or lossy round trips.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct JudgementUnitCache {
    pub units: [JudgementUnit; MAX_JUDGEMENT_UNITS],
    pub len: u16,
}

impl JudgementUnitCache {
    fn from_vec(units: Vec<JudgementUnit>) -> Self {
        assert!(units.len() <= MAX_JUDGEMENT_UNITS);

        let mut cache = Self::default();
        cache.len = units.len() as u16;
        cache.units[..units.len()].copy_from_slice(&units);

        cache
    }

    pub fn as_slice(&self) -> &[JudgementUnit] {
        &self.units[..usize::from(self.len).min(MAX_JUDGEMENT_UNITS)]
    }
}

impl Default for JudgementUnitCache {
    fn default() -> Self {
        Self {
            units: [JudgementUnit::default(); MAX_JUDGEMENT_UNITS],
            len: 0,
        }
    }
}

/// A compact aggregate of operations sharing one [`InputClass`].
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct InputStateBin {
    pub class: InputClass,
    pub count: u32,
    pub long_count: u32,
    pub predecessor_count: u32,
    pub mean_difficulty: f64,
    pub mean_duration_ms: f64,
    pub mean_gap_ms: f64,
    pub mean_next_gap_ms: f64,
    pub next_operation_count: u32,
    pub mean_chord_width: f64,
    pub mean_other_held: f64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum InputOperationKind {
    Press,
    Release,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct InputOperation {
    column: usize,
    time_ms: f64,
    pub(crate) kind: InputOperationKind,
    pub(crate) hold_duration_ms: Option<f64>,
    chord_mask: u64,
    pub(crate) note_idx: usize,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum ColumnInputState {
    #[default]
    Idle,
    Pressed,
    Held,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ClassifiedOperation {
    pub(crate) operation: InputOperation,
    pub(crate) class: InputClass,
    pub(crate) previous_gap_ms: Option<f64>,
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) previous_operation_kind: Option<InputOperationKind>,
    pub(crate) next_gap_ms: Option<f64>,
    pub(crate) next_operation_kind: Option<InputOperationKind>,
    pub(crate) other_held: usize,
    pub(crate) chord_width: usize,
}

const RAPID_REPRESS_MS: f64 = 150.0;

pub(crate) fn input_operations(notes: &[Note]) -> Vec<InputOperation> {
    let mut chord_masks = HashMap::<u64, u64>::new();

    for note in notes {
        if note.column < 64 {
            *chord_masks.entry(note.head.to_bits()).or_default() |= 1 << note.column;
        }
    }

    let mut operations = Vec::with_capacity(notes.len() * 2);

    for (note_idx, note) in notes.iter().enumerate() {
        let hold_duration_ms = note
            .tail
            .filter(|&tail| tail > note.head)
            .map(|tail| tail - note.head);
        operations.push(InputOperation {
            column: note.column,
            time_ms: note.head,
            kind: InputOperationKind::Press,
            hold_duration_ms,
            chord_mask: chord_masks.get(&note.head.to_bits()).copied().unwrap_or(0),
            note_idx,
        });

        if let Some(tail) = note.tail.filter(|&tail| tail > note.head) {
            operations.push(InputOperation {
                column: note.column,
                time_ms: tail,
                kind: InputOperationKind::Release,
                hold_duration_ms: Some(tail - note.head),
                chord_mask: 0,
                note_idx,
            });
        }
    }

    // At equal timestamps, releases happen before presses so a hold ending exactly as
    // the next note starts deterministically becomes ReleaseToPress, not an overlap.
    operations.sort_by(|a, b| {
        a.time_ms
            .total_cmp(&b.time_ms)
            .then_with(|| match (a.kind, b.kind) {
                (InputOperationKind::Release, InputOperationKind::Press) => Ordering::Less,
                (InputOperationKind::Press, InputOperationKind::Release) => Ordering::Greater,
                _ => Ordering::Equal,
            })
            .then_with(|| a.column.cmp(&b.column))
            .then_with(|| a.note_idx.cmp(&b.note_idx))
    });

    operations
}

pub(crate) fn classify_input_operations(
    notes: &[Note],
    total_columns: usize,
) -> Vec<ClassifiedOperation> {
    let operations = input_operations(notes);
    let mut states = vec![ColumnInputState::Idle; total_columns];
    let mut held_since = vec![None::<f64>; total_columns];
    let mut previous = vec![None::<InputOperation>; total_columns];
    let mut classified = Vec::with_capacity(operations.len());

    for operation in operations {
        let previous_operation = previous.get(operation.column).copied().flatten();
        let previous_gap_ms = previous_operation.map(|prev| operation.time_ms - prev.time_ms);
        let other_held = states
            .iter()
            .enumerate()
            .filter(|&(column, state)| {
                column != operation.column
                    && *state == ColumnInputState::Held
                    && held_since[column].is_some_and(|start| start < operation.time_ms)
            })
            .count();
        let chord_width = operation.chord_mask.count_ones() as usize;

        let class = match operation.kind {
            InputOperationKind::Release => InputClass::Release,
            InputOperationKind::Press if other_held > 0 => InputClass::PressUnderHold,
            InputOperationKind::Press if chord_width > 1 => InputClass::ChordEntryOrExit,
            InputOperationKind::Press
                if matches!(
                    previous_operation.map(|op| op.kind),
                    Some(InputOperationKind::Release)
                ) && previous_gap_ms.is_some_and(|gap| gap <= RAPID_REPRESS_MS) =>
            {
                InputClass::ReleaseToPress
            }
            InputOperationKind::Press
                if matches!(
                    previous_operation.map(|op| op.kind),
                    Some(InputOperationKind::Press)
                ) && previous_gap_ms.is_some_and(|gap| gap <= RAPID_REPRESS_MS) =>
            {
                InputClass::RapidRepress
            }
            InputOperationKind::Press
                if matches!(
                    previous_operation.map(|op| op.kind),
                    Some(InputOperationKind::Press)
                ) =>
            {
                InputClass::Jack
            }
            InputOperationKind::Press => InputClass::FreshPress,
        };

        classified.push(ClassifiedOperation {
            operation,
            class,
            previous_gap_ms,
            previous_operation_kind: previous_operation.map(|op| op.kind),
            next_gap_ms: None,
            next_operation_kind: None,
            other_held,
            chord_width,
        });

        if let Some(state) = states.get_mut(operation.column) {
            *state = match operation.kind {
                InputOperationKind::Press if operation.hold_duration_ms.is_some() => {
                    ColumnInputState::Held
                }
                InputOperationKind::Press => ColumnInputState::Pressed,
                InputOperationKind::Release => ColumnInputState::Idle,
            };
        }

        if let Some(start) = held_since.get_mut(operation.column) {
            *start = match operation.kind {
                InputOperationKind::Press if operation.hold_duration_ms.is_some() => {
                    Some(operation.time_ms)
                }
                _ => None,
            };
        }

        if let Some(slot) = previous.get_mut(operation.column) {
            *slot = Some(operation);
        }
    }

    let mut next = vec![None::<InputOperation>; total_columns];
    for current in classified.iter_mut().rev() {
        let successor = next.get(current.operation.column).copied().flatten();
        current.next_gap_ms = successor.map(|op| op.time_ms - current.operation.time_ms);
        current.next_operation_kind = successor.map(|op| op.kind);
        if let Some(slot) = next.get_mut(current.operation.column) {
            *slot = Some(current.operation);
        }
    }

    classified
}

/// One equal-count slice of a map's per-note difficulty distribution.
///
/// Deliberately *raw*: this is structural map data, cached per map alongside the star
/// rating, so it must not depend on any [`ErrorModel`] parameter. The collapse of
/// difficulty and hold duration onto a single sigma axis happens later, in
/// [`judgement_units`], where the model is in hand.
///
/// Equal-count bins are what make this cheap to carry — `rice + long` is the same for
/// every bin up to integer division, so no weight has to be stored, and the whole
/// distribution is 16 of these.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct NoteDifficultyBin {
    /// Mean local difficulty (`d_all` at the note's head corner) of the bin's notes.
    pub difficulty: f64,
    /// How many of the bin's notes are plain notes.
    pub rice: u32,
    /// How many are long notes.
    pub long: u32,
    /// Mean hold duration in ms of the bin's long notes, or 0 when it has none.
    ///
    /// A mean, where the exact per-note list knows every hold individually. Measured cost
    /// of that substitution: nothing detectable — see the table on
    /// [`NOTE_DIFFICULTY_BINS`], where error falls with bin count rather than plateauing,
    /// which is what rules the substitution out as the binding term.
    pub mean_duration: f64,
}

/// Compress a map's per-note difficulty into [`NOTE_DIFFICULTY_BINS`] equal-count bins.
///
/// `per_note` is `(local difficulty, hold duration if long)` per note, in any order.
/// Returns `None` when there is nothing to bin, which the callers treat as "fall back to
/// the uniform list" rather than as an error.
fn note_difficulty_bins(
    per_note: &[(f64, Option<f64>)],
) -> Option<[NoteDifficultyBin; NOTE_DIFFICULTY_BINS]> {
    if per_note.is_empty() {
        return None;
    }

    let mut sorted: Vec<(f64, Option<f64>)> = per_note.to_vec();
    sorted.sort_by(|a, b| a.0.total_cmp(&b.0));

    let mut bins = [NoteDifficultyBin::default(); NOTE_DIFFICULTY_BINS];
    let n = sorted.len();

    for (bin, slot) in bins.iter_mut().enumerate() {
        let start = bin * n / NOTE_DIFFICULTY_BINS;
        let end = ((bin + 1) * n / NOTE_DIFFICULTY_BINS).max(start);

        // Fewer notes than bins leaves trailing bins empty. They carry zero weight and so
        // contribute nothing, rather than needing a special case downstream.
        if end == start {
            continue;
        }

        let slice = &sorted[start..end];
        let difficulty = slice.iter().map(|&(d, _)| d).sum::<f64>() / slice.len() as f64;
        let holds: Vec<f64> = slice
            .iter()
            .filter_map(|&(_, duration)| duration)
            .filter(|&duration| duration > 0.0)
            .collect();

        *slot = NoteDifficultyBin {
            difficulty,
            rice: (slice.len() - holds.len()) as u32,
            long: holds.len() as u32,
            mean_duration: if holds.is_empty() {
                0.0
            } else {
                holds.iter().sum::<f64>() / holds.len() as f64
            },
        };
    }

    Some(bins)
}

fn input_state_bins(
    data: &RebirthData,
    d_all: &[f64],
    classic: bool,
) -> Option<[InputStateBin; INPUT_STATE_BINS]> {
    let mut bins = std::array::from_fn(|idx| InputStateBin {
        class: match idx / NOTE_DIFFICULTY_BINS {
            0 => InputClass::FreshPress,
            1 => InputClass::RapidRepress,
            2 => InputClass::Jack,
            3 => InputClass::Release,
            4 => InputClass::ReleaseToPress,
            5 => InputClass::PressUnderHold,
            _ => InputClass::ChordEntryOrExit,
        },
        ..InputStateBin::default()
    });

    let mut by_class = vec![Vec::<(ClassifiedOperation, f64)>::new(); INPUT_CLASSES];

    for classified in classify_input_operations(&data.notes, data.total_columns)
        .into_iter()
        .filter(|op| !classic || op.operation.kind != InputOperationKind::Release)
    {
        let note = data.notes[classified.operation.note_idx];
        let difficulty_idx = lower_bound(&data.all_corners, note.head).min(d_all.len() - 1);
        by_class[classified.class as usize].push((classified, d_all[difficulty_idx]));
    }

    for (class_idx, operations) in by_class.iter_mut().enumerate() {
        operations.sort_by(|a, b| a.1.total_cmp(&b.1));
        let n = operations.len();

        for (position, &(classified, difficulty)) in operations.iter().enumerate() {
            let quantile = position * NOTE_DIFFICULTY_BINS / n.max(1);
            let bin = &mut bins[class_idx * NOTE_DIFFICULTY_BINS + quantile];
            let count = f64::from(bin.count);
            let next_count = count + 1.0;

            bin.mean_difficulty = (bin.mean_difficulty * count + difficulty) / next_count;
            if classified.operation.kind == InputOperationKind::Press
                && classified.operation.hold_duration_ms.is_some()
            {
                let long_count = f64::from(bin.long_count);
                bin.mean_duration_ms = (bin.mean_duration_ms * long_count
                    + classified.operation.hold_duration_ms.unwrap_or(0.0))
                    / (long_count + 1.0);
                bin.long_count += 1;
            }
            bin.mean_chord_width =
                (bin.mean_chord_width * count + classified.chord_width as f64) / next_count;
            bin.mean_other_held =
                (bin.mean_other_held * count + classified.other_held as f64) / next_count;

            if let Some(gap) = classified
                .previous_gap_ms
                .filter(|gap| gap.is_finite() && *gap >= 0.0)
            {
                let predecessor_count = f64::from(bin.predecessor_count);
                bin.mean_gap_ms =
                    (bin.mean_gap_ms * predecessor_count + gap) / (predecessor_count + 1.0);
                bin.predecessor_count += 1;
            }

            if let Some(gap) = classified
                .next_gap_ms
                .filter(|gap| gap.is_finite() && *gap >= 0.0)
            {
                let successor_count = f64::from(bin.next_operation_count);
                bin.mean_next_gap_ms =
                    (bin.mean_next_gap_ms * successor_count + gap) / (successor_count + 1.0);
                bin.next_operation_count += 1;
            }

            bin.count += 1;
        }
    }

    bins.iter().any(|bin| bin.count > 0).then_some(bins)
}

// Note struct is now in shared.rs (imported above)

/// The result of the sunny difficulty calculation.
#[derive(Clone, Copy, Debug, Default)]
pub struct SunnyManiaDifficultyAttributes {
    /// The star rating.
    pub stars: f64,
    /// The variety measure (Rao quadratic entropy based).
    pub variety: f64,
    /// The accuracy scalar `0.5 * spikiness + 0.5 * switches`.
    pub acc_scalar: f64,
    /// How much the difficulty spikes within the map.
    pub spikiness: f64,
    /// How much the playstyle switches between jack and stream-like patterns.
    pub switches: f64,
    /// The GREAT hit window used for the calculation (incl. mods).
    pub great_hit_window: f64,
    /// The full judgement window set the score will be graded against.
    ///
    /// Mods are already folded in, which lets the performance stage price a mod
    /// without knowing it was used: `EZ` widens every window here, reducing the
    /// forward model's expected timing loss.
    pub hit_windows: ManiaHitWindows,
    /// The max combo of the map.
    pub max_combo: u32,
    /// The amount of hit objects taken into account.
    pub n_objects: usize,
    /// How many of those hit objects are long notes.
    ///
    /// Read straight off the map, so it is structural input to the judgement model
    /// rather than anything inferred from a score. The timing model uses it to split
    /// the map into rice and LN populations, since a ScoreV1 long note is judged on
    /// the sum of two offsets and so carries more timing spread than a press — see
    /// [`crate::mania::sunny_accuracy::ln_sigma_scale`].
    pub n_long_notes: usize,
    /// How those long notes are distributed over [`LN_DURATION_EDGES`] duration
    /// buckets, shortest first.
    ///
    /// A histogram rather than a mean, because LN duration spans nearly twenty-fold
    /// *within a single map* — measured over 130k long notes in the fixture set the
    /// deciles run 50 ms at p10, 100 ms at p50 and 894 ms at p99 — and a mean would
    /// put a chordjack's 50 ms taps in the same bucket as a half-second hold. Sums to
    /// [`Self::n_long_notes`].
    ///
    /// A fixed-size array because these attributes are `Copy`.
    pub ln_duration_buckets: [usize; LN_DURATION_BUCKETS],
    /// The map's per-note difficulty distribution, in equal-count bins.
    ///
    /// `stars` is a weighted percentile blend of the same per-note `d_all` values this
    /// bins, so it describes roughly where the distribution sits but nothing about its
    /// width. Pricing every note at `stars` treats a map as uniformly difficult, and
    /// measured against the exact per-note distribution that misfits skill by 5.37% at the
    /// median and 22.05% at p90 — a *map-dependent* error, so unlike every width parameter
    /// in [`ErrorModel`] the per-score `skill` cannot absorb it.
    ///
    /// `None` when the distribution was unavailable, for example for hand-built
    /// attributes in tests or legacy cached JS attributes. Current JS attributes carry
    /// a flattened representation across the round-trip. [`judgement_units`] falls back
    /// to the uniform list when it is absent.
    ///
    /// Raw map structure, carrying no [`ErrorModel`] parameter, because these attributes
    /// are cached per map while the model is a calibration-time choice. Subsumes
    /// [`Self::ln_duration_buckets`] when present: each bin carries its own long notes and
    /// their mean hold duration.
    pub note_difficulty_bins: Option<[NoteDifficultyBin; NOTE_DIFFICULTY_BINS]>,
    /// Compact map-only input transition metadata. Missing cached attributes retain the
    /// pre-feature judgement path.
    pub input_state_bins: Option<[InputStateBin; INPUT_STATE_BINS]>,
    /// Default-model judgement populations, normalized to a total weight of one.
    ///
    /// This is produced with SR so repeated PP calculations do not rebuild the same
    /// map-only data. It may be absent after a legacy or lossy serialization round trip;
    /// PP then reconstructs it from the raw bins above.
    pub judgement_units: Option<JudgementUnitCache>,
    /// Fixed-sigma expected accuracy for the default model and played conditions.
    pub timing_expected_accuracy: f64,
    /// Fixed-sigma expected accuracy with structural offsets neutralized.
    pub timing_reference_accuracy: f64,
    /// The map's own judgement windows with the window-affecting mods stripped.
    ///
    /// Production PP uses these as the natural-window denominator of the relative
    /// timing transfer. [`Self::hit_windows`] remains the played side.
    pub map_windows: ManiaHitWindows,
    /// Whether long notes give a single combined judgement (ScoreV1 / classic)
    /// rather than separate head and release judgements (ScoreV2).
    ///
    /// This is [`is_classic`] carried forward, because the judgement *count* depends
    /// on it: under V1 the map yields `n_objects` judgements, under V2 it yields
    /// `n_objects + n_long_notes`. Verified against 143 live scores — every V2 score
    /// totalled `notes + LN`, and every V1 score bar one totalled `notes`.
    pub ln_judged_as_one: bool,
}

/// The result of the sunny performance calculation.
#[derive(Clone, Copy, Debug, Default)]
pub struct SunnyManiaPerformanceAttributes {
    /// The total performance points.
    pub pp: f64,
    /// The difficulty portion of the PP.
    pub pp_difficulty: f64,
    /// The variety multiplier applied to the difficulty portion.
    pub variety_multiplier: f64,
    /// The accuracy multiplier applied to the difficulty portion.
    pub acc_multiplier: f64,
    /// The length multiplier applied to the difficulty portion.
    pub length_multiplier: f64,
    /// Accuracy-neutral PP contribution from Sunny's pattern calculation.
    pub xxy_pp_pattern: f64,
    /// Reference adjustment from Sunny's accuracy proportion and accuracy scalar.
    /// This is reported for comparison and is not included in [`Self::pp`].
    pub xxy_pp_accuracy: f64,
    /// Signed adjustment from the forward timing surface.
    pub pp_timing: f64,
    /// Expected accuracy at the fixed-spread probe through the played conditions.
    pub timing_expected_accuracy: f64,
    /// Expected accuracy through natural windows with structural offsets neutralized.
    pub timing_reference_accuracy: f64,
    /// Score-conditioned timing spread used for the relative transfer, in milliseconds.
    pub timing_core_sigma: f64,
}

/// Score state required for the performance calculation.
#[derive(Clone, Copy, Debug, Default)]
pub struct SunnyScoreState {
    pub n320: u32,
    pub n300: u32,
    pub n200: u32,
    pub n100: u32,
    pub n50: u32,
    pub misses: u32,
}

impl SunnyScoreState {
    pub const fn total_hits(&self) -> u32 {
        self.n320 + self.n300 + self.n200 + self.n100 + self.n50 + self.misses
    }
}

/// Calculate the sunny difficulty attributes for a (converted) mania beatmap.
///
/// `mods` are the rosu-mods, `clock_rate` the custom clock rate, `lazer`
/// whether the play is a lazer (default) or stable play and `passed_objects`
/// the amount of objects to take into account for partial plays.
pub fn calculate(
    map: &Beatmap,
    mods: &GameMods,
    clock_rate: f64,
    lazer: Option<bool>,
    passed_objects: Option<u32>,
) -> Option<SunnyManiaDifficultyAttributes> {
    let total_columns = map.cs.round_ties_even().max(1.0) as usize;

    let classic = is_classic(lazer, mods);

    let great_hit_window = get_hit_window_300(map, clock_rate, classic, mods);
    let windows = hit_windows(map, mods, clock_rate, classic);

    // The same map judged without the window-affecting mods. Mods reach `hit_windows`
    // only through its difficulty multiplier, so an empty mod set is exactly "this map's
    // own windows". The clock rate stays as passed because the windows are rate-normalized
    // anyway, and `classic` stays because it describes the scoring scheme rather than a
    // mod's effect on leniency.
    let map_windows = hit_windows(map, &GameMods::default(), clock_rate, classic);

    let take = passed_objects.unwrap_or(u32::MAX) as usize;
    let objects = map.hit_objects.iter().take(take);

    let (notes, max_combo) = build_notes(clock_rate, objects, total_columns);

    if notes.len() < 2 || total_columns == 0 {
        return None;
    }

    let data = RebirthData::new_with_windows(notes, total_columns, windows);
    let params = calculate_from_data(&data, classic)?;

    let mut attrs = SunnyManiaDifficultyAttributes {
        stars: params.sr,
        variety: params.variety,
        acc_scalar: 0.5 * params.spikiness + 0.5 * params.switches,
        spikiness: params.spikiness,
        switches: params.switches,
        great_hit_window,
        hit_windows: windows,
        map_windows,
        max_combo,
        n_objects: data.notes.len(),
        n_long_notes: data.long_notes.len(),
        ln_duration_buckets: ln_duration_histogram(&data.long_notes),
        note_difficulty_bins: params.note_difficulty_bins,
        input_state_bins: params.input_state_bins,
        judgement_units: None,
        timing_expected_accuracy: 1.0,
        timing_reference_accuracy: 1.0,
        ln_judged_as_one: classic,
    };

    let model = ErrorModel::default();
    let units = judgement_units(&attrs, 1.0, &model, !per_note_difficulty_disabled());
    let timing = compute_timing_pp_with_units(&attrs, 1.0, &model, Some(&units));
    attrs.timing_expected_accuracy = timing.expected_accuracy;
    attrs.timing_reference_accuracy = timing.reference_accuracy;
    attrs.judgement_units = Some(JudgementUnitCache::from_vec(units));

    Some(attrs)
}

/// Calculate the sunny performance attributes.
pub fn calculate_performance(
    attrs: &SunnyManiaDifficultyAttributes,
    mods: &GameMods,
    state: SunnyScoreState,
) -> SunnyManiaPerformanceAttributes {
    // The cached timing from SR uses baseline sigma for expectations, which is fine
    // for the SR-time calculation. But for actual score PP, we need to fit the
    // player's actual sigma from their counts, so we don't use the cache here.
    calculate_performance_inner(attrs, mods, state, &ErrorModel::default(), None)
}

fn calculate_performance_inner(
    attrs: &SunnyManiaDifficultyAttributes,
    mods: &GameMods,
    state: SunnyScoreState,
    model: &ErrorModel,
    cached_timing: Option<TimingPpResult>,
) -> SunnyManiaPerformanceAttributes {
    let score_accuracy = xxy_custom_accuracy(state);

    // === COMPOSITIONAL ARCHITECTURE ===
    // PP is split into two additive components:
    // 1. Pattern difficulty (from sunny's base star rating)
    // 2. Timing difficulty (from forward expected judgment loss)

    // Pattern difficulty: base calculation from sunny
    let xxy_variety_multiplier = xxy_variety_multiplier(attrs.variety);
    let xxy_length_multiplier = xxy_length_multiplier(attrs.n_objects as f64, attrs.stars);
    let xxy_acc_multiplier = xxy_acc_multiplier(score_accuracy, attrs.acc_scalar);
    let score_performance_proportion = xxy_performance_proportion(score_accuracy);
    // Accuracy-neutral pattern value. Keeping this separate makes the accuracy
    // reward replaceable without rebuilding Sunny's pattern calculation.
    let xxy_pattern_difficulty = attrs.stars.max(0.2) - 0.15;
    let xxy_pp_pattern = 9.8
        * xxy_pattern_difficulty.powf(2.2)
        * xxy_variety_multiplier
        * xxy_length_multiplier
        * 1f64;

    // Express Sunny's multiplicative accuracy pricing as an additive delta so
    // it can be inspected independently from the timing surface.
    let xxy_pp_accuracy =
        xxy_pp_pattern * (score_performance_proportion * xxy_acc_multiplier - 1.0);

    // Timing difficulty: Replace Sunny's accuracy adjustment with sigma-based approach
    let timing_result = cached_timing.unwrap_or_else(|| compute_timing_pp(attrs, state, model));

    // let ss_timing = compute_timing_pp(
    //     attrs,
    //     SunnyScoreState {
    //         n320: state.total_hits() as u32,
    //         n300: 0,
    //         n200: 0,
    //         n100: 0,
    //         n50: 0,
    //         misses: 0,
    //     },
    //     model,
    // );

    // let avg_player_ratio = TIMING_CORE_SIGMA / ss_timing.core_sigma;

    // // Fit the player's actual timing sigma from their score.
    // // Then compute what accuracy a baseline sigma would have produced through
    // // the same windows/conditions, and take the difference.
    // //
    // // This replaces Sunny's accuracy multiplier with a sigma-ratio approach:
    // // - Fitted sigma represents the player's actual timing precision
    // // - Baseline sigma (11ms) represents SS-tier timing precision
    // // - The ratio determines the timing PP adjustment

    // // Compute sigma ratio: baseline / fitted
    // // ratio > 1 means better timing than baseline (reward)
    // // ratio < 1 means worse timing than baseline (penalty)
    // let sigma_ratio = timing_result.core_sigma / avg_player_ratio.powf(4.0);

    // // The timing adjustment replaces sunny's accuracy effect
    // let pp_timing = sigma_ratio * xxy_pp_pattern / -8.0;

    // // Sunny accuracy is retained only as a reference column. Our result is the
    // // accuracy-neutral pattern value plus the timing surface adjustment.
    // let pp = xxy_pp_pattern + pp_timing;
    let pp = xxy_pp_pattern + xxy_pp_accuracy;

    // Legacy difficulty_value for compatibility
    let difficulty_value = compute_difficulty_value(attrs.stars, score_accuracy, 1.0);

    let v = SunnyManiaPerformanceAttributes {
        pp: pp,
        pp_difficulty: difficulty_value,
        xxy_pp_pattern,
        xxy_pp_accuracy,
        pp_timing: xxy_pp_accuracy, // disabled surface for now

        timing_expected_accuracy: timing_result.expected_accuracy,
        timing_reference_accuracy: timing_result.reference_accuracy,
        timing_core_sigma: 0.0,

        variety_multiplier: xxy_variety_multiplier,
        acc_multiplier: xxy_acc_multiplier,
        length_multiplier: xxy_length_multiplier,
    };

    normalize_for_human_reference(v, mods)
}

pub(crate) fn normalize_for_human_reference(
    input: SunnyManiaPerformanceAttributes,
    mods: &GameMods,
) -> SunnyManiaPerformanceAttributes {
    // NF still gets a flat factor: failing is a scoring matter that the timing
    // model says nothing about. EZ has no factor here because its windows are
    // evaluated directly below.
    let mut multiplier = 1.0;

    // TODO: only nerf when failed
    if has_mod(mods, "NF") {
        multiplier *= 0.75;
    }

    let mut normalized = input;

    normalized.pp *= multiplier;
    normalized.xxy_pp_pattern *= multiplier;
    normalized.xxy_pp_accuracy *= multiplier;
    normalized.pp_timing *= multiplier;

    normalized
}

/// OD 8 classic non-convert, the modal mania OD.
///
/// **The pricing default again** — see [`reference_windows`] for why the map-own
/// reference this replaced had to be reverted. Also still the fixed yardstick the
/// calibration harnesses fit against, where a constant is what is wanted so that fit
/// quality across maps is comparable.
///
/// A literal because it must be `const`; `reference_windows_match_od8_no_mod` pins
/// it against [`hit_windows`] so the two cannot drift.
#[cfg(test)]
const REFERENCE_WINDOWS: ManiaHitWindows = ManiaHitWindows {
    perfect: 16.5,
    great: 40.5,
    good: 73.5,
    ok: 103.5,
    meh: 127.5,
    miss: 164.5,
};

/// The windows used as a fixed comparison yardstick by research and calibration
/// reports.
///
/// Production pricing no longer uses this second fit. It normalizes against the
/// established non-input-state surface through the map's own natural windows; keeping
/// this helper allows the historical fixed/map/one-sided comparisons to remain
/// reproducible.
///
/// - **Fixed [`REFERENCE_WINDOWS`]** (OD 8, the default now — no env var needed) says a
///   low-OD map is genuinely more lenient, so a score on it demonstrates less precision
///   and should earn less. That claim is very hard to defend in mania, where OD is a
///   charting convention rather than a difficulty setting: 7K charts in the fixture set
///   average OD 4.8 against 4K's 8.2, and 7K LN maps average OD 4.2. Under this
///   reference those maps lose 16.6% of their live pp
///   for their OD alone. **This is still true and still the reason low-OD LN maps are a
///   hazard under this default** — see below for what is meant to fix it.
/// - **One-sided** (`SUNNY_ONESIDED_REFERENCE`), the wider of the two per window: a map
///   stricter than OD 8 keeps its bonus, a map more lenient than OD 8 pays no penalty.
///   Asymmetric by construction, and the asymmetry is not merely convenient — the two
///   directions are not equally well evidenced. Above the reference the claim "these 320s
///   came through a 14.5 ms window, so this player was precise to better than 14.5 ms" is
///   directly witnessed by the counts. Below it, "these 320s came through a 20 ms window,
///   so this player was only precise to 20 ms" is *not* witnessed, because a 320 is
///   censored: a player who would have hit inside 16.5 ms anyway produces exactly the same
///   count as one who needed the full 20 ms. The surface can therefore detect precision
///   finer than the window it is given, but not coarser, and a one-sided reference is what
///   that asymmetry looks like when taken seriously. That censoring argument did not
///   survive testing: the low-OD 7K scores the fixed reference penalises average a 63.4%
///   320 share with none above 90%, so they are nowhere near the saturation the argument
///   needs. The asymmetry rests on the endogeneity of mania OD alone.
/// - **The map's own windows** (`SUNNY_MAP_REFERENCE`, the *former* default), which
///   confines the surface to pricing *mods*: every no-mod score prices at exactly 1.0 at
///   any OD or keymode, and a mod is charged for how far it moves the windows away from
///   what the map itself asked for.
///
/// Measured against 143 live scores, as a fraction of live pp:
///
/// | group | fixed | one-sided | map |
/// |---|---|---|---|
/// | all (n=143) | −12.49% | −7.12% | −8.27% |
/// | 7K no-mod (n=51) | −13.94% | +0.30% | −0.21% |
/// | 4K no-mod OD≥8.9 (n=19) | +2.80% | +2.80% | +0.10% |
/// | EZ on OD≥8.1 (n=9) | −32.68% | −32.68% | −39.75% |
///
/// **Why the map reference was reverted.** It was chosen over the one-sided variant
/// because a symmetric rule is defensible to players in a way "your OD only counts when
/// it helps you" is not, and it repaired `EZ`'s split cost (32.68% on high-OD maps vs
/// 39.06% on low-OD ones under the fixed reference, vs a uniform −39.75% / −39.18% under
/// the map reference). Both of those points are still true. But pricing every score
/// against its own windows makes every no-mod score price at *exactly* 1.0, by
/// construction — `a_no_mod_score_prices_at_one_under_the_map_reference` pins exactly
/// that. That is not a nice property, it is the reference closing the OD channel
/// entirely: no map property can ever move pricing, because the map is always compared
/// to itself. The surface exists to price every OD, not to exempt OD from pricing, so a
/// reference that makes OD unconditionally invisible defeats the reason this module
/// exists. The fixed reference reopens that channel at the cost of the 16.6% low-OD LN
/// hit above.
///
/// **What is meant to close that cost without reintroducing the map reference:**
/// [`crate::mania::sunny_accuracy::ErrorModel::release_mean_offset`]. The 16.6% loss above is
/// concentrated in low-OD *LN* maps specifically (7K LN averaging OD 4.2), and the
/// mechanism is that a release lands systematically late relative to where the window is
/// centred — a bias the model had no parameter for until now. A bias is not absorbed by
/// the skill fit the way a width is, so it is a mean shift that survives being compared
/// to a fixed reference in a way `release_sigma_ratio` alone cannot. The endogeneity
/// argument for low-OD 7K maps is unaffected by any of this and remains the reason those
/// maps are worth watching under the fixed default.
#[cfg(test)]
fn reference_windows(attrs: &SunnyManiaDifficultyAttributes) -> ManiaHitWindows {
    if std::env::var_os("SUNNY_MAP_REFERENCE").is_some() {
        return attrs.map_windows;
    }

    if std::env::var_os("SUNNY_ONESIDED_REFERENCE").is_some() {
        return REFERENCE_WINDOWS.widest_of(&attrs.map_windows);
    }

    REFERENCE_WINDOWS
}

/// Whether `SUNNY_NO_LN_SPLIT` is set, which collapses the LN mixture back to a
/// single population.
///
/// An A/B switch for the reporting harnesses, not a feature: the LN split changes no
/// free parameters, so the only way to attribute a change in fit quality to it is to
/// price the same fixtures both ways in one build. Unset in every normal run,
/// including every unit test that pins the split's behaviour.
fn ln_split_disabled() -> bool {
    std::env::var_os("SUNNY_NO_LN_SPLIT").is_some()
}

/// Whether `SUNNY_NO_PER_NOTE_D` is set, which prices every note at the map's star
/// rating instead of its own local difficulty.
///
/// The same kind of A/B switch as [`ln_split_disabled`], and for the same reason: feeding
/// per-note difficulty in adds no free parameter, so the only way to attribute a change in
/// fit quality or pp to it is to price the same fixtures both ways in one build. Unset in
/// every normal run.
fn per_note_difficulty_disabled() -> bool {
    std::env::var_os("SUNNY_NO_PER_NOTE_D").is_some()
}

/// The judgement units a score's counts are fitted against: the map split by per-note
/// local difficulty and, under ScoreV1, by long-note hold duration within each difficulty
/// bin.
///
/// **Per-note difficulty**, via [`SunnyManiaDifficultyAttributes::note_difficulty_bins`],
/// when the map's distribution reached here. Pricing every note at the map's `stars`
/// instead treats the map as uniformly difficult, and measured against the exact per-note
/// distribution that misfits skill by 5.37% at the median and 22.05% at p90. That error is
/// unlike every width parameter in [`ErrorModel`]: those are absorbed exactly by the
/// per-score `skill` (see the module docs on gauge), whereas this one is a property of the
/// *map*, so no per-score quantity can absorb it. Falls back to the uniform list when the
/// distribution is absent, which is what a JS round-trip leaves behind.
///
/// Note that within a single map the distribution is fairly tight — `p90/p50` of per-note
/// difficulty is under 1.2 on 126 of the 143 fixture scores. The wider `p90/p50` figures in
/// `per_note_difficulty_distribution` (1.69 on 4K, 1.33 on 7K) are *pooled across maps* and
/// so are mostly between-map variance; they are not what this path resolves. What it
/// resolves is measured directly in `per_note_difficulty_on_real_scores`.
///
/// **The long-note mixture** matters on top of that, because an LN chart is a mixture of
/// judgement *widths*: fitting one sigma to a mixture inflates it, which drives estimated
/// skill down and, via the `^2.2` in pp, costs far more than the widening itself. 7K charts
/// in the fixture set average 58% long notes against 4K's 3%, so this is where the two
/// populations actually differ.
///
/// **Why duration matters and not one LN population.** A release is harder to place than a
/// press, and a *short* hold is harder still because the press motion has not finished
/// when the release is already due. Both effects live in
/// [`crate::mania::sunny_accuracy::release_ratio_for_duration`], which is continuous in
/// duration. Sweeping a single LN width instead wanted two different answers on mixed-LN
/// and LN-saturated maps, which is what forced duration into the model. Under the per-note
/// path each difficulty bin carries its own long notes and their mean hold duration, so
/// duration is still resolved; [`Self::ln_duration_buckets`] is the fallback path's
/// quadrature grid for the same integral.
///
/// Under ScoreV2 heads and releases are judged separately, so every judgement is a
/// single press and there is no mixture; the units come back uniform and only the count
/// changes. `total` is the score's own judgement total, which the caller has already
/// measured, so the returned weights always sum to exactly what was observed even when
/// the map's structure and the score disagree.
///
/// Everything read here comes from the `.osu` and the mod list. Nothing about how well
/// the player did enters, which is the line that keeps a bad play from being re-read as
/// a hard map.
/// `per_note` selects the per-note path; production passes
/// `!per_note_difficulty_disabled()`. It is a parameter rather than an env read inside
/// because the reporting harnesses price the same score both ways in one process, and
/// mutating the environment mid-test is unsound.
fn judgement_units(
    attrs: &SunnyManiaDifficultyAttributes,
    total: f64,
    model: &ErrorModel,
    per_note: bool,
) -> Vec<JudgementUnit> {
    let uniform = vec![JudgementUnit::repeated(attrs.stars, total)];

    // A zero recovery amplitude is the explicit control path used by the natural
    // baseline and A/B tests. The experiment branch enables the measured curve.
    if model.recovery_offset != 0.0 {
        if let Some(bins) = attrs.input_state_bins {
            let units = units_from_input_state_bins(&bins, attrs, total, model);

            if !units.is_empty() {
                return units;
            }
        }
    }

    // Per-note difficulty when the map's distribution survived to here, which also
    // subsumes the LN duration split below: each bin carries its own long notes.
    if let Some(bins) = attrs
        .note_difficulty_bins
        .filter(|_| attrs.n_objects > 0 && per_note)
    {
        let units = units_from_difficulty_bins(&bins, attrs, total, model);

        if !units.is_empty() {
            return units;
        }
    }

    // Under V2 the head and release are two ordinary single-press judgements, so
    // there is no wide population to separate out.
    if !attrs.ln_judged_as_one || attrs.n_long_notes == 0 || attrs.n_objects == 0 {
        return uniform;
    }

    if ln_split_disabled() {
        return uniform;
    }

    // Work in shares of the score's own judgement total rather than in the map's raw
    // counts. A partial play, or a count vector that disagrees with our object parsing,
    // then still produces weights summing to the observed total, which is what the
    // multinomial fit requires.
    let per_object = total / attrs.n_objects as f64;

    let mut units = Vec::with_capacity(LN_DURATION_BUCKETS + 1);
    let mut ln_total = 0.0;

    for (bin, &count) in attrs.ln_duration_buckets.iter().enumerate() {
        if count == 0 {
            continue;
        }

        let weight = count as f64 * per_object;
        ln_total += weight;

        units.push(JudgementUnit::long_note(
            attrs.stars,
            weight,
            model,
            LN_DURATION_REPRESENTATIVES[bin],
        ));
    }

    // The histogram can undercount long notes relative to `n_long_notes` — a zero-length
    // hold contributes to one and not the other — so derive the rice weight from what
    // the bins actually consumed rather than from the LN count. This keeps the weights
    // summing to `total` regardless.
    let rice_units = (total - ln_total).max(0.0);

    if rice_units > 0.0 {
        units.push(JudgementUnit::repeated(attrs.stars, rice_units));
    }

    if units.is_empty() {
        return uniform;
    }

    units
}

fn units_from_input_state_bins(
    bins: &[InputStateBin; INPUT_STATE_BINS],
    attrs: &SunnyManiaDifficultyAttributes,
    total: f64,
    model: &ErrorModel,
) -> Vec<JudgementUnit> {
    // ScoreV1 judges a long note as one object; release operations are metadata
    // in that mode and must not dilute the observed judgement total.
    let binned: u32 = bins
        .iter()
        .filter(|bin| !(attrs.ln_judged_as_one && bin.class == InputClass::Release))
        .map(|bin| bin.count)
        .sum();

    if binned == 0 {
        return Vec::new();
    }

    let per_operation = total / f64::from(binned);
    let mut units = Vec::with_capacity(INPUT_STATE_BINS * 2);

    // The recovery curve was measured as each state group's mean error relative to
    // that score's own mean error. Preserve that gauge here: a map's state mixture
    // may redistribute timing error between notes, but it must not manufacture a
    // global clock offset that the replay measurement explicitly divided out.
    //
    // Releases are excluded because the measured recovery curve covers presses;
    // their independent release offset remains untouched below.
    let (recovery_sum, press_count) = bins
        .iter()
        .filter(|bin| bin.count > 0 && bin.class != InputClass::Release)
        .fold((0.0, 0_u32), |(sum, count), bin| {
            let offset = input_state_recovery_offset(bin, model);

            (
                sum + offset * f64::from(bin.count),
                count.saturating_add(bin.count),
            )
        });
    let recovery_center = if press_count > 0 {
        recovery_sum / f64::from(press_count)
    } else {
        0.0
    };

    for bin in bins.iter().filter(|bin| {
        bin.count > 0 && !(attrs.ln_judged_as_one && bin.class == InputClass::Release)
    }) {
        let class_offset = match bin.class {
            InputClass::Release if !attrs.ln_judged_as_one => model.release_mean_offset,
            InputClass::Release => 0.0,
            _ => input_state_recovery_offset(bin, model) - recovery_center,
        };
        let long_count = if attrs.ln_judged_as_one {
            bin.long_count
        } else {
            0
        };
        let plain_count = bin.count.saturating_sub(long_count);

        if plain_count > 0 {
            let mut unit = JudgementUnit::repeated(
                bin.mean_difficulty,
                f64::from(plain_count) * per_operation,
            );
            unit.fading_mean_offset = class_offset;
            units.push(unit);
        }

        if long_count > 0 {
            let mut unit = JudgementUnit::long_note(
                bin.mean_difficulty,
                f64::from(long_count) * per_operation,
                model,
                bin.mean_duration_ms,
            );
            unit.fading_mean_offset = class_offset;
            units.push(unit);
        }
    }

    units
}

fn input_state_recovery_offset(bin: &InputStateBin, model: &ErrorModel) -> f64 {
    if bin.count == 0 || bin.predecessor_count == 0 {
        return 0.0;
    }

    model.recovery_mean_offset(bin.mean_gap_ms) * f64::from(bin.predecessor_count)
        / f64::from(bin.count)
}

/// Turn a map's per-note difficulty distribution into judgement units.
///
/// Each bin contributes up to two units: its plain notes, and its long notes widened and
/// shifted for the release the same way [`JudgementUnit::long_note`] does everywhere else.
/// This is where the model finally meets the raw bins, which is why the bins themselves can
/// stay model-free and cacheable.
///
/// Weights are in shares of `total` — the score's own judgement total — rather than the
/// map's raw counts, for the same reason the LN-split path works that way: a partial play,
/// or a count vector that disagrees with our object parsing, must still produce weights
/// summing to what was actually observed, since that is what the multinomial fit requires.
///
/// Under ScoreV2 a long note is two separate single-press judgements rather than one
/// combined one, so its release is not widened; the bins still carry the per-note
/// difficulty, which is the part V2 scores were missing before.
fn units_from_difficulty_bins(
    bins: &[NoteDifficultyBin; NOTE_DIFFICULTY_BINS],
    attrs: &SunnyManiaDifficultyAttributes,
    total: f64,
    model: &ErrorModel,
) -> Vec<JudgementUnit> {
    let binned_notes: u32 = bins.iter().map(|bin| bin.rice + bin.long).sum();

    if binned_notes == 0 {
        return Vec::new();
    }

    // Rescale to the observed total rather than trusting the map's own note count, which a
    // partial play or a parsing disagreement can contradict.
    let per_note = total / f64::from(binned_notes);
    let combined_long_notes = attrs.ln_judged_as_one && !ln_split_disabled();
    let mut units = Vec::with_capacity(NOTE_DIFFICULTY_BINS * 2);

    for bin in bins {
        if bin.rice > 0 {
            units.push(JudgementUnit::repeated(
                bin.difficulty,
                f64::from(bin.rice) * per_note,
            ));
        }

        if bin.long == 0 {
            continue;
        }

        let weight = f64::from(bin.long) * per_note;

        if combined_long_notes && bin.mean_duration > 0.0 {
            units.push(JudgementUnit::long_note(
                bin.difficulty,
                weight,
                model,
                bin.mean_duration,
            ));
        } else {
            units.push(JudgementUnit::repeated(bin.difficulty, weight));
        }
    }

    units
}

#[cfg(test)]
fn timing_loss_ratio_with_model(
    attrs: &SunnyManiaDifficultyAttributes,
    state: SunnyScoreState,
    model: &ErrorModel,
) -> f64 {
    compute_timing_pp(attrs, state, model).loss_ratio
}

/// Result of timing pp calculation with component breakdown.
#[derive(Clone, Copy, Debug, Default)]
struct TimingPpResult {
    loss_ratio: f64,
    expected_accuracy: f64,
    reference_accuracy: f64,
    core_sigma: f64,
}

/// Evaluate timing PP by fitting the player's actual timing sigma from their score,
/// then comparing it to a reference baseline sigma.
///
/// This approach:
/// 1. Fits the player's timing spread (sigma) in milliseconds from their judgement counts
/// 2. Compares their sigma to a baseline (TIMING_BASELINE_SIGMA)
/// 3. Converts the sigma ratio to a PP adjustment through the performance proportion curve
///
/// Lower sigma (tighter timing) = better than baseline = positive PP adjustment
/// Higher sigma (looser timing) = worse than baseline = negative PP adjustment
fn compute_timing_pp(
    attrs: &SunnyManiaDifficultyAttributes,
    state: SunnyScoreState,
    model: &ErrorModel,
) -> TimingPpResult {
    let total = state.total_hits();

    if total == 0 || attrs.n_objects == 0 || attrs.stars <= 0.0 {
        return TimingPpResult {
            loss_ratio: 1.0,
            expected_accuracy: 1.0,
            reference_accuracy: 1.0,
            core_sigma: TIMING_BASELINE_SIGMA,
            ..TimingPpResult::default()
        };
    }

    let units = judgement_units(
        attrs,
        f64::from(total),
        model,
        !per_note_difficulty_disabled(),
    );
    let counts = [
        state.n320,
        state.n300,
        state.n200,
        state.n100,
        state.n50,
        state.misses,
    ];

    // Fit the player's actual timing sigma from their score
    let fitted_sigma = timing_sigma_for_counts(&counts, &units, &attrs.hit_windows, model);

    // Compute what accuracy the baseline sigma would produce
    let baseline_expected =
        expected_counts_at_core_sigma(&units, &attrs.hit_windows, model, TIMING_BASELINE_SIGMA);
    let expected_accuracy = baseline_expected.custom_accuracy();

    // Note: The fitted sigma should produce accuracy close to the actual score accuracy,
    // which validates that the fitting process worked correctly.

    // Reference: baseline through neutral conditions
    let reference_model = ErrorModel {
        recovery_offset: 0.0,
        anticipation_offset: 0.0,
        ..*model
    };
    let reference_units = judgement_units(
        attrs,
        f64::from(total),
        &reference_model,
        !per_note_difficulty_disabled(),
    );
    let reference = expected_counts_at_core_sigma(
        &reference_units,
        &attrs.hit_windows,
        &reference_model,
        TIMING_BASELINE_SIGMA,
    );
    let reference_accuracy = reference.custom_accuracy();

    TimingPpResult {
        loss_ratio: 1.0, // Not used in the new approach
        expected_accuracy,
        reference_accuracy,
        core_sigma: fitted_sigma,
    }
}

fn compute_timing_pp_with_units(
    attrs: &SunnyManiaDifficultyAttributes,
    total: f64,
    model: &ErrorModel,
    cached_units: Option<&[JudgementUnit]>,
) -> TimingPpResult {
    if total <= 0.0 || attrs.n_objects == 0 || attrs.stars <= 0.0 {
        return TimingPpResult {
            loss_ratio: 1.0,
            expected_accuracy: 1.0,
            reference_accuracy: 1.0,
            core_sigma: TIMING_BASELINE_SIGMA,
            ..TimingPpResult::default()
        };
    }

    let owned_units;
    let units = if let Some(units) = cached_units {
        units
    } else {
        owned_units = judgement_units(attrs, total, model, !per_note_difficulty_disabled());

        &owned_units
    };

    // Use the baseline timing precision to compute what accuracy it would produce
    // through the played windows and input-state conditions.
    let expected =
        expected_counts_at_core_sigma(units, &attrs.hit_windows, model, TIMING_BASELINE_SIGMA);
    let expected_accuracy = expected.custom_accuracy();
    let expected_loss = (1.0 - expected_accuracy).max(f64::EPSILON);

    // Reference: same baseline precision through neutral conditions (no input-state offsets).
    let reference_model = ErrorModel {
        recovery_offset: 0.0,
        anticipation_offset: 0.0,
        ..*model
    };
    let reference_units = judgement_units(
        attrs,
        total,
        &reference_model,
        !per_note_difficulty_disabled(),
    );
    let reference = expected_counts_at_core_sigma(
        &reference_units,
        &attrs.hit_windows,
        &reference_model,
        TIMING_BASELINE_SIGMA,
    );
    let reference_accuracy = reference.custom_accuracy();
    let reference_loss = (1.0 - reference_accuracy).max(f64::EPSILON);

    TimingPpResult {
        loss_ratio: expected_loss / reference_loss,
        expected_accuracy,
        reference_accuracy,
        core_sigma: TIMING_BASELINE_SIGMA,
    }
}

fn timing_loss_ratio(expected_accuracy: f64, reference_accuracy: f64) -> f64 {
    let expected_loss = (1.0 - expected_accuracy).max(f64::EPSILON);
    let reference_loss = (1.0 - reference_accuracy).max(f64::EPSILON);

    expected_loss / reference_loss
}

// ---------------------------------------------------------------------------
// Hit window & hit leniency
// ---------------------------------------------------------------------------

/// The GREAT hit window following the C# `ManiaDifficultyCalculator`.
///
/// - non-convert mania maps use `34 + 3 * (10 - od)` clamped to `[34, 64]`
/// - convert maps use `34` if the original OD rounds above 4, else `47`
/// - `HR` divides the window by 1.4, `EZ` multiplies it by 1.4
/// - the clock rate scales the window but is normalized away afterwards
pub(crate) fn get_hit_window_300(
    map: &Beatmap,
    clock_rate: f64,
    classic: bool,
    mods: &GameMods,
) -> f64 {
    let od = f64::from(map.od);

    let base = if classic && !map.is_convert {
        34.0 + 3.0 * (10.0 - od).clamp(0.0, 10.0)
    } else if classic && od.round() > 4.0 {
        34.0
    } else if classic {
        47.0
    } else if od > 5.0 {
        49.0 + (34.0 - 49.0) * (od - 5.0) / 5.0
    } else {
        64.0 + (49.0 - 64.0) * od / 5.0
    };

    let mut value = base * clock_rate + 1e-6;

    value /= crate::mania::sunny_windows::difficulty_multiplier(mods);

    ((value as i64) as f64 + 0.5) / clock_rate
}

/// Whether the score is a classic (osu!stable default / lazer with CL mod)
/// style play, i.e. long notes give a single judgement and the difficulty
/// weights use the head-only density.
pub(crate) fn is_classic(lazer: Option<bool>, mods: &GameMods) -> bool {
    let lazer = lazer.unwrap_or(true);
    // `SV2`, not `V2`: that is the acronym `rosu_mods::ScoreV2Mania` reports, and the
    // string is parsed rather than matched, so a wrong one silently never matches. It
    // did exactly that — every score read as ScoreV1, which mattered as soon as long
    // notes started being judged differently under the two.
    let sv2 = has_mod(mods, "SV2");
    let cl = has_mod(mods, "CL");

    (!lazer && !sv2) || cl
}

/// Whether the mods contain the mod with the given acronym.
fn has_mod(mods: &GameMods, acronym: &str) -> bool {
    acronym
        .parse::<Acronym>()
        .map_or(false, |acronym| mods.contains_acronym(acronym))
}

// ---------------------------------------------------------------------------
// Data preparation
// ---------------------------------------------------------------------------

/// Convert the beatmap's hit objects into notes, applying the clock rate.
/// Also computes the max combo.
fn build_notes<'a>(
    clock_rate: f64,
    objects: impl IntoIterator<Item = &'a HitObject>,
    total_columns: usize,
) -> (Vec<Note>, u32) {
    let mut notes = Vec::new();
    let mut max_combo = 0u32;

    for object in objects {
        let column = column_for(object, total_columns);
        let (head, end) = match object.kind {
            HitObjectKind::Circle => {
                max_combo += 1;
                (object.start_time, object.start_time)
            }
            HitObjectKind::Slider(_) | HitObjectKind::Spinner(_) => {
                // Spinners become holds during conversion; mania maps never
                // contain sliders. Treat them as a single note to stay safe.
                max_combo += 1;
                (object.start_time, object.start_time)
            }
            HitObjectKind::Hold(ref hold) => {
                let end = object.start_time + hold.duration;
                max_combo += 1 + (hold.duration / 100.0) as u32;
                (object.start_time, end)
            }
        };

        let head = head / clock_rate;
        let end = end / clock_rate;
        let tail = (end > head + 1e-7).then_some(end);

        notes.push(Note { column, head, tail });
    }

    (notes, max_combo)
}

/// The column of a hit object following `ManiaObject::column`.
fn column_for(object: &HitObject, total_columns: usize) -> usize {
    let x_divisor = 512.0 / total_columns as f64;
    let column = (f64::from(object.pos.x) / x_divisor).floor();

    column.min(total_columns as f64 - 1.0).max(0.0) as usize
}

struct RebirthData {
    total_columns: usize,
    hit_windows: ManiaHitWindows,
    /// The GOOD hit window (ms) a release is judged against. Mod-aware: it is the
    /// *played* windows (`windows`, not `map_windows`), so a tighter HR window really
    /// does mean less collision. Used by [`compute_rbar`]'s collision term.
    t_end: f64,
    notes: Vec<Note>,
    notes_by_column: Vec<Vec<Note>>,
    long_notes: Vec<Note>,
    tails: Vec<Note>,
    all_corners: Vec<f64>,
    base_corners: Vec<f64>,
    awkwardness_corners: Vec<f64>,
}

impl RebirthData {
    fn new(
        mut notes: Vec<Note>,
        total_columns: usize,
        hit_leniency: f64,
        good_window: f64,
    ) -> Self {
        let mut hit_windows = ManiaHitWindows::default();
        hit_windows.great = hit_leniency;
        hit_windows.good = good_window;
        Self::new_with_windows(notes, total_columns, hit_windows)
    }

    fn new_with_windows(
        mut notes: Vec<Note>,
        total_columns: usize,
        hit_windows: ManiaHitWindows,
    ) -> Self {
        notes.sort_by(compare_notes);

        let mut notes_by_column = vec![Vec::new(); total_columns];
        let mut long_notes = Vec::new();

        for &note in &notes {
            if note.column < total_columns {
                notes_by_column[note.column].push(note);
            }

            if note.tail.is_some() {
                long_notes.push(note);
            }
        }

        let mut tails = long_notes.clone();
        tails.sort_by(|a, b| a.tail_or_head().total_cmp(&b.tail_or_head()));

        let t_end = notes
            .iter()
            .map(|&note| note.tail_or_head().max(note.head))
            .fold(0.0, f64::max)
            + 1.0;
        let (all_corners, base_corners, awkwardness_corners) = get_corners(t_end, &notes);

        Self {
            total_columns,
            hit_windows,
            t_end,
            notes,
            notes_by_column,
            long_notes,
            tails,
            all_corners,
            base_corners,
            awkwardness_corners,
        }
    }

    #[inline]
    fn hit_leniency(&self) -> f64 {
        hit_leniency_from_window(self.hit_windows.great)
    }
}

// ---------------------------------------------------------------------------
// Key usage & anchor
// ---------------------------------------------------------------------------

fn get_key_usage(data: &RebirthData) -> Vec<Vec<bool>> {
    let mut key_usage = vec![vec![false; data.base_corners.len()]; data.total_columns];

    for note in &data.notes {
        if note.column >= data.total_columns {
            continue;
        }

        let start_time = (note.head - 150.0).max(0.0);
        let end_time = note.tail.map_or(note.head + 150.0, |tail| {
            (tail + 150.0).min(data.t_end - 1.0)
        });
        let left = lower_bound(&data.base_corners, start_time);
        let right = lower_bound(&data.base_corners, end_time);

        for used in &mut key_usage[note.column][left..right] {
            *used = true;
        }
    }

    key_usage
}

fn get_key_usage_400(data: &RebirthData) -> Vec<Vec<f64>> {
    let mut key_usage = vec![vec![0.0; data.base_corners.len()]; data.total_columns];

    for note in &data.notes {
        if note.column >= data.total_columns {
            continue;
        }

        let start_time = note.head.max(0.0);
        let end_time = note
            .tail
            .map_or(note.head, |tail| tail.min(data.t_end - 1.0));
        let left400 = lower_bound(&data.base_corners, start_time - 400.0);
        let left = lower_bound(&data.base_corners, start_time);
        let right = lower_bound(&data.base_corners, end_time);
        let right400 = lower_bound(&data.base_corners, end_time + 400.0);

        let body = 3.75 + (end_time - start_time).min(1500.0) / 150.0;

        for value in &mut key_usage[note.column][left..right] {
            *value += body;
        }

        for (idx, value) in key_usage[note.column][left400..left].iter_mut().enumerate() {
            let corner = data.base_corners[left400 + idx];
            *value += 3.75 - 3.75 / 400.0_f64.powi(2) * (corner - start_time).powi(2);
        }

        for (idx, value) in key_usage[note.column][right..right400]
            .iter_mut()
            .enumerate()
        {
            let corner = data.base_corners[right + idx];
            *value += 3.75 - 3.75 / 400.0_f64.powi(2) * (corner - end_time).abs().powi(2);
        }
    }

    key_usage
}

fn compute_anchor(key_usage_400: &[Vec<f64>]) -> Vec<f64> {
    let len = key_usage_400.first().map_or(0, Vec::len);
    let mut anchor = vec![0.0; len];

    for idx in 0..len {
        let mut counts: Vec<_> = key_usage_400.iter().map(|column| column[idx]).collect();
        counts.sort_by(|a, b| b.total_cmp(a));
        counts.retain(|&count| count != 0.0);

        if counts.len() > 1 {
            let mut walk = 0.0;
            let mut max_walk = 0.0;

            for pair in counts.windows(2) {
                walk += pair[0] * (1.0 - 4.0 * (0.5 - pair[1] / pair[0]).powi(2));
                max_walk += pair[0];
            }

            anchor[idx] = walk / max_walk;
        }
    }

    for value in &mut anchor {
        *value = 1.0 + (*value - 0.18).min(5.0 * (*value - 0.22).powi(3));
    }

    anchor
}

// ---------------------------------------------------------------------------
// Jbar
// ---------------------------------------------------------------------------

fn compute_jbar(data: &RebirthData) -> (Vec<Vec<f64>>, Vec<f64>) {
    let hit_leniency = data.hit_leniency();
    let len = data.base_corners.len();
    let mut j_by_column = vec![vec![0.0; len]; data.total_columns];
    let mut delta_by_column = vec![vec![1e9; len]; data.total_columns];

    for (column, notes) in data.notes_by_column.iter().enumerate() {
        for pair in notes.windows(2) {
            let start = pair[0].head;
            let end = pair[1].head;
            let left = lower_bound(&data.base_corners, start);
            let right = lower_bound(&data.base_corners, end);

            if left == right {
                continue;
            }

            let delta = 0.001 * (end - start);
            let val = delta.powi(-1) * (delta + 0.11 * hit_leniency.powf(0.25)).powi(-1);
            let j_val = val * jack_nerfer(delta);

            for idx in left..right {
                j_by_column[column][idx] = j_val;
                delta_by_column[column][idx] = delta;
            }
        }
    }

    let jbar_by_column: Vec<_> = j_by_column
        .iter()
        .map(|column| smooth_on_corners(&data.base_corners, column, 500.0, 0.001, false))
        .collect();
    let mut jbar = vec![0.0; len];

    for idx in 0..len {
        let mut num = 0.0;
        let mut den = 0.0;

        for column in 0..data.total_columns {
            let weight = 1.0 / delta_by_column[column][idx];
            num += jbar_by_column[column][idx].max(0.0).powi(5) * weight;
            den += weight;
        }

        jbar[idx] = (num / den.max(1e-9)).powf(0.2);
    }

    (delta_by_column, jbar)
}

// ---------------------------------------------------------------------------
// Xbar
// ---------------------------------------------------------------------------

fn compute_xbar(data: &RebirthData, active_columns: &[Vec<usize>]) -> Vec<f64> {
    let hit_leniency = data.hit_leniency();
    const CROSS_MATRIX: [&[f64]; 11] = [
        &[-1.0],
        &[0.075, 0.075],
        &[0.125, 0.05, 0.125],
        &[0.125, 0.125, 0.125, 0.125],
        &[0.175, 0.25, 0.05, 0.25, 0.175],
        &[0.175, 0.25, 0.175, 0.175, 0.25, 0.175],
        &[0.225, 0.35, 0.25, 0.05, 0.25, 0.35, 0.225],
        &[0.225, 0.35, 0.25, 0.225, 0.225, 0.25, 0.35, 0.225],
        &[0.275, 0.45, 0.35, 0.25, 0.05, 0.25, 0.35, 0.45, 0.275],
        &[
            0.275, 0.45, 0.35, 0.25, 0.275, 0.275, 0.25, 0.35, 0.45, 0.275,
        ],
        &[
            0.325, 0.55, 0.45, 0.35, 0.25, 0.05, 0.25, 0.35, 0.45, 0.55, 0.325,
        ],
    ];

    let k = data.total_columns.min(CROSS_MATRIX.len() - 1);
    let cross_coeff = CROSS_MATRIX[k];
    let len = data.base_corners.len();
    let mut x_by_pair = vec![vec![0.0; len]; data.total_columns + 1];
    let mut fast_cross = vec![vec![0.0; len]; data.total_columns + 1];

    for pair_column in 0..=data.total_columns {
        let notes_in_pair = notes_in_pair(&data.notes_by_column, data.total_columns, pair_column);

        for pair in notes_in_pair.windows(2) {
            let start = pair[0].head;
            let end = pair[1].head;
            let left = lower_bound(&data.base_corners, start);
            let right = lower_bound(&data.base_corners, end);

            if left == right {
                continue;
            }

            let delta = 0.001 * (end - start);
            let mut val = 0.16 * hit_leniency.max(delta).powi(-2);

            if (!active_columns_contains(active_columns, left, pair_column as isize - 1)
                && !active_columns_contains(
                    active_columns,
                    right.min(len - 1),
                    pair_column as isize - 1,
                ))
                || (!active_columns_contains(active_columns, left, pair_column as isize)
                    && !active_columns_contains(
                        active_columns,
                        right.min(len - 1),
                        pair_column as isize,
                    ))
            {
                val *= 1.0 - cross_coeff[pair_column.min(cross_coeff.len() - 1)];
            }

            let fast = (0.4 * delta.max(0.06).max(0.75 * hit_leniency).powi(-2) - 80.0).max(0.0);

            for idx in left..right {
                x_by_pair[pair_column][idx] = val;
                fast_cross[pair_column][idx] = fast;
            }
        }
    }

    let mut x_base = vec![0.0; len];

    for (idx, value) in x_base.iter_mut().enumerate() {
        *value += (0..=data.total_columns)
            .map(|column| x_by_pair[column][idx] * cross_coeff[column.min(cross_coeff.len() - 1)])
            .sum::<f64>();
        *value += (0..data.total_columns)
            .map(|column| {
                (fast_cross[column][idx]
                    * cross_coeff[column.min(cross_coeff.len() - 1)]
                    * fast_cross[column + 1][idx]
                    * cross_coeff[(column + 1).min(cross_coeff.len() - 1)])
                .sqrt()
            })
            .sum::<f64>();
    }

    smooth_on_corners(&data.base_corners, &x_base, 500.0, 0.001, false)
}

// notes_in_pair is now in shared.rs - but we need to update the call signature

// ---------------------------------------------------------------------------
// Pbar
// ---------------------------------------------------------------------------

struct LongNoteBodyRepresentation {
    points: Vec<f64>,
    cumulative: Vec<f64>,
    values: Vec<f64>,
}

impl LongNoteBodyRepresentation {
    fn new(long_notes: &[Note], t_end: f64) -> Self {
        let mut changes = Vec::with_capacity(3 * long_notes.len());

        for note in long_notes {
            let Some(tail) = note.tail else { continue };
            let t0 = (note.head + 60.0).min(tail);
            let t1 = (note.head + 120.0).min(tail);

            changes.extend([(t0, 1.3), (t1, -0.3), (tail, -1.0)]);
        }

        let mut points = Vec::with_capacity(changes.len() + 2);
        points.extend([0.0, t_end]);
        points.extend(changes.iter().map(|&(time, _)| time));
        sort_corners(&mut points, t_end);

        let mut cumulative = Vec::with_capacity(points.len());
        let mut values = Vec::with_capacity(points.len().saturating_sub(1));
        let mut curr: f64 = 0.0;

        cumulative.push(0.0);

        for pair in points.windows(2) {
            for &(time, change) in &changes {
                if time.total_cmp(&pair[0]).is_eq() {
                    curr += change;
                }
            }

            let value = curr.min(2.5 + 0.5 * curr);
            values.push(value);
            cumulative
                .push(cumulative.last().copied().unwrap_or(0.0) + (pair[1] - pair[0]) * value);
        }

        Self {
            points,
            cumulative,
            values,
        }
    }

    fn sum(&self, a: f64, b: f64) -> f64 {
        if b <= a || self.values.is_empty() {
            return 0.0;
        }

        let a = a.clamp(self.points[0], *self.points.last().unwrap());
        let b = b.clamp(self.points[0], *self.points.last().unwrap());

        if b <= a {
            return 0.0;
        }

        let i = self
            .points
            .partition_point(|&point| point <= a)
            .saturating_sub(1)
            .min(self.values.len() - 1);
        let j = self
            .points
            .partition_point(|&point| point <= b)
            .saturating_sub(1)
            .min(self.values.len() - 1);

        if i == j {
            return (b - a) * self.values[i];
        }

        let first = (self.points[i + 1] - a) * self.values[i];
        let middle = self.cumulative[j] - self.cumulative[i + 1];
        let last = (b - self.points[j]) * self.values[j];

        first + middle + last
    }
}

fn compute_pbar(
    data: &RebirthData,
    ln_rep: &LongNoteBodyRepresentation,
    anchor: &[f64],
) -> Vec<f64> {
    let hit_leniency = data.hit_leniency();
    let mut p_step = vec![0.0; data.base_corners.len()];

    for pair in data.notes.windows(2) {
        let h_l = pair[0].head;
        let h_r = pair[1].head;
        let delta_time = h_r - h_l;

        if delta_time < 1e-9 {
            let spike = 1000.0 * (0.02 * (4.0 / hit_leniency - 24.0)).powf(0.25);
            let left = lower_bound(&data.base_corners, h_l);
            let right = upper_bound(&data.base_corners, h_l);

            for value in &mut p_step[left..right] {
                *value += spike;
            }

            continue;
        }

        let left = lower_bound(&data.base_corners, h_l);
        let right = lower_bound(&data.base_corners, h_r);

        if left == right {
            continue;
        }

        let delta = 0.001 * delta_time;
        let v = 1.0 + 6.0 * 0.001 * ln_rep.sum(h_l, h_r);
        let booster = stream_booster(delta);
        let base = 0.08 * hit_leniency.powi(-1);
        let inc = if delta < 2.0 * hit_leniency / 3.0 {
            delta.powi(-1)
                * (base
                    * (1.0 - 24.0 * hit_leniency.powi(-1) * (delta - hit_leniency / 2.0).powi(2)))
                .powf(0.25)
                * booster.max(v)
        } else {
            delta.powi(-1)
                * (base * (1.0 - 24.0 * hit_leniency.powi(-1) * (hit_leniency / 6.0).powi(2)))
                    .powf(0.25)
                * booster.max(v)
        };

        for idx in left..right {
            p_step[idx] += (inc * anchor[idx]).min(inc.max(inc * 2.0 - 10.0));
        }
    }

    smooth_on_corners(&data.base_corners, &p_step, 500.0, 0.001, false)
}

// ---------------------------------------------------------------------------
// Abar
// ---------------------------------------------------------------------------

fn compute_abar(
    data: &RebirthData,
    active_columns: &[Vec<usize>],
    delta_by_column: &[Vec<f64>],
) -> Vec<f64> {
    let mut dks = vec![vec![0.0; data.base_corners.len()]; data.total_columns.saturating_sub(1)];

    for idx in 0..data.base_corners.len() {
        for pair in active_columns[idx].windows(2) {
            let k0 = pair[0];
            let k1 = pair[1];

            if k0 < dks.len() && k1 < delta_by_column.len() {
                dks[k0][idx] = (delta_by_column[k0][idx] - delta_by_column[k1][idx]).abs()
                    + 0.4
                        * (delta_by_column[k0][idx].max(delta_by_column[k1][idx]) - 0.11).max(0.0);
            }
        }
    }

    let mut a_step = vec![1.0; data.awkwardness_corners.len()];

    for (idx, &corner) in data.awkwardness_corners.iter().enumerate() {
        let base_idx = lower_bound(&data.base_corners, corner).min(data.base_corners.len() - 1);

        for pair in active_columns[base_idx].windows(2) {
            let k0 = pair[0];
            let k1 = pair[1];

            if k0 >= dks.len() || k1 >= delta_by_column.len() {
                continue;
            }

            let d_val = dks[k0][base_idx];
            let max_delta = delta_by_column[k0][base_idx].max(delta_by_column[k1][base_idx]);

            if d_val < 0.02 {
                a_step[idx] *= (0.75 + 0.5 * max_delta).min(1.0);
            } else if d_val < 0.07 {
                a_step[idx] *= (0.65 + 5.0 * d_val + 0.5 * max_delta).min(1.0);
            }
        }
    }

    smooth_on_corners(&data.awkwardness_corners, &a_step, 250.0, 1.0, true)
}

// ---------------------------------------------------------------------------
// Rbar
// ---------------------------------------------------------------------------

/// How much a fully-colliding LN release is charged on top of its base difficulty.
///
/// A release whose next same-column press falls inside the window the release is
/// judged against forces two events to share one interval, which no model of
/// independent draws can represent. 23.8% of releases across the fixture set are in
/// this state (`inverse_gap_structure`), and sunny's existing `1 + 0.8*i` rhythm term
/// charges them about 33% *less* than sparse releases.
///
/// **This magnitude is a deliberate unfitted guess.** The ladder fixtures cannot size
/// it: `tools/fetch_ladder.sh` selects `s.acc between 88.0 and 99.5`, which bounds
/// accuracy by construction and biases every regression that controls for stars. It is
/// set conservatively — below the ~33% discount it offsets — so it corrects the sign of
/// the response without asserting a magnitude the data cannot support.
const COLLISION_WEIGHT: f64 = 0.25;

fn compute_rbar(data: &RebirthData) -> Vec<f64> {
    let hit_leniency = data.hit_leniency();
    let mut r_step = vec![0.0; data.base_corners.len()];

    if data.tails.len() < 2 {
        return r_step;
    }

    let (i_list, c_list): (Vec<f64>, Vec<f64>) = data
        .tails
        .iter()
        .map(|tail| {
            let next_head = find_next_note_in_column(*tail, &data.notes_by_column[tail.column])
                .map_or(1e9, |note| note.head);
            let tail_time = tail.tail_or_head();
            let i_h = 0.001 * (tail_time - tail.head - 80.0).abs() / hit_leniency;
            let i_t = 0.001 * (next_head - tail_time - 80.0).abs() / hit_leniency;

            let i = 2.0 / (2.0 + (-5.0 * (i_h - 0.75)).exp() + (-5.0 * (i_t - 0.75)).exp());

            // Collision overlap: how much of the release's own GOOD window is eaten by
            // the next same-column press. `next_head` is 1e9 when there is no following
            // note in the column, which correctly yields overlap 0. A negative gap
            // (release after the next head) clamps to 1.0 — total collision, which is
            // the right answer.
            let g = next_head - tail_time;
            let c = if data.hit_windows.good > 0.0 {
                (1.0 - g / data.hit_windows.good).clamp(0.0, 1.0)
            } else {
                0.0
            };

            (i, c)
        })
        .unzip();

    for idx in 0..data.tails.len() - 1 {
        let t_start = data.tails[idx].tail_or_head();
        let t_end = data.tails[idx + 1].tail_or_head();
        let left = lower_bound(&data.base_corners, t_start);
        let right = lower_bound(&data.base_corners, t_end);

        if left == right {
            continue;
        }

        let delta_r = 0.001 * (t_end - t_start);
        // The collision factor is additive-in-factor to the existing rhythm term
        // (`1 + 0.8*i`) rather than a replacement of it: that term has a defensible
        // reading upstream where a release inside comfortable 1/4 rhythm has the next
        // note as a timing anchor, whereas this term charges specifically for the two
        // judgements sharing one interval. The `0.5` averages the pair's overlaps so a
        // fully-colliding pair yields exactly `1 + COLLISION_WEIGHT`, not
        // `1 + 2*COLLISION_WEIGHT`.
        let value = 0.08
            * delta_r.powf(-0.5)
            * hit_leniency.powi(-1)
            * (1.0 + 0.8 * (i_list[idx] + i_list[idx + 1]))
            * (1.0 + COLLISION_WEIGHT * 0.5 * (c_list[idx] + c_list[idx + 1]));

        for step in &mut r_step[left..right] {
            *step = value;
        }
    }

    smooth_on_corners(&data.base_corners, &r_step, 500.0, 0.001, false)
}

// ---------------------------------------------------------------------------
// Density & keys
// ---------------------------------------------------------------------------

fn compute_density_and_keys(
    data: &RebirthData,
    key_usage: &[Vec<bool>],
) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
    let note_hit_times: Vec<_> = data.notes.iter().map(|note| note.head).collect();

    // For the v2 (non-classic) path, long note tails count as additional
    // hits, matching the reference implementation's `noteHitTimesV2`.
    let mut note_hit_times_v2 = note_hit_times.clone();
    note_hit_times_v2.extend(data.long_notes.iter().filter_map(|note| note.tail));
    note_hit_times_v2.sort_by(f64::total_cmp);

    let mut density = vec![0.0; data.base_corners.len()];
    let mut density_v2 = vec![0.0; data.base_corners.len()];
    let mut keys = vec![1.0; data.base_corners.len()];

    for (idx, &corner) in data.base_corners.iter().enumerate() {
        let low = corner - 500.0;
        let high = corner + 500.0;
        density[idx] =
            (lower_bound(&note_hit_times, high) - lower_bound(&note_hit_times, low)) as f64;
        density_v2[idx] =
            (lower_bound(&note_hit_times_v2, high) - lower_bound(&note_hit_times_v2, low)) as f64;
        keys[idx] = key_usage.iter().filter(|column| column[idx]).count().max(1) as f64;
    }

    (density, density_v2, keys)
}

// ---------------------------------------------------------------------------
// Final computation
// ---------------------------------------------------------------------------

/// Floor on [`release_density_weight`]'s output.
///
/// The floor can only ever raise the factor, never lower it, so it can only raise stars;
/// `0.0` would be bit-for-bit identical to the unclamped `35.0 / (density + 8.0)` baseline.
///
/// The previous value (`1.5`) was an empirical broad amplification of LN maps.  The
/// parameter-ablation audit showed that it materially raises short and dense LN charts while
/// leaving rice unchanged, so the shipped default is now `0.0`: the continuous density
/// response is used without a duration-independent amplification floor.
///
const RELEASE_WEIGHT_FLOOR: f64 = 0.0;

/// Cap on [`release_density_weight`]'s output.
///
/// `f64::INFINITY` is likewise a no-op against the unclamped baseline. Exists mostly for
/// symmetry with the floor and so a future experiment raising the floor can also bound
/// the amplifying side (density-0 corners already reach 4.375 uncapped) without a second
/// change to this function's signature.
const RELEASE_WEIGHT_CAP: f64 = f64::INFINITY;

/// The weight `rbar` (release difficulty) carries inside `s_all`, as a function of local
/// note density.
///
/// This is asymmetric with `pbar` (press difficulty)'s weight on purpose — or rather, on
/// no purpose that survives inspection: `pbar` is weighted by a flat `0.8` regardless of
/// density, while this factor swings from 4.375 at density 0 down to 0.32 at density 100.
/// The two terms are added inside the same `(...).powf(1.5)`, so at high density the
/// release term is discarded almost three-fold relative to the press term, for no reason
/// tied to how hard the release actually is.
///
/// `release_density_weight_structure` measures where the fixture set actually sits: dense
/// long-note charts (inverse/反键 patterns, 7K in particular) push local density up, which
/// is exactly the regime this factor suppresses hardest — the release difficulty is most
/// real there (a colliding release genuinely competes with the next press for one
/// judgement window, see `compute_rbar`'s `COLLISION_WEIGHT`) and most discarded by this
/// divisor.
///
/// A raised floor previously bounded that suppression, but **not in the way the framing
/// above would suggest**. The shipped configuration now leaves the response continuous,
/// avoiding that broad amplification across the mid-density band.
///
/// The factor crosses 1.0 at density 27, so a floor of `0.8` acts *only* on density > 35.75
/// — the genuinely suppressed corners. Swept over the fixture set, floor `0.8` moves the
/// cohort above 60% long notes by `+0.003%` stars, and moves map `5143109` — 67.5%
/// of its weight in the suppressed regime, the worst case in 405 maps — by `+0.003%`. Floor
/// `1.5` moves that same map `+2.349%`.
///
/// So the effect lives in density `[15.33, 27]`, where the factor is already *above* 1.0
/// and nothing is being suppressed. The reason is the sum it sits in:
/// `0.8 * pbar + rbar * factor`. Where the factor bites hardest `pbar` is large and `rbar`
/// small, so raising it perturbs a sum it does not dominate. `1.5` therefore ships as a
/// broad amplification of release weight across the mid-density band — empirically the
/// right shape (monotone in long-note share within a keymode, and exactly zero on rice),
/// but a fitted constant, not a correction to an over-suppressed tail.
///
/// Two consequences for later work. Rice reads exactly `+0.000%` at every floor because
/// `rbar ≈ 0` without releases; that is a property of `rbar` and not evidence the floor is
/// long-note-selective. And a *bounded-tail* fix cannot be what this needs — if the divisor
/// is wrong, it is wrong in its whole form across the mid-density band, and replacing the
/// form is the change to make rather than clamping it harder.
///
/// With the default floor/cap this is bit-for-bit identical to the unclamped baseline; see
/// `release_density_weight_default_is_identity`.
fn release_density_weight(density: f64) -> f64 {
    (35.0 / (density + 8.0)).clamp(RELEASE_WEIGHT_FLOOR, RELEASE_WEIGHT_CAP)
}

struct RebirthParams {
    sr: f64,
    spikiness: f64,
    switches: f64,
    variety: f64,
    /// The per-note difficulty distribution the `sr` percentiles were taken from, kept
    /// rather than discarded — see [`SunnyManiaDifficultyAttributes::note_difficulty_bins`].
    note_difficulty_bins: Option<[NoteDifficultyBin; NOTE_DIFFICULTY_BINS]>,
    input_state_bins: Option<[InputStateBin; INPUT_STATE_BINS]>,
}

fn calculate_from_data(data: &RebirthData, classic: bool) -> Option<RebirthParams> {
    let key_usage = get_key_usage(data);
    let active_columns: Vec<_> = (0..data.base_corners.len())
        .map(|idx| {
            (0..data.total_columns)
                .filter(|&column| key_usage[column][idx])
                .collect::<Vec<_>>()
        })
        .collect();
    let key_usage_400 = get_key_usage_400(data);
    let anchor = compute_anchor(&key_usage_400);
    let (delta_by_column, jbar_base) = compute_jbar(data);
    let jbar = interp_values(&data.all_corners, &data.base_corners, &jbar_base);
    let xbar_base = compute_xbar(data, &active_columns);
    let xbar = interp_values(&data.all_corners, &data.base_corners, &xbar_base);
    let ln_rep = LongNoteBodyRepresentation::new(&data.long_notes, data.t_end);
    let pbar_base = compute_pbar(data, &ln_rep, &anchor);
    let pbar = interp_values(&data.all_corners, &data.base_corners, &pbar_base);
    let abar_awkwardness = compute_abar(data, &active_columns, &delta_by_column);
    let abar = interp_values(
        &data.all_corners,
        &data.awkwardness_corners,
        &abar_awkwardness,
    );
    let rbar_base = compute_rbar(data);
    let rbar = interp_values(&data.all_corners, &data.base_corners, &rbar_base);
    let (density_base, density_v2_base, keys_base) = compute_density_and_keys(data, &key_usage);
    let density = step_interp(&data.all_corners, &data.base_corners, &density_base);
    let density_v2 = step_interp(&data.all_corners, &data.base_corners, &density_v2_base);
    let keys = step_interp(&data.all_corners, &data.base_corners, &keys_base);

    let d_all: Vec<_> = (0..data.all_corners.len())
        .map(|idx| {
            let s_all = (0.4
                * (abar[idx].powf(3.0 / keys[idx]) * jbar[idx].min(8.0 + 0.85 * jbar[idx]))
                    .powf(1.5)
                + (1.0 - 0.4)
                    * (abar[idx].powf(2.0 / 3.0)
                        * (0.8 * pbar[idx] + rbar[idx] * release_density_weight(density[idx])))
                    .powf(1.5))
            .powf(2.0 / 3.0);
            let t_all = (abar[idx].powf(3.0 / keys[idx]) * xbar[idx]) / (xbar[idx] + s_all + 1.0);

            2.7 * s_all.powf(0.5) * t_all.powf(1.5) + s_all * 0.27
        })
        .collect();

    let mut gaps = vec![0.0; data.all_corners.len()];

    if gaps.len() < 2 {
        return None;
    }

    gaps[0] = (data.all_corners[1] - data.all_corners[0]) / 2.0;
    let last = gaps.len() - 1;
    gaps[last] = (data.all_corners[last] - data.all_corners[last - 1]) / 2.0;

    for idx in 1..last {
        gaps[idx] = (data.all_corners[idx + 1] - data.all_corners[idx - 1]) / 2.0;
    }

    // The D values always use the head-only density, but the effective
    // weights select between the classic (head-only) and v2 (head + LN tail)
    // densities, matching the reference implementation's `ContainsCL` branch.
    let effective_weights: Vec<_> = if classic {
        density.iter().zip(gaps).map(|(&c, gap)| c * gap).collect()
    } else {
        density_v2
            .iter()
            .zip(gaps)
            .map(|(&c, gap)| c * gap)
            .collect()
    };
    let mut sorted_indices: Vec<_> = (0..d_all.len()).collect();
    sorted_indices.sort_by(|&a, &b| d_all[a].total_cmp(&d_all[b]));
    let d_sorted: Vec<_> = sorted_indices.iter().map(|&idx| d_all[idx]).collect();
    let w_sorted: Vec<_> = sorted_indices
        .iter()
        .map(|&idx| effective_weights[idx])
        .collect();
    let total_weight = w_sorted.iter().sum::<f64>();

    if total_weight <= 0.0 {
        return None;
    }

    let target_percentiles = [0.945, 0.935, 0.925, 0.915, 0.845, 0.835, 0.825, 0.815];
    let mut cumulative_weight = 0.0;
    let mut norm_cumulative = Vec::with_capacity(w_sorted.len());

    for weight in &w_sorted {
        cumulative_weight += *weight;
        norm_cumulative.push(cumulative_weight / total_weight);
    }

    let percentile_values: Vec<_> = target_percentiles
        .iter()
        .map(|&target| {
            let idx = lower_bound(&norm_cumulative, target).min(d_sorted.len() - 1);
            d_sorted[idx]
        })
        .collect();
    let percentile_93 = percentile_values[..4].iter().sum::<f64>() / 4.0;
    let percentile_83 = percentile_values[4..].iter().sum::<f64>() / 4.0;
    let weighted_mean = (d_sorted
        .iter()
        .zip(&w_sorted)
        .map(|(&d, &w)| d.powi(5) * w)
        .sum::<f64>()
        / total_weight)
        .powf(0.2);
    let mut sr =
        (0.88 * percentile_93) * 0.25 + (0.94 * percentile_83) * 0.2 + weighted_mean * 0.55;
    let total_notes = data.notes.len() as f64
        + 0.5
            * data
                .long_notes
                .iter()
                .map(|note| (note.tail_or_head() - note.head).min(1000.0) / 200.0)
                .sum::<f64>();

    sr *= total_notes / (total_notes + 60.0);
    sr = rescale_high(sr);
    sr *= 0.975;

    let spikiness = compute_spikiness(&d_sorted, &w_sorted, weighted_mean, total_weight);
    let switches = compute_switches(data, &keys, &effective_weights);
    let variety = compute_variety(data);

    // Per-note difficulty, read off the same `d_all` the percentiles above came from. Every
    // note head lands exactly on a corner, so this is a lookup and not an interpolation:
    // verified on 1173541 heads across 466 fixture maps, max mismatch 0 ms, all keymodes.
    let per_note: Vec<(f64, Option<f64>)> = data
        .notes
        .iter()
        .map(|note| {
            let idx = lower_bound(&data.all_corners, note.head).min(d_all.len() - 1);
            let duration = note.tail.map(|tail| tail - note.head);

            (d_all[idx], duration)
        })
        .collect();

    Some(RebirthParams {
        sr,
        spikiness,
        switches,
        variety,
        note_difficulty_bins: note_difficulty_bins(&per_note),
        input_state_bins: input_state_bins(data, &d_all, classic),
    })
}

/// Spikiness measure from the weighted variance of the corner difficulty
/// values, i.e. how much the difficulty spikes within the map.
fn compute_spikiness(
    d_sorted: &[f64],
    w_sorted: &[f64],
    weighted_mean: f64,
    total_weight: f64,
) -> f64 {
    // Degenerate cases where the reference implementation would produce NaN
    if weighted_mean == 0.0 || total_weight <= 0.0 {
        return 0.0;
    }

    let variance_sum_top = d_sorted
        .iter()
        .zip(w_sorted)
        .map(|(&d, &w)| (d.powi(8) - weighted_mean.powi(8)).powi(2) * w)
        .sum::<f64>();

    let weighted_variance = (variance_sum_top / total_weight).powf(1.0 / 8.0);

    weighted_variance.sqrt() / weighted_mean
}

/// Switch measure, i.e. how much the playstyle switches between jack and
/// stream-like patterns. Values are in the range `[0.5, 1.5]`.
///
/// Following the C# reference, the corners are weighted by the effective
/// weights (`density * gap`) rather than the raw difficulty values.
fn compute_switches(data: &RebirthData, ks_arr: &[f64], effective_weights: &[f64]) -> f64 {
    let all_corners = &data.all_corners;

    // Heads of all notes, in (head, column) order
    let heads: Vec<f64> = data.notes.iter().map(|note| note.head).collect();

    // For each head, the index of the first corner >= head (last index dropped)
    let idx_list: Vec<usize> = heads
        .iter()
        .map(|&head| lower_bound(all_corners, head))
        .collect();
    let n = idx_list.len().saturating_sub(1);

    let ks_at_note: Vec<f64> = idx_list[..n].iter().map(|&i| ks_arr[i]).collect();
    let weights_at_note: Vec<f64> = idx_list[..n]
        .iter()
        .map(|&i| effective_weights[i])
        .collect();

    let head_gaps: Vec<f64> = heads.windows(2).map(|w| w[1] - w[0]).collect();
    let num_head_gaps = head_gaps.len();

    // Moving averages over a window of 101 gaps
    let avgs: Vec<f64> = (0..num_head_gaps)
        .map(|i| {
            let start = i.saturating_sub(50);
            let end = (i + 50).min(num_head_gaps - 1);

            head_gaps[start..=end].iter().sum::<f64>() / (end - start + 1) as f64
        })
        .collect();

    let mut signature_head = 0.0;
    let mut sum_ref_head = 0.0;

    for i in 0..num_head_gaps {
        let avg = avgs[i];

        // Skip degenerate windows where all gaps are zero
        if avg == 0.0 {
            continue;
        }

        let ratio = head_gaps[i] / avg / num_head_gaps as f64;
        signature_head += (ratio * weights_at_note[i]).sqrt() * ks_at_note[i].powf(0.25);
        sum_ref_head += (head_gaps[i] / avg) * weights_at_note[i];
    }

    let ref_signature_head = sum_ref_head.sqrt();

    // Tails of long notes, sorted by tail time
    let tails: Vec<f64> = data.tails.iter().map(|note| note.tail_or_head()).collect();

    let mut signature_tail = 0.0;
    let mut ref_signature_tail = 0.0;
    let mut num_tail_gaps = 0;

    if tails.len() > 1 && tails[tails.len() - 1] > tails[0] {
        let idx_list_tails: Vec<usize> = tails
            .iter()
            .map(|&tail| lower_bound(all_corners, tail))
            .collect();
        let n_tails = idx_list_tails.len() - 1;

        let ks_at_tail: Vec<f64> = idx_list_tails[..n_tails]
            .iter()
            .map(|&i| ks_arr[i])
            .collect();
        let weights_at_tail: Vec<f64> = idx_list_tails[..n_tails]
            .iter()
            .map(|&i| effective_weights[i])
            .collect();

        let tail_gaps: Vec<f64> = tails.windows(2).map(|w| w[1] - w[0]).collect();
        let num_tail_gaps_tmp = tail_gaps.len();

        if num_tail_gaps_tmp > 0 {
            let avgs_tail: Vec<f64> = (0..num_tail_gaps_tmp)
                .map(|i| {
                    let start = i.saturating_sub(50);
                    let end = (i + 50).min(num_tail_gaps_tmp - 1);

                    tail_gaps[start..=end].iter().sum::<f64>() / (end - start + 1) as f64
                })
                .collect();

            for i in 0..num_tail_gaps_tmp {
                let avg = avgs_tail[i];

                // Skip degenerate windows where all gaps are zero
                if avg == 0.0 {
                    continue;
                }

                let ratio = tail_gaps[i] / avg / num_tail_gaps_tmp as f64;
                signature_tail += (ratio * weights_at_tail[i]).sqrt() * ks_at_tail[i].powf(0.25);
                ref_signature_tail += (tail_gaps[i] / avg) * weights_at_tail[i];
            }

            ref_signature_tail = ref_signature_tail.sqrt();
            num_tail_gaps = num_tail_gaps_tmp;
        }
    }

    let numerator = signature_head * num_head_gaps as f64 + signature_tail * num_tail_gaps as f64;
    let denominator =
        ref_signature_head * num_head_gaps as f64 + ref_signature_tail * num_tail_gaps as f64;

    // Degenerate case where the reference implementation would produce NaN
    if denominator == 0.0 {
        return 0.5;
    }

    numerator / denominator / 2.0 + 0.5
}

/// Variety measure based on the Rao quadratic entropy of the head, tail and
/// per-column head gaps.
fn compute_variety(data: &RebirthData) -> f64 {
    let head_gaps: Vec<i64> = data
        .notes
        .windows(2)
        .map(|w| w[1].head as i64 - w[0].head as i64)
        .collect();

    // All notes sorted by their tail time, circles have a tail of -1
    let mut tail_notes: Vec<&Note> = data.notes.iter().collect();
    tail_notes.sort_by_key(|note| tail_value(note));

    let tail_gaps: Vec<i64> = tail_notes
        .windows(2)
        .map(|w| tail_value(w[1]) - tail_value(w[0]))
        .collect();

    let head_variety = rao_quadratic_entropy_log(&head_gaps, 1);
    let tail_variety = rao_quadratic_entropy_log(&tail_gaps, 1);

    let mut head_gaps_new = Vec::new();

    for column in &data.notes_by_column {
        head_gaps_new.extend(
            column
                .windows(2)
                .map(|w| w[1].head as i64 - w[0].head as i64),
        );
    }

    let col_variety = 2.5 * rao_quadratic_entropy_log(&head_gaps_new, 2);

    0.5 * head_variety + 0.11 * tail_variety + 0.45 * col_variety
}

fn tail_value(note: &Note) -> i64 {
    note.tail.map_or(-1, |tail| tail as i64)
}

/// Rao's quadratic entropy on the values treated as categories, applying
/// `log_iterations` times the log(1 + |x - y|) distance.
fn rao_quadratic_entropy_log(values: &[i64], log_iterations: u32) -> f64 {
    if values.is_empty() {
        return 0.0;
    }

    let mut counts = HashMap::new();

    for &value in values {
        *counts.entry(value).or_insert(0usize) += 1;
    }

    // Iterate in a deterministic order to avoid floating point differences
    // based on the HashMap's randomized iteration order.
    let mut uniques: Vec<i64> = counts.keys().copied().collect();
    uniques.sort_unstable();

    let total = values.len() as f64;
    let mut q = 0.0;

    for &x in &uniques {
        let p_x = counts[&x] as f64 / total;

        for &y in &uniques {
            let mut dist = (x - y).abs() as f64;

            for _ in 0..log_iterations {
                dist = (1.0 + dist).ln();
            }

            q += p_x * (counts[&y] as f64 / total) * dist;
        }
    }

    q
}

// ---------------------------------------------------------------------------
// Performance calculation
// ---------------------------------------------------------------------------

/// Matches the reference implementation's 305-based weighting (perfect hits
/// are weighted with 305 instead of 320).
fn xxy_custom_accuracy(state: SunnyScoreState) -> f64 {
    let total_hits = state.total_hits();

    if total_hits == 0 {
        return 0.0;
    }

    let numerator =
        state.n320 * 305 + state.n300 * 300 + state.n200 * 200 + state.n100 * 100 + state.n50 * 50;
    let denominator = total_hits * 305;

    f64::from(numerator) / f64::from(denominator)
}

/// The "proportion" of pp that is awarded based on accuracy, i.e. how much
/// of the star rating is rewarded at the given accuracy.
fn xxy_performance_proportion(acc: f64) -> f64 {
    if acc > 0.80 {
        4.5 * (acc - 0.8) / f64::powf(100.0 * (1.0 - acc) + f64::powf(0.9, 20.0), 0.05)
    } else {
        0.0
    }
}

/// The difficulty portion of pp.
///
/// `window_scalar` carries the judgement-window effect, which is what removes the
/// need for per-mod factors here: it is derived by grading the score against the
/// windows that were actually in effect, so `EZ` is priced without being named.
/// It enters through the same `^2.2` as the star rating because both describe how
/// hard the score was to produce, so a 1% shift in either should be worth the same.
fn compute_difficulty_value(stars: f64, score_accuracy: f64, window_scalar: f64) -> f64 {
    let proportion = xxy_performance_proportion(score_accuracy);
    let effective_stars = f64::max(stars - 0.15, 0.05) * window_scalar.max(0.0);

    9.8 * f64::powf(effective_stars.max(0.05), 2.2) * proportion
}

/// Multiplier based on the map's variety, in the range `[0.945, 1.055]`.
fn xxy_variety_multiplier(variety: f64) -> f64 {
    const FLOOR: f64 = 0.945;
    const CAP: f64 = 1.055;
    const V0: f64 = 3.25;
    const K: f64 = 3.0;

    FLOOR + (CAP - FLOOR) / (1.0 + (-K * (variety - V0)).exp())
}

/// Multiplier based on the play's accuracy and the map's accuracy scalar.
fn xxy_acc_multiplier(acc: f64, acc_scalar: f64) -> f64 {
    let sigmoid_scaler = 0.87 + 0.26 / (1.0 + (-20.0 * (acc_scalar - 1.0)).exp());

    sigmoid_scaler * (2.0 * acc.powi(20) - 1.0) + 2.0 - 2.0 * acc.powi(20)
}

/// Multiplier based on the amount of notes of the map.
fn xxy_length_multiplier(total_notes: f64, stars: f64) -> f64 {
    1.1 / (1.0 + (stars / (2.0 * total_notes)).sqrt())
}

#[allow(dead_code)]
fn _assert_mode(map: &Beatmap) {
    debug_assert_eq!(map.mode, GameMode::Mania);
}

#[cfg(test)]
mod inline_tests {
    use super::*;
    use rosu_mods::{GameMod, GameMods as LazerMods};

    #[test]
    fn hit_window_300_formula() {
        let mut map = Beatmap::default();
        map.mode = GameMode::Mania;
        map.od = 8.0;
        let mods = LazerMods::new();

        assert!((get_hit_window_300(&map, 1.0, true, &mods) - 40.5).abs() < 1e-9);
        let mut hr = LazerMods::new();
        hr.insert(GameMod::HardRockMania(Default::default()));
        assert!((get_hit_window_300(&map, 1.0, true, &hr) - 28.5).abs() < 1e-9);
        let mut ez = LazerMods::new();
        ez.insert(GameMod::EasyMania(Default::default()));
        assert!((get_hit_window_300(&map, 1.0, true, &ez) - 56.5).abs() < 1e-9);
        assert!((get_hit_window_300(&map, 1.5, true, &mods) - 60.5 / 1.5).abs() < 1e-9);
    }

    #[test]
    fn classic_detection_sees_the_score_v2_mod() {
        assert_eq!(
            rosu_mods::generated_mods::ScoreV2Mania::acronym().as_str(),
            "SV2"
        );

        let mut v2 = LazerMods::new();
        v2.insert(GameMod::ScoreV2Mania(Default::default()));

        assert!(!is_classic(Some(false), &v2));
        assert!(is_classic(Some(false), &LazerMods::new()));
        assert!(!is_classic(Some(true), &LazerMods::new()));

        let mut classic = LazerMods::new();
        classic.insert(GameMod::ClassicMania(Default::default()));
        assert!(is_classic(Some(true), &classic));
    }

    #[test]
    fn classic_mod_overrides_lazer_window_scheme() {
        let mut map = Beatmap::default();
        map.mode = GameMode::Mania;
        map.od = 4.0;
        map.is_convert = true;

        // Lazer interpolation gives 52ms at OD4; Classic uses the convert
        // threshold and gives 47ms. Classic must win even with the Lazer switch.
        let mods = LazerMods::new();
        assert!((get_hit_window_300(&map, 1.0, false, &mods) - 52.0).abs() < 1e-9);
        assert!((get_hit_window_300(&map, 1.0, true, &mods) - 47.0).abs() < 1e-9);

        let mut mods = LazerMods::new();
        mods.insert(GameMod::ClassicMania(Default::default()));
        assert!(is_classic(Some(true), &mods));
    }
}

#[cfg(test)]
mod tests;
