//! Shared utilities for rebirth and sunny difficulty calculations.
//!
//! This module contains pure utility functions and data structures used by both
//! the rebirth and sunny algorithms to avoid code duplication.

use std::cmp::Ordering;

/// A single mania note (or hold-note) extracted from a beatmap.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Note {
    pub column: usize,
    pub head: f64,
    pub tail: Option<f64>,
}

impl Note {
    pub fn tail_or_head(self) -> f64 {
        self.tail.unwrap_or(self.head)
    }
}

/// Compare notes by head time, then by column.
pub(crate) fn compare_notes(a: &Note, b: &Note) -> Ordering {
    a.head
        .total_cmp(&b.head)
        .then_with(|| a.column.cmp(&b.column))
}

/// Convert a GREAT hit window to hit leniency.
pub(crate) fn hit_leniency_from_window(great_hit_window: f64) -> f64 {
    let x = 0.3 * (great_hit_window / 500.0).sqrt();
    x.min(0.6 * (x - 0.09) + 0.09)
}

/// Binary search for the first index where `values[i] >= target`.
pub(crate) fn lower_bound(values: &[f64], target: f64) -> usize {
    values.partition_point(|&value| value < target)
}

/// Binary search for the first index where `values[i] > target`.
pub(crate) fn upper_bound(values: &[f64], target: f64) -> usize {
    values.partition_point(|&value| value <= target)
}

/// Generate corner timestamps for difficulty calculation.
pub(crate) fn get_corners(t_end: f64, notes: &[Note]) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
    let mut base = Vec::new();
    let mut awkwardness = Vec::new();

    for note in notes {
        let boundaries = [Some(note.head), note.tail];

        for boundary in boundaries.into_iter().flatten() {
            base.extend([boundary, boundary + 501.0, boundary - 499.0, boundary + 1.0]);
            awkwardness.extend([boundary, boundary + 1000.0, boundary - 1000.0]);
        }
    }

    base.extend([0.0, t_end]);
    awkwardness.extend([0.0, t_end]);

    sort_corners(&mut base, t_end);
    sort_corners(&mut awkwardness, t_end);

    let mut all = Vec::with_capacity(base.len() + awkwardness.len());
    all.extend_from_slice(&base);
    all.extend_from_slice(&awkwardness);
    sort_corners(&mut all, t_end);

    (all, base, awkwardness)
}

/// Sort and deduplicate corners, keeping only those in `[0, t_end]`.
pub(crate) fn sort_corners(corners: &mut Vec<f64>, t_end: f64) {
    corners.retain(|&corner| (0.0..=t_end).contains(&corner));
    corners.sort_by(f64::total_cmp);
    corners.dedup_by(|a, b| a.total_cmp(b).is_eq());
}

/// Compute cumulative sum for piecewise-constant function `f` on grid `x`.
pub(crate) fn cumulative_sum(x: &[f64], f: &[f64]) -> Vec<f64> {
    let mut cumulative = vec![0.0; x.len()];

    for i in 1..x.len() {
        cumulative[i] = cumulative[i - 1] + f[i - 1] * (x[i] - x[i - 1]);
    }

    cumulative
}

/// Query cumulative sum at point `q`.
pub(crate) fn query_cumsum(q: f64, x: &[f64], cumulative: &[f64], f: &[f64]) -> f64 {
    let Some((&first, &last)) = x.first().zip(x.last()) else {
        return 0.0;
    };

    if q <= first {
        return 0.0;
    }

    if q >= last {
        return cumulative.last().copied().unwrap_or(0.0);
    }

    let i = x.partition_point(|&value| value < q).saturating_sub(1);

    cumulative[i] + f[i] * (q - x[i])
}

/// Smooth piecewise-constant function `f` on grid `x` using a sliding window.
pub(crate) fn smooth_on_corners(x: &[f64], f: &[f64], window: f64, scale: f64, average: bool) -> Vec<f64> {
    let Some((&first, &last)) = x.first().zip(x.last()) else {
        return Vec::new();
    };

    let cumulative = cumulative_sum(x, f);

    x.iter()
        .map(|&s| {
            let a = (s - window).max(first);
            let b = (s + window).min(last);
            let val = query_cumsum(b, x, &cumulative, f) - query_cumsum(a, x, &cumulative, f);

            if average {
                if b > a { val / (b - a) } else { 0.0 }
            } else {
                scale * val
            }
        })
        .collect()
}

/// Linearly interpolate values from `old_x` grid to `new_x` grid.
pub(crate) fn interp_values(new_x: &[f64], old_x: &[f64], old_vals: &[f64]) -> Vec<f64> {
    if old_x.is_empty() || old_vals.is_empty() {
        return vec![0.0; new_x.len()];
    }

    new_x
        .iter()
        .map(|&x| {
            if x <= old_x[0] {
                return old_vals[0];
            }

            let last = old_x.len() - 1;

            if x >= old_x[last] {
                return old_vals[last];
            }

            let right = old_x.partition_point(|&value| value < x);
            let left = right - 1;
            let width = old_x[right] - old_x[left];

            if width == 0.0 {
                old_vals[left]
            } else {
                let t = (x - old_x[left]) / width;
                old_vals[left] + (old_vals[right] - old_vals[left]) * t
            }
        })
        .collect()
}

/// Step interpolation (right-continuous): value at `x` is the value at the
/// largest grid point `<= x`.
pub(crate) fn step_interp(new_x: &[f64], old_x: &[f64], old_vals: &[f64]) -> Vec<f64> {
    if old_x.is_empty() || old_vals.is_empty() {
        return vec![0.0; new_x.len()];
    }

    new_x
        .iter()
        .map(|&x| {
            let idx = old_x
                .partition_point(|&value| value <= x)
                .saturating_sub(1)
                .min(old_vals.len() - 1);

            old_vals[idx]
        })
        .collect()
}

/// Check if `active_columns[idx]` contains the given column.
pub(crate) fn active_columns_contains(active_columns: &[Vec<usize>], idx: usize, column: isize) -> bool {
    usize::try_from(column).is_ok_and(|column| active_columns[idx].contains(&column))
}

/// Get all notes in a column pair (for cross-column pattern detection).
pub(crate) fn notes_in_pair(notes_by_column: &[Vec<Note>], total_columns: usize, pair_column: usize) -> Vec<Note> {
    match pair_column {
        0 => notes_by_column.first().cloned().unwrap_or_default(),
        column if column == total_columns => {
            notes_by_column.last().cloned().unwrap_or_default()
        }
        column => {
            let mut notes = Vec::with_capacity(
                notes_by_column[column - 1].len() + notes_by_column[column].len(),
            );
            notes.extend_from_slice(&notes_by_column[column - 1]);
            notes.extend_from_slice(&notes_by_column[column]);
            notes.sort_by(compare_notes);
            notes
        }
    }
}

/// Find the next note in the same column after the given note.
pub(crate) fn find_next_note_in_column(note: Note, notes: &[Note]) -> Option<Note> {
    let idx = notes.partition_point(|candidate| candidate.head < note.head);

    notes.get(idx + 1).copied()
}

/// Jack nerfer factor: reduces jack difficulty slightly.
pub(crate) fn jack_nerfer(delta: f64) -> f64 {
    1.0 - 7e-5 * (0.15 + (delta - 0.08).abs()).powi(-4)
}

/// Stream booster factor: amplifies difficulty in the 160-360 BPM range.
pub(crate) fn stream_booster(delta: f64) -> f64 {
    let bpm = 7.5 / delta;

    if (160.0..360.0).contains(&bpm) {
        1.0 + 1.7e-7 * (bpm - 160.0) * (bpm - 360.0).powi(2)
    } else {
        1.0
    }
}

/// Rescale high star ratings (above 9.0) to reduce inflation.
pub(crate) fn rescale_high(sr: f64) -> f64 {
    if sr <= 9.0 {
        sr
    } else {
        9.0 + (sr - 9.0) / 1.2
    }
}
