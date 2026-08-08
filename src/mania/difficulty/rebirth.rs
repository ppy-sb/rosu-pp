#![allow(dead_code)]

use std::cmp::Ordering;
use std::collections::HashMap;

use crate::{
    Beatmap, Difficulty,
    mania::object::{ManiaObject, ObjectParams},
};

#[derive(Clone, Copy, Debug)]
struct Note {
    column: usize,
    head: f64,
    tail: Option<f64>,
}

impl Note {
    fn tail_or_head(self) -> f64 {
        self.tail.unwrap_or(self.head)
    }
}

/// The result of the rebirth difficulty calculation, containing the star
/// rating as well as the additional measures required for performance
/// calculation.
#[derive(Clone, Copy, Debug, Default)]
pub(super) struct RebirthParams {
    pub sr: f64,
    pub spikiness: f64,
    pub switches: f64,
    pub variety: f64,
}

struct RebirthData {
    total_columns: usize,
    hit_leniency: f64,
    t_end: f64,
    notes: Vec<Note>,
    notes_by_column: Vec<Vec<Note>>,
    long_notes: Vec<Note>,
    tails: Vec<Note>,
    all_corners: Vec<f64>,
    base_corners: Vec<f64>,
    awkwardness_corners: Vec<f64>,
}

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

pub(super) fn calculate_stars(difficulty: &Difficulty, map: &Beatmap) -> f64 {
    calculate_params(difficulty, map).sr
}

pub(super) fn calculate_params(difficulty: &Difficulty, map: &Beatmap) -> RebirthParams {
    let total_columns = map.cs.round_ties_even().max(1.0) as usize;
    let clock_rate = difficulty.get_clock_rate();
    let take = difficulty.get_passed_objects();
    let mut params = ObjectParams::new(map);
    let objects = map
        .hit_objects
        .iter()
        .map(|h| ManiaObject::new(h, total_columns as f32, &mut params))
        .take(take);

    calculate_params_for_objects(total_columns, map.od, clock_rate, objects)
}

pub(super) fn calculate_stars_for_objects(
    total_columns: usize,
    od: f32,
    clock_rate: f64,
    objects: impl IntoIterator<Item = ManiaObject>,
) -> f64 {
    calculate_params_for_objects(total_columns, od, clock_rate, objects).sr
}

pub(super) fn calculate_params_for_objects(
    total_columns: usize,
    od: f32,
    clock_rate: f64,
    objects: impl IntoIterator<Item = ManiaObject>,
) -> RebirthParams {
    let Some(data) = prepare_data(total_columns, od, clock_rate, objects) else {
        return RebirthParams::default();
    };

    calculate_from_data(data)
}

fn prepare_data(
    total_columns: usize,
    od: f32,
    clock_rate: f64,
    objects: impl IntoIterator<Item = ManiaObject>,
) -> Option<RebirthData> {
    let mut notes = build_notes(clock_rate, objects);
    notes.sort_by(compare_notes);

    if notes.len() < 2 || total_columns == 0 {
        return None;
    }

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

    Some(RebirthData {
        total_columns,
        hit_leniency: hit_leniency(f64::from(od)),
        t_end,
        notes,
        notes_by_column,
        long_notes,
        tails,
        all_corners,
        base_corners,
        awkwardness_corners,
    })
}

fn build_notes(clock_rate: f64, objects: impl IntoIterator<Item = ManiaObject>) -> Vec<Note> {
    objects
        .into_iter()
        .map(|object| {
            let head = object.start_time / clock_rate;
            let end = object.end_time / clock_rate;
            let tail = (end > head + 1e-7).then_some(end);

            Note {
                column: object.column,
                head,
                tail,
            }
        })
        .collect()
}

fn compare_notes(a: &Note, b: &Note) -> Ordering {
    a.head
        .total_cmp(&b.head)
        .then_with(|| a.column.cmp(&b.column))
}

fn hit_leniency(od: f64) -> f64 {
    let x = 0.3 * ((64.5 - (od * 3.0).ceil()) / 500.0).sqrt();
    x.min(0.6 * (x - 0.09) + 0.09)
}

fn get_corners(t_end: f64, notes: &[Note]) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
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

fn sort_corners(corners: &mut Vec<f64>, t_end: f64) {
    corners.retain(|&corner| (0.0..=t_end).contains(&corner));
    corners.sort_by(f64::total_cmp);
    corners.dedup_by(|a, b| a.total_cmp(b).is_eq());
}

fn cumulative_sum(x: &[f64], f: &[f64]) -> Vec<f64> {
    let mut cumulative = vec![0.0; x.len()];

    for i in 1..x.len() {
        cumulative[i] = cumulative[i - 1] + f[i - 1] * (x[i] - x[i - 1]);
    }

    cumulative
}

fn query_cumsum(q: f64, x: &[f64], cumulative: &[f64], f: &[f64]) -> f64 {
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

fn smooth_on_corners(x: &[f64], f: &[f64], window: f64, scale: f64, average: bool) -> Vec<f64> {
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

fn interp_values(new_x: &[f64], old_x: &[f64], old_vals: &[f64]) -> Vec<f64> {
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

fn step_interp(new_x: &[f64], old_x: &[f64], old_vals: &[f64]) -> Vec<f64> {
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

fn compute_jbar(data: &RebirthData) -> (Vec<Vec<f64>>, Vec<f64>) {
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
            let val = delta.powi(-1) * (delta + 0.11 * data.hit_leniency.powf(0.25)).powi(-1);
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

fn compute_xbar(data: &RebirthData, active_columns: &[Vec<usize>]) -> Vec<f64> {
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
        let notes_in_pair = notes_in_pair(data, pair_column);

        for pair in notes_in_pair.windows(2) {
            let start = pair[0].head;
            let end = pair[1].head;
            let left = lower_bound(&data.base_corners, start);
            let right = lower_bound(&data.base_corners, end);

            if left == right {
                continue;
            }

            let delta = 0.001 * (end - start);
            let mut val = 0.16 * data.hit_leniency.max(delta).powi(-2);

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

            let fast =
                (0.4 * delta.max(0.06).max(0.75 * data.hit_leniency).powi(-2) - 80.0).max(0.0);

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

fn compute_pbar(
    data: &RebirthData,
    ln_rep: &LongNoteBodyRepresentation,
    anchor: &[f64],
) -> Vec<f64> {
    let mut p_step = vec![0.0; data.base_corners.len()];

    for pair in data.notes.windows(2) {
        let h_l = pair[0].head;
        let h_r = pair[1].head;
        let delta_time = h_r - h_l;

        if delta_time < 1e-9 {
            let spike = 1000.0 * (0.02 * (4.0 / data.hit_leniency - 24.0)).powf(0.25);
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
        let base = 0.08 * data.hit_leniency.powi(-1);
        let inc = if delta < 2.0 * data.hit_leniency / 3.0 {
            delta.powi(-1)
                * (base
                    * (1.0
                        - 24.0
                            * data.hit_leniency.powi(-1)
                            * (delta - data.hit_leniency / 2.0).powi(2)))
                .powf(0.25)
                * booster.max(v)
        } else {
            delta.powi(-1)
                * (base
                    * (1.0 - 24.0 * data.hit_leniency.powi(-1) * (data.hit_leniency / 6.0).powi(2)))
                .powf(0.25)
                * booster.max(v)
        };

        for idx in left..right {
            p_step[idx] += (inc * anchor[idx]).min(inc.max(inc * 2.0 - 10.0));
        }
    }

    smooth_on_corners(&data.base_corners, &p_step, 500.0, 0.001, false)
}

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

fn compute_rbar(data: &RebirthData) -> Vec<f64> {
    let mut r_step = vec![0.0; data.base_corners.len()];

    if data.tails.len() < 2 {
        return r_step;
    }

    let i_list: Vec<_> = data
        .tails
        .iter()
        .map(|tail| {
            let next_head = find_next_note_in_column(*tail, &data.notes_by_column[tail.column])
                .map_or(1e9, |note| note.head);
            let tail_time = tail.tail_or_head();
            let i_h = 0.001 * (tail_time - tail.head - 80.0).abs() / data.hit_leniency;
            let i_t = 0.001 * (next_head - tail_time - 80.0).abs() / data.hit_leniency;

            2.0 / (2.0 + (-5.0 * (i_h - 0.75)).exp() + (-5.0 * (i_t - 0.75)).exp())
        })
        .collect();

    for idx in 0..data.tails.len() - 1 {
        let t_start = data.tails[idx].tail_or_head();
        let t_end = data.tails[idx + 1].tail_or_head();
        let left = lower_bound(&data.base_corners, t_start);
        let right = lower_bound(&data.base_corners, t_end);

        if left == right {
            continue;
        }

        let delta_r = 0.001 * (t_end - t_start);
        let value = 0.08
            * delta_r.powf(-0.5)
            * data.hit_leniency.powi(-1)
            * (1.0 + 0.8 * (i_list[idx] + i_list[idx + 1]));

        for step in &mut r_step[left..right] {
            *step = value;
        }
    }

    smooth_on_corners(&data.base_corners, &r_step, 500.0, 0.001, false)
}

fn compute_density_and_keys(data: &RebirthData, key_usage: &[Vec<bool>]) -> (Vec<f64>, Vec<f64>) {
    let note_hit_times: Vec<_> = data.notes.iter().map(|note| note.head).collect();
    let mut density = vec![0.0; data.base_corners.len()];
    let mut keys = vec![1.0; data.base_corners.len()];

    for (idx, &corner) in data.base_corners.iter().enumerate() {
        let low = corner - 500.0;
        let high = corner + 500.0;
        density[idx] =
            (lower_bound(&note_hit_times, high) - lower_bound(&note_hit_times, low)) as f64;
        keys[idx] = key_usage.iter().filter(|column| column[idx]).count().max(1) as f64;
    }

    (density, keys)
}

fn calculate_from_data(data: RebirthData) -> RebirthParams {
    let key_usage = get_key_usage(&data);
    let active_columns: Vec<_> = (0..data.base_corners.len())
        .map(|idx| {
            (0..data.total_columns)
                .filter(|&column| key_usage[column][idx])
                .collect::<Vec<_>>()
        })
        .collect();
    let key_usage_400 = get_key_usage_400(&data);
    let anchor = compute_anchor(&key_usage_400);
    let (delta_by_column, jbar_base) = compute_jbar(&data);
    let jbar = interp_values(&data.all_corners, &data.base_corners, &jbar_base);
    let xbar_base = compute_xbar(&data, &active_columns);
    let xbar = interp_values(&data.all_corners, &data.base_corners, &xbar_base);
    let ln_rep = LongNoteBodyRepresentation::new(&data.long_notes, data.t_end);
    let pbar_base = compute_pbar(&data, &ln_rep, &anchor);
    let pbar = interp_values(&data.all_corners, &data.base_corners, &pbar_base);
    let abar_awkwardness = compute_abar(&data, &active_columns, &delta_by_column);
    let abar = interp_values(
        &data.all_corners,
        &data.awkwardness_corners,
        &abar_awkwardness,
    );
    let rbar_base = compute_rbar(&data);
    let rbar = interp_values(&data.all_corners, &data.base_corners, &rbar_base);
    let (density_base, keys_base) = compute_density_and_keys(&data, &key_usage);
    let density = step_interp(&data.all_corners, &data.base_corners, &density_base);
    let keys = step_interp(&data.all_corners, &data.base_corners, &keys_base);

    let d_all: Vec<_> = (0..data.all_corners.len())
        .map(|idx| {
            let s_all = (0.4
                * (abar[idx].powf(3.0 / keys[idx]) * jbar[idx].min(8.0 + 0.85 * jbar[idx]))
                    .powf(1.5)
                + (1.0 - 0.4)
                    * (abar[idx].powf(2.0 / 3.0)
                        * (0.8 * pbar[idx] + rbar[idx] * 35.0 / (density[idx] + 8.0)))
                        .powf(1.5))
            .powf(2.0 / 3.0);
            let t_all = (abar[idx].powf(3.0 / keys[idx]) * xbar[idx]) / (xbar[idx] + s_all + 1.0);

            2.7 * s_all.powf(0.5) * t_all.powf(1.5) + s_all * 0.27
        })
        .collect();

    let mut gaps = vec![0.0; data.all_corners.len()];

    if gaps.len() < 2 {
        return RebirthParams::default();
    }

    gaps[0] = (data.all_corners[1] - data.all_corners[0]) / 2.0;
    let last = gaps.len() - 1;
    gaps[last] = (data.all_corners[last] - data.all_corners[last - 1]) / 2.0;

    for idx in 1..last {
        gaps[idx] = (data.all_corners[idx + 1] - data.all_corners[idx - 1]) / 2.0;
    }

    let effective_weights: Vec<_> = density.iter().zip(gaps).map(|(&c, gap)| c * gap).collect();
    let mut sorted_indices: Vec<_> = (0..d_all.len()).collect();
    sorted_indices.sort_by(|&a, &b| d_all[a].total_cmp(&d_all[b]));
    let d_sorted: Vec<_> = sorted_indices.iter().map(|&idx| d_all[idx]).collect();
    let w_sorted: Vec<_> = sorted_indices
        .iter()
        .map(|&idx| effective_weights[idx])
        .collect();
    let total_weight = w_sorted.iter().sum::<f64>();

    if total_weight <= 0.0 {
        return RebirthParams::default();
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
    let switches = compute_switches(&data, &keys, &d_all);
    let variety = compute_variety(&data);

    RebirthParams {
        sr,
        spikiness,
        switches,
        variety,
    }
}

/// Spikiness measure from the weighted variance of the corner difficulty
/// values, i.e. how much the difficulty spikes within the map.
fn compute_spikiness(d_sorted: &[f64], w_sorted: &[f64], weighted_mean: f64, total_weight: f64) -> f64 {
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
fn compute_switches(data: &RebirthData, ks_arr: &[f64], d_all: &[f64]) -> f64 {
    let all_corners = &data.all_corners;

    // Heads of all notes, in (head, column) order
    let heads: Vec<f64> = data.notes.iter().map(|note| note.head).collect();

    // For each head, the index of the first corner >= head (last index dropped)
    let idx_list: Vec<usize> = heads.iter().map(|&head| lower_bound(all_corners, head)).collect();
    let n = idx_list.len().saturating_sub(1);

    let ks_at_note: Vec<f64> = idx_list[..n].iter().map(|&i| ks_arr[i]).collect();
    let weights_at_note: Vec<f64> = idx_list[..n].iter().map(|&i| d_all[i]).collect();

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
        let idx_list_tails: Vec<usize> =
            tails.iter().map(|&tail| lower_bound(all_corners, tail)).collect();
        let n_tails = idx_list_tails.len() - 1;

        let ks_at_tail: Vec<f64> = idx_list_tails[..n_tails].iter().map(|&i| ks_arr[i]).collect();
        let weights_at_tail: Vec<f64> = idx_list_tails[..n_tails].iter().map(|&i| d_all[i]).collect();

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
        head_gaps_new.extend(column.windows(2).map(|w| w[1].head as i64 - w[0].head as i64));
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

fn notes_in_pair(data: &RebirthData, pair_column: usize) -> Vec<Note> {
    match pair_column {
        0 => data.notes_by_column.first().cloned().unwrap_or_default(),
        column if column == data.total_columns => {
            data.notes_by_column.last().cloned().unwrap_or_default()
        }
        column => {
            let mut notes = Vec::with_capacity(
                data.notes_by_column[column - 1].len() + data.notes_by_column[column].len(),
            );
            notes.extend_from_slice(&data.notes_by_column[column - 1]);
            notes.extend_from_slice(&data.notes_by_column[column]);
            notes.sort_by(compare_notes);
            notes
        }
    }
}

fn find_next_note_in_column(note: Note, notes: &[Note]) -> Option<Note> {
    let idx = notes.partition_point(|candidate| candidate.head < note.head);

    notes.get(idx + 1).copied()
}

fn active_columns_contains(active_columns: &[Vec<usize>], idx: usize, column: isize) -> bool {
    usize::try_from(column).is_ok_and(|column| active_columns[idx].contains(&column))
}

fn lower_bound(values: &[f64], target: f64) -> usize {
    values.partition_point(|&value| value < target)
}

fn upper_bound(values: &[f64], target: f64) -> usize {
    values.partition_point(|&value| value <= target)
}

fn jack_nerfer(delta: f64) -> f64 {
    1.0 - 7e-5 * (0.15 + (delta - 0.08).abs()).powi(-4)
}

fn stream_booster(delta: f64) -> f64 {
    let bpm = 7.5 / delta;

    if (160.0..360.0).contains(&bpm) {
        1.0 + 1.7e-7 * (bpm - 160.0) * (bpm - 360.0).powi(2)
    } else {
        1.0
    }
}

fn rescale_high(sr: f64) -> f64 {
    if sr <= 9.0 {
        sr
    } else {
        9.0 + (sr - 9.0) / 1.2
    }
}

fn logistic(value: f64, midpoint: f64, multiplier: f64) -> f64 {
    1.0 / (1.0 + (-multiplier * (value - midpoint)).exp())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hit_leniency_matches_reference_formula() {
        assert!((hit_leniency(8.0) - 0.08538149682454625).abs() < 1e-12);
    }

    #[test]
    fn build_notes_applies_clock_rate() {
        let objects = [
            ManiaObject {
                start_time: 1500.0,
                end_time: 1500.0,
                column: 0,
            },
            ManiaObject {
                start_time: 3000.0,
                end_time: 3300.0,
                column: 1,
            },
        ];

        let notes = build_notes(1.5, objects);

        assert_eq!(notes[0].head, 1000.0);
        assert_eq!(notes[0].tail, None);
        assert_eq!(notes[1].head, 2000.0);
        assert_eq!(notes[1].tail, Some(2200.0));
    }

    #[test]
    fn cumulative_query_handles_piecewise_constant_values() {
        let x = [0.0, 10.0, 20.0];
        let f = [2.0, 4.0, 4.0];
        let cumulative = cumulative_sum(&x, &f);

        assert_eq!(cumulative, vec![0.0, 20.0, 60.0]);
        assert_eq!(query_cumsum(5.0, &x, &cumulative, &f), 10.0);
        assert_eq!(query_cumsum(15.0, &x, &cumulative, &f), 40.0);
    }

    #[test]
    fn corners_include_note_boundaries_and_reference_offsets() {
        let notes = [
            Note {
                column: 0,
                head: 1000.0,
                tail: None,
            },
            Note {
                column: 1,
                head: 1500.0,
                tail: Some(1900.0),
            },
        ];

        let (all, base, a) = get_corners(2000.0, &notes);

        assert!(base.contains(&501.0));
        assert!(base.contains(&1001.0));
        assert!(base.contains(&1501.0));
        assert!(a.contains(&0.0));
        assert!(a.contains(&2000.0));
        assert!(all.windows(2).all(|pair| pair[0] < pair[1]));
    }

    #[test]
    fn long_note_body_sum_is_zero_without_long_notes() {
        let rep = LongNoteBodyRepresentation::new(&[], 1000.0);
        assert_eq!(rep.sum(100.0, 900.0), 0.0);
    }

    #[test]
    fn rao_entropy_log_matches_reference() {
        // Rao quadratic entropy of [1, 2, 3] with one log iteration:
        // (4 * ln 2 + 2 * ln 3) / 9
        let q = rao_quadratic_entropy_log(&[1, 2, 3], 1);
        let expected = (4.0 * 2.0_f64.ln() + 2.0 * 3.0_f64.ln()) / 9.0;

        assert!((q - expected).abs() < 1e-12);
    }

    #[test]
    fn rao_entropy_log_empty_and_single_value() {
        assert_eq!(rao_quadratic_entropy_log(&[], 1), 0.0);
        assert_eq!(rao_quadratic_entropy_log(&[42], 2), 0.0);
    }
}
