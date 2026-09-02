//! Replay measurement harness.
//!
//! This is test-only because its stable-specific judgement reconstruction exists to
//! validate Sunny research, not as part of rosu-pp's public replay API.

use std::{
    collections::HashMap,
    fs,
    io::{Cursor, Read},
    path::Path,
};

use rayon::prelude::*;
use xz2::{read::XzDecoder, stream::Stream};

use crate::{Beatmap, model::hit_object::HitObjectKind};

const MOD_EZ: u32 = 1 << 1;
const MOD_HR: u32 = 1 << 4;
const MOD_DT: u32 = 1 << 6;
const MOD_HT: u32 = 1 << 8;
const MOD_NC: u32 = 1 << 9;
const MOD_SCORE_V2: u32 = 1 << 29;
const MOD_MIRROR: u32 = 1 << 30;

const MAX: usize = 0;
const N300: usize = 1;
const N200: usize = 2;
const N100: usize = 3;
const N50: usize = 4;
const MISS: usize = 5;

#[derive(Debug)]
struct Reader<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> Reader<'a> {
    fn take(&mut self, len: usize) -> Result<&'a [u8], String> {
        let end = self
            .offset
            .checked_add(len)
            .ok_or("replay offset overflow")?;
        let value = self.bytes.get(self.offset..end).ok_or("truncated replay")?;
        self.offset = end;
        Ok(value)
    }

    fn u8(&mut self) -> Result<u8, String> {
        Ok(self.take(1)?[0])
    }

    fn u16(&mut self) -> Result<u16, String> {
        Ok(u16::from_le_bytes(self.take(2)?.try_into().unwrap()))
    }

    fn u32(&mut self) -> Result<u32, String> {
        Ok(u32::from_le_bytes(self.take(4)?.try_into().unwrap()))
    }

    fn i32(&mut self) -> Result<i32, String> {
        Ok(i32::from_le_bytes(self.take(4)?.try_into().unwrap()))
    }

    fn u64(&mut self) -> Result<u64, String> {
        Ok(u64::from_le_bytes(self.take(8)?.try_into().unwrap()))
    }

    fn string(&mut self) -> Result<String, String> {
        match self.u8()? {
            0 => Ok(String::new()),
            0x0b => {
                let mut len = 0usize;
                for shift in (0..usize::BITS).step_by(7) {
                    let byte = self.u8()?;
                    len |= usize::from(byte & 0x7f) << shift;

                    if byte & 0x80 == 0 {
                        return Ok(String::from_utf8_lossy(self.take(len)?).into_owned());
                    }
                }

                Err("invalid replay string length".to_owned())
            }
            marker => Err(format!("invalid replay string marker 0x{marker:02x}")),
        }
    }
}

#[derive(Debug)]
struct Replay {
    mode: u8,
    map_md5: String,
    player: String,
    counts: [u32; 6],
    mods: u32,
    frames: Vec<(i32, u32)>,
}

impl Replay {
    fn parse(path: &Path) -> Result<Self, String> {
        let bytes = fs::read(path).map_err(|err| format!("{}: {err}", path.display()))?;
        let mut reader = Reader {
            bytes: &bytes,
            offset: 0,
        };
        let mode = reader.u8()?;
        let _version = reader.u32()?;
        let map_md5 = reader.string()?;
        let player = reader.string()?;
        let _replay_md5 = reader.string()?;
        let n300 = reader.u16()? as u32;
        let n100 = reader.u16()? as u32;
        let n50 = reader.u16()? as u32;
        let n320 = reader.u16()? as u32;
        let n200 = reader.u16()? as u32;
        let misses = reader.u16()? as u32;
        let _score = reader.i32()?;
        let _max_combo = reader.u16()?;
        let _perfect = reader.u8()?;
        let mods = reader.u32()?;
        let _life_bar = reader.string()?;
        let _timestamp = reader.u64()?;
        let compressed_len = reader.u32()? as usize;
        let compressed = reader.take(compressed_len)?;
        let frames = decode_frames(compressed)?;

        Ok(Self {
            mode,
            map_md5,
            player,
            counts: [n320, n300, n200, n100, n50, misses],
            mods,
            frames,
        })
    }

    fn clock_rate(&self) -> f64 {
        if self.mods & (MOD_DT | MOD_NC) != 0 {
            1.5
        } else if self.mods & MOD_HT != 0 {
            0.75
        } else {
            1.0
        }
    }
}

fn decode_frames(compressed: &[u8]) -> Result<Vec<(i32, u32)>, String> {
    if compressed.is_empty() {
        return Ok(Vec::new());
    }

    let mut raw = String::new();
    let stream = Stream::new_lzma_decoder(u64::MAX)
        .map_err(|err| format!("failed to initialize LZMA decoder: {err}"))?;
    XzDecoder::new_stream(Cursor::new(compressed), stream)
        .read_to_string(&mut raw)
        .map_err(|err| format!("invalid LZMA replay data: {err}"))?;

    let mut time = 0i32;
    let mut frames = Vec::new();

    for frame in raw.split(',').filter(|frame| !frame.is_empty()) {
        let mut fields = frame.split('|');
        let Some(delta) = fields.next().and_then(|value| value.parse::<i32>().ok()) else {
            continue;
        };
        let Some(keys) = fields.next().and_then(|value| value.parse::<f64>().ok()) else {
            continue;
        };

        // This sentinel stores the RNG seed rather than a time delta.
        if delta == -12345 {
            continue;
        }

        time = time.saturating_add(delta);
        frames.push((time, keys as u32));
    }

    Ok(frames)
}

#[derive(Clone, Debug)]
pub(super) struct Note {
    pub(super) time: i32,
    pub(super) column: usize,
    pub(super) duration: i32,
}

impl Note {
    fn end(&self) -> i32 {
        self.time + self.duration
    }
}

#[derive(Debug)]
struct Action {
    time: i32,
    column: usize,
    duration: i32,
}

impl Action {
    fn end(&self) -> i32 {
        self.time + self.duration
    }
}

#[derive(Debug)]
pub(super) struct ReplayAnalysis {
    pub(super) score_id: u64,
    pub(super) map_id: u64,
    pub(super) player: String,
    pub(super) map_md5: String,
    pub(super) mods: u32,
    pub(super) clock_rate: f64,
    pub(super) od: f64,
    pub(super) keys: usize,
    pub(super) notes: Vec<Note>,
    pub(super) errors: Vec<(usize, f64)>,
    pub(super) reported: [u32; 6],
    pub(super) reconstructed: [u32; 6],
    pub(super) unmatched_presses: usize,
}

pub(super) fn report_from_env() -> Result<(), String> {
    let batch = std::env::var_os("SUNNY_REPLAY_BATCH")
        .map(std::path::PathBuf::from)
        .ok_or("set SUNNY_REPLAY_BATCH to a TSV with id and mapid columns")?;
    let text = fs::read_to_string(&batch).map_err(|err| format!("{}: {err}", batch.display()))?;
    let mut lines = text.lines();
    let header: Vec<_> = lines
        .next()
        .ok_or("replay batch is empty")?
        .split('\t')
        .collect();
    let id = header
        .iter()
        .position(|field| *field == "id")
        .ok_or("batch has no id column")?;
    let mapid = header
        .iter()
        .position(|field| *field == "mapid")
        .ok_or("batch has no mapid column")?;
    let parent = batch.parent().unwrap_or_else(|| Path::new("."));
    let root = if parent.file_name().is_some_and(|name| name == "cohorts") {
        parent.parent().unwrap_or(parent)
    } else {
        parent
    };
    let pairs: Vec<_> = lines
        .filter_map(|line| {
            let fields: Vec<_> = line.split('\t').collect();
            Some((fields.get(id)?.to_string(), fields.get(mapid)?.to_string()))
        })
        .collect();
    let results: Vec<_> = pairs
        .par_iter()
        .filter_map(|(score, map)| {
            let osr = root.join("replays").join(format!("{score}.osr"));
            let osu = root.join("maps").join(format!("{map}.osu"));
            if !osr.exists() || !osu.exists() {
                return None;
            }
            match analyse(&osr, &osu) {
                Ok(result) => Some(result),
                Err(err) => {
                    eprintln!("skip {score}: {err}");
                    None
                }
            }
        })
        .collect();

    println!(
        "{:>9} {:>9} {:>12} {:>5} {:>4} {:>2} {:>6} {:>6} {:>8} {:>8} {:>5}",
        "score", "map", "player", "mods", "od", "k", "notes", "paired", "mean", "UR", "error"
    );
    let mut within_tolerance = 0;
    for result in &results {
        let errors: Vec<_> = result.errors.iter().map(|(_, error)| *error).collect();
        let mean = errors.iter().sum::<f64>() / errors.len().max(1) as f64;
        let variance = errors
            .iter()
            .map(|error| (error - mean).powi(2))
            .sum::<f64>()
            / errors.len().max(1) as f64;
        let total_error: u32 = result
            .reported
            .iter()
            .zip(result.reconstructed)
            .map(|(&expected, actual)| expected.abs_diff(actual))
            .sum();
        let holds = result
            .notes
            .iter()
            .filter(|note| note.duration != 0)
            .count();
        let columns_used = result
            .notes
            .iter()
            .map(|note| note.column)
            .max()
            .map_or(0, |column| column + 1);
        let first_time = result.notes.first().map_or(0, |note| note.time);
        let tolerance = (result.notes.len() as f64 * 0.005) as u32;
        within_tolerance += usize::from(total_error <= tolerance);
        println!(
            "{:>9} {:>9} {:>12.12} {:>5} {:>4.1} {:>2} {:>6} {:>6} {:>8.2} {:>8.1} {:>5}",
            result.score_id,
            result.map_id,
            result.player,
            result.mods,
            result.od,
            result.keys,
            result.notes.len(),
            result.errors.len(),
            mean,
            variance.sqrt() * 10.0,
            total_error
        );
        println!(
            "          md5={} rate={:.2} holds={} columns={} first={}ms stray={}",
            result.map_md5,
            result.clock_rate,
            holds,
            columns_used,
            first_time,
            result.unmatched_presses
        );
    }
    println!(
        "\n{within_tolerance}/{} within 0.5% of notes on every band",
        results.len()
    );

    Ok(())
}

pub(super) fn analyse(osr_path: &Path, osu_path: &Path) -> Result<ReplayAnalysis, String> {
    let replay = Replay::parse(osr_path)?;
    let map =
        Beatmap::from_path(osu_path).map_err(|err| format!("{}: {err}", osu_path.display()))?;

    if replay.mode != 3 || map.mode != rosu_map::section::general::GameMode::Mania {
        return Err("replay and beatmap must both be mania".to_owned());
    }

    let keys = (map.cs as usize).max(1);
    let rate = replay.clock_rate();
    let mirror = replay.mods & MOD_MIRROR != 0;
    let mut notes: Vec<_> = map
        .hit_objects
        .iter()
        .map(|object| {
            let raw_column = ((object.pos.x as usize) * keys / 512).min(keys - 1);
            let column = if mirror {
                keys - raw_column - 1
            } else {
                raw_column
            };
            let duration = match &object.kind {
                HitObjectKind::Hold(hold) => hold.duration,
                _ => 0.0,
            };

            Note {
                time: scale(object.start_time as i32, rate),
                column,
                duration: scale(duration as i32, rate),
            }
        })
        .collect();
    notes.sort_by_key(|note| (note.time, note.column));

    let mut actions = actions_from_frames(&replay.frames, keys);
    for action in &mut actions {
        action.time = scale(action.time, rate);
        action.duration = scale(action.duration, rate);
    }

    let windows = mania_windows(map.od as f64, replay.mods);
    let (errors, reconstructed, unmatched_presses) = judge(
        &notes,
        &actions,
        keys,
        &windows,
        replay.mods & MOD_SCORE_V2 != 0,
    );

    Ok(ReplayAnalysis {
        score_id: numeric_stem(osr_path)?,
        map_id: numeric_stem(osu_path)?,
        player: replay.player,
        map_md5: replay.map_md5,
        mods: replay.mods,
        clock_rate: rate,
        od: map.od as f64,
        keys,
        notes,
        errors,
        reported: replay.counts,
        reconstructed,
        unmatched_presses,
    })
}

fn numeric_stem(path: &Path) -> Result<u64, String> {
    path.file_stem()
        .and_then(|stem| stem.to_str())
        .ok_or_else(|| format!("{} has no file stem", path.display()))?
        .parse()
        .map_err(|err| format!("{} must have a numeric file stem: {err}", path.display()))
}

fn scale(value: i32, rate: f64) -> i32 {
    (f64::from(value) / rate) as i32
}

fn actions_from_frames(frames: &[(i32, u32)], keys: usize) -> Vec<Action> {
    let mut starts = vec![None; keys];
    let mut actions = Vec::new();

    for &(time, mask) in frames {
        for (column, start) in starts.iter_mut().enumerate() {
            if mask & (1 << column) != 0 {
                if start.is_none() {
                    *start = Some(time);
                }
            } else if let Some(pressed_at) = start.take() {
                actions.push(Action {
                    time: pressed_at,
                    column,
                    duration: time - pressed_at,
                });
            }
        }
    }

    actions.sort_by_key(|action| (action.time, action.column));
    actions
}

fn difficulty_range(od: f64, low: f64, mid: f64, high: f64, mods: u32) -> f64 {
    let difficulty = if mods & MOD_HR != 0 {
        (od * 1.4).min(10.0)
    } else if mods & MOD_EZ != 0 {
        (od / 2.0).max(0.0)
    } else {
        od
    };
    let value = if difficulty > 5.0 {
        mid + (high - mid) * (difficulty - 5.0) / 5.0
    } else {
        mid - (mid - low) * (5.0 - difficulty) / 5.0
    };

    value.trunc()
}

fn mania_windows(od: f64, mods: u32) -> [f64; 6] {
    if mods & MOD_SCORE_V2 != 0 {
        return [
            difficulty_range(od, 22.4, 19.4, 13.9, mods),
            difficulty_range(od, 64.0, 49.0, 34.0, mods),
            difficulty_range(od, 97.0, 82.0, 67.0, mods),
            difficulty_range(od, 127.0, 112.0, 97.0, mods),
            difficulty_range(od, 151.0, 136.0, 121.0, mods),
            difficulty_range(od, 188.0, 173.0, 158.0, mods),
        ];
    }

    let rate = if mods & MOD_HR != 0 {
        1.0 / 1.4
    } else if mods & MOD_EZ != 0 {
        1.4
    } else {
        1.0
    };
    let mut windows = [16.0, 34.0, 67.0, 97.0, 121.0, 158.0];

    for (index, window) in windows.iter_mut().enumerate() {
        if index > 0 {
            *window += 3.0 * (10.0 - od);
        }

        *window = (*window * rate).trunc();
    }

    windows
}

fn match_column(
    notes: &[(usize, &Note)],
    actions: &[(usize, &Action)],
    windows: &[f64; 6],
) -> HashMap<usize, usize> {
    let mut claimed = HashMap::new();
    let mut next_note = 0;

    for &(action_index, action) in actions {
        while let Some(&(note_index, note)) = notes.get(next_note) {
            let difference = action.time - note.time;

            if f64::from(difference) >= windows[N100] {
                next_note += 1;
                continue;
            }

            if f64::from(-difference) > windows[N100] {
                break;
            }

            claimed.insert(note_index, action_index);
            next_note += 1;
            break;
        }

        if next_note == notes.len() {
            break;
        }
    }

    claimed
}

fn judgement(error: f64, windows: &[f64; 6]) -> usize {
    (0..MISS)
        .find(|&band| error <= windows[band])
        .unwrap_or(MISS)
}

fn judge(
    notes: &[Note],
    actions: &[Action],
    keys: usize,
    windows: &[f64; 6],
    score_v2: bool,
) -> (Vec<(usize, f64)>, [u32; 6], usize) {
    let mut notes_by_column = vec![Vec::new(); keys];
    let mut actions_by_column = vec![Vec::new(); keys];

    for (index, note) in notes.iter().enumerate() {
        notes_by_column[note.column].push((index, note));
    }
    for (index, action) in actions.iter().enumerate() {
        actions_by_column[action.column].push((index, action));
    }

    let mut claimed = HashMap::new();
    for column in 0..keys {
        claimed.extend(match_column(
            &notes_by_column[column],
            &actions_by_column[column],
            windows,
        ));
    }

    let mut hold_bands = HashMap::new();
    for (&note_index, &action_index) in &claimed {
        let note = &notes[note_index];
        let action = &actions[action_index];

        if note.duration == 0 || score_v2 {
            continue;
        }

        let end_error = (action.end() - note.end()).abs() as f64;
        let mut start_error = (action.time - note.time).abs() as f64;
        if f64::from(note.time - action.time) > windows[N50] {
            start_error = (note.time - (note.end() - 1)).abs() as f64;
        }
        let total_error = start_error + end_error;

        if f64::from(action.end() - note.end()) < -windows[N50] {
            continue;
        }

        let band = [(MAX, 1.2), (N300, 1.1), (N200, 1.0), (N100, 1.0)]
            .into_iter()
            .find(|&(band, rate)| {
                let window = windows[band] * rate;
                start_error <= window && total_error <= 2.0 * window
            })
            .map_or(N50, |(band, _)| band);
        hold_bands.insert(note_index, band);
    }

    let mut errors = Vec::new();
    let mut counts = [0; 6];
    for (note_index, note) in notes.iter().enumerate() {
        let band = if let Some(&action_index) = claimed.get(&note_index) {
            let error = f64::from(actions[action_index].time - note.time);
            errors.push((note_index, error));
            hold_bands
                .get(&note_index)
                .copied()
                .unwrap_or_else(|| judgement(error.abs(), windows))
        } else {
            MISS
        };
        counts[band] += 1;
    }

    (errors, counts, actions.len().saturating_sub(claimed.len()))
}

#[test]
fn classic_windows_match_measured_values() {
    assert_eq!(
        mania_windows(8.0, 0),
        [16.0, 40.0, 73.0, 103.0, 127.0, 164.0]
    );
    assert_eq!(
        mania_windows(8.0, MOD_EZ),
        [22.0, 56.0, 102.0, 144.0, 177.0, 229.0]
    );
}

#[test]
fn frame_edges_become_actions() {
    let actions = actions_from_frames(&[(10, 1), (15, 3), (25, 2), (30, 0)], 2);
    assert_eq!(
        (actions[0].time, actions[0].column, actions[0].duration),
        (10, 0, 15)
    );
    assert_eq!(
        (actions[1].time, actions[1].column, actions[1].duration),
        (15, 1, 15)
    );
}

#[test]
fn early_stray_does_not_consume_note() {
    let note = Note {
        time: 500,
        column: 0,
        duration: 0,
    };
    let early = Action {
        time: 300,
        column: 0,
        duration: 1,
    };
    let hit = Action {
        time: 510,
        column: 0,
        duration: 1,
    };
    let notes = [(0, &note)];
    let actions = [(0, &early), (1, &hit)];

    assert_eq!(
        match_column(&notes, &actions, &mania_windows(8.0, 0)).get(&0),
        Some(&1)
    );
}
