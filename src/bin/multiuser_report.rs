//! Multiuser report tool for analyzing score data.
//!
//! Run with: `cargo run --release --features reports --bin multiuser_report -- --help`

use clap::{Parser, ValueEnum};
use comfy_table::{Cell, CellAlignment, Color, ContentArrangement, Table};
use rayon::prelude::*;
use rosu_pp::mania::sunny::{
    SunnyManiaDifficultyAttributes, SunnyManiaPerformanceAttributes, SunnyScoreState,
    calculate_performance,
};
use rosu_pp::model::beatmap::Beatmap;
use rosu_pp::report_utils::{BatchCalculator, batch, calculate, mods_for};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[derive(Clone, Copy, Debug, ValueEnum)]
#[value(rename_all = "lower")]
enum OutputFormat {
    Text,
    Tsv,
    Csv,
}

#[derive(Parser)]
#[command(name = "multiuser_report")]
#[command(about = "Analyzes score data from real players to validate the algorithm")]
struct Args {
    /// Path to multiuser TSV file
    #[arg(long, default_value = "local-fixtures/multiuser.tsv")]
    tsv: PathBuf,

    /// Directory containing .osu map files
    #[arg(long, default_value = "local-fixtures/maps")]
    maps: PathBuf,

    /// Output format. Delimited formats contain score rows only.
    #[arg(long, value_enum, default_value_t = OutputFormat::Text)]
    output_format: OutputFormat,
}

#[derive(Clone)]
struct MultiRow {
    uid: String,
    map_id: String,
    mods: String,
    artist: String,
    live_stars: f64,
    keys: u32,
    counts: [u32; 6],
    acc: f64,
    live_pp: f64,
    title: String,
    version: String,
}

struct LoadedUser {
    row: MultiRow,
    map: Arc<Beatmap>,
    attrs: Arc<SunnyManiaDifficultyAttributes>,
}

struct Calculator {
    maps_dir: PathBuf,
}

impl BatchCalculator<MultiRow> for Calculator {
    type Attributes = SunnyManiaDifficultyAttributes;
    type Map = Beatmap;
    type MapKey = String;
    type ModKey = String;

    fn map_key(&self, row: &MultiRow) -> Self::MapKey {
        row.map_id.clone()
    }

    fn mod_key(&self, row: &MultiRow) -> Self::ModKey {
        row.mods.clone()
    }

    fn load_map(&self, map_id: &Self::MapKey) -> Option<Self::Map> {
        let bytes = std::fs::read(self.maps_dir.join(format!("{map_id}.osu"))).ok()?;
        Beatmap::from_bytes(&bytes).ok()
    }

    fn calculate(&self, map: &Self::Map, mod_names: &Self::ModKey) -> Option<Self::Attributes> {
        let (mods, clock_rate) = mods_for(mod_names);
        calculate(
            map,
            &mods,
            clock_rate,
            Some(!mod_names.contains("V2")),
            None,
        )
    }
}

struct MultiPriced {
    row: MultiRow,
    map: Arc<Beatmap>,
    attrs: Arc<SunnyManiaDifficultyAttributes>,
    perf: SunnyManiaPerformanceAttributes,
}

struct ScoreRow {
    map: u32,
    mods: String,
    k: u8,
    od: String,
    rice: u32,
    ln: u32,
    ttj: u32,
    n320: u32,
    n300: u32,
    n200: u32,
    n100: u32,
    n50: u32,
    miss: u32,
    sr: String,
    live_stars: String,
    acc_pct: String,
    expected_pct: String,
    sigma: String,
    live: String,
    rebirth: String,
    current: String,
    cur_live_pct: String,
    cur_rebirth_pct: String,
    map_f: String,
    score_a: String,
    loss_d: String,
}

/// Loads users and calculates map-level attributes, batching identical map/mod jobs.
fn load_user(tsv_path: &Path, maps_dir: &Path) -> Vec<LoadedUser> {
    let Ok(text) = std::fs::read_to_string(tsv_path) else {
        eprintln!("Cannot read {}", tsv_path.display());
        return Vec::new();
    };
    let rows: Vec<_> = text
        .lines()
        .filter_map(|line| {
            let f: Vec<&str> = line.split('\t').collect();
            if f.len() < 18 || f[0] == "uid" {
                return None;
            }
            let u = |s: &str| s.parse::<u32>().unwrap_or(0);
            Some(MultiRow {
                uid: f[0].to_owned(),
                map_id: f[2].to_owned(),
                mods: f[3].to_owned(),
                artist: f[15].to_owned(),
                live_stars: f[4].parse().unwrap_or(0.0),
                keys: u(f[6]),
                counts: [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])],
                acc: f[13].parse().unwrap_or(0.0),
                live_pp: f[14].parse().unwrap_or(0.0),
                title: f[16].to_owned(),
                version: f[17].to_owned(),
            })
        })
        .collect();
    batch(
        rows,
        Calculator {
            maps_dir: maps_dir.to_path_buf(),
        },
        |(map, _, attrs, rows)| {
            Some(
                rows.into_iter()
                    .map(|row| LoadedUser {
                        row,
                        map: Arc::clone(&map),
                        attrs: Arc::clone(&attrs),
                    })
                    .collect::<Vec<_>>(),
            )
        },
    )
    .flatten()
    .collect()
}

/// Calculates performance for all loaded users in parallel.
fn batched_calculation(users: Vec<LoadedUser>) -> Vec<MultiPriced> {
    users
        .into_par_iter()
        .map(|user| {
            let (mods, _) = mods_for(&user.row.mods);
            let state = SunnyScoreState {
                n320: user.row.counts[0],
                n300: user.row.counts[1],
                n200: user.row.counts[2],
                n100: user.row.counts[3],
                n50: user.row.counts[4],
                misses: user.row.counts[5],
            };
            let perf = calculate_performance(&user.attrs, &mods, state);
            MultiPriced {
                row: user.row,
                map: user.map,
                attrs: user.attrs,
                perf,
            }
        })
        .collect()
}

fn score_row(r: &MultiPriced) -> ScoreRow {
    let rebirth = r.perf.xxy_pp_pattern + r.perf.xxy_pp_accuracy;
    let delta = if r.row.live_pp > 0.0 {
        (r.perf.pp / r.row.live_pp - 1.0) * 100.0
    } else {
        0.0
    };
    let local_delta = if rebirth > 0.0 {
        (r.perf.pp / rebirth - 1.0) * 100.0
    } else {
        0.0
    };
    let rice = r.attrs.n_objects - r.attrs.n_long_notes;
    let ttj = if r.attrs.ln_judged_as_one {
        rice + r.attrs.n_long_notes
    } else {
        rice + r.attrs.n_long_notes * 2
    };
    ScoreRow {
        map: r.row.map_id.parse().unwrap_or(0),
        mods: r.row.mods.clone(),
        k: r.row.keys as u8,
        od: format!("{:.1}", r.map.od),
        rice: rice as u32,
        ln: r.attrs.n_long_notes as u32,
        ttj: ttj as u32,
        n320: r.row.counts[0],
        n300: r.row.counts[1],
        n200: r.row.counts[2],
        n100: r.row.counts[3],
        n50: r.row.counts[4],
        miss: r.row.counts[5],
        sr: format!("{:.3}", r.attrs.stars),
        live_stars: format!("{:.3}", r.row.live_stars),
        acc_pct: format!("{:.3}", r.row.acc),
        expected_pct: format!("{:.3}", r.attrs.timing_expected_accuracy * 100.0),
        sigma: format!("{:.3}", r.perf.timing_core_sigma),
        live: format!("{:.1}", r.row.live_pp),
        rebirth: format!("{:.1}", rebirth),
        current: format!("{:.1}", r.perf.pp),
        cur_live_pct: format!("{delta:+.2}"),
        cur_rebirth_pct: format!("{local_delta:+.2}"),
        map_f: format!("{:.3}", r.perf.timing_map_factor),
        score_a: format!("{:.3}", r.perf.timing_score_adjustment),
        loss_d: format!("{:+.4}", r.perf.timing_loss_diff),
    }
}

fn truncate(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_owned()
    } else {
        format!("{}...", &s[..max_len.saturating_sub(3)])
    }
}

fn summarise_group(label: &str, rows: &[&MultiPriced]) {
    if rows.is_empty() {
        return;
    }
    let n = rows.len() as f64;
    let mean_live = rows.iter().map(|r| r.row.live_pp).sum::<f64>() / n;
    let mean_current = rows.iter().map(|r| r.perf.pp).sum::<f64>() / n;
    let delta_pct = if mean_live > 0.0 {
        (mean_current / mean_live - 1.0) * 100.0
    } else {
        0.0
    };
    println!(
        "  {:<50} n={:<4}  live {:.1}  current {:.1}  delta {:+.2}%",
        label,
        rows.len(),
        mean_live,
        mean_current,
        delta_pct
    );
}

fn output_text(scores: &[MultiPriced]) {
    let mut by_uid: BTreeMap<&str, Vec<&MultiPriced>> = BTreeMap::new();
    for score in scores {
        by_uid
            .entry(score.row.uid.as_str())
            .or_default()
            .push(score);
    }
    for (uid, rows) in &by_uid {
        let mut rows = rows.clone();
        rows.sort_by(|a, b| b.perf.pp.total_cmp(&a.perf.pp));
        println!("\n=== uid {uid} ({} scores)", rows.len());
        let table_rows: Vec<ScoreRow> = rows.iter().map(|r| score_row(r)).collect();
        let mut table = Table::new();
        table.load_style(comfy_table::presets::UTF8_FULL_CONDENSED.with_rounded_corners());
        table.set_content_arrangement(ContentArrangement::Dynamic);
        table.set_header([
            "map",
            "mods",
            "k",
            "od",
            "rice",
            "ln",
            "ttj",
            "320",
            "300",
            "200",
            "100",
            "50",
            "miss",
            "sr",
            "live_stars",
            "acc%",
            "expected%",
            "sigma",
            "live",
            "rebirth",
            "current",
            "cur/live%",
            "cur/rebirth%",
            "map_f",
            "score_a",
            "loss_d",
        ]);
        for row in table_rows {
            let tint = {
                let moved = [row.cur_live_pct.as_str(), row.cur_rebirth_pct.as_str()]
                    .iter()
                    .filter_map(|value| value.parse::<f64>().ok())
                    .map(f64::abs)
                    .fold(0.0, f64::max);
                if moved > 20.0 {
                    Some((Color::DarkYellow, Color::Black))
                } else if moved > 10.0 {
                    Some((Color::DarkRed, Color::Reset))
                } else {
                    None
                }
            };
            let mut cells = vec![
                Cell::new(row.map.to_string()),
                Cell::new(row.mods),
                Cell::new(row.k.to_string()),
                Cell::new(row.od),
                Cell::new(row.rice.to_string()),
                Cell::new(row.ln.to_string()),
                Cell::new(row.ttj.to_string()),
                Cell::new(row.n320.to_string()),
                Cell::new(row.n300.to_string()),
                Cell::new(row.n200.to_string()),
                Cell::new(row.n100.to_string()),
                Cell::new(row.n50.to_string()),
                Cell::new(row.miss.to_string()),
                Cell::new(row.sr),
                Cell::new(row.live_stars),
                Cell::new(row.acc_pct),
                Cell::new(row.expected_pct),
                Cell::new(row.sigma),
                Cell::new(row.live),
                Cell::new(row.rebirth),
                Cell::new(row.current),
                Cell::new(row.cur_live_pct),
                Cell::new(row.cur_rebirth_pct),
                Cell::new(row.map_f),
                Cell::new(row.score_a),
                Cell::new(row.loss_d),
            ];
            if let Some((bg, fg)) = tint {
                cells = cells.into_iter().map(|cell| cell.bg(bg).fg(fg)).collect();
            }
            table.add_row(cells);
        }
        for column in table.column_iter_mut() {
            column.set_cell_alignment(CellAlignment::Right);
        }
        println!("{table}");
        println!("  beatmaps:");
        for r in rows.iter().take(8) {
            println!(
                "    {:>8}  {} [{}]",
                r.row.map_id,
                truncate(&r.row.title, 52),
                truncate(&r.row.version, 34)
            );
        }
        if rows.len() > 8 {
            println!("    ... and {} more", rows.len() - 8);
        }
        summarise_group(&format!("uid {uid} total"), &rows);
    }
    let all: Vec<&MultiPriced> = scores.iter().collect();
    println!(
        "\n=== overall ({} scores, {} users)",
        all.len(),
        by_uid.len()
    );
    summarise_group("all", &all);
    println!("\nby window-affecting mod:");
    summarise_group(
        "EZ (windows widened)",
        &all.iter()
            .copied()
            .filter(|r| r.row.mods.contains("EZ"))
            .collect::<Vec<_>>(),
    );
    summarise_group(
        "HR (windows narrowed)",
        &all.iter()
            .copied()
            .filter(|r| r.row.mods.contains("HR"))
            .collect::<Vec<_>>(),
    );
    summarise_group(
        "no window mod",
        &all.iter()
            .copied()
            .filter(|r| !r.row.mods.contains("EZ") && !r.row.mods.contains("HR"))
            .collect::<Vec<_>>(),
    );
    println!("\nno-window-mod scores by OD (natural-window baseline):");
    let plain: Vec<&MultiPriced> = all
        .iter()
        .copied()
        .filter(|r| !r.row.mods.contains("EZ") && !r.row.mods.contains("HR"))
        .collect();
    for (lo, hi) in [(0.0, 7.0), (7.0, 7.9), (7.9, 8.1), (8.1, 8.9), (8.9, 11.0)] {
        let band: Vec<&MultiPriced> = plain
            .iter()
            .copied()
            .filter(|r| f64::from(r.map.od) >= lo && f64::from(r.map.od) < hi)
            .collect();
        if band.is_empty() {
            continue;
        }
        let n = band.len() as f64;
        let scalars: Vec<f64> = band
            .iter()
            .map(|r| {
                if r.perf.xxy_pp_pattern.abs() > f64::EPSILON {
                    r.perf.pp / r.perf.xxy_pp_pattern
                } else {
                    1.0
                }
            })
            .collect();
        println!(
            "  OD {lo:>4.1}-{hi:<4.1} n={:<4} mean scalar {:.4} ({:.4}..{:.4})  mean dPP {:+.2}%",
            band.len(),
            scalars.iter().sum::<f64>() / n,
            scalars.iter().copied().fold(f64::INFINITY, f64::min),
            scalars.iter().copied().fold(f64::NEG_INFINITY, f64::max),
            band.iter()
                .map(|r| (r.perf.pp / r.row.live_pp - 1.0) * 100.0)
                .sum::<f64>()
                / n
        );
    }
    println!("\nby key count (reported as a structural cohort, not a model input):");
    for keys in [4u32, 5, 6, 7, 8, 9, 10] {
        let band: Vec<&MultiPriced> = all.iter().copied().filter(|r| r.row.keys == keys).collect();
        if band.is_empty() {
            continue;
        }
        let n = band.len() as f64;
        let mean_od = band.iter().map(|r| f64::from(r.map.od)).sum::<f64>() / n;
        let mean_ln = band
            .iter()
            .map(|r| {
                if r.attrs.n_objects > 0 {
                    r.attrs.n_long_notes as f64 / r.attrs.n_objects as f64
                } else {
                    0.0
                }
            })
            .sum::<f64>()
            / n;
        summarise_group(
            &format!("{keys}k (mean OD {mean_od:.1}, LN {:.0}%)", 100.0 * mean_ln),
            &band,
        );
        if band.len() >= 8 {
            for (sub, lo, hi) in [("  rice <30% LN", 0.0, 0.3), ("  LN >=30%", 0.3, 1.01)] {
                let inner: Vec<&MultiPriced> = band
                    .iter()
                    .copied()
                    .filter(|r| {
                        let ratio = if r.attrs.n_objects > 0 {
                            r.attrs.n_long_notes as f64 / r.attrs.n_objects as f64
                        } else {
                            0.0
                        };
                        ratio >= lo && ratio < hi
                    })
                    .collect();
                if inner.len() >= 3 {
                    let mean_inner_od =
                        inner.iter().map(|r| f64::from(r.map.od)).sum::<f64>() / inner.len() as f64;
                    summarise_group(&format!("{sub} (mean OD {mean_inner_od:.1})"), &inner);
                }
            }
        }
    }
}

fn output_delimited(scores: &[MultiPriced], delimiter: char) {
    let rows: Vec<_> = scores.iter().collect();
    let headers = [
        "map",
        "artist",
        "title",
        "diff",
        "mods",
        "k",
        "od",
        "rice",
        "ln",
        "ttj",
        "320",
        "300",
        "200",
        "100",
        "50",
        "miss",
        "sr",
        "live_stars",
        "acc%",
        "expected%",
        "sigma",
        "live",
        "rebirth",
        "current",
        "cur/live%",
        "cur/rebirth%",
        "map_f",
        "score_a",
        "loss_d",
    ];
    println!("{}", headers.join(&delimiter.to_string()));
    for row in rows {
        let line = [
            row.row.map_id.parse::<u32>().unwrap_or(0).to_string(),
            row.row.artist.clone(),
            row.row.title.clone(),
            row.row.version.clone(),
            row.row.mods.clone(),
            row.row.keys.to_string(),
            format!("{:.1}", row.map.od),
            (row.attrs.n_objects - row.attrs.n_long_notes).to_string(),
            row.attrs.n_long_notes.to_string(),
            (if row.attrs.ln_judged_as_one {
                row.attrs.n_objects
            } else {
                row.attrs.n_objects + row.attrs.n_long_notes
            })
            .to_string(),
            row.row.counts[0].to_string(),
            row.row.counts[1].to_string(),
            row.row.counts[2].to_string(),
            row.row.counts[3].to_string(),
            row.row.counts[4].to_string(),
            row.row.counts[5].to_string(),
            format!("{:.3}", row.attrs.stars),
            format!("{:.3}", row.row.live_stars),
            format!("{:.3}", row.row.acc),
            format!("{:.3}", row.attrs.timing_expected_accuracy * 100.0),
            row.perf.timing_core_sigma.to_string(),
            row.row.live_pp.to_string(),
            (row.perf.xxy_pp_pattern + row.perf.xxy_pp_accuracy).to_string(),
            row.perf.pp.to_string(),
            ((row.perf.pp / row.row.live_pp - 1.0) * 100.0).to_string(),
            ((row.perf.pp / (row.perf.xxy_pp_pattern + row.perf.xxy_pp_accuracy) - 1.0) * 100.0)
                .to_string(),
            row.perf.timing_map_factor.to_string(),
            row.perf.timing_score_adjustment.to_string(),
            row.perf.timing_loss_diff.to_string(),
        ];
        println!(
            "{}",
            line.iter()
                .map(|value| quote_field(value, delimiter))
                .collect::<Vec<_>>()
                .join(&delimiter.to_string())
        );
    }
}

fn quote_field(value: &str, delimiter: char) -> String {
    if value.contains(delimiter)
        || value.contains('"')
        || value.contains('\n')
        || value.contains('\r')
    {
        format!("\"{}\"", value.replace('"', "\"\""))
    } else {
        value.to_owned()
    }
}

fn output(scores: &[MultiPriced], format: OutputFormat) {
    match format {
        OutputFormat::Text => output_text(scores),
        OutputFormat::Tsv => output_delimited(scores, '\t'),
        OutputFormat::Csv => output_delimited(scores, ','),
    }
}

fn main() {
    let args = Args::parse();
    let users = load_user(&args.tsv, &args.maps);
    let scores = batched_calculation(users);
    if scores.is_empty() {
        match args.output_format {
            OutputFormat::Text => {
                eprintln!(
                    "no fixtures present ({}); nothing to report",
                    args.tsv.display()
                )
            }
            OutputFormat::Csv | OutputFormat::Tsv => {}
        }
        return;
    }
    output(&scores, args.output_format);
}

#[cfg(test)]
mod tests {
    use super::quote_field;
    #[test]
    fn delimited_fields_are_escaped() {
        assert_eq!(quote_field("a,b", ','), "\"a,b\"");
        assert_eq!(quote_field("a\"b", ','), "\"a\"\"b\"");
        assert_eq!(quote_field("a\tb", '\t'), "\"a\tb\"");
    }
}
