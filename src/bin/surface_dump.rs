use clap::Parser;
use rosu_mods::GameMod;
use rosu_pp::mania::sunny::{judgement_units, per_note_difficulty};
use rosu_pp::mania::sunny_accuracy::{
    expected_counts_at_core_sigma, ln_sigma_scale_for_duration, sigma_scale_from_difficulty_ratio,
    ErrorModel, JudgementUnit, TIMING_BASELINE_SIGMA,
};
use rosu_pp::mania::sunny_windows::{ManiaHitWindows, ManiaJudgement};
use rosu_pp::report_utils::{calculate, parse, single_mod};
use rosu_pp::{Difficulty, GameMods};
use std::fmt::Write as _;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "surface_dump")]
#[command(about = "Generates CSV data for visualizing the accuracy surface")]
struct Args {
    /// Path to .osu file (optional, defaults to synthetic Decoy slice)
    #[arg(long)]
    map: Option<PathBuf>,

    /// Clock rate multiplier
    #[arg(long, default_value = "1.0")]
    clock_rate: f64,

    /// Core timing spread for per-note overlay (ms)
    #[arg(long)]
    core_sigma: Option<f64>,

    /// Comma-separated sigma values to sample
    #[arg(long, value_delimiter = ',')]
    sigmas: Vec<f64>,
    /// Directory for generated CSV data. Defaults to a temporary directory.
    #[arg(long, default_value = "target/surface", env = "SURFACE_DATA_DIR")]
    data_dir: PathBuf,
}

const REFERENCE_WINDOWS: ManiaHitWindows = ManiaHitWindows {
    perfect: 16.5,
    great: 40.5,
    good: 76.0,
    ok: 106.0,
    meh: 127.0,
    miss: 164.0,
};

fn main() {
    let args = Args::parse();
    let model = ErrorModel::default();
    let dir = args.data_dir;
    std::fs::create_dir_all(&dir).unwrap();

    // A real map supplies the slice difficulty, judgement-unit population, and
    // actual NM/EZ/HR windows. With no map, preserve the reproducible synthetic
    // Decoy slice used by the original visualiser.
    let map_slice = args.map.as_ref().map(|path| {
        let map = parse(&path.to_string_lossy())
            .unwrap_or_else(|| panic!("cannot parse {}", path.display()));
        let attrs = calculate(
            &map,
            &GameMods::default(),
            args.clock_rate,
            Some(true),
            None,
        )
        .unwrap_or_else(|| panic!("{} is not a mania map", path.display()));

        (path.to_string_lossy().to_string(), map, attrs)
    });

    // Log-spaced in both axes: skill spans orders of magnitude and difficulty is
    // multiplicative in `sigma`, so a linear grid would waste most of its rows.
    let geom = |low: f64, high: f64, steps: usize| -> Vec<f64> {
        (0..steps)
            .map(|i| {
                let t = i as f64 / (steps - 1) as f64;
                low * (high / low).powf(t)
            })
            .collect()
    };

    let difficulties = geom(2.0, 20.0, 121);

    // Use provided sigmas or default sigma calculation
    let sigmas: Vec<f64> = if !args.sigmas.is_empty() {
        args.sigmas.clone()
    } else {
        // Default: map skill range to sigma range for backward compatibility
        let skills = geom(0.5, 60.0, 161);
        skills
            .iter()
            .map(|&s| {
                // Map skill range [0.5, 60] to sigma range [30, 5] (inverse relationship)
                // Higher skill -> lower sigma (tighter timing)
                let log_skill = s.ln();
                let log_min = 0.5_f64.ln();
                let log_max = 60.0_f64.ln();
                let t = (log_skill - log_min) / (log_max - log_min);
                30.0 * (5.0_f64 / 30.0).powf(t.clamp(0.0, 1.0))
            })
            .collect()
    };

    let mut grid = String::from("difficulty,sigma,accuracy,miss_rate\n");

    for &difficulty in &difficulties {
        for &sigma in &sigmas {
            let units = [JudgementUnit::new(difficulty)];
            let expected = expected_counts_at_core_sigma(&units, &REFERENCE_WINDOWS, &model, sigma);
            writeln!(
                grid,
                "{difficulty},{sigma},{},{}",
                expected.custom_accuracy(),
                expected.get(ManiaJudgement::Miss) / expected.total()
            )
            .unwrap();
        }
    }

    std::fs::write(dir.join("grid.csv"), grid).unwrap();

    let map_difficulty = map_slice
        .as_ref()
        .map_or(13.774, |(_, _, attrs)| attrs.stars);
    let source = map_slice
        .as_ref()
        .map_or("default (Decoy DT)", |(path, _, _)| path.as_str());
    let units = map_slice
        .as_ref()
        .map(|(_, _, attrs)| judgement_units(attrs, 1.0, &model, true))
        .unwrap_or_else(|| vec![JudgementUnit::new(map_difficulty)]);
    // Match the sigma used by Sunny's pp calculation for this map. The CLI
    // override remains useful for inspecting alternate player spreads.
    let default_sigma = map_slice
        .as_ref()
        .map_or(TIMING_BASELINE_SIGMA, |(_, _, attrs)| {
            attrs.base_timing_sigma
        });
    let core_sigma = args
        .core_sigma
        .filter(|sigma| sigma.is_finite() && *sigma > 0.0)
        .unwrap_or(default_sigma);

    std::fs::write(
        dir.join("surface_2d_meta.csv"),
        format!(
            "difficulty,clock_rate,core_sigma,source\n{map_difficulty},{},{core_sigma},{source}\n",
            args.clock_rate
        ),
    )
    .unwrap();

    println!("core sigma: {core_sigma:.3} ms (default: {default_sigma:.3} ms)");

    if let Some((_, _, attrs)) = map_slice.as_ref() {
        if let Some(bins) = attrs.input_state_bins {
            let mut csv = String::from(
                "class,bin,count,mean_difficulty,mean_duration_ms,mean_gap_ms,mean_chord_width,mean_other_held\n",
            );
            for (idx, bin) in bins.iter().enumerate() {
                if bin.count == 0 {
                    continue;
                }
                writeln!(
                    csv,
                    "{:?},{},{},{},{},{},{},{}",
                    bin.class,
                    idx % rosu_pp::mania::sunny::NOTE_DIFFICULTY_BINS,
                    bin.count,
                    bin.mean_difficulty,
                    bin.mean_duration_ms,
                    bin.mean_gap_ms,
                    bin.mean_chord_width,
                    bin.mean_other_held
                )
                .unwrap();
            }
            std::fs::write(dir.join("input_state_bins.csv"), csv).unwrap();
        }
    }
    if let Some((_, map, _)) = map_slice.as_ref() {
        let difficulty = Difficulty::new();
        if let Some(per_note) = per_note_difficulty(&difficulty, map) {
            let mut csv = String::from("time_ms,note_index,difficulty,hold_duration_ms\n");
            for (idx, ((difficulty, duration), object)) in
                per_note.iter().zip(&map.hit_objects).enumerate()
            {
                writeln!(
                    csv,
                    "{},{},{},{}",
                    object.start_time,
                    idx,
                    difficulty,
                    duration.unwrap_or(0.0)
                )
                .unwrap();
            }
            std::fs::write(dir.join("per_note_difficulty.csv"), csv).unwrap();

            if let Some((_, _, attrs)) = map_slice.as_ref() {
                let model = ErrorModel::default();
                let reference_difficulty =
                    per_note.iter().map(|(d, _)| d).sum::<f64>() / per_note.len() as f64;
                let mut csv = String::from(
                    "time_ms,note_index,variant,difficulty,miss,p50,p100,p200,p300,p320,custom_accuracy,acc_d\n",
                );
                for (idx, ((difficulty, duration), object)) in
                    per_note.iter().zip(&map.hit_objects).enumerate()
                {
                    for variant in ["baseline", "ln_as_rice"] {
                        let unit = if variant == "baseline" {
                            duration.map_or_else(
                                || {
                                    JudgementUnit::new(*difficulty).with_sigma_scale(
                                        sigma_scale_from_difficulty_ratio(
                                            *difficulty,
                                            reference_difficulty,
                                        ),
                                    )
                                },
                                |duration| {
                                    JudgementUnit::long_note(*difficulty, 1.0, &model, duration)
                                        .with_sigma_scale(
                                            sigma_scale_from_difficulty_ratio(
                                                *difficulty,
                                                reference_difficulty,
                                            ) * ln_sigma_scale_for_duration(&model, duration),
                                        )
                                },
                            )
                        } else {
                            JudgementUnit::new(*difficulty).with_sigma_scale(
                                sigma_scale_from_difficulty_ratio(
                                    *difficulty,
                                    reference_difficulty,
                                ),
                            )
                        };
                        let counts = expected_counts_at_core_sigma(
                            &[unit],
                            &attrs.hit_windows,
                            &model,
                            core_sigma,
                        );
                        let p = counts.as_array();
                        let accuracy = counts.custom_accuracy();
                        let acc_d = difficulty * (1.0 - accuracy);
                        writeln!(
                            csv,
                            "{},{},{},{},{},{},{},{},{},{},{},{}",
                            object.start_time,
                            idx,
                            variant,
                            difficulty,
                            p[5], // miss
                            p[4], // p50
                            p[3], // p100
                            p[2], // p200
                            p[1], // p300
                            p[0], // p320
                            accuracy,
                            acc_d
                        )
                        .unwrap();
                    }
                }
                std::fs::write(dir.join("per_note_expected_counts.csv"), csv).unwrap();
            }
        }
    }

    // In the new timing-based system, we vary core_sigma instead of skill
    let mut bands = String::from("sigma,n320,n300,n200,n100,n50,miss,accuracy\n");

    for &sigma in &sigmas {
        let windows = map_slice
            .as_ref()
            .map_or(REFERENCE_WINDOWS, |(_, _, attrs)| attrs.hit_windows);
        let expected = expected_counts_at_core_sigma(&units, &windows, &model, sigma);
        let total = expected.total();
        let share = |judgement| expected.get(judgement) / total;

        writeln!(
            bands,
            "{},{},{},{},{},{},{},{}",
            sigma,
            share(ManiaJudgement::Perfect),
            share(ManiaJudgement::Great),
            share(ManiaJudgement::Good),
            share(ManiaJudgement::Ok),
            share(ManiaJudgement::Meh),
            share(ManiaJudgement::Miss),
            expected.custom_accuracy()
        )
        .unwrap();
    }

    std::fs::write(dir.join("bands.csv"), bands).unwrap();

    // The same slice under different windows. Named by GREAT window since that is
    // the single parameter the rest are derived from.
    let window_sets = if let Some((_, map, attrs)) = &map_slice {
        let hr_mods = single_mod(GameMod::HardRockMania(Default::default()));
        let ez_mods = single_mod(GameMod::EasyMania(Default::default()));
        let hr_attrs = calculate(map, &hr_mods, args.clock_rate, Some(true), None).unwrap();
        let ez_attrs = calculate(map, &ez_mods, args.clock_rate, Some(true), None).unwrap();

        vec![
            ("HR", hr_attrs.hit_windows.great),
            ("natural", attrs.map_windows.great),
            ("NM", attrs.hit_windows.great),
            ("EZ", ez_attrs.hit_windows.great),
        ]
    } else {
        vec![
            ("HR OD7 DT", 30.5_f64),
            ("reference OD8", 40.5),
            ("OD7 DT", 43.0),
            ("EZ OD7 DT", 60.3),
        ]
    };

    let mut windows_csv = String::from("label,great,sigma,accuracy\n");

    for (label, great) in window_sets {
        let windows = if label == "natural" {
            map_slice.as_ref().unwrap().2.map_windows
        } else if (great - 40.5).abs() < 1e-9 {
            REFERENCE_WINDOWS
        } else {
            ManiaHitWindows {
                perfect: 16.5,
                great,
                good: 76.0,
                ok: 106.0,
                meh: 127.0,
                miss: 164.0,
            }
        };

        for &sigma in &sigmas {
            let accuracy =
                expected_counts_at_core_sigma(&units, &windows, &model, sigma).custom_accuracy();
            writeln!(windows_csv, "{label},{great},{sigma},{accuracy}").unwrap();
        }
    }

    std::fs::write(dir.join("windows.csv"), windows_csv).unwrap();

    println!(
        "wrote {} (grid {} x {}, slice {:.3} stars from {})",
        dir.display(),
        difficulties.len(),
        sigmas.len(),
        map_difficulty,
        source,
    );
}
