//! Batch score calculator.
//!
//! Run with: `cargo run --release --features reports --bin batch -- --help`

use clap::{Parser, ValueEnum};
use rayon::iter::ParallelIterator;
use rosu_pp::mania::{
    Mania, ManiaDifficultyAttributes, ManiaPerformance, ManiaScoreState,
    SunnyManiaDifficultyAttributes, SunnyScoreState,
};
use rosu_pp::model::mode::IGameMode;
use rosu_pp::report_utils::{BatchCalculator, batch, mods_for};
use rosu_pp::{Difficulty, GameMods};
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, ValueEnum)]
#[value(rename_all = "lower")]
enum Algorithm {
    Rebirth,
    Sunny,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
#[value(rename_all = "kebab-case")]
enum InputSchema {
    Score,
    MultiuserReport,
}

#[derive(Parser)]
#[command(about = "Batch-computes mania difficulty and performance values")]
#[command(
    name = "batch",
    after_help = "Input TSV schemas (without a header):\n  score:            map_id\\tmods\\tkeys\\t320\\t300\\t200\\t100\\t50\\tmiss\n  multiuser-report: uid\\tscore_id\\tmap_id\\tmods\\tlive_stars\\tod\\tkeys\\t320\\t300\\t200\\t100\\t50\\tmiss\\taccuracy\\tlive_pp\\tartist\\ttitle\\tversion"
)]
struct Args {
    /// Path to the input score TSV file.
    #[arg(long, default_value = "local-fixtures/batch.tsv")]
    tsv: PathBuf,

    /// Directory containing .osu map files.
    #[arg(long, default_value = "local-fixtures/maps")]
    maps: PathBuf,

    /// Algorithm used for the computed stars and pp values.
    #[arg(long, value_enum, default_value_t = Algorithm::Sunny)]
    algorithm: Algorithm,

    /// Input TSV schema.
    #[arg(long, value_enum, default_value_t = InputSchema::Score)]
    input_schema: InputSchema,
}

#[derive(Clone)]
struct InputRow {
    map_id: String,
    mods: String,
    keys: String,
    counts: [u32; 6],
}

struct OutputRow {
    input: InputRow,
    stars: f64,
    pp: f64,
}

enum Attributes {
    Rebirth(Arc<ManiaDifficultyAttributes>),
    Sunny(Arc<SunnyManiaDifficultyAttributes>),
}

struct Calculator {
    maps: PathBuf,
    algorithm: Algorithm,
}

impl BatchCalculator<InputRow> for Calculator {
    type Attributes = Attributes;
    type Map = rosu_pp::Beatmap;
    type MapKey = String;
    type ModKey = String;

    fn map_key(&self, row: &InputRow) -> Self::MapKey {
        row.map_id.clone()
    }

    fn mod_key(&self, row: &InputRow) -> Self::ModKey {
        row.mods.clone()
    }

    fn load_map(&self, map_id: &Self::MapKey) -> Option<Self::Map> {
        let bytes = std::fs::read(self.maps.join(format!("{map_id}.osu"))).ok()?;
        rosu_pp::Beatmap::from_bytes(&bytes).ok()
    }

    fn calculate(&self, map: &Self::Map, mod_names: &Self::ModKey) -> Option<Self::Attributes> {
        let (mods, clock_rate) = mods_for(mod_names);

        match self.algorithm {
            Algorithm::Rebirth => Some(Attributes::Rebirth(Arc::new(
                Mania::difficulty(&Difficulty::new().mods(mods).clock_rate(clock_rate), map)
                    .ok()?,
            ))),
            Algorithm::Sunny => Some(Attributes::Sunny(Arc::new(
                rosu_pp::report_utils::calculate(
                    map,
                    &mods,
                    clock_rate,
                    Some(!mod_names.contains("V2")),
                    None,
                )?,
            ))),
        }
    }
}

fn load_rows(path: &PathBuf, schema: InputSchema) -> Vec<InputRow> {
    let Ok(text) = std::fs::read_to_string(path) else {
        eprintln!("Cannot read {}", path.display());
        return Vec::new();
    };

    text.lines()
        .filter_map(|line| {
            let fields: Vec<_> = line.split('\t').collect();
            let number = |field: &str| field.parse().unwrap_or(0);
            let (map_id, mods, keys, counts) = match schema {
                InputSchema::Score if fields.len() >= 9 => (
                    fields[0],
                    fields[1],
                    fields[2],
                    [
                        number(fields[3]),
                        number(fields[4]),
                        number(fields[5]),
                        number(fields[6]),
                        number(fields[7]),
                        number(fields[8]),
                    ],
                ),
                InputSchema::MultiuserReport if fields.len() >= 18 => (
                    fields[2],
                    fields[3],
                    fields[6],
                    [
                        number(fields[7]),
                        number(fields[8]),
                        number(fields[9]),
                        number(fields[10]),
                        number(fields[11]),
                        number(fields[12]),
                    ],
                ),
                _ => return None,
            };

            if map_id == "map_id" || map_id == "uid" {
                return None;
            }

            Some(InputRow {
                map_id: map_id.to_owned(),
                mods: mods.to_owned(),
                keys: keys.to_owned(),
                counts,
            })
        })
        .collect()
}

fn calculate_sunny(
    attrs: &SunnyManiaDifficultyAttributes,
    mods: &GameMods,
    row: &InputRow,
) -> (f64, f64) {
    let state = SunnyScoreState {
        n320: row.counts[0],
        n300: row.counts[1],
        n200: row.counts[2],
        n100: row.counts[3],
        n50: row.counts[4],
        misses: row.counts[5],
    };
    let perf = rosu_pp::mania::sunny::calculate_performance(attrs, mods, state);
    (attrs.stars, perf.pp)
}

fn run(rows: Vec<InputRow>, maps: PathBuf, algorithm: Algorithm) -> Vec<OutputRow> {
    batch(
        rows,
        Calculator { maps, algorithm },
        |(_, mod_names, attrs, rows)| {
            let (mods, _) = mods_for(&mod_names);
            let output: Vec<_> = rows
                .into_iter()
                .filter_map(|row| {
                    let (stars, pp) = match attrs.as_ref() {
                        Attributes::Rebirth(attrs) => {
                            let state = ManiaScoreState {
                                n320: row.counts[0],
                                n300: row.counts[1],
                                n200: row.counts[2],
                                n100: row.counts[3],
                                n50: row.counts[4],
                                misses: row.counts[5],
                            };
                            let perf = ManiaPerformance::new(attrs.as_ref().clone())
                                .mods(mods.clone())
                                .state(state)
                                .calculate()
                                .ok()?;
                            (perf.stars(), perf.pp())
                        }
                        Attributes::Sunny(attrs) => calculate_sunny(attrs, &mods, &row),
                    };
                    Some(OutputRow {
                        input: row,
                        stars,
                        pp,
                    })
                })
                .collect();
            Some(output)
        },
    )
    .flatten()
    .collect()
}

fn main() {
    let args = Args::parse();
    let rows = run(
        load_rows(&args.tsv, args.input_schema),
        args.maps,
        args.algorithm,
    );
    println!("map\tmods\tkeys\t320\t300\t200\t100\t50\tmiss\tstars\tpp");
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{:.6}\t{:.6}",
            row.input.map_id,
            row.input.mods,
            row.input.keys,
            row.input.counts[0],
            row.input.counts[1],
            row.input.counts[2],
            row.input.counts[3],
            row.input.counts[4],
            row.input.counts[5],
            row.stars,
            row.pp
        );
    }
}
