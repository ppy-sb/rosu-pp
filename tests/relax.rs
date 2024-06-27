#[macro_use(lazy_static)]
extern crate lazy_static;

use std::{collections::HashMap, sync::Mutex};

use rosu_pp::{Beatmap, Difficulty};

const TEST_CASES: [(u32, Difficulty); 2] =
    [(2785319, Difficulty::new()), (2785319, Difficulty::new())];

lazy_static! {
    pub static ref PROCEDURE: Mutex<HashMap<String, String>> = Mutex::new(HashMap::new());
}

#[test]
fn calculate_batch() {
    println!(
        "{0: <10} | {1: <10} | {2: <10} | {3: <10}",
        "map_id", "pp_value", "procedure_1", "procedure_2"
    );
    for (beatmap_id, diff) in TEST_CASES {
        PROCEDURE.lock().unwrap().clear();
        let map = Beatmap::from_path(format!("./resources/{}.osu", beatmap_id)).unwrap();
        let performance = rosu_pp::Performance::new(diff.calculate(&map)).calculate();
        println!(
            "{0: <10} | {1: <10} | {2: <10} | {3: <10}",
            beatmap_id,
            format!("{:.2}", performance.pp()),
            0,
            0
        );
    }
}
