#[macro_use(lazy_static)]
extern crate lazy_static;

use std::{collections::HashMap, sync::Mutex};

use rosu_pp::{Beatmap, Difficulty};

const TEST_CASES: [(&str, u32, Difficulty); 7] = [
    ("Sunglow", 2486881, Difficulty::new().mods(200)), // HDDTRX
    ("Sidetracked Day", 1537566, Difficulty::new().mods(200)), // HDDTRX
    ("Team Magma", 2097898, Difficulty::new().mods(200)), // HDDTRX
    ("Kimi no Kioku", 1045757, Difficulty::new().mods(200)), // HDDTRX
    ("ROR", 869222, Difficulty::new().mods(200)),      // HDDTRX
    ("Euphoria", 1861487, Difficulty::new().mods(144)), // HRRX
    ("Genryuu Kaiko", 433005, Difficulty::new().mods(144)), // HDDTRX
];

lazy_static! {
    pub static ref PROCEDURE: Mutex<HashMap<String, String>> = Mutex::new(HashMap::new());
}

#[test]
fn calculate_batch() {
    println!(
        "{0: <20} | {1: <10} | {2: <10} | {3: <10}",
        "title", "pp_value", "procedure_1", "procedure_2"
    );
    for (title, beatmap_id, diff) in TEST_CASES {
        PROCEDURE.lock().unwrap().clear();
        let map = Beatmap::from_path(format!("./resources/{}.osu", beatmap_id)).unwrap();
        let performance = rosu_pp::Performance::new(diff.calculate(&map));
        let pp_value = performance.accuracy(99.5).calculate().pp();
        println!(
            "{0: <20} | {1: <10} | {2: <10} | {3: <10}",
            title,
            format!("{:.2}", pp_value),
            0,
            0
        );
    }
}
