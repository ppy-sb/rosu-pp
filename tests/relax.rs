#[macro_use(lazy_static)]
extern crate lazy_static;

use std::{collections::HashMap, sync::Mutex};

use rosu_pp::{Beatmap, Difficulty};

const TEST_CASES: [(&str, u32, Difficulty); 23] = [
    ("Sunglow", 2486881, Difficulty::new().mods(200)), // HDDTRX
    ("Sidetracked Day", 1537566, Difficulty::new().mods(200)), // HDDTRX
    ("Team Magma", 2097898, Difficulty::new().mods(200)), // HDDTRX
    ("Kimi no Kioku", 1045757, Difficulty::new().mods(200)), // HDDTRX
    ("ROR", 869222, Difficulty::new().mods(200)),      // HDDTRX
    ("Euphoria", 1861487, Difficulty::new().mods(144)), // HRRX
    ("Oshama Scramble", 1972244, Difficulty::new().mods(144)), // HRRX
    ("The Sun The Moon", 1949106, Difficulty::new().mods(200)), // HRRX
    ("Roar of The Jungle", 3722572, Difficulty::new().mods(200)), // HDDTRX
    ("Cosmic cortex", 787307, Difficulty::new().mods(200)), // HDDTRX
    ("Chronostasis", 2874408, Difficulty::new().mods(200)), // HDDTRX
    ("quaver", 918723, Difficulty::new().mods(200)), // HDDTRX
    ("raise my sword", 1860169, Difficulty::new().mods(200)), // HDDTRX
    ("Hardware Store", 1989203, Difficulty::new().mods(216)), // HDDTRX
    ("Songs Compilation", 1849580, Difficulty::new().mods(200)), // HDDTRX
    ("Shinryu Monogatari", 2818772, Difficulty::new().mods(144)), // HRRX
    ("THE PLATINUM", 4352925, Difficulty::new().mods(0)), // RX
    ("Charge-Parity", 4349848, Difficulty::new().mods(0)), // RX
    ("Fallen Symphony", 4042579, Difficulty::new().mods(0)), // RX
    ("TERRAFORGE", 4439703, Difficulty::new().mods(0)), // RX
    ("Lovely Icecream", 1415526, Difficulty::new().mods(144)), // HRRX
    ("Scarlet Rose", 131564, Difficulty::new().mods(200)), // HDDTRX
    ("True truly love", 955615, Difficulty::new().mods(0)), // RX
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
