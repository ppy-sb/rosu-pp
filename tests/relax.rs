use rosu_pp::{Beatmap, Difficulty};

const TEST_CASES: [(&str, u32, Difficulty, u32); 23] = [
    ("Sunglow", 2486881, Difficulty::new().mods(200), 1000), // HDDTRX
    (
        "Sidetracked Day",
        1537566,
        Difficulty::new().mods(200),
        1100,
    ), // HDDTRX
    ("Team Magma", 2097898, Difficulty::new().mods(200), 870), // HDDTRX
    ("Kimi no Kioku", 1045757, Difficulty::new().mods(200), 750), // HDDTRX
    ("ROR", 869222, Difficulty::new().mods(200), 1250),      // HDDTRX
    ("Euphoria", 1861487, Difficulty::new().mods(144), 1000), // HRRX
    ("Genryuu Kaiko", 433005, Difficulty::new().mods(144), 900), // HRRX
    (
        "The Sun The Moon",
        1949106,
        Difficulty::new().mods(200),
        1850,
    ), // HRRX
    (
        "Roar of The Jungle",
        3306465,
        Difficulty::new().mods(200),
        3400,
    ), // HDDTRX
    ("Cosmic cortex", 787307, Difficulty::new().mods(200), 1350), // HDDTRX
    ("Chronostasis", 2874408, Difficulty::new().mods(200), 1950), // HDDTRX
    ("quaver", 918723, Difficulty::new().mods(200), 1750),   // HDDTRX
    ("raise my sword", 1860169, Difficulty::new().mods(200), 1350), // HDDTRX
    ("Hardware Store", 1989203, Difficulty::new().mods(216), 1375), // HDHRDTRX
    (
        "Songs Compilation",
        1849580,
        Difficulty::new().mods(200),
        1575,
    ), // HDDTRX
    (
        "Shinryu Monogatari",
        2818772,
        Difficulty::new().mods(144),
        1125,
    ), // HRRX
    ("THE PLATINUM", 4352925, Difficulty::new().mods(128), 1350), // RX
    ("Charge-Parity", 4349848, Difficulty::new().mods(128), 1000), // RX
    (
        "Fallen Symphony",
        4042579,
        Difficulty::new().mods(128),
        1000,
    ), // RX
    ("TERRAFORGE", 4439703, Difficulty::new().mods(128), 1150), // RX
    ("Lovely Icecream", 1415526, Difficulty::new().mods(144), 750), // HRRX
    ("Scarlet Rose", 131564, Difficulty::new().mods(200), 600), // HDDTRX
    ("True truly love", 955615, Difficulty::new().mods(128), 825), // RX
];

#[test]
fn calculate_batch() {
    println!(
        "{0: <20} | {1: <12} | {2: <12} | {3: <10} | {4: <10}",
        "title", "pp", "weighted_pp", "target_pp", "delta"
    );
    let weight = get_weight();
    for (title, beatmap_id, diff, target_pp) in TEST_CASES {
        let map = Beatmap::from_path(format!("./resources/{}.osu", beatmap_id)).unwrap();
        let performance = rosu_pp::Performance::new(diff.calculate(&map));
        let pp_value = performance.accuracy(99.5).calculate().pp();
        let pp_weighted = pp_value * weight;
        println!(
            "{0: <20} | {1: <12} | {2: <12} | {3: <10} | {4: <10}",
            title,
            format!("{:.2}", pp_value),
            format!("{:.2}", pp_weighted),
            format!("{:.2}", target_pp),
            format!("{:.2}", pp_weighted - target_pp as f64)
        );
    }
}

fn get_weight() -> f64 {
    let map = Beatmap::from_path("./resources/2486881.osu").unwrap();
    let performance = rosu_pp::Performance::new(Difficulty::new().mods(200).calculate(&map));
    let pp_value = performance.accuracy(99.5).calculate().pp();
    1000.0 / pp_value
}
