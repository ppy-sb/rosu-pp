use rosu_pp::Beatmap;

const TEST_CASES: [(&str, u32, u32, u32); 23] = [
    ("Sunglow", 2486881, 200, 1000),            // HDDTRX
    ("Sidetracked Day", 1537566, 200, 1100),    // HDDTRX
    ("Team Magma", 2097898, 200, 870),          // HDDTRX
    ("Kimi no Kioku", 1045757, 200, 750),       // HDDTRX
    ("Scarlet Rose", 131564, 200, 600),         // HDDTRX
    ("ROR", 869222, 200, 1250),                 // HDDTRX
    ("Euphoria", 1861487, 144, 1150),           // HRRX
    ("Genryuu Kaiko", 433005, 144, 950),        // HRRX
    ("Lovely Icecream", 1415526, 144, 650),     // HRRX
    ("True truly love", 955615, 128, 850),      // RX
    ("The Sun The Moon", 1949106, 200, 1900),   // HDDTRX
    ("Roar of The Jungle", 3306465, 200, 3400), // HDDTRX
    ("Cosmic cortex", 787307, 200, 1600),       // HDDTRX
    ("Chronostasis", 2874408, 200, 1950),       // HDDTRX
    ("quaver", 918723, 200, 1750),              // HDDTRX
    ("raise my sword", 1860169, 200, 1350),     // HDDTRX
    ("Hardware Store", 1989203, 216, 1375),     // HDHRDTRX
    ("Songs Compilation", 1849580, 200, 1575),  // HDDTRX
    ("Shinryu Monogatari", 2818772, 144, 1125), // HRRX
    ("THE PLATINUM", 4352925, 128, 1350),       // RX
    ("Charge-Parity", 4349848, 128, 1000),      // RX
    ("Fallen Symphony", 4042579, 128, 1000),    // RX
    ("TERRAFORGE", 4439703, 128, 1150),         // RX
];

#[test]
fn calculate_batch() {
    println!(
        "{0: <20} | {1: <10} | {2: <12} | {3: <12} | {4: <10} | {5: <10}",
        "title", "mods", "pp", "weighted_pp", "target_pp", "delta"
    );
    let weight = get_weight();
    for (title, beatmap_id, mods, target_pp) in TEST_CASES {
        let map = Beatmap::from_path(format!("./resources/{}.osu", beatmap_id)).unwrap();
        let performance = rosu_pp::Performance::new(&map);
        let pp_value = performance.accuracy(99.5).mods(mods).calculate().pp();
        let pp_weighted = pp_value * weight;
        println!(
            "{0: <20} | {1: <10} | {2: <12} | {3: <12} | {4: <10} | {5: <10}",
            title,
            mods,
            format!("{:.2}", pp_value),
            format!("{:.2}", pp_weighted),
            format!("{:.2}", target_pp),
            format!("{:.2}", pp_weighted - target_pp as f64)
        );
    }
}

fn get_weight() -> f64 {
    let map = Beatmap::from_path("./resources/2486881.osu").unwrap();
    let performance = rosu_pp::Performance::new(&map);
    let pp_value = performance.accuracy(99.5).mods(200).calculate().pp();
    1000.0 / pp_value
}
