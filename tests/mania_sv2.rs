use rosu_pp::Beatmap;

#[test]
fn calculate_batch() {
    // Kikai No Tsubasa (feat. Kasane Teto)
    let map = Beatmap::from_path("./resources/5269878.osu").unwrap();

    let cases = [(0u32, true), (0u32, false), (1u32 << 29, false)];

    println!("\n{:<12} {:<10} {:>10}", "mods", "lazer", "pp");
    println!("{}", "-".repeat(34));

    for (mods, lazer) in cases {
        let pp = rosu_pp::Performance::new(&map)
            .mods(mods)
            .lazer(lazer)
            .n_geki(6988)
            .n300(5064)
            .n_katu(1804)
            .n100(467)
            .n50(275)
            .misses(296)
            .calculate()
            .pp();

        println!("{:<12} {:<10} {:>10.2}", mods, lazer, pp);
    }
}
