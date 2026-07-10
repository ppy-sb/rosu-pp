use rosu_pp::{Beatmap, Difficulty, mania::Mania};

const MANIA: &str = "./resources/1638954.osu";

#[track_caller]
fn assert_close(actual: f64, expected: f64) {
    let diff = (actual - expected).abs();
    assert!(diff < 1e-9, "{actual} != {expected}; diff={diff}");
}

#[test]
fn sunnyxxy_reference_mania_nm() {
    let map = Beatmap::from_path(MANIA).unwrap();
    let attrs = Difficulty::new().calculate_for_mode::<Mania>(&map).unwrap();

    assert_close(attrs.stars, 3.7126062330097978);
    assert_eq!(attrs.n_objects, 594);
    assert_eq!(attrs.n_hold_notes, 121);
    assert_eq!(attrs.max_combo, 956);
    assert!(!attrs.is_convert);
}

#[test]
fn sunnyxxy_reference_mania_dt() {
    let map = Beatmap::from_path(MANIA).unwrap();
    let attrs = Difficulty::new()
        .mods(1 << 6)
        .calculate_for_mode::<Mania>(&map)
        .unwrap();

    assert_close(attrs.stars, 5.4512352746465265);
    assert_eq!(attrs.n_objects, 594);
    assert_eq!(attrs.n_hold_notes, 121);
    assert_eq!(attrs.max_combo, 956);
    assert!(!attrs.is_convert);
}

#[test]
fn gradual_final_stars_match_full_difficulty() {
    let map = Beatmap::from_path(MANIA).unwrap();

    let full = Difficulty::new().calculate_for_mode::<Mania>(&map).unwrap();

    let gradual = Difficulty::new()
        .gradual_difficulty_for_mode::<Mania>(&map)
        .unwrap()
        .last()
        .unwrap();

    assert_close(gradual.stars, full.stars);
    assert_eq!(gradual.n_objects, full.n_objects);
    assert_eq!(gradual.n_hold_notes, full.n_hold_notes);
    assert_eq!(gradual.max_combo, full.max_combo);
}

#[test]
fn empty_map_has_zero_stars() {
    let map = Beatmap::from_bytes(&[]).unwrap();
    let attrs = Difficulty::new().calculate_for_mode::<Mania>(&map).unwrap();

    assert_eq!(attrs.stars, 0.0);
    assert_eq!(attrs.n_objects, 0);
}
