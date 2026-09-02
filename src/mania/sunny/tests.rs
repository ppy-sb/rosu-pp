use super::*;
use rosu_mods::{GameMod, GameMods as LazerMods};

mod recovery;
mod replay;

/// Parse and verify every replay in `SUNNY_REPLAY_BATCH`.
///
/// Run with:
/// `SUNNY_REPLAY_BATCH=local-fixtures/ladder.tsv cargo test --release replay_report -- --ignored --nocapture`
#[test]
#[ignore = "requires local replay fixtures"]
fn replay_report() {
    replay::report_from_env().expect("replay report failed");
}

const MAP_1638954: &str =
    r"C:\Users\uuzof\AppData\Local\Temp\opencode\rosu-pp\resources\1638954.osu";
const MAP_5269878: &str =
    r"C:\Users\uuzof\AppData\Local\Temp\opencode\rosu-pp\resources\5269878.osu";

fn single_mod(mods: &mut LazerMods, gamemod: GameMod) {
    mods.insert(gamemod);
}

/// Parse a beatmap, skipping the test if the resource file is unavailable.
fn parse(path: &str) -> Option<Beatmap> {
    let bytes = std::fs::read(path).ok()?;
    Beatmap::from_bytes(&bytes).ok()
}

/// A synthetic 4k map: `notes` evenly spaced notes cycling across columns.
///
/// The reference `.osu` files the older tests use live at absolute Windows
/// paths and are unavailable here, so those tests silently skip. Anything that
/// must actually run needs a map built in memory.
fn synthetic_map(od: f32, notes: usize, spacing: f64) -> Beatmap {
    let mut map = Beatmap::default();
    map.mode = GameMode::Mania;
    map.od = od;
    map.cs = 4.0;
    map.is_convert = false;

    map.hit_objects = (0..notes)
        .map(|idx| HitObject {
            pos: crate::model::hit_object::Pos {
                // Column from x position: lazer maps x to a column index by
                // `x * columns / 512`.
                x: (idx % 4) as f32 * 128.0 + 64.0,
                y: 192.0,
            },
            start_time: idx as f64 * spacing,
            kind: HitObjectKind::Circle,
        })
        .collect();

    map
}

/// As [`synthetic_map`], with every `hold_every`th note a hold of `hold_ms`.
///
/// [`synthetic_map`] emits only circles, so on its output every bin's `long` count is
/// zero and the long-note branch of the per-note path never runs. Varying the hold
/// length with the index also gives the bins something to average, so
/// `mean_duration` is exercised rather than being one repeated value.
fn synthetic_map_with_holds(
    od: f32,
    notes: usize,
    spacing: f64,
    hold_every: usize,
    hold_ms: f64,
) -> Beatmap {
    let mut map = synthetic_map(od, notes, spacing);

    for (idx, object) in map.hit_objects.iter_mut().enumerate() {
        if hold_every > 0 && idx % hold_every == 0 {
            object.kind = HitObjectKind::Hold(crate::model::hit_object::HoldNote {
                duration: hold_ms + (idx % 5) as f64 * 40.0,
            });
        }
    }

    map
}

fn input_note(column: usize, head: f64, tail: Option<f64>) -> Note {
    Note { column, head, tail }
}

/// Evaluate a candidate transition model before operations are grouped into bins.
fn exact_transition_oracle<F>(
    notes: &[Note],
    total_columns: usize,
    mut candidate: F,
) -> Vec<(ClassifiedOperation, f64)>
where
    F: FnMut(&ClassifiedOperation) -> f64,
{
    classify_input_operations(notes, total_columns)
        .into_iter()
        .map(|operation| {
            let offset = candidate(&operation);
            (operation, offset)
        })
        .collect()
}

#[test]
fn input_operations_have_deterministic_order_and_classes() {
    let notes = vec![
        input_note(0, 0.0, Some(100.0)),
        input_note(1, 0.0, None),
        input_note(0, 100.0, None),
        input_note(2, 200.0, Some(500.0)),
        input_note(1, 300.0, None),
        input_note(1, 600.0, None),
        input_note(1, 720.0, None),
        input_note(1, 1000.0, None),
        input_note(0, 1100.0, None),
        input_note(3, 1100.0, None),
    ];

    let classified = classify_input_operations(&notes, 4);
    let at_100: Vec<_> = classified
        .iter()
        .filter(|op| op.operation.time_ms == 100.0 && op.operation.column == 0)
        .collect();

    assert_eq!(at_100.len(), 2);
    assert_eq!(at_100[0].operation.kind, InputOperationKind::Release);
    assert_eq!(at_100[0].class, InputClass::Release);
    assert_eq!(at_100[1].operation.kind, InputOperationKind::Press);
    assert_eq!(at_100[1].class, InputClass::ReleaseToPress);

    let class = |note_idx| {
        classified
            .iter()
            .find(|op| {
                op.operation.note_idx == note_idx && op.operation.kind == InputOperationKind::Press
            })
            .unwrap()
            .class
    };

    assert_eq!(class(4), InputClass::PressUnderHold);
    assert_eq!(class(5), InputClass::Jack);
    assert_eq!(class(6), InputClass::RapidRepress);
    assert_eq!(class(7), InputClass::Jack);
    assert_eq!(class(8), InputClass::ChordEntryOrExit);
    assert_eq!(class(9), InputClass::ChordEntryOrExit);

    let note_5 = classified
        .iter()
        .find(|op| op.operation.note_idx == 5 && op.operation.kind == InputOperationKind::Press)
        .unwrap();
    assert_eq!(note_5.previous_gap_ms, Some(300.0));
    assert_eq!(note_5.next_gap_ms, Some(120.0));
    assert_eq!(
        note_5.previous_operation_kind,
        Some(InputOperationKind::Press)
    );
    assert_eq!(note_5.next_operation_kind, Some(InputOperationKind::Press));

    let note_6 = classified
        .iter()
        .find(|op| op.operation.note_idx == 6 && op.operation.kind == InputOperationKind::Press)
        .unwrap();
    assert_eq!(note_6.next_gap_ms, Some(280.0));

    let invalid = input_operations(&[
        input_note(0, 0.0, Some(0.0)),
        input_note(1, 0.0, Some(-1.0)),
    ]);
    assert_eq!(
        invalid.len(),
        2,
        "zero and negative holds are plain presses"
    );
    assert!(
        invalid
            .iter()
            .all(|op| op.kind == InputOperationKind::Press)
    );
}

#[test]
fn input_state_bins_match_each_scoring_modes_judgement_count() {
    let map = synthetic_map_with_holds(8.0, 400, 120.0, 3, 90.0);

    for classic in [true, false] {
        let attrs = calculate(&map, &GameMods::default(), 1.0, Some(!classic), None).unwrap();
        let bins = attrs.input_state_bins.unwrap();
        let count: u32 = bins.iter().map(|bin| bin.count).sum();
        let expected = attrs.n_objects + usize::from(!classic) * attrs.n_long_notes;

        assert_eq!(count as usize, expected);
        assert!(bins.iter().all(|bin| bin.long_count <= bin.count));
    }
}

#[test]
fn input_state_surface_is_effective_by_default_and_can_be_disabled() {
    let map = synthetic_map(8.0, 400, 90.0);
    let attrs = calculate(&map, &GameMods::default(), 1.0, Some(true), None).unwrap();
    let without_bins = SunnyManiaDifficultyAttributes {
        input_state_bins: None,
        ..attrs
    };
    let enabled = ErrorModel::default();
    let disabled = ErrorModel {
        recovery_offset: 0.0,
        anticipation_offset: 0.0,
        ..enabled
    };
    let with_disabled = judgement_units(&attrs, 400.0, &disabled, true);
    let without_disabled = judgement_units(&without_bins, 400.0, &disabled, true);

    assert_eq!(with_disabled, without_disabled);

    let with_enabled = judgement_units(&attrs, 400.0, &enabled, true);
    let without_enabled = judgement_units(&without_bins, 400.0, &enabled, true);
    let expected_with = crate::mania::sunny_accuracy::expected_counts(
        &with_enabled,
        &attrs.hit_windows,
        &enabled,
        8.0,
    );
    let expected_without = crate::mania::sunny_accuracy::expected_counts(
        &without_enabled,
        &attrs.hit_windows,
        &enabled,
        8.0,
    );

    assert_ne!(expected_with.as_array(), expected_without.as_array());

    let weighted_recovery_mean: f64 = with_enabled
        .iter()
        .map(|unit| unit.weight * unit.fading_mean_offset)
        .sum::<f64>()
        / with_enabled.iter().map(|unit| unit.weight).sum::<f64>();
    assert!(
        weighted_recovery_mean.abs() < 1e-12,
        "per-score-relative recovery offsets must not introduce a global shift: {weighted_recovery_mean} ms"
    );

    let ss_ceiling = crate::mania::sunny_accuracy::expected_counts(
        &with_enabled,
        &attrs.hit_windows,
        &enabled,
        1.0e6,
    )
    .get(crate::mania::sunny_windows::ManiaJudgement::Perfect)
        / 400.0;
    assert!(
        ss_ceiling > 0.999_999,
        "input-state conditioning must still permit an SS, ceiling={ss_ceiling}"
    );
}

#[test]
#[ignore = "reads one gitignored fixture and prints an input-state diagnostic"]
fn diagnose_input_state_map_4772182() {
    use crate::mania::sunny_accuracy::expected_counts;
    use crate::mania::sunny_windows::ManiaJudgement;

    let map = parse("local-fixtures/maps/4772182.osu").expect("fixture map 4772182");
    let (mods, clock_rate) = mods_for("DT");
    let attrs = calculate(&map, &mods, clock_rate, Some(false), None).unwrap();
    let counts = [2453, 423, 0, 0, 0, 0];
    let total = counts.iter().sum::<u32>() as f64;

    let baseline = ErrorModel::default();
    let candidate = ErrorModel {
        recovery_offset: 73.12,
        ..baseline
    };

    for (label, model) in [("baseline", baseline), ("candidate", candidate)] {
        let units = judgement_units(&attrs, total, &model, true);
        let played = fit_with_quality(&counts, &units, &attrs.hit_windows, &model);
        let reference = fit_with_quality(&counts, &units, &reference_windows(&attrs), &model);
        let ceiling = expected_counts(&units, &attrs.hit_windows, &model, 1.0e6)
            .get(ManiaJudgement::Perfect)
            / total;
        println!(
            "{label}: units={} played_skill={:.6} reference_skill={:.6} scalar={:.8} g={:.3}/{:.3} perfect_ceiling={:.6}",
            units.len(),
            played.skill,
            reference.skill,
            played.skill / reference.skill,
            played.g_timing,
            reference.g_timing,
            ceiling,
        );
        assert!(label == "baseline" || ceiling > 0.999_999);
    }

    println!("class quantiles:");
    for bin in attrs
        .input_state_bins
        .unwrap()
        .iter()
        .filter(|bin| bin.count > 0)
    {
        let offset = if bin.predecessor_count > 0 {
            candidate.recovery_mean_offset(bin.mean_gap_ms) * f64::from(bin.predecessor_count)
                / f64::from(bin.count)
        } else {
            0.0
        };
        println!(
            "  {:?}: n={} pred={} d={:.3} gap={:.2} chord={:.2} held={:.2} offset={:+.2}",
            bin.class,
            bin.count,
            bin.predecessor_count,
            bin.mean_difficulty,
            bin.mean_gap_ms,
            bin.mean_chord_width,
            bin.mean_other_held,
            offset,
        );
    }
}

#[test]
#[ignore = "reads one gitignored fixture and prints a low-OD LN diagnostic"]
fn diagnose_input_state_map_3217217() {
    use crate::mania::sunny_accuracy::expected_counts;
    use crate::mania::sunny_windows::ManiaJudgement;

    let map = parse("local-fixtures/maps/3217217.osu").expect("fixture map 3217217");
    let (mods, clock_rate) = mods_for("MR");
    let attrs = calculate(&map, &mods, clock_rate, Some(false), None).unwrap();
    let counts = [1381, 2071, 49, 9, 11, 32];
    let total = counts.iter().sum::<u32>() as f64;
    let live_pp = 877.228;

    println!(
        "map: objects={} long_notes={} LN={:.1}% judgements={} OD={:.1} windows={:?}",
        attrs.n_objects,
        attrs.n_long_notes,
        100.0 * attrs.n_long_notes as f64 / attrs.n_objects as f64,
        total,
        map.od,
        attrs.hit_windows,
    );

    let baseline = ErrorModel::default();
    let candidate = ErrorModel {
        recovery_offset: 73.12,
        ..baseline
    };

    for (label, model) in [("baseline", baseline), ("candidate", candidate)] {
        let units = judgement_units(&attrs, total, &model, true);
        let played = fit_with_quality(&counts, &units, &attrs.hit_windows, &model);
        let reference = fit_with_quality(&counts, &units, &reference_windows(&attrs), &model);
        let perf = calculate_performance_with_model(
            &attrs,
            &mods,
            SunnyScoreState {
                n320: counts[0],
                n300: counts[1],
                n200: counts[2],
                n100: counts[3],
                n50: counts[4],
                misses: counts[5],
            },
            &model,
        );
        let ceiling = expected_counts(&units, &attrs.hit_windows, &model, 1.0e6)
            .get(ManiaJudgement::Perfect)
            / total;

        println!(
            "{label}: units={} pp={:.2} live_ratio={:.2}% played_skill={:.6} reference_skill={:.6} scalar={:.8} g={:.3}/{:.3} perfect_ceiling={:.6}",
            units.len(),
            perf.pp,
            100.0 * perf.pp / live_pp,
            played.skill,
            reference.skill,
            played.skill / reference.skill,
            played.g_timing,
            reference.g_timing,
            ceiling,
        );
    }

    let total_columns = map.cs.round_ties_even().max(1.0) as usize;
    let (notes, _) = build_notes(clock_rate, map.hit_objects.iter(), total_columns);
    let windows = hit_windows(&map, &mods, clock_rate, false);
    let great = get_hit_window_300(&map, clock_rate, has_mod(&mods, "HR"), has_mod(&mods, "EZ"));
    let data = RebirthData::new(
        notes,
        total_columns,
        hit_leniency_from_window(great),
        windows.good,
    );
    let (_, _, per_note) = per_note_difficulty(&map).expect("per-note difficulty");
    let gaps = same_column_gaps(&data);
    let per_unit = total / per_note.len() as f64;
    let mut exact = Vec::with_capacity(per_note.len());

    for (idx, &(difficulty, duration)) in per_note.iter().enumerate() {
        let (sigma_scale, release_offset) = match duration {
            Some(duration) if attrs.ln_judged_as_one => (
                crate::mania::sunny_accuracy::ln_sigma_scale_for_duration(&candidate, duration),
                candidate.release_mean_offset,
            ),
            _ => (1.0, 0.0),
        };

        exact.push(JudgementUnit {
            difficulty,
            weight: per_unit,
            sigma_scale,
            mean_offset: release_offset,
            fading_mean_offset: candidate.recovery_mean_offset(gaps[idx]),
        });
    }

    let exact_played = fit_with_quality(&counts, &exact, &attrs.hit_windows, &candidate);
    let exact_reference = fit_with_quality(&counts, &exact, &reference_windows(&attrs), &candidate);
    let exact_scalar = exact_played.skill / exact_reference.skill;
    let compact_pp = calculate_performance_with_model(
        &attrs,
        &mods,
        SunnyScoreState {
            n320: counts[0],
            n300: counts[1],
            n200: counts[2],
            n100: counts[3],
            n50: counts[4],
            misses: counts[5],
        },
        &candidate,
    )
    .pp;
    let compact_units = judgement_units(&attrs, total, &candidate, true);
    let compact_played = fit_with_quality(&counts, &compact_units, &attrs.hit_windows, &candidate);
    let compact_reference = fit_with_quality(
        &counts,
        &compact_units,
        &reference_windows(&attrs),
        &candidate,
    );
    let compact_scalar = compact_played.skill / compact_reference.skill;
    println!(
        "exact candidate: units={} played_skill={:.6} reference_skill={:.6} scalar={:.8} g={:.3}/{:.3} implied_pp={:.2} compact_pp={:.2}",
        exact.len(),
        exact_played.skill,
        exact_reference.skill,
        exact_scalar,
        exact_played.g_timing,
        exact_reference.g_timing,
        compact_pp * (exact_scalar / compact_scalar).powf(2.2),
        compact_pp,
    );

    println!("class quantiles:");
    for bin in attrs
        .input_state_bins
        .unwrap()
        .iter()
        .filter(|bin| bin.count > 0)
    {
        let offset = if bin.predecessor_count > 0 {
            candidate.recovery_mean_offset(bin.mean_gap_ms) * f64::from(bin.predecessor_count)
                / f64::from(bin.count)
        } else {
            0.0
        };
        println!(
            "  {:?}: n={} long={} pred={} d={:.3} duration={:.1} gap={:.2} chord={:.2} held={:.2} offset={:+.2}",
            bin.class,
            bin.count,
            bin.long_count,
            bin.predecessor_count,
            bin.mean_difficulty,
            bin.mean_duration_ms,
            bin.mean_gap_ms,
            bin.mean_chord_width,
            bin.mean_other_held,
            offset,
        );
    }
}

/// The per-note path must hand the fit exactly the score that was played.
///
/// [`crate::mania::sunny_accuracy::skill_for_counts`] fits a multinomial, so the unit weights
/// are the trial count. If they sum to anything other than the observed judgement
/// total, the fit is answering a question about a different score — and because the
/// weights are derived from the *map's* note count while the total comes from the
/// *score*, the two can legitimately disagree: a partial play, or a count vector our
/// object parsing reads differently. Asserted rather than left to the reports because
/// the failure is silent and would misprice every score on the map.
#[test]
fn per_note_units_emit_exactly_the_observed_judgement_total() {
    let model = ErrorModel::default();

    // Rice-only and LN-bearing, and under both judgement regimes: a ScoreV1 long note
    // is one widened judgement while a V2 one is two plain ones, so the weights are
    // built differently in each case.
    let cases = [
        ("rice, V1", synthetic_map(8.0, 400, 120.0), true),
        ("rice, V2", synthetic_map(8.0, 400, 120.0), false),
        (
            "holds, V1",
            synthetic_map_with_holds(8.0, 400, 120.0, 3, 90.0),
            true,
        ),
        (
            "holds, V2",
            synthetic_map_with_holds(8.0, 400, 120.0, 3, 90.0),
            false,
        ),
        (
            "all holds, V1",
            synthetic_map_with_holds(8.0, 400, 120.0, 1, 300.0),
            true,
        ),
    ];

    for (label, map, classic) in cases {
        let attrs = calculate(&map, &GameMods::default(), 1.0, Some(classic), None).unwrap();
        let bins = attrs
            .note_difficulty_bins
            .unwrap_or_else(|| panic!("{label}: a 400-note map should carry a distribution"));

        // Guards the guard: a case meant to exercise the long-note branch must actually
        // contain long notes, or this test passes while testing nothing.
        let long: u32 = bins.iter().map(|bin| bin.long).sum();

        if label.contains("holds") {
            assert!(
                long > 0,
                "{label}: expected long notes in the bins, found none"
            );
            assert!(
                bins.iter().any(|bin| bin.mean_duration > 0.0),
                "{label}: expected a non-zero mean hold duration"
            );
        } else {
            assert_eq!(long, 0, "{label}: expected no long notes, found {long}");
        }

        // Deliberately includes totals that disagree with the map's own note count in
        // both directions, since that disagreement is why the rescaling exists.
        for total in [1u32, 137, 399, 400, 401, 812] {
            let units = judgement_units(&attrs, f64::from(total), &model, true);
            let weight: f64 = units.iter().map(|unit| unit.weight).sum();

            assert!(
                (weight - f64::from(total)).abs() < 1e-9,
                "{label}: a {total}-hit score got {weight} units of weight"
            );

            let emitted = crate::mania::sunny_accuracy::expected_counts(
                &units,
                &attrs.hit_windows,
                &model,
                9.0,
            );
            let emitted_total: f64 = crate::mania::sunny_windows::ManiaJudgement::ALL
                .iter()
                .map(|&judgement| emitted.get(judgement))
                .sum();

            assert!(
                (emitted_total - f64::from(total)).abs() < 1e-6,
                "{label}: a {total}-hit score got {emitted_total} predicted judgements"
            );
        }
    }
}

/// Attributes without either per-note distribution keep working.
///
/// Older cached JS attributes can lack both distributions, and inventing either one
/// would fabricate map structure they carry no trace of. The uniform fallback is
/// therefore load-bearing on a shipping path, not just in tests.
#[test]
fn a_missing_per_note_distribution_falls_back_to_the_uniform_list() {
    let map = synthetic_map(8.0, 400, 120.0);
    let attrs = calculate(&map, &GameMods::default(), 1.0, Some(true), None).unwrap();
    let model = ErrorModel::default();

    let stripped = SunnyManiaDifficultyAttributes {
        note_difficulty_bins: None,
        input_state_bins: None,
        ..attrs
    };

    let units = judgement_units(&stripped, 400.0, &model, true);
    let weight: f64 = units.iter().map(|unit| unit.weight).sum();

    assert!(
        (weight - 400.0).abs() < 1e-9,
        "the fallback list must still weigh the whole score, got {weight}"
    );
    assert!(
        units.iter().all(|unit| unit.difficulty == stripped.stars),
        "the fallback list prices every note at the map's star rating"
    );

    // And the pricing path survives it, which is the property the JS binding relies on.
    let state = SunnyScoreState {
        n320: 380,
        n300: 20,
        n200: 0,
        n100: 0,
        n50: 0,
        misses: 0,
    };
    let scalar = window_scalar_with_model(&stripped, state, &ErrorModel::default());

    assert!(
        scalar.is_finite() && scalar > 0.0,
        "a stripped attribute set must still price, got {scalar}"
    );
}

/// Equal-count bins, which is what lets the attribute omit per-bin weights.
#[test]
fn per_note_bins_partition_the_map() {
    let map = synthetic_map(8.0, 400, 120.0);
    let attrs = calculate(&map, &GameMods::default(), 1.0, Some(true), None).unwrap();
    let bins = attrs.note_difficulty_bins.unwrap();

    let counted: u32 = bins.iter().map(|bin| bin.rice + bin.long).sum();

    assert_eq!(
        counted as usize, attrs.n_objects,
        "the bins must account for every note exactly once"
    );

    // Sorted by construction, which the p50/p90 reads in the reports depend on.
    for pair in bins.windows(2) {
        assert!(
            pair[0].difficulty <= pair[1].difficulty,
            "bins must be ordered by difficulty: {} then {}",
            pair[0].difficulty,
            pair[1].difficulty
        );
    }
}

/// The Python reference (Star-Rating-Rebirth) uses the OD-based hit
/// leniency while this port uses the C# great-hit-window based one, so
/// the SR values differ by a small margin.
#[test]
fn matches_python_reference_1638954() {
    let Some(map) = parse(MAP_1638954) else {
        return;
    };
    let mods = GameMods::default();
    let attrs = calculate(&map, &mods, 1.0, Some(true), None).unwrap();

    // Python reference: 3.712606
    let relative = (attrs.stars - 3.712606).abs() / 3.712606;
    assert!(relative < 0.03, "SR {} deviates by {relative}", attrs.stars);
}

#[test]
fn matches_python_reference_5269878() {
    let Some(map) = parse(MAP_5269878) else {
        return;
    };
    let mods = GameMods::default();
    let attrs = calculate(&map, &mods, 1.0, Some(true), None).unwrap();

    // Python reference: 9.299379
    let relative = (attrs.stars - 9.299379).abs() / 9.299379;
    assert!(relative < 0.03, "SR {} deviates by {relative}", attrs.stars);
}

#[test]
fn ez_hr_affect_star_rating() {
    let Some(map) = parse(MAP_1638954) else {
        return;
    };
    let nm = calculate(&map, &GameMods::default(), 1.0, Some(true), None).unwrap();

    let mut hr_mods = LazerMods::new();
    single_mod(&mut hr_mods, GameMod::HardRockMania(Default::default()));
    let hr = calculate(&map, &hr_mods, 1.0, Some(true), None).unwrap();

    let mut ez_mods = LazerMods::new();
    single_mod(&mut ez_mods, GameMod::EasyMania(Default::default()));
    let ez = calculate(&map, &ez_mods, 1.0, Some(true), None).unwrap();

    assert!(
        ez.stars < nm.stars && nm.stars < hr.stars,
        "expected EZ {} < NM {} < HR {}",
        ez.stars,
        nm.stars,
        hr.stars
    );
}

#[test]
fn performance_formula() {
    let Some(map) = parse(MAP_1638954) else {
        return;
    };
    let mods = GameMods::default();
    let attrs = calculate(&map, &mods, 1.0, Some(true), None).unwrap();

    // SS play
    let state = SunnyScoreState {
        n320: attrs.n_objects as u32,
        ..Default::default()
    };
    let perf = calculate_performance(&attrs, &mods, state);

    assert!(perf.pp > 0.0);
    assert!((perf.variety_multiplier - 0.945..=1.055).contains(&perf.variety_multiplier));
    assert!(perf.length_multiplier > 0.0 && perf.length_multiplier < 1.1);
    assert!(
        (perf.pp
            - perf.pp_difficulty
                * perf.variety_multiplier
                * perf.acc_multiplier
                * perf.length_multiplier)
            .abs()
            < 1e-6
    );

    // NF keeps its flat factor: failing is a scoring matter the timing surface
    // says nothing about.
    let mut nf_mods = LazerMods::new();
    single_mod(&mut nf_mods, GameMod::NoFailMania(Default::default()));
    let perf_nf = calculate_performance(&attrs, &nf_mods, state);
    assert!((perf_nf.pp - perf.pp * 0.75).abs() < 1e-6);
}

/// The core of the design: `EZ` is priced by grading the score against the
/// windows it was played under, not by a mod-specific factor. The same
/// judgement counts through wider windows imply less precision, so they are
/// worth less — and `calculate_performance` never looks up `EZ` to do it.
#[test]
fn ez_is_priced_by_the_windows_not_a_multiplier() {
    let map = synthetic_map(8.0, 900, 125.0);
    let nm_mods = GameMods::default();

    let mut ez_mods = LazerMods::new();
    single_mod(&mut ez_mods, GameMod::EasyMania(Default::default()));

    let nm = calculate(&map, &nm_mods, 1.0, Some(true), None).unwrap();
    let ez = calculate(&map, &ez_mods, 1.0, Some(true), None).unwrap();

    // EZ must actually widen the windows, otherwise the rest proves nothing.
    assert!(
        ez.hit_windows.great > nm.hit_windows.great,
        "EZ should widen GREAT: {} vs {}",
        ez.hit_windows.great,
        nm.hit_windows.great
    );
    assert!(
        ez.hit_windows.perfect > nm.hit_windows.perfect,
        "EZ is the only thing that moves PERFECT: {} vs {}",
        ez.hit_windows.perfect,
        nm.hit_windows.perfect
    );

    // One observed score, both window sets. Not an SS: a saturated fit carries
    // no information about precision, so the score has to leave some headroom.
    let notes = nm.n_objects as u32;
    let n320 = notes * 92 / 100;
    let state = SunnyScoreState {
        n320,
        n300: notes - n320,
        ..Default::default()
    };

    let perf_nm = calculate_performance(&nm, &nm_mods, state);
    let perf_ez = calculate_performance(&ez, &ez_mods, state);

    println!(
        "surface A/B: NM pattern={:.6} timing={:.6} pp={:.6} scalar={:.6}; \
EZ pattern={:.6} timing={:.6} pp={:.6} scalar={:.6}",
        perf_nm.pp_pattern,
        perf_nm.pp_timing,
        perf_nm.pp,
        perf_nm.window_scalar,
        perf_ez.pp_pattern,
        perf_ez.pp_timing,
        perf_ez.pp,
        perf_ez.window_scalar
    );

    assert!(
        perf_ez.window_scalar < 1.0,
        "wider windows should discount the score, got {}",
        perf_ez.window_scalar
    );

    assert!(
        perf_ez.pp < perf_nm.pp,
        "the same counts through EZ windows should be worth less: {} vs {}",
        perf_ez.pp,
        perf_nm.pp
    );

    // And the discount is the windows, not a hidden factor: passing NM windows
    // with the EZ mod list set gives the NM value back.
    let mislabelled = calculate_performance(&nm, &ez_mods, state);

    assert!(
        (mislabelled.pp - perf_nm.pp).abs() < 1e-9,
        "pp should depend on the windows, not the mod list: {} vs {}",
        mislabelled.pp,
        perf_nm.pp
    );
}

/// HR is the mirror image and needs no separate rule: it narrows the windows,
/// so the same counts imply *more* precision and are worth more.
#[test]
fn hr_is_rewarded_by_the_same_mechanism() {
    let map = synthetic_map(8.0, 900, 125.0);
    let nm_mods = GameMods::default();

    let mut hr_mods = LazerMods::new();
    single_mod(&mut hr_mods, GameMod::HardRockMania(Default::default()));

    let nm = calculate(&map, &nm_mods, 1.0, Some(true), None).unwrap();
    let hr = calculate(&map, &hr_mods, 1.0, Some(true), None).unwrap();

    let notes = nm.n_objects as u32;
    let n320 = notes * 92 / 100;
    let state = SunnyScoreState {
        n320,
        n300: notes - n320,
        ..Default::default()
    };

    let perf_hr = calculate_performance(&hr, &hr_mods, state);

    assert!(
        perf_hr.window_scalar > 1.0,
        "narrower windows should reward the score, got {}",
        perf_hr.window_scalar
    );
}

/// [`REFERENCE_WINDOWS`] has to be a hand-written literal to stay `const`, so it
/// can silently disagree with what [`hit_windows`] actually produces. It did:
/// GOOD/OK were written +36/+66 from GREAT when the classic non-convert scheme
/// offsets them by +33/+63, which priced an OD 8 no-mod score at 1.0072 instead
/// of exactly 1.
#[test]
fn reference_windows_match_od8_no_mod() {
    let map = synthetic_map(8.0, 100, 200.0);
    let mods = GameMods::default();

    let generated = hit_windows(&map, &mods, 1.0, true);

    assert_eq!(
        generated, REFERENCE_WINDOWS,
        "reference set drifted from the OD 8 classic non-convert windows"
    );
}

/// Legacy reference switches remain available to research reports, but production
/// pricing must no longer read them.
#[test]
fn production_pricing_ignores_legacy_reference_switches() {
    let map = synthetic_map(4.2, 2000, 120.0);
    let mods = GameMods::default();
    let attrs = calculate(&map, &mods, 1.0, Some(true), None).unwrap();
    let state = SunnyScoreState {
        n320: 1400,
        n300: 480,
        n200: 90,
        n100: 20,
        n50: 5,
        misses: 5,
    };
    let baseline = calculate_performance(&attrs, &mods, state);

    for switch in ["SUNNY_MAP_REFERENCE", "SUNNY_ONESIDED_REFERENCE"] {
        // Safety: these process-global switches are only read by research helpers
        // now; the assertion below pins that production calculation is isolated.
        unsafe { std::env::set_var(switch, "1") };
        let switched = calculate_performance(&attrs, &mods, state);
        unsafe { std::env::remove_var(switch) };

        assert!((switched.pp - baseline.pp).abs() < 1e-9);
        assert!((switched.window_scalar - baseline.window_scalar).abs() < 1e-9);
    }
}

/// The one case with nothing to measure. Everything else gets priced, however
/// badly the model fits — see [`window_scalar`].
#[test]
fn an_empty_score_has_no_windows_to_price() {
    let map = synthetic_map(8.0, 900, 125.0);
    let mods = GameMods::default();
    let attrs = calculate(&map, &mods, 1.0, Some(true), None).unwrap();

    let empty = calculate_performance(&attrs, &mods, SunnyScoreState::default());

    assert_eq!(
        empty.window_scalar, 1.0,
        "an empty score has nothing to fit"
    );
}

/// Some real scores still fit poorly even with a calibrated tail, so pricing must
/// not depend on fit quality: gating on it left most `EZ` scores at their
/// unmodified value, which is the bug this pins against returning.
///
/// The counts are a real score from the live server (map 4229780), the worst fit
/// in that set both before and after the error model gained its lapse component —
/// `g_timing` went from 688 to 100, an enormous improvement that still leaves it
/// implausible, which is precisely why this test is about pricing rather than fit.
/// They are graded through `EZ` windows here to check that pricing happens; the
/// original play was no-mod.
#[test]
fn an_implausible_fit_is_still_priced() {
    let map = synthetic_map(8.0, 3635, 90.0);
    let mut ez_mods = LazerMods::new();
    single_mod(&mut ez_mods, GameMod::EasyMania(Default::default()));
    let attrs = calculate(&map, &ez_mods, 1.5, Some(true), None).unwrap();

    let state = SunnyScoreState {
        n320: 2459,
        n300: 963,
        n200: 144,
        n100: 56,
        n50: 13,
        misses: 0,
    };

    let counts = [
        state.n320,
        state.n300,
        state.n200,
        state.n100,
        state.n50,
        state.misses,
    ];
    let units = [JudgementUnit::repeated(
        attrs.stars,
        f64::from(state.total_hits()),
    )];
    let fit = fit_with_quality(&counts, &units, &attrs.hit_windows, &ErrorModel::default());

    assert!(
        !fit.is_plausible(),
        "fixture should be an implausible fit, got g_timing={}",
        fit.g_timing
    );

    let perf = calculate_performance(&attrs, &ez_mods, state);

    assert!(
        perf.window_scalar < 0.95,
        "an implausible fit must still be priced by its windows, got {}",
        perf.window_scalar
    );
}

// -----------------------------------------------------------------------
// Real-score comparison
// -----------------------------------------------------------------------

/// One real score from the live server, with the pp it was awarded there.
struct Row {
    map: &'static str,
    n320: u32,
    n300: u32,
    n200: u32,
    n100: u32,
    n50: u32,
    miss: u32,
    live_pp: f64,
    live_acc: f64,
    mods: &'static str,
}

/// The top scores of uid 10107, an `EZ` pp exploiter, fetched from the ppy-sb
/// tRPC API. Beatmaps live alongside in `local-fixtures/maps/`; both are
/// gitignored, so this report skips when they are absent.
const REAL_SCORES: &[Row] = &[
    Row {
        map: "4633018",
        n320: 1987,
        n300: 1710,
        n200: 593,
        n100: 20,
        n50: 8,
        miss: 138,
        live_pp: 1379.012,
        live_acc: 91.241,
        mods: "EZ+DT",
    },
    Row {
        map: "5583718",
        n320: 1399,
        n300: 980,
        n200: 324,
        n100: 46,
        n50: 5,
        miss: 13,
        live_pp: 1356.142,
        live_acc: 94.368,
        mods: "EZ+DT",
    },
    Row {
        map: "3663002",
        n320: 1975,
        n300: 1863,
        n200: 591,
        n100: 34,
        n50: 0,
        miss: 210,
        live_pp: 1313.038,
        live_acc: 90.01,
        mods: "EZ+DT",
    },
    Row {
        map: "4870605",
        n320: 1436,
        n300: 1194,
        n200: 600,
        n100: 35,
        n50: 0,
        miss: 42,
        live_pp: 1279.841,
        live_acc: 91.181,
        mods: "EZ+DT",
    },
    Row {
        map: "4870608",
        n320: 2590,
        n300: 2266,
        n200: 928,
        n100: 132,
        n50: 30,
        miss: 50,
        live_pp: 1240.625,
        live_acc: 92.123,
        mods: "EZ+DT",
    },
    Row {
        map: "3583718",
        n320: 1359,
        n300: 1458,
        n200: 783,
        n100: 52,
        n50: 3,
        miss: 65,
        live_pp: 1199.563,
        live_acc: 89.357,
        mods: "EZ+DT",
    },
    Row {
        map: "5583724",
        n320: 1323,
        n300: 1366,
        n200: 550,
        n100: 102,
        n50: 29,
        miss: 17,
        live_pp: 1183.49,
        live_acc: 91.364,
        mods: "EZ+DT",
    },
    Row {
        map: "4459721",
        n320: 1306,
        n300: 1502,
        n200: 648,
        n100: 34,
        n50: 0,
        miss: 71,
        live_pp: 1095.582,
        live_acc: 90.408,
        mods: "EZ+DT",
    },
    Row {
        map: "4459716",
        n320: 1240,
        n300: 1120,
        n200: 486,
        n100: 93,
        n50: 1,
        miss: 18,
        live_pp: 1094.649,
        live_acc: 91.791,
        mods: "EZ+DT",
    },
    Row {
        map: "4807505",
        n320: 2407,
        n300: 1825,
        n200: 761,
        n100: 128,
        n50: 35,
        miss: 54,
        live_pp: 1065.901,
        live_acc: 91.897,
        mods: "EZ+DT",
    },
    Row {
        map: "5583717",
        n320: 1095,
        n300: 1149,
        n200: 544,
        n100: 34,
        n50: 1,
        miss: 105,
        live_pp: 1028.791,
        live_acc: 88.565,
        mods: "EZ+DT",
    },
    Row {
        map: "4870609",
        n320: 1048,
        n300: 940,
        n200: 326,
        n100: 17,
        n50: 0,
        miss: 49,
        live_pp: 984.332,
        live_acc: 92.098,
        mods: "EZ+DT",
    },
    Row {
        map: "4459712",
        n320: 1415,
        n300: 1016,
        n200: 393,
        n100: 47,
        n50: 7,
        miss: 13,
        live_pp: 965.078,
        live_acc: 93.733,
        mods: "EZ+DT",
    },
    Row {
        map: "4459715",
        n320: 1203,
        n300: 1536,
        n200: 779,
        n100: 37,
        n50: 0,
        miss: 65,
        live_pp: 945.481,
        live_acc: 89.414,
        mods: "EZ+DT",
    },
    Row {
        map: "4459717",
        n320: 1213,
        n300: 1138,
        n200: 583,
        n100: 38,
        n50: 4,
        miss: 38,
        live_pp: 920.466,
        live_acc: 90.503,
        mods: "EZ+DT",
    },
    Row {
        map: "4706643",
        n320: 882,
        n300: 538,
        n200: 195,
        n100: 48,
        n50: 1,
        miss: 19,
        live_pp: 895.026,
        live_acc: 93.058,
        mods: "EZ+DT",
    },
    Row {
        map: "4459723",
        n320: 940,
        n300: 953,
        n200: 410,
        n100: 30,
        n50: 0,
        miss: 47,
        live_pp: 852.64,
        live_acc: 90.591,
        mods: "EZ+DT",
    },
    Row {
        map: "4229780",
        n320: 2459,
        n300: 963,
        n200: 144,
        n100: 56,
        n50: 13,
        miss: 82,
        live_pp: 722.134,
        live_acc: 95.207,
        mods: "",
    },
    Row {
        map: "3477077",
        n320: 1482,
        n300: 637,
        n200: 84,
        n100: 12,
        n50: 3,
        miss: 42,
        live_pp: 707.994,
        live_acc: 96.438,
        mods: "",
    },
    Row {
        map: "3477076",
        n320: 1587,
        n300: 598,
        n200: 66,
        n100: 8,
        n50: 1,
        miss: 16,
        live_pp: 672.317,
        live_acc: 98.059,
        mods: "",
    },
];

/// One fixture reduced to what the surface needs: the windows it was played
/// under, its star rating, and its judgement counts.
struct LoadedScore {
    map: &'static str,
    mods: &'static str,
    stars: f64,
    windows: ManiaHitWindows,
    counts: [u32; 6],
}

/// Load every fixture that is present on disk. Returns empty when the gitignored
/// fixture directory is absent, which is how the reports skip cleanly.
fn load_real_scores() -> Vec<LoadedScore> {
    let mut loaded = Vec::new();

    for row in REAL_SCORES {
        let path = format!("local-fixtures/maps/{}.osu", row.map);
        let Some(map) = parse(&path) else {
            continue;
        };

        let mut mods = LazerMods::new();
        if row.mods.contains("EZ") {
            single_mod(&mut mods, GameMod::EasyMania(Default::default()));
        }
        let clock_rate = if row.mods.contains("DT") { 1.5 } else { 1.0 };

        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(true), None) else {
            continue;
        };

        loaded.push(LoadedScore {
            map: row.map,
            mods: row.mods,
            stars: attrs.stars,
            windows: attrs.hit_windows,
            counts: [row.n320, row.n300, row.n200, row.n100, row.n50, row.miss],
        });
    }

    loaded
}

/// Mean `g_timing` across the loaded scores under a candidate model.
///
/// The mean is the right pooling here precisely because `g_timing` does not grow
/// with map length — every score contributes on the same scale regardless of note
/// count, so averaging weights each score equally rather than letting the
/// six-thousand-note maps dominate.
fn mean_g_timing(scores: &[LoadedScore], model: &ErrorModel) -> f64 {
    if scores.is_empty() {
        return f64::INFINITY;
    }

    let mut total = 0.0;

    for score in scores {
        let units = [JudgementUnit::repeated(
            score.stars,
            f64::from(score.counts.iter().sum::<u32>()),
        )];
        let fit = fit_with_quality(&score.counts, &units, &score.windows, model);

        if !fit.g_timing.is_finite() {
            return f64::INFINITY;
        }

        total += fit.g_timing;
    }

    total / scores.len() as f64
}

/// Not an assertion — the calibration itself. Searches `sigma_ref`,
/// `lapse_weight` and `lapse_ratio` for the combination that best explains the 20
/// real scores, holding `skill_exponent` and `difficulty_floor` fixed.
///
/// Those two are held deliberately. The fixture set is one player whose fitted
/// skill sits at 0.96-1.72x the star rating on every map, and `skill_exponent` is
/// only identified by *variation* in that ratio — at a ratio of 1, `sigma` equals
/// `sigma_ref` whatever the exponent is, so the two are nearly jointly
/// unidentified here. Fitting the exponent on this data would mostly absorb one
/// player's idiosyncrasy while silently resetting the entire mod response, since
/// it alone sets how the scalar answers a window change.
///
/// Run with `cargo test calibration_search -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn calibration_search() {
    let scores = load_real_scores();

    if scores.is_empty() {
        println!("no fixtures present; nothing to calibrate");
        return;
    }

    let baseline = ErrorModel::default();

    // The single normal this work replaced, kept as the comparison point so the
    // improvement stays visible now that the mixture *is* the default.
    let single_normal = ErrorModel {
        lapse_weight: 0.0,
        ..baseline
    };

    println!(
        "single normal:   mean g_timing={:.2}",
        mean_g_timing(&scores, &single_normal)
    );
    println!(
        "current default: lapse_weight={:.4} lapse_ratio={:.3} mean g_timing={:.2}",
        baseline.lapse_weight,
        baseline.lapse_ratio,
        mean_g_timing(&scores, &baseline)
    );

    // `sigma_ref` is deliberately not searched. It is structurally
    // unidentifiable, not merely weakly identified: it sets the unit skill is
    // measured in, and skill is refit per score, so any change in `sigma_ref` is
    // absorbed exactly by the fitted skill and no observable moves at all. The
    // sweep further down demonstrates this — `g_timing` is identical to four
    // decimals across a 16x range, with skill scaling as
    // `sigma_ref^(1/skill_exponent)`. It stays at its existing value as a gauge
    // choice, which also keeps fitted skill roughly on the star-rating scale.
    //
    // The real fit is therefore two-dimensional, over the shape parameters only.
    // Search from the single normal rather than from the current default, so the
    // result does not depend on the answer already being baked into the defaults.
    let mut best = single_normal;
    let mut best_score = mean_g_timing(&scores, &single_normal);

    let mut weight = 0.0;
    while weight <= 0.60 {
        let mut ratio = 1.5;
        while ratio <= 20.0 {
            let candidate = ErrorModel {
                lapse_weight: weight,
                lapse_ratio: ratio,
                ..baseline
            };
            let value = mean_g_timing(&scores, &candidate);

            if value < best_score {
                best_score = value;
                best = candidate;
            }

            ratio += 0.25;
        }
        weight += 0.005;
    }

    println!(
        "grid best: lapse_weight={:.4} lapse_ratio={:.2} mean g_timing={:.2}",
        best.lapse_weight, best.lapse_ratio, best_score
    );

    // Coordinate descent with a shrinking step, refining the grid winner.
    let mut step = [0.0025, 0.125];

    for _ in 0..60 {
        for (axis, &size) in step.iter().enumerate() {
            for direction in [-1.0, 1.0] {
                let mut candidate = best;
                let delta = size * direction;

                if axis == 0 {
                    candidate.lapse_weight = (best.lapse_weight + delta).clamp(0.0, 0.95);
                } else {
                    candidate.lapse_ratio = (best.lapse_ratio + delta).max(1.0);
                }

                let value = mean_g_timing(&scores, &candidate);

                if value < best_score {
                    best_score = value;
                    best = candidate;
                }
            }
        }

        for entry in &mut step {
            *entry *= 0.75;
        }
    }

    println!(
        "refined:   lapse_weight={:.4} lapse_ratio={:.3} mean g_timing={:.2}",
        best.lapse_weight, best.lapse_ratio, best_score
    );

    // Profile `lapse_ratio`: at each fixed ratio, re-optimise the other two and
    // report the best achievable objective. A flat profile means the ratio is not
    // separately identified by this data and the value chosen inside the flat
    // region is arbitrary — which is worth knowing before treating any single
    // triple as "the" calibration.
    println!("\nprofile over lapse_ratio (others re-optimised at each point):");
    println!(
        "{:>7} {:>10} {:>10} {:>10}",
        "ratio", "weight", "g_timing", "ez_scalar"
    );

    for &ratio in &[3.0, 3.5, 4.0, 4.25, 4.5, 4.75, 5.0, 5.5, 6.0, 10.0, 20.0] {
        let mut local = ErrorModel {
            lapse_ratio: ratio,
            ..best
        };
        let mut local_score = mean_g_timing(&scores, &local);
        let mut local_step = 0.05;

        for _ in 0..50 {
            for direction in [-1.0, 1.0] {
                let mut candidate = local;
                candidate.lapse_weight =
                    (local.lapse_weight + local_step * direction).clamp(0.0, 0.95);

                let value = mean_g_timing(&scores, &candidate);

                if value < local_score {
                    local_score = value;
                    local = candidate;
                }
            }

            local_step *= 0.8;
        }

        // The EZ scalar at this point, so the profile shows whether the flat
        // region is also flat in the quantity that actually reaches pp.
        let mut ez_here = Vec::new();

        for score in &scores {
            if !score.mods.contains("EZ") {
                continue;
            }

            let units = [JudgementUnit::repeated(
                score.stars,
                f64::from(score.counts.iter().sum::<u32>()),
            )];
            let played = fit_with_quality(&score.counts, &units, &score.windows, &local);
            let reference = fit_with_quality(&score.counts, &units, &REFERENCE_WINDOWS, &local);

            if played.skill > 0.0 && reference.skill > 0.0 {
                ez_here.push(played.skill / reference.skill);
            }
        }

        let ez_mean = ez_here.iter().sum::<f64>() / ez_here.len().max(1) as f64;

        println!(
            "{ratio:>7.1} {:>10.4} {local_score:>10.2} {ez_mean:>10.4}",
            local.lapse_weight
        );
    }

    // Is `sigma_ref` identified at all? The profile above wanders it over 7.5-12.6
    // while the objective moves in the third decimal, which suggests not. Sweep it
    // alone, holding the shape fixed, and print the fitted skill alongside.
    println!("\nsigma_ref sweep at fixed shape (skill of the first score shown):");
    println!("{:>10} {:>10} {:>12}", "sigma_ref", "g_timing", "skill[0]");

    for &sigma_ref in &[4.5, 9.0, 18.0, 36.0, 72.0] {
        let candidate = ErrorModel { sigma_ref, ..best };
        let first = &scores[0];
        let units = [JudgementUnit::repeated(
            first.stars,
            f64::from(first.counts.iter().sum::<u32>()),
        )];
        let fit = fit_with_quality(&first.counts, &units, &first.windows, &candidate);

        println!(
            "{sigma_ref:>10.2} {:>10.4} {:>12.4}",
            mean_g_timing(&scores, &candidate),
            fit.skill
        );
    }

    // What the calibrated shape does to the thing under test: the window scalar,
    // and so the mod response. Reported rather than asserted — there is no pp
    // target for EZ, the figure is an output of the calibration.
    let mut ez = Vec::new();
    let mut nm = Vec::new();

    println!(
        "\n{:>9} {:>7} {:>7} {:>9} {:>9}",
        "map", "mods", "scalar", "g_before", "g_after"
    );

    for score in &scores {
        let units = [JudgementUnit::repeated(
            score.stars,
            f64::from(score.counts.iter().sum::<u32>()),
        )];

        let before = fit_with_quality(&score.counts, &units, &score.windows, &single_normal);
        let after = fit_with_quality(&score.counts, &units, &score.windows, &best);
        let reference = fit_with_quality(&score.counts, &units, &REFERENCE_WINDOWS, &best);

        let scalar = if after.skill > 0.0 && reference.skill > 0.0 {
            after.skill / reference.skill
        } else {
            1.0
        };

        println!(
            "{:>9} {:>7} {:>7.4} {:>9.1} {:>9.1}",
            score.map,
            if score.mods.is_empty() {
                "NM"
            } else {
                score.mods
            },
            scalar,
            before.g_timing,
            after.g_timing,
        );

        if score.mods.contains("EZ") {
            ez.push(scalar);
        } else {
            nm.push(scalar);
        }
    }

    let summarise = |label: &str, values: &[f64]| {
        if values.is_empty() {
            return;
        }
        let mean = values.iter().sum::<f64>() / values.len() as f64;
        // pp moves as the scalar to the ~1.1 power through
        // `compute_difficulty_value`; reported as the ratio itself here since the
        // pp mapping is the report above's job.
        println!("{label}: n={} mean scalar {mean:.4}", values.len());
    };

    println!();
    summarise("EZ", &ez);
    summarise("NM", &nm);
}

/// Where the lapse optimum sits when the objective is the *median* rather than the
/// mean, and whether the `EZ` cohort wants the same point as the pooled set.
///
/// Both questions exist because `calibrate_lapse_on_multiuser` minimises a pooled
/// mean `g_timing`, and on this data the mean (42.3) is roughly 1.7x the median
/// (25.2). A mean objective is therefore steered by the worst tenth of fits. `EZ`
/// is broken out because it is the only cohort whose windows differ, so it is the
/// one the mod response is actually read from, and it is only ~5% of the rows.
///
/// Run with `cargo test lapse_objective_disagreement -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn lapse_objective_disagreement() {
    let Ok(text) = std::fs::read_to_string("local-fixtures/multiuser.tsv") else {
        println!("no fixtures present");
        return;
    };

    let mut data = Vec::new();
    for line in text.lines() {
        let f: Vec<&str> = line.split('\t').collect();
        if f.len() < 18 || f[0] == "uid" {
            continue;
        }

        let u = |s: &str| s.parse::<u32>().unwrap_or(0);
        let mods_str = f[3];
        let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", f[2])) else {
            continue;
        };
        let (mods, clock_rate) = mods_for(mods_str);
        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(false), None) else {
            continue;
        };

        let counts = [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])];
        let total = f64::from(counts.iter().sum::<u32>());
        let is_ez = mods_str.contains("EZ");

        data.push((attrs, counts, total, is_ez));
    }

    if data.is_empty() {
        println!("no valid scores loaded");
        return;
    }

    let stats = |model: &ErrorModel, ez_only: bool| {
        let mut v: Vec<f64> = Vec::new();
        for (attrs, counts, total, is_ez) in &data {
            if ez_only && !is_ez {
                continue;
            }
            let units = judgement_units(attrs, *total, model, !per_note_difficulty_disabled());
            let fit = fit_with_quality(counts, &units, &attrs.hit_windows, model);
            if fit.g_timing.is_finite() {
                v.push(fit.g_timing);
            }
        }
        v.sort_by(f64::total_cmp);
        let n = v.len();
        let mean = v.iter().sum::<f64>() / n as f64;
        let median = v[n / 2];
        (n, median, mean)
    };

    println!(
        "loaded {} scores ({} EZ)",
        data.len(),
        data.iter().filter(|d| d.3).count()
    );
    println!("weight ratio |  pooled n   med    mean |     EZ n   med    mean");

    for &weight in &[0.020, 0.0296, 0.034, 0.045] {
        for &ratio in &[2.5, 3.0, 3.339, 3.75, 4.4, 5.0] {
            let model = ErrorModel {
                lapse_weight: weight,
                lapse_ratio: ratio,
                ..ErrorModel::default()
            };
            let (pn, pmed, pmean) = stats(&model, false);
            let (en, emed, emean) = stats(&model, true);
            println!(
                "{weight:.4} {ratio:<5.3} | {pn:>8} {pmed:>6.1} {pmean:>7.1} | {en:>6} {emed:>6.1} {emean:>7.1}"
            );
        }
    }
}

/// Calibrate lapse parameters on the full multiuser dataset rather than the
/// hardcoded REAL_SCORES. The multiuser dataset is larger and more diverse.
///
/// Minimises a *pooled mean* `g_timing`. See [`lapse_objective_disagreement`] for
/// why that objective is not obviously the right one, and read its output before
/// shipping anything this test recommends.
///
/// Run with `cargo test calibrate_lapse_on_multiuser -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn calibrate_lapse_on_multiuser() {
    let Ok(text) = std::fs::read_to_string("local-fixtures/multiuser.tsv") else {
        println!("no fixtures present");
        return;
    };

    // Parse TSV and prepare data for calibration
    let mut data = Vec::new();
    for line in text.lines() {
        let f: Vec<&str> = line.split('\t').collect();
        if f.len() < 18 || f[0] == "uid" {
            continue;
        }

        let u = |s: &str| s.parse::<u32>().unwrap_or(0);
        let map_id = f[2];
        let mods_str = f[3];

        let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", map_id)) else {
            continue;
        };

        let (mods, clock_rate) = mods_for(mods_str);
        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(false), None) else {
            continue;
        };

        let counts = [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])];
        let total = f64::from(counts.iter().sum::<u32>());

        data.push((attrs, counts, total));
    }

    if data.is_empty() {
        println!("no valid scores loaded");
        return;
    }

    println!("loaded {} scores", data.len());

    // Helper to compute mean g_timing with a candidate model
    let mean_g = |model: &ErrorModel| {
        let mut sum = 0.0;
        let mut count = 0;

        for (attrs, counts, total) in &data {
            let units = judgement_units(attrs, *total, model, !per_note_difficulty_disabled());
            let fit = fit_with_quality(counts, &units, &attrs.hit_windows, model);

            if fit.g_timing.is_finite() {
                sum += fit.g_timing;
                count += 1;
            }
        }

        if count == 0 {
            f64::INFINITY
        } else {
            sum / count as f64
        }
    };

    let baseline = ErrorModel::default();
    let single_normal = ErrorModel {
        lapse_weight: 0.0,
        ..baseline
    };

    println!(
        "single normal:   mean g_timing={:.2}",
        mean_g(&single_normal)
    );
    println!(
        "current default: lapse_weight={:.4} lapse_ratio={:.3} mean g_timing={:.2}",
        baseline.lapse_weight,
        baseline.lapse_ratio,
        mean_g(&baseline)
    );

    // Grid search (narrower range since multiuser might be different)
    let mut best = single_normal;
    let mut best_score = mean_g(&single_normal);

    println!("\nGrid search...");
    let mut weight = 0.0;
    while weight <= 0.10 {
        let mut ratio = 2.0;
        while ratio <= 8.0 {
            let candidate = ErrorModel {
                lapse_weight: weight,
                lapse_ratio: ratio,
                ..baseline
            };
            let value = mean_g(&candidate);

            if value < best_score {
                best_score = value;
                best = candidate;
                println!(
                    "  new best: weight={:.4} ratio={:.2} g={:.2}",
                    weight, ratio, value
                );
            }

            ratio += 0.25;
        }
        weight += 0.005;
    }

    println!(
        "\nGrid best: lapse_weight={:.4} lapse_ratio={:.2} mean g_timing={:.2}",
        best.lapse_weight, best.lapse_ratio, best_score
    );

    // Refine
    let mut step = [0.0025, 0.125];
    for _ in 0..60 {
        for (axis, &size) in step.iter().enumerate() {
            for direction in [-1.0, 1.0] {
                let mut candidate = best;
                let delta = size * direction;

                if axis == 0 {
                    candidate.lapse_weight = (best.lapse_weight + delta).clamp(0.0, 0.95);
                } else {
                    candidate.lapse_ratio = (best.lapse_ratio + delta).max(1.0);
                }

                let value = mean_g(&candidate);

                if value < best_score {
                    best_score = value;
                    best = candidate;
                }
            }
        }

        for entry in &mut step {
            *entry *= 0.75;
        }
    }

    println!(
        "Refined:   lapse_weight={:.4} lapse_ratio={:.3} mean g_timing={:.2}",
        best.lapse_weight, best.lapse_ratio, best_score
    );
}

/// Not an assertion — a report. Prices every real score through the current
/// pipeline and prints the window scalar next to what the live server paid, so
/// the mod response can be read off real data rather than synthetics.
///
/// Run with `cargo test real_score_report -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn real_score_report() {
    let mut priced = 0usize;
    let mut ez_scalars = Vec::new();
    let mut nm_scalars = Vec::new();

    println!(
        "{:>9} {:>7} {:>4} {:>4} {:>6} {:>7} {:>8} {:>8} {:>7} {:>7} {:>9} {:>8}",
        "map",
        "mods",
        "od",
        "cvt",
        "stars",
        "acc%",
        "livePP",
        "ourPP",
        "scalar",
        "ppRatio",
        "g_timing",
        "plaus"
    );

    for row in REAL_SCORES {
        let path = format!("local-fixtures/maps/{}.osu", row.map);
        let Some(map) = parse(&path) else {
            println!("{:>9} missing beatmap", row.map);
            continue;
        };

        let has_ez = row.mods.contains("EZ");
        let has_dt = row.mods.contains("DT");

        let mut mods = LazerMods::new();
        if has_ez {
            single_mod(&mut mods, GameMod::EasyMania(Default::default()));
        }
        let clock_rate = if has_dt { 1.5 } else { 1.0 };

        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(true), None) else {
            println!("{:>9} no difficulty attributes", row.map);
            continue;
        };

        let state = SunnyScoreState {
            n320: row.n320,
            n300: row.n300,
            n200: row.n200,
            n100: row.n100,
            n50: row.n50,
            misses: row.miss,
        };

        let perf = calculate_performance(&attrs, &mods, state);

        let counts = [
            state.n320,
            state.n300,
            state.n200,
            state.n100,
            state.n50,
            state.misses,
        ];
        let units = [JudgementUnit::repeated(
            attrs.stars,
            f64::from(state.total_hits()),
        )];
        let fit = fit_with_quality(&counts, &units, &attrs.hit_windows, &ErrorModel::default());

        // What the same score would be worth with the scalar switched off, so
        // the window effect can be read directly in pp rather than in skill.
        let unpriced = compute_difficulty_value(attrs.stars, custom_accuracy(state), 1.0);
        let pp_ratio = if unpriced > 0.0 {
            compute_difficulty_value(attrs.stars, custom_accuracy(state), perf.window_scalar)
                / unpriced
        } else {
            1.0
        };

        println!(
            "{:>9} {:>7} {:>4.1} {:>4} {:>6.2} {:>7.3} {:>8.1} {:>8.1} {:>7.4} {:>7.4} {:>9.1} {:>8}",
            row.map,
            if row.mods.is_empty() { "NM" } else { row.mods },
            map.od,
            map.is_convert,
            attrs.stars,
            row.live_acc,
            row.live_pp,
            perf.pp,
            perf.window_scalar,
            pp_ratio,
            fit.g_timing,
            fit.is_plausible()
        );

        priced += 1;
        if has_ez {
            ez_scalars.push((perf.window_scalar, pp_ratio));
        } else {
            nm_scalars.push((perf.window_scalar, pp_ratio));
        }
    }

    if priced == 0 {
        println!("no fixtures present; nothing to report");
        return;
    }

    let summarise = |label: &str, values: &[(f64, f64)]| {
        if values.is_empty() {
            return;
        }
        let n = values.len() as f64;
        let mean_scalar = values.iter().map(|v| v.0).sum::<f64>() / n;
        let mean_pp = values.iter().map(|v| v.1).sum::<f64>() / n;
        let min = values.iter().map(|v| v.0).fold(f64::INFINITY, f64::min);
        let max = values.iter().map(|v| v.0).fold(f64::NEG_INFINITY, f64::max);
        println!(
            "{label}: n={} mean scalar {mean_scalar:.4} ({min:.4}..{max:.4})  \
                 mean pp ratio {mean_pp:.4}",
            values.len()
        );
    };

    println!();
    summarise("EZ", &ez_scalars);
    summarise("NM", &nm_scalars);
}

/// Not an assertion — the one real external check available on the surface.
///
/// A score screenshot supplied an *Unstable Rate*, which is `10 * sigma` of the
/// player's hit errors. That is a direct measurement of the exact quantity the
/// surface otherwise has to infer from judgement counts alone, so it tests the
/// model against ground truth rather than against its own residuals — something
/// the tRPC fixtures in [`REAL_SCORES`] cannot do, since the API carries no UR.
///
/// Note this works even though `sigma_ref` is unidentifiable
/// (`sigma_ref_only_sets_the_scale_of_skill`): the *implied sigma* at the fitted
/// skill is identified, because sigma is the only channel difficulty and skill
/// enter through. The gauge cancels.
///
/// Run with `cargo test unstable_rate_check -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn unstable_rate_check() {
    use crate::mania::sunny_accuracy::expected_counts;
    use crate::mania::sunny_windows::ManiaJudgement;

    // Yooh - Decoy [Rachel's Ruins "Buffed Ver."], played by Reflec with DT.
    // OD7 4K non-convert, 3860 notes, no long notes. Counts read off the result
    // screen; the 92.53% shown is ScoreV1 weighting (rainbow 300 counts as 300),
    // which the counts reproduce as 92.54%.
    let Some(map) = parse("local-fixtures/maps/4055699.osu") else {
        println!("beatmap 4055699 absent; nothing to check");
        return;
    };

    let measured_ur = 498.40;
    let measured_sigma = measured_ur / 10.0;
    let mean_error = 34.25;

    let state = SunnyScoreState {
        n320: 1542,
        n300: 1595,
        n200: 603,
        n100: 91,
        n50: 15,
        misses: 14,
    };
    let counts = [
        state.n320,
        state.n300,
        state.n200,
        state.n100,
        state.n50,
        state.misses,
    ];
    let total = state.total_hits();

    // Classic (stable) scoring: the screenshot is osu!stable ScoreV1.
    let mods = GameMods::default();
    let attrs = calculate(&map, &mods, 1.5, Some(false), None).unwrap();

    let units = [JudgementUnit::repeated(attrs.stars, f64::from(total))];
    let model = ErrorModel::default();
    let fit = fit_with_quality(&counts, &units, &attrs.hit_windows, &model);

    let windows = attrs.hit_windows;
    println!(
        "map: OD {} convert {} | {total} notes | stars {:.2} (DT 1.5x, classic)",
        map.od, map.is_convert, attrs.stars
    );
    println!(
        "windows: perfect {:.1} great {:.1} good {:.1} ok {:.1} meh {:.1} miss {:.1}",
        windows.perfect, windows.great, windows.good, windows.ok, windows.meh, windows.miss
    );

    let implied_sigma = model.sigma(attrs.stars, fit.skill);

    println!(
        "\nfitted skill {:.3} (ratio {:.3} of stars) -> implied sigma {:.2} ms",
        fit.skill,
        fit.skill / attrs.stars,
        implied_sigma
    );
    println!(
        "measured: UR {measured_ur:.2} -> sigma {measured_sigma:.2} ms  \
             (mean error {mean_error:.2} ms)",
    );
    println!(
        "ratio implied/measured = {:.3}",
        implied_sigma / measured_sigma
    );

    // The mixture has two widths; the single number comparable to a measured UR is
    // the mixture's own standard deviation, not the core width. For a zero-mean
    // two-component mixture, variance = (1-w)*s^2 + w*(k*s)^2.
    let weight = model.lapse_weight;
    let ratio = model.lapse_ratio;
    let mixture_sigma = implied_sigma * ((1.0 - weight) + weight * ratio * ratio).sqrt();

    println!(
        "mixture sigma (both components) = {mixture_sigma:.2} ms  \
             -> UR {:.1}, ratio to measured {:.3}",
        mixture_sigma * 10.0,
        mixture_sigma / measured_sigma
    );

    // Cross-check that does not involve the model at all: what sigma does the
    // observed PERFECT rate alone imply, for a plain normal? P(|e| < w) = share
    // inverts to sigma = w / z where z is the normal quantile. If this lands near
    // the fit's sigma but far from the measured UR, then the UR and the judgement
    // counts disagree with each other, and the model is siding with the counts.
    let perfect_share = f64::from(state.n320) / f64::from(total - state.misses);
    // z such that P(|Z| < z) = share, by bisection on the standard normal.
    let mut lo = 1e-6;
    let mut hi = 10.0;
    for _ in 0..200 {
        let mid = 0.5 * (lo + hi);
        // P(|Z| < mid) = 1 - erfc(mid / sqrt(2))
        let inside = 1.0 - crate::mania::sunny_accuracy::erfc(mid / std::f64::consts::SQRT_2);
        if inside < perfect_share {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    let z = 0.5 * (lo + hi);
    let sigma_from_320 = windows.perfect / z;

    println!(
        "\nmodel-free check: {:.1}% of hit notes inside the {:.1} ms PERFECT window\n\
             implies sigma {:.2} ms for a plain normal (z = {z:.4})",
        perfect_share * 100.0,
        windows.perfect,
        sigma_from_320
    );

    // The same inversion in the real-time frame, in case the client reports UR in
    // real milliseconds rather than map milliseconds. Under DT the two differ by
    // the clock rate, and it is worth showing both since the conclusion should not
    // rest on a convention.
    println!(
        "if the UR is real-time rather than map-time, the measured sigma is \
             {:.2} ms in map time instead",
        measured_sigma * 1.5
    );

    // Observed vs predicted band shares, conditioned on the note being hit.
    let expected = expected_counts(&units, &windows, &model, fit.skill);
    let observed_timing = f64::from(total - state.misses);
    let expected_timing = expected.total() - expected.get(ManiaJudgement::Miss);

    println!(
        "\n{:>10} {:>10} {:>10}",
        "judgement", "observed", "predicted"
    );

    for (label, judgement, observed) in [
        ("320", ManiaJudgement::Perfect, state.n320),
        ("300", ManiaJudgement::Great, state.n300),
        ("200", ManiaJudgement::Good, state.n200),
        ("100", ManiaJudgement::Ok, state.n100),
        ("50", ManiaJudgement::Meh, state.n50),
    ] {
        println!(
            "{label:>10} {:>10.4} {:>10.4}",
            f64::from(observed) / observed_timing,
            expected.get(judgement) / expected_timing
        );
    }

    println!(
        "\nmisses: observed {} predicted {:.1}",
        state.misses,
        expected.get(ManiaJudgement::Miss) / expected.total() * f64::from(total)
    );
    println!(
        "g_timing {:.1} plausible {} identifiable {}",
        fit.g_timing,
        fit.is_plausible(),
        fit.is_identifiable()
    );

    // What it prices at.
    let perf = calculate_performance(&attrs, &mods, state);
    println!(
        "\npp {:.1} | window_scalar {:.4} | custom_accuracy {:.3}%",
        perf.pp,
        perf.window_scalar,
        custom_accuracy(state) * 100.0
    );
}

/// Not an assertion — prints sunny's own star rating for every map named in a
/// ladder TSV, so a fit against replay-measured sigma can use the difficulty the
/// model actually grades on.
///
/// `tools/fetch_ladder.sh` selects on bancho.py's stored `maps.diff`, which is a
/// *different* difficulty calculation. That is fine for choosing a spread of maps
/// but wrong to regress against: the exponent in
/// `sigma = sigma_ref * ((d + floor)/skill)^skill_exponent` is only meaningful in
/// the units `d` is expressed in. Reads map ids from stdin, one per line.
///
/// Run with
/// `cut -f4 local-fixtures/ladder.tsv | tail -n +2 | cargo test ladder_stars --
/// --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn ladder_stars() {
    use std::io::BufRead as _;

    println!("map_id,stars,od,keys,is_convert");
    for line in std::io::stdin().lock().lines() {
        let Ok(line) = line else { break };
        let id = line.trim();
        if id.is_empty() || id == "mapid" {
            continue;
        }
        let path = format!("local-fixtures/maps/{id}.osu");
        let Some(map) = parse(&path) else {
            eprintln!("skip {id}: cannot parse");
            continue;
        };
        // No mods and rate 1.0: the ladder is deliberately no-mod/NF only, so the
        // windows and note timings are the map's own.
        let Some(attrs) = calculate(&map, &GameMods::default(), 1.0, Some(false), None) else {
            eprintln!("skip {id}: not a mania map");
            continue;
        };
        println!(
            "{id},{:.4},{},{},{}",
            attrs.stars, map.od, map.cs as u32, map.is_convert
        );
    }
}

/// Compute Sunny stars for `mapid<TAB>legacy_mod_bits` pairs.
///
/// Run with:
/// `SUNNY_STAR_PAIRS=local-fixtures/star-pairs.tsv cargo test --release fixture_stars -- --ignored --nocapture`
#[test]
#[ignore = "reads gitignored beatmaps; prints a fixture report"]
fn fixture_stars() {
    let pairs_path = std::env::var_os("SUNNY_STAR_PAIRS")
        .map(std::path::PathBuf::from)
        .expect("set SUNNY_STAR_PAIRS to mapid<TAB>legacy_mod_bits");
    let maps_dir = std::env::var_os("SUNNY_MAPS")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| "local-fixtures/maps".into());
    let pairs = std::fs::read_to_string(&pairs_path).expect("failed to read star pairs");

    for (line_no, line) in pairs.lines().enumerate() {
        if line.trim().is_empty() {
            continue;
        }

        let (map_id, mods) = line.split_once('\t').unwrap_or_else(|| {
            panic!("{}:{}: expected mapid<TAB>mods", pairs_path.display(), line_no + 1)
        });
        let mods: u32 = mods.parse().unwrap_or_else(|err| {
            panic!("{}:{}: invalid mods: {err}", pairs_path.display(), line_no + 1)
        });
        let path = maps_dir.join(format!("{map_id}.osu"));
        let Some(map) = parse(path.to_str().expect("non-UTF-8 beatmap path")) else {
            eprintln!("skip {map_id}: cannot parse {}", path.display());
            continue;
        };
        let mods = rosu_mods::GameModsIntermode::from_bits(mods)
            .with_mode(rosu_mods::GameMode::Mania);
        let Some(attrs) = calculate(&map, &mods, 1.0, Some(false), None) else {
            eprintln!("skip {map_id}: not a mania map");
            continue;
        };

        println!("{map_id}\t{}\t{}", mods.bits(), attrs.stars);
    }
}

/// Prices every score in a ladder TSV and reports what the surface says about it.
///
/// The point of difference from [`real_score_report`] is coverage: that set is 20
/// scores chosen to be EZ-heavy, all from strong players on 8-13 star maps, which
/// is the right shape for reading the mod response and the wrong shape for
/// checking whether the fit behaves across the population. This reads the ladder
/// fixtures instead — 270 no-mod scores, 9 players in two disjoint skill bands,
/// 2.3 to 10.0 stars — and groups by player so the skill estimate can be seen
/// tracking difficulty within one person rather than across a mixed field.
///
/// The `pp` column in the TSV comes from the live ppy.sb server, which runs an
/// older algorithm and not sunny, so it is reported as context rather than as a
/// target: a ratio against it measures the gap between two algorithms and not the
/// error in this one.
///
/// Usage:
/// `cargo test --release ladder_report -- --ignored --nocapture --exact
/// sunny::tests::ladder_report < local-fixtures/ladder.tsv`
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn ladder_report() {
    use crate::mania::sunny_accuracy::skill_for_counts;
    use std::collections::BTreeMap;
    use std::io::BufRead as _;

    struct Row {
        stars: f64,
        od: f32,
        acc: f64,
        live_pp: f64,
        our_pp: f64,
        skill: f64,
        scalar: f64,
        g_timing: f64,
        plausible: bool,
        notes: u32,
    }

    let mut by_player: BTreeMap<String, Vec<Row>> = BTreeMap::new();
    let mut skipped = 0usize;

    for line in std::io::stdin().lock().lines() {
        let Ok(line) = line else { break };
        let fields: Vec<&str> = line.trim_end().split('\t').collect();
        if fields.len() < 16 || fields[0] == "cohort" {
            continue;
        }

        let parse_u32 = |s: &str| s.parse::<u32>().unwrap_or(0);
        let state = SunnyScoreState {
            n320: parse_u32(fields[10]),
            n300: parse_u32(fields[11]),
            n200: parse_u32(fields[12]),
            n100: parse_u32(fields[13]),
            n50: parse_u32(fields[14]),
            misses: parse_u32(fields[15]),
        };

        let path = format!("local-fixtures/maps/{}.osu", fields[3]);
        let Some(map) = parse(&path) else {
            skipped += 1;
            continue;
        };
        // The ladder is no-mod/NF only by construction, so no window or rate mods
        // apply and the map's own timings are the right ones.
        let Some(attrs) = calculate(&map, &GameMods::default(), 1.0, Some(false), None) else {
            skipped += 1;
            continue;
        };

        let perf = calculate_performance(&attrs, &GameMods::default(), state);
        let counts = [
            state.n320,
            state.n300,
            state.n200,
            state.n100,
            state.n50,
            state.misses,
        ];
        let units = [JudgementUnit::repeated(
            attrs.stars,
            f64::from(state.total_hits()),
        )];
        let model = ErrorModel::default();
        let fit = fit_with_quality(&counts, &units, &attrs.hit_windows, &model);

        by_player
            .entry(fields[0].to_owned())
            .or_default()
            .push(Row {
                stars: attrs.stars,
                od: map.od,
                acc: fields[8].parse().unwrap_or(0.0),
                live_pp: fields[9].parse().unwrap_or(0.0),
                our_pp: perf.pp,
                skill: skill_for_counts(&counts, &units, &attrs.hit_windows, &model),
                scalar: perf.window_scalar,
                g_timing: fit.g_timing,
                plausible: fit.is_plausible(),
                notes: state.total_hits(),
            });
    }

    if by_player.is_empty() {
        println!("no rows read; pipe a ladder TSV on stdin");
        return;
    }

    let mut all: Vec<&Row> = Vec::new();

    for (player, rows) in &by_player {
        let mut rows: Vec<&Row> = rows.iter().collect();
        rows.sort_by(|a, b| a.stars.total_cmp(&b.stars));

        println!("\n=== player {player} ({} scores)", rows.len());
        println!(
            "{:>6} {:>4} {:>6} {:>7} {:>8} {:>8} {:>7} {:>7} {:>9} {:>6}",
            "stars",
            "od",
            "notes",
            "acc%",
            "livePP",
            "ourPP",
            "skill",
            "sk/st",
            "g_timing",
            "plaus"
        );

        // Every third row: the shape across difficulty is the point, and 30 lines
        // per player would bury it.
        for row in rows.iter().step_by(3) {
            println!(
                "{:>6.2} {:>4.1} {:>6} {:>7.3} {:>8.1} {:>8.1} {:>7.2} {:>7.2} {:>9.1} {:>6}",
                row.stars,
                row.od,
                row.notes,
                row.acc,
                row.live_pp,
                row.our_pp,
                row.skill,
                row.skill / row.stars,
                row.g_timing,
                row.plausible
            );
        }

        let mean = |f: &dyn Fn(&Row) -> f64| -> f64 {
            rows.iter().map(|r| f(r)).sum::<f64>() / rows.len() as f64
        };
        println!(
            "  mean skill {:.2}, mean skill/stars {:.2}, plausible {}/{}",
            mean(&|r| r.skill),
            mean(&|r| r.skill / r.stars),
            rows.iter().filter(|r| r.plausible).count(),
            rows.len()
        );

        all.extend(rows);
    }

    println!("\n=== overall ({} scores, {skipped} skipped)", all.len());

    let scalars: Vec<f64> = all.iter().map(|r| r.scalar).collect();
    let lo = scalars.iter().copied().fold(f64::INFINITY, f64::min);
    let hi = scalars.iter().copied().fold(f64::NEG_INFINITY, f64::max);
    println!("window scalar: {lo:.4}..{hi:.4} (no-mod, so departures from 1 are OD alone)");

    let plausible = all.iter().filter(|r| r.plausible).count();
    println!(
        "plausible: {plausible}/{} ({:.0}%)",
        all.len(),
        100.0 * plausible as f64 / all.len() as f64
    );

    let mut g: Vec<f64> = all.iter().map(|r| r.g_timing).collect();
    g.sort_by(f64::total_cmp);
    println!(
        "g_timing median {:.1}, p90 {:.1}",
        g[g.len() / 2],
        g[g.len() * 9 / 10]
    );

    // Does the fit place players consistently? Within one player, skill should be
    // roughly flat across difficulty; a trend means the exponent is off.
    println!("\nskill/stars by star band (flat = the exponent is right):");
    for (lo, hi) in [(2.0, 4.0), (4.0, 6.0), (6.0, 8.0), (8.0, 11.0)] {
        let band: Vec<&&Row> = all
            .iter()
            .filter(|r| r.stars >= lo && r.stars < hi)
            .collect();
        if band.is_empty() {
            continue;
        }
        let ratio = band.iter().map(|r| r.skill / r.stars).sum::<f64>() / band.len() as f64;
        println!(
            "  {lo:>4.1}-{hi:<4.1} n={:<4} mean skill/stars {ratio:.3}",
            band.len()
        );
    }
}

/// Not an assertion — dumps the surface to CSV under `target/surface/` so it can
/// be plotted. Three files, each a different slice of the same object:
///
/// - `grid.csv`: 305-weighted accuracy over (difficulty, skill) at
///   [`REFERENCE_WINDOWS`]. This *is* the surface.
/// - `bands.csv`: the five timing-band shares plus miss rate against skill at one
///   fixed difficulty — the mechanism the surface is built from.
/// - `windows.csv`: accuracy against skill at one difficulty for several window
///   sets, which is what [`window_scalar`] reads horizontally.
///
/// Run with `cargo test surface_dump -- --ignored --nocapture`.
#[test]
#[ignore = "writes CSV for plotting rather than asserting"]
fn surface_dump() {
    use crate::mania::sunny_accuracy::expected_counts;
    use crate::mania::sunny_windows::{ManiaJudgement, windows_from_great};
    use std::fmt::Write as _;

    let model = ErrorModel::default();
    let dir = std::path::Path::new("target/surface");
    std::fs::create_dir_all(dir).unwrap();

    let env = |key: &str| std::env::var(key).ok().filter(|value| !value.is_empty());
    let clock_rate = env("SURFACE_CLOCK_RATE")
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(1.0);

    // A real map supplies the slice difficulty, judgement-unit population, and
    // actual NM/EZ/HR windows. With no map, preserve the reproducible synthetic
    // Decoy slice used by the original visualiser.
    let map_slice = env("SURFACE_MAP").map(|path| {
        let map = parse(&path).unwrap_or_else(|| panic!("cannot parse {path}"));
        let attrs = calculate(&map, &GameMods::default(), clock_rate, Some(true), None)
            .unwrap_or_else(|| panic!("{path} is not a mania map"));

        (path, map, attrs)
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
    let skills = geom(0.5, 60.0, 161);

    let mut grid = String::from("difficulty,skill,accuracy,miss_rate\n");

    for &difficulty in &difficulties {
        for &skill in &skills {
            let units = [JudgementUnit::new(difficulty)];
            let expected = expected_counts(&units, &REFERENCE_WINDOWS, &model, skill);
            writeln!(
                grid,
                "{difficulty},{skill},{},{}",
                expected.custom_accuracy(),
                expected.get(ManiaJudgement::Miss) / expected.total()
            )
            .unwrap();
        }
    }

    std::fs::write(dir.join("grid.csv"), grid).unwrap();

    let difficulty = map_slice
        .as_ref()
        .map_or(13.774, |(_, _, attrs)| attrs.stars);
    let source = map_slice
        .as_ref()
        .map_or("default (Decoy DT)", |(path, _, _)| path.as_str());
    let units = map_slice
        .as_ref()
        .map(|(_, _, attrs)| judgement_units(attrs, 1.0, &model, true))
        .unwrap_or_else(|| vec![JudgementUnit::new(difficulty)]);

    std::fs::write(
        dir.join("surface_2d_meta.csv"),
        format!("difficulty,clock_rate,source\n{difficulty},{clock_rate},{source}\n"),
    )
    .unwrap();

    if let Some((_, _, attrs)) = map_slice.as_ref() {
        if let Some(bins) = attrs.input_state_bins {
            let mut csv = String::from("class,bin,count,mean_difficulty,mean_duration_ms,mean_gap_ms,mean_chord_width,mean_other_held\n");
            for (idx, bin) in bins.iter().enumerate() {
                if bin.count == 0 { continue; }
                writeln!(csv, "{:?},{},{},{},{},{},{},{}", bin.class, idx % crate::mania::sunny::NOTE_DIFFICULTY_BINS, bin.count, bin.mean_difficulty, bin.mean_duration_ms, bin.mean_gap_ms, bin.mean_chord_width, bin.mean_other_held).unwrap();
            }
            std::fs::write(dir.join("input_state_bins.csv"), csv).unwrap();
        }
    }
    if let Some((_, map, _)) = map_slice.as_ref() {
        if let Some((_, _, per_note)) = per_note_difficulty(map) {
            let mut csv = String::from("time_ms,note_index,difficulty,hold_duration_ms\n");
            for (idx, ((difficulty, duration), object)) in per_note.iter().zip(&map.hit_objects).enumerate() {
                writeln!(csv, "{},{},{},{}", object.start_time, idx, difficulty, duration.unwrap_or(0.0)).unwrap();
            }
            std::fs::write(dir.join("per_note_difficulty.csv"), csv).unwrap();

            if let Some((_, _, attrs)) = map_slice.as_ref() {
                let model = ErrorModel::default();
                let skill = attrs.stars.max(0.001);
                let mut csv = String::from("time_ms,note_index,variant,difficulty,miss,p50,p100,p200,p300,p320,custom_accuracy,acc_d\n");
                for (idx, ((difficulty, duration), object)) in per_note.iter().zip(&map.hit_objects).enumerate() {
                    for variant in ["baseline", "ln_as_rice"] {
                        let unit = if variant == "baseline" {
                            duration.map_or_else(|| JudgementUnit::new(*difficulty), |duration| JudgementUnit::long_note(*difficulty, 1.0, &model, duration))
                        } else {
                            JudgementUnit::new(*difficulty)
                        };
                        let counts = expected_counts(&[unit], &attrs.hit_windows, &model, skill);
                        let p = counts.as_array();
                        let accuracy = counts.custom_accuracy();
                        let acc_d = difficulty * (1.0 - accuracy);
                        writeln!(csv, "{},{},{},{},{},{},{},{},{},{},{},{}", object.start_time, idx, variant, difficulty, p[5], p[4], p[3], p[2], p[1], p[0], accuracy, acc_d).unwrap();
                    }
                }
                std::fs::write(dir.join("per_note_expected_counts.csv"), csv).unwrap();
            }
        }
    }

    let mut bands = String::from("skill,sigma,n320,n300,n200,n100,n50,miss,accuracy\n");

    for &skill in &skills {
        let windows = map_slice
            .as_ref()
            .map_or(REFERENCE_WINDOWS, |(_, _, attrs)| attrs.hit_windows);
        let expected = expected_counts(&units, &windows, &model, skill);
        let total = expected.total();
        let share = |judgement| expected.get(judgement) / total;

        writeln!(
            bands,
            "{skill},{},{},{},{},{},{},{},{}",
            model.sigma(difficulty, skill),
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
        let mut hr = GameMods::default();
        single_mod(&mut hr, GameMod::HardRockMania(Default::default()));
        let mut ez = GameMods::default();
        single_mod(&mut ez, GameMod::EasyMania(Default::default()));
        let hr_attrs = calculate(map, &hr, clock_rate, Some(true), None).unwrap();
        let ez_attrs = calculate(map, &ez, clock_rate, Some(true), None).unwrap();

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

    let mut windows_csv = String::from("label,great,skill,accuracy\n");

    for (label, great) in window_sets {
        let windows = if label == "natural" {
            map_slice.as_ref().unwrap().2.map_windows
        } else if (great - 40.5).abs() < 1e-9 {
            REFERENCE_WINDOWS
        } else {
            windows_from_great(great)
        };

        for &skill in &skills {
            let accuracy = expected_counts(&units, &windows, &model, skill).custom_accuracy();
            writeln!(windows_csv, "{label},{great},{skill},{accuracy}").unwrap();
        }
    }

    std::fs::write(dir.join("windows.csv"), windows_csv).unwrap();

    println!(
        "wrote {} (grid {} x {}, slice {:.3} stars from {})",
        dir.display(),
        difficulties.len(),
        skills.len(),
        difficulty,
        source,
    );
}

/// Not an assertion — dumps `target/surface/od_grid.csv`: every judgement band's
/// share over (OD, skill), with and without `EZ`, for plotting as 3D surfaces.
///
/// OD is the interesting third axis because mania's classic scheme treats it
/// unevenly — GREAT and below shift by `3 * (10 - od)` ms while PERFECT is pinned
/// at a flat 16 ms. So the 320 surface should be *flat* in OD and the others
/// should tilt, and `EZ` should be the only thing that ever moves 320. Both
/// scoring schemes are dumped since lazer interpolates PERFECT over OD instead.
///
/// The difficulty the OD/skill grid is taken at defaults to the Decoy score's
/// 13.77 stars so the dump reproduces without any setup, but `SURFACE_MAP` points
/// it at a real beatmap instead (its rated difficulty under `SURFACE_CLOCK_RATE`
/// is used), and `SURFACE_STARS` sets the number directly. `tools/mania_surface.py`
/// passes these through so any map can be inspected.
///
/// Run with `cargo test od_surface_dump -- --ignored --nocapture`.
#[test]
#[ignore = "writes CSV for plotting rather than asserting"]
fn od_surface_dump() {
    use crate::mania::sunny_accuracy::expected_counts;
    use crate::mania::sunny_windows::{ManiaJudgement, hit_windows};
    use std::fmt::Write as _;

    let model = ErrorModel::default();
    let dir = std::path::Path::new("target/surface");
    std::fs::create_dir_all(dir).unwrap();

    let env = |key: &str| std::env::var(key).ok().filter(|value| !value.is_empty());
    let clock_rate = env("SURFACE_CLOCK_RATE")
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(1.0);

    // Where the slice is taken. A real map wins over an explicit star value, which
    // wins over the Decoy default.
    let (difficulty, source) = if let Some(path) = env("SURFACE_MAP") {
        let map = parse(&path).unwrap_or_else(|| panic!("cannot parse {path}"));
        let attrs = calculate(&map, &GameMods::default(), clock_rate, Some(true), None)
            .unwrap_or_else(|| panic!("{path} is not a mania map"));

        (attrs.stars, path)
    } else if let Some(stars) = env("SURFACE_STARS").and_then(|v| v.parse::<f64>().ok()) {
        (stars, "SURFACE_STARS".to_owned())
    } else {
        (13.774, "default (Decoy DT)".to_owned())
    };

    println!("slice at {difficulty:.3} stars from {source} (clock rate {clock_rate})");
    std::fs::write(
        dir.join("meta.csv"),
        format!("difficulty,clock_rate,source\n{difficulty},{clock_rate},{source}\n"),
    )
    .unwrap();

    let ods: Vec<f64> = (0..=100).map(|i| f64::from(i) / 10.0).collect();
    let skills: Vec<f64> = (0..161)
        .map(|i| {
            let t = i as f64 / 160.0;
            0.5 * (60.0 / 0.5_f64).powf(t)
        })
        .collect();

    let mut with_ez = LazerMods::new();
    single_mod(&mut with_ez, GameMod::EasyMania(Default::default()));

    let mut out = String::from(
        "scheme,mod,od,skill,great,perfect,sigma,n320,n300,n200,n100,n50,miss,accuracy\n",
    );

    for (scheme, classic) in [("classic", true), ("lazer", false)] {
        for (mod_label, mods) in [("NM", GameMods::default()), ("EZ", with_ez.clone())] {
            for &od in &ods {
                // A bare non-convert map at this OD; only `od`/`is_convert` reach
                // the window construction. Converts are deliberately not swept:
                // their classic scheme keys off a single `round(od) > 4` threshold,
                // so an OD axis would be two flat plateaus rather than a surface.
                let mut map = Beatmap::default();
                map.mode = GameMode::Mania;
                map.od = od as f32;

                let windows = hit_windows(&map, &mods, clock_rate, classic);

                for &skill in &skills {
                    let units = [JudgementUnit::new(difficulty)];
                    let expected = expected_counts(&units, &windows, &model, skill);
                    let total = expected.total();
                    let share = |judgement| expected.get(judgement) / total;

                    writeln!(
                        out,
                        "{scheme},{mod_label},{od},{skill},{},{},{},{},{},{},{},{},{},{}",
                        windows.great,
                        windows.perfect,
                        model.sigma(difficulty, skill),
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
            }
        }
    }

    std::fs::write(dir.join("od_grid.csv"), out).unwrap();
    println!(
        "wrote od_grid.csv ({} od x {} skill x 2 schemes x 2 mod states)",
        ods.len(),
        skills.len()
    );
}

/// Not an assertion — prices one real score with and without EZ, holding the
/// judgement counts fixed.
///
/// Holding counts fixed is the whole point: it asks "what is this exact
/// performance worth if it had been produced through wider windows", which is
/// the question a mod multiplier answers by fiat and the surface answers by
/// refitting skill. Nothing here inspects the mod list — EZ enters only by
/// widening [`ManiaHitWindows`], and the pp difference is whatever that
/// widening does to the fit.
///
/// Run with `cargo test decoy_ez_comparison -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn decoy_ez_comparison() {
    // Yooh - Decoy [Rachel's Ruins "Buffed Ver."], played by Reflec with DT.
    let Some(map) = parse("local-fixtures/maps/4055699.osu") else {
        println!("beatmap 4055699 absent; nothing to compare");
        return;
    };

    let state = SunnyScoreState {
        n320: 1542,
        n300: 1595,
        n200: 603,
        n100: 91,
        n50: 15,
        misses: 14,
    };
    let counts = [
        state.n320,
        state.n300,
        state.n200,
        state.n100,
        state.n50,
        state.misses,
    ];
    let total = state.total_hits();
    let units_for = |stars: f64| [JudgementUnit::repeated(stars, f64::from(total))];
    let model = ErrorModel::default();

    let mut with_ez = LazerMods::new();
    single_mod(&mut with_ez, GameMod::EasyMania(Default::default()));

    let mut rows = Vec::new();

    for (label, mods) in [("DT", GameMods::default()), ("DT+EZ", with_ez)] {
        // Classic (stable) scoring, DT 1.5x, as played.
        let attrs = calculate(&map, &mods, 1.5, Some(false), None).unwrap();
        let fit = fit_with_quality(&counts, &units_for(attrs.stars), &attrs.hit_windows, &model);
        let perf = calculate_performance(&attrs, &mods, state);

        rows.push((label, attrs, fit, perf));
    }

    println!(
        "map: OD {} convert {} | {total} notes | counts 320:{} 300:{} 200:{} 100:{} 50:{} miss:{}",
        map.od,
        map.is_convert,
        state.n320,
        state.n300,
        state.n200,
        state.n100,
        state.n50,
        state.misses
    );
    println!("custom_accuracy {:.3}%\n", custom_accuracy(state) * 100.0);

    println!(
        "{:>7} {:>7} {:>7} {:>7} {:>7} {:>7} {:>7}",
        "mods", "great", "perfect", "stars", "skill", "sigma", "g_tim"
    );

    for (label, attrs, fit, _) in &rows {
        println!(
            "{label:>7} {:>7.1} {:>7.1} {:>7.3} {:>7.3} {:>7.2} {:>7.1}",
            attrs.hit_windows.great,
            attrs.hit_windows.perfect,
            attrs.stars,
            fit.skill,
            model.sigma(attrs.stars, fit.skill),
            fit.g_timing
        );
    }

    println!(
        "\n{:>7} {:>10} {:>10} {:>10}",
        "mods", "scalar", "pp_diff", "pp"
    );

    for (label, _, _, perf) in &rows {
        println!(
            "{label:>7} {:>10.4} {:>10.1} {:>10.1}",
            perf.window_scalar, perf.pp_difficulty, perf.pp
        );
    }

    let (_, _, _, nm) = &rows[0];
    let (_, _, _, ez) = &rows[1];

    println!(
        "\nEZ prices at {:.4}x the no-mod pp ({:.1} -> {:.1}, {:+.1})",
        ez.pp / nm.pp,
        nm.pp,
        ez.pp,
        ez.pp - nm.pp
    );
    println!(
        "of which the window scalar contributes {:.4}x",
        ez.window_scalar / nm.window_scalar
    );
}

/// Not an assertion — a report on *where* the fit misses. Prints each real
/// score's observed timing-band shares next to what the fitted surface
/// predicts, so the shape of the residual can be read directly instead of
/// being inferred from a single `g_timing` number.
///
/// Run with `cargo test residual_shape_report -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn residual_shape_report() {
    use crate::mania::sunny_accuracy::expected_counts;
    use crate::mania::sunny_windows::ManiaJudgement;

    println!(
        "{:>9} {:>7} {:>6} {:>6} {:>7} {:>39} {:>39}",
        "map",
        "mods",
        "stars",
        "skill",
        "g_tim",
        "observed 320/300/200/100/50",
        "predicted 320/300/200/100/50"
    );

    for row in REAL_SCORES {
        let path = format!("local-fixtures/maps/{}.osu", row.map);
        let Some(map) = parse(&path) else {
            continue;
        };

        let mut mods = LazerMods::new();
        if row.mods.contains("EZ") {
            single_mod(&mut mods, GameMod::EasyMania(Default::default()));
        }
        let clock_rate = if row.mods.contains("DT") { 1.5 } else { 1.0 };

        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(true), None) else {
            continue;
        };

        let counts = [row.n320, row.n300, row.n200, row.n100, row.n50, row.miss];
        let total: u32 = counts.iter().sum();
        let units = [JudgementUnit::repeated(attrs.stars, f64::from(total))];
        let model = ErrorModel::default();
        let fit = fit_with_quality(&counts, &units, &attrs.hit_windows, &model);
        let expected = expected_counts(&units, &attrs.hit_windows, &model, fit.skill);

        // Both sides conditioned on the note having been hit, which is the
        // space the fit actually works in.
        let observed_timing = f64::from(total - row.miss);
        let expected_timing = expected.total() - expected.get(ManiaJudgement::Miss);

        let fmt = |shares: [f64; 5]| {
            shares
                .iter()
                .map(|share| format!("{share:>7.4}"))
                .collect::<Vec<_>>()
                .join(" ")
        };

        let observed_shares = [
            f64::from(row.n320) / observed_timing,
            f64::from(row.n300) / observed_timing,
            f64::from(row.n200) / observed_timing,
            f64::from(row.n100) / observed_timing,
            f64::from(row.n50) / observed_timing,
        ];
        let predicted_shares = [
            expected.get(ManiaJudgement::Perfect) / expected_timing,
            expected.get(ManiaJudgement::Great) / expected_timing,
            expected.get(ManiaJudgement::Good) / expected_timing,
            expected.get(ManiaJudgement::Ok) / expected_timing,
            expected.get(ManiaJudgement::Meh) / expected_timing,
        ];

        println!(
            "{:>9} {:>7} {:>6.2} {:>6.2} {:>7.1} {} {}",
            row.map,
            if row.mods.is_empty() { "NM" } else { row.mods },
            attrs.stars,
            fit.skill,
            fit.g_timing,
            fmt(observed_shares),
            fmt(predicted_shares),
        );
    }
}

/// Not an assertion — a report. Sweeps [`ErrorModel::sigma_floor`] over the
/// physically motivated 1-5 ms band and prints what each value does to fit
/// quality *and* to pricing, on the same 20 real scores as
/// [`real_score_report`].
///
/// The band comes from the client rather than from a fit: osu! judges at 1000
/// ticks per second, so 1 ms is a hard physical floor on the timing anyone can
/// resolve, and keyboard scan plus OS scheduling jitter add a few ms on top of
/// it. That argument is independent of the replay measurement, which is what
/// makes it worth testing — the replay-derived 10 ms is refuted by judgement
/// counts (see the `sigma_floor` docs and
/// `a_sigma_floor_would_forbid_scores_that_exist`) but a value in this band is
/// not.
///
/// Two things are being separated here. Fit quality asks whether the floor
/// describes the counts better; pricing asks whether it changes any pp. They
/// are different questions, and the answers turn out to be "not at all" and
/// "yes, slightly", which is the least convenient pair.
///
/// The result: `mean_g_timing` is *bit-identical* at 51.552835 across the whole
/// 0-10 ms sweep, so the judgement counts of these 20 scores cannot see the
/// floor at any value. It is unfittable here for the same reason `sigma_ref` is
/// unfittable anywhere — see the comment in the body for the quadrature
/// arithmetic. Meanwhile the EZ window scalar slides 0.8273 to 0.8123 and total
/// pp falls 2.7% at 10 ms. Within the 1-5 ms band the pricing effect is
/// -0.03% to -0.69%, small but not nil.
///
/// Run with `cargo test sigma_floor_sweep -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn sigma_floor_sweep() {
    let scores = load_real_scores();

    if scores.is_empty() {
        println!("no fixtures present; nothing to sweep");
        return;
    }

    // The window scalar with the model's floor overridden. Mirrors
    // `window_scalar`, which takes the default model and so cannot be pointed at
    // a candidate.
    let scalar_with = |score: &LoadedScore, model: &ErrorModel| {
        let total: u32 = score.counts.iter().sum();
        if total == 0 || score.stars <= 0.0 {
            return 1.0;
        }
        let units = [JudgementUnit::repeated(score.stars, f64::from(total))];
        let played = fit_with_quality(&score.counts, &units, &score.windows, model);
        let reference = fit_with_quality(&score.counts, &units, &REFERENCE_WINDOWS, model);
        if played.skill <= 0.0 || reference.skill <= 0.0 {
            return 1.0;
        }
        played.skill / reference.skill
    };

    // `mean_g` is printed to six decimals deliberately. The floor is very nearly
    // a gauge parameter on this data — the fit absorbs it into skill almost
    // exactly, the way it absorbs `sigma_ref` perfectly — and only that many
    // digits show the residual movement at all.
    println!(
        "{:>6} {:>13} {:>9} {:>8} {:>8} {:>9} {:>9} {:>8}",
        "floor", "mean_g", "median_g", "plaus", "EZ_scal", "NM_scal", "totalPP", "dPP%"
    );

    let mut baseline_pp = 0.0;

    for floor in [0.0, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 10.0] {
        let model = ErrorModel {
            sigma_floor: floor,
            ..ErrorModel::default()
        };

        let mut gs = Vec::new();
        let mut ez = Vec::new();
        let mut nm = Vec::new();
        let mut total_pp = 0.0;

        for score in &scores {
            let total: u32 = score.counts.iter().sum();
            let units = [JudgementUnit::repeated(score.stars, f64::from(total))];
            let fit = fit_with_quality(&score.counts, &units, &score.windows, &model);
            gs.push(fit.g_timing);

            let scalar = scalar_with(score, &model);
            if score.mods.contains("EZ") {
                ez.push(scalar);
            } else {
                nm.push(scalar);
            }

            let state = SunnyScoreState {
                n320: score.counts[0],
                n300: score.counts[1],
                n200: score.counts[2],
                n100: score.counts[3],
                n50: score.counts[4],
                misses: score.counts[5],
            };

            // Everything except the scalar is floor-independent, so recomposing
            // the difficulty value is enough to see the pp effect.
            total_pp += compute_difficulty_value(score.stars, custom_accuracy(state), scalar);
        }

        if baseline_pp == 0.0 {
            baseline_pp = total_pp;
        }

        gs.sort_by(f64::total_cmp);
        let mean = |v: &[f64]| v.iter().sum::<f64>() / v.len() as f64;

        println!(
            "{floor:>6.1} {:>13.6} {:>9.1} {:>8} {:>8.4} {:>9.4} {:>9.1} {:>+8.2}",
            mean(&gs),
            gs[gs.len() / 2],
            gs.iter().filter(|g| **g < 30.0).count(),
            mean(&ez),
            mean(&nm),
            total_pp,
            100.0 * (total_pp / baseline_pp - 1.0),
        );
    }

    // Why `mean_g` does not move: the counts pin *sigma*, and the fit is free to
    // move skill, so a floor is absorbed by shrinking the skill term to keep
    // `hypot(floor, skill_term)` where the counts want it. At a 16 ms sigma a 2 ms
    // floor needs the skill term to fall to 15.875 ms — a 0.78% change, which
    // skill^-1.7 supplies exactly. The floor only becomes visible once the skill
    // term is itself small (a 2 ms floor inflates a 2 ms skill term by 41%), which
    // is the saturating regime the ladder's `acc between 88 and 99.5` filter
    // excludes by construction.
    //
    // The scalar moves anyway, and that asymmetry is the whole problem. It is a
    // ratio of skills fitted at two *different* window sets, hence two different
    // sigmas, and quadrature is nonlinear — the two skills do not scale by a
    // common factor, so the ratio shifts even though every `g_timing` is
    // unchanged. A floor is therefore unconstrained by this data while still
    // repricing it, which is a worse position than either fitting it or leaving
    // it out.

    // The ceiling a floor imposes, which is the constraint that killed 10 ms.
    // Independent of skill: it is what the model allows at infinite skill.
    println!("\nmax reachable 320 share at infinite skill (OD8, 16ms PERFECT):");
    let windows = crate::mania::sunny_windows::windows_from_great(40.0);
    for floor in [0.0, 1.0, 2.0, 3.0, 5.0, 10.0] {
        let model = ErrorModel {
            sigma_floor: floor,
            ..ErrorModel::default()
        };
        let units = [JudgementUnit::repeated(2.0, 1506.0)];
        let counts = crate::mania::sunny_accuracy::expected_counts(&units, &windows, &model, 1.0e4);
        let share = counts.get(crate::mania::sunny_windows::ManiaJudgement::Perfect) / 1506.0;
        println!(
            "  {floor:>4.1} ms -> {:>7.3}%  ({:>6.2} of 1506 notes forced off 320)",
            share * 100.0,
            1506.0 * (1.0 - share)
        );
    }
}

#[test]
fn classic_flag_uses_head_only_density() {
    let Some(map) = parse(MAP_1638954) else {
        return;
    };
    let mods = GameMods::default();

    let lazer = calculate(&map, &mods, 1.0, Some(true), None).unwrap();
    let stable = calculate(&map, &mods, 1.0, Some(false), None).unwrap();

    // The values may differ slightly between lazer and stable (classic)
    // plays because of the density weighting.
    assert!(stable.stars > 0.0 && lazer.stars > 0.0);
}

/// A row of `local-fixtures/multiuser.tsv`: one real score from the prod tRPC
/// API, carrying enough to name the beatmap in a report as well as price it.
struct MultiRow {
    uid: String,
    map_id: String,
    mods: String,
    live_stars: f64,
    keys: u32,
    counts: [u32; 6],
    acc: f64,
    live_pp: f64,
    title: String,
    version: String,
}

/// One priced score: what the surface makes of a [`MultiRow`].
struct MultiPriced {
    row: MultiRow,
    stars: f64,
    od: f32,
    is_convert: bool,
    current_pp: f64,
    neutral_pp: f64,
    base_difficulty_pp: f64,
    accuracy_proportion: f64,
    surface_multiplier: f64,
    acc_multiplier: f64,
    variety_multiplier: f64,
    length_multiplier: f64,
    scalar: f64,
    skill: f64,
    g_timing: f64,
    plausible: bool,
    notes: u32,
    /// The map's long-note share, and the axis the LN mixture actually acts on.
    /// Key count only stands in for it — 7K charts here average 58% long notes
    /// against 4K's 3% — so grouping by this separates the mechanism from the
    /// convention.
    ln_fraction: f64,
    /// Whether the score's long notes were judged as one unit (V1) or two (V2).
    ln_judged_as_one: bool,
    /// The skill [`window_scalar`] fits against the fixed reference windows, kept
    /// separately from [`Self::skill`] (the played-windows fit) because
    /// `window_scalar` is their *ratio* and can move in the opposite direction
    /// from either fit alone — see `ln_offset_under_the_fixed_reference`.
    reference_skill: f64,
    /// The reference-side fit's `g_timing`, alongside [`Self::g_timing`] (the
    /// played-side fit's). Neither is the fit quality of a real player against a
    /// real map; the reference side grades the *same observed counts* against a
    /// windows set the player never actually played under, so a bad reference
    /// `g_timing` means the reference windows are a poor description of those
    /// counts, not that the player misplayed.
    reference_g_timing: f64,
}

/// Builds the mod state for a report row from its mod-name string.
///
/// Only mods that reach the sunny path are translated: `EZ` and `HR` scale the
/// windows, `NF` carries the flat factor, `V2` decides how long notes are judged,
/// and `DT`/`NC`/`HT` are a clock rate rather than a `GameMod`. `MR` is ignored,
/// since mirroring does not change difficulty in this calculator.
///
/// `V2` used to be ignored here too, on the grounds that it "only changes the
/// score number, not the judgements". That is true for rice and false for long
/// notes, and the fixture set settles it: all 45 V2 rows have a judgement total of
/// `notes + LN` while 97 of 98 non-V2 rows total `notes`. So V2 splits an LN into
/// two judgements and V1 combines them, which changes both the count and the
/// spread — see [`crate::mania::sunny_accuracy::LN_SIGMA_SCALE`].
fn mods_for(names: &str) -> (LazerMods, f64) {
    let mut mods = LazerMods::new();
    if names.contains("V2") {
        single_mod(&mut mods, GameMod::ScoreV2Mania(Default::default()));
    }
    if names.contains("EZ") {
        single_mod(&mut mods, GameMod::EasyMania(Default::default()));
    }
    if names.contains("HR") {
        single_mod(&mut mods, GameMod::HardRockMania(Default::default()));
    }
    if names.contains("NF") {
        single_mod(&mut mods, GameMod::NoFailMania(Default::default()));
    }

    let clock_rate = if names.contains("DT") || names.contains("NC") {
        1.5
    } else if names.contains("HT") {
        0.75
    } else {
        1.0
    };

    (mods, clock_rate)
}

/// One score priced under two [`ErrorModel`]s, for before/after comparison of an
/// error-model change on a fixed dataset.
///
/// This exists because [`multiuser_report`] compares *our* pp against the live
/// server's, which cannot answer "what did this parameter change do" — live pp is
/// itself sunny, so both sides move when the model does. Here both columns come
/// from this build and differ only in the model, so the delta is attributable.
struct AbPriced {
    uid: String,
    map_id: String,
    mods: String,
    keys: u32,
    od: f32,
    acc: f64,
    notes: u32,
    ln_fraction: f64,
    live_pp: f64,
    before_pp: f64,
    after_pp: f64,
    before_g: f64,
    after_g: f64,
    before_plausible: bool,
    after_plausible: bool,
    before_scalar: f64,
    before_difficulty_value: f64,
    before_acc_multiplier: f64,
    after_scalar: f64,
    after_difficulty_value: f64,
    after_acc_multiplier: f64,
}

fn report_error_model() -> ErrorModel {
    ErrorModel::default()
}

fn composition_from_units(
    attrs: &SunnyManiaDifficultyAttributes,
    mods: &LazerMods,
    state: SunnyScoreState,
    model: &ErrorModel,
    units: &[crate::mania::sunny_accuracy::JudgementUnit],
) -> (f64, f64, f64, f64) {
    let counts = [
        state.n320,
        state.n300,
        state.n200,
        state.n100,
        state.n50,
        state.misses,
    ];
    let played = fit_with_quality(&counts, units, &attrs.hit_windows, model);
    let baseline_model = ErrorModel {
        recovery_offset: 0.0,
        anticipation_offset: 0.0,
        ..*model
    };
    let baseline_units =
        judgement_units(attrs, f64::from(state.total_hits()), &baseline_model, true);
    let baseline = fit_with_quality(
        &counts,
        &baseline_units,
        &attrs.map_windows,
        &baseline_model,
    );
    let scalar = if played.skill > 0.0 && baseline.skill > 0.0 {
        played.skill / baseline.skill
    } else {
        1.0
    };
    let multiplier = if has_mod(mods, "NF") { 0.75 } else { 1.0 };
    let difficulty_value = compute_difficulty_value(attrs.stars, custom_accuracy(state), scalar);
    let acc = acc_multiplier(custom_accuracy(state), attrs.acc_scalar);
    let pp = difficulty_value
        * multiplier
        * variety_multiplier(attrs.variety)
        * acc
        * length_multiplier(attrs.n_objects as f64, attrs.stars);
    (pp, scalar, difficulty_value, acc)
}

/// Prices every `multiuser.tsv` row under `before` and `after`, so an error-model
/// change can be read as a pp delta and a fit delta on identical scores.
///
/// Only the [`ErrorModel`] differs between the two columns — the map, mods, star
/// rating and counts are parsed once and shared, so nothing but the model can
/// explain a difference.
fn load_multiuser_ab(before: &ErrorModel, after: &ErrorModel) -> Vec<AbPriced> {
    let Ok(text) = std::fs::read_to_string("local-fixtures/multiuser.tsv") else {
        return Vec::new();
    };

    let mut out = Vec::new();

    for line in text.lines() {
        let f: Vec<&str> = line.split('\t').collect();
        if f.len() < 18 || f[0] == "uid" {
            continue;
        }

        let u = |s: &str| s.parse::<u32>().unwrap_or(0);
        let counts = [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])];

        let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", f[2])) else {
            continue;
        };

        let (mods, clock_rate) = mods_for(f[3]);

        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(false), None) else {
            continue;
        };

        let state = SunnyScoreState {
            n320: counts[0],
            n300: counts[1],
            n200: counts[2],
            n100: counts[3],
            n50: counts[4],
            misses: counts[5],
        };
        let total = f64::from(state.total_hits());
        let per_note = !per_note_difficulty_disabled();

        // The only thing that varies across the pair.
        let price = |model: &ErrorModel| {
            let units = judgement_units(&attrs, total, model, per_note);
            let fit = fit_with_quality(&counts, &units, &attrs.hit_windows, model);
            let perf = calculate_performance_with_model(&attrs, &mods, state, model);

            let (_, scalar, difficulty_value, acc_multiplier) =
                composition_from_units(&attrs, &mods, state, model, &units);
            (
                perf.pp,
                fit.g_timing,
                fit.is_plausible(),
                scalar,
                difficulty_value,
                acc_multiplier,
            )
        };

        let (
            before_pp,
            before_g,
            before_plausible,
            before_scalar,
            before_difficulty_value,
            before_acc_multiplier,
        ) = price(before);
        let (
            after_pp,
            after_g,
            after_plausible,
            after_scalar,
            after_difficulty_value,
            after_acc_multiplier,
        ) = price(after);

        out.push(AbPriced {
            uid: f[0].to_owned(),
            map_id: f[2].to_owned(),
            mods: f[3].to_owned(),
            keys: u(f[6]),
            od: map.od,
            acc: f[13].parse().unwrap_or(0.0),
            notes: state.total_hits(),
            ln_fraction: if attrs.n_objects > 0 {
                attrs.n_long_notes as f64 / attrs.n_objects as f64
            } else {
                0.0
            },
            live_pp: f[14].parse().unwrap_or(0.0),
            before_pp,
            after_pp,
            before_g,
            after_g,
            before_plausible,
            after_plausible,
            before_scalar,
            before_difficulty_value,
            before_acc_multiplier,
            after_scalar,
            after_difficulty_value,
            after_acc_multiplier,
        });
    }

    out
}

/// Summarises one cohort of [`AbPriced`] rows: pp movement and fit movement.
///
/// Both are reported because they answer different questions and can disagree —
/// a change that moves pp while worsening `g_timing` is repricing on a worse fit,
/// which is what [[bias-channel-works-amplitude-wrong]] caught before.
fn summarise_ab(label: &str, rows: &[&AbPriced]) {
    if rows.is_empty() {
        return;
    }

    let n = rows.len() as f64;
    let before_sum: f64 = rows.iter().map(|r| r.before_pp).sum();
    let after_sum: f64 = rows.iter().map(|r| r.after_pp).sum();
    let live_sum: f64 = rows.iter().map(|r| r.live_pp).sum();

    let mut deltas: Vec<f64> = rows
        .iter()
        .filter(|r| r.before_pp > 0.0)
        .map(|r| (r.after_pp / r.before_pp - 1.0) * 100.0)
        .collect();
    deltas.sort_by(f64::total_cmp);

    let median = if deltas.is_empty() {
        0.0
    } else {
        deltas[deltas.len() / 2]
    };
    let mean = if deltas.is_empty() {
        0.0
    } else {
        deltas.iter().sum::<f64>() / deltas.len() as f64
    };

    let raised = deltas.iter().filter(|d| **d > 0.01).count();
    let lowered = deltas.iter().filter(|d| **d < -0.01).count();

    let before_g: f64 = rows.iter().map(|r| r.before_g).sum::<f64>() / n;
    let after_g: f64 = rows.iter().map(|r| r.after_g).sum::<f64>() / n;
    let finite_mean = |pick: fn(&AbPriced) -> f64| {
        let values: Vec<f64> = rows
            .iter()
            .map(|r| pick(r))
            .filter(|v| v.is_finite())
            .collect();
        if values.is_empty() {
            f64::NAN
        } else {
            values.iter().sum::<f64>() / values.len() as f64
        }
    };
    let before_scalar = finite_mean(|r| r.before_scalar);
    let before_dv = finite_mean(|r| r.before_difficulty_value);
    let before_acc = finite_mean(|r| r.before_acc_multiplier);
    let after_scalar = finite_mean(|r| r.after_scalar);
    let after_dv = finite_mean(|r| r.after_difficulty_value);
    let after_acc = finite_mean(|r| r.after_acc_multiplier);

    // Median alongside the mean, because a handful of pathological fits move the
    // mean a long way and that is exactly where a shape change shows up first.
    let median_of = |pick: fn(&AbPriced) -> f64| {
        let mut v: Vec<f64> = rows
            .iter()
            .map(|r| pick(r))
            .filter(|g| g.is_finite())
            .collect();
        v.sort_by(f64::total_cmp);
        if v.is_empty() {
            f64::NAN
        } else {
            v[v.len() / 2]
        }
    };
    let before_g_med = median_of(|r| r.before_g);
    let after_g_med = median_of(|r| r.after_g);

    // `plausible` is a hard threshold on the same `g_timing` printed above, so it
    // carries no information the g columns do not. Kept only as a rough count of
    // how many fits sit near the cutoff; never treat a change in it as a result on
    // its own.
    let before_plaus = rows.iter().filter(|r| r.before_plausible).count();
    let after_plaus = rows.iter().filter(|r| r.after_plausible).count();

    println!(
        "  {label}: n={:<4} pp {:.0} -> {:.0} ({:+.2}%)  live {:.0} ratios {:.1}% -> {:.1}%  med {:+.2}% mean {:+.2}%  \
             up/down {raised}/{lowered}  g med {:.1} -> {:.1}  mean {:.1} -> {:.1}  \
             (plaus {before_plaus} -> {after_plaus})  composition scalar {:.5} -> {:.5} difficulty {:.3} -> {:.3} acc_mult {:.5} -> {:.5}",
        rows.len(),
        before_sum,
        after_sum,
        if before_sum > 0.0 {
            (after_sum / before_sum - 1.0) * 100.0
        } else {
            0.0
        },
        live_sum,
        if live_sum > 0.0 {
            100.0 * before_sum / live_sum
        } else {
            0.0
        },
        if live_sum > 0.0 {
            100.0 * after_sum / live_sum
        } else {
            0.0
        },
        median,
        mean,
        before_g_med,
        after_g_med,
        before_g,
        after_g,
        before_scalar,
        after_scalar,
        before_dv,
        after_dv,
        before_acc,
        after_acc,
    );
}

/// Before/after comparison of an [`ErrorModel`] change across the whole multiuser
/// dataset. Set the two models at the top of the test.
///
/// This is the harness that was missing when the lapse refit shipped: the fit was
/// scored on the small hardcoded `REAL_SCORES` table, which said the change was a
/// 14.6x improvement, while the full dataset said it was worse than no lapse at
/// all. A calibration is only as good as the set it was scored on, so the set has
/// to be the same one production sees.
///
/// Run with `cargo test model_ab_report -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn model_ab_report() {
    use std::collections::BTreeMap;

    // The pair under test. `before` should be the shipped default so the report
    // reads as "what would this change do".
    //
    // `MODEL_AB_NULL=1` sets both sides to the default, which must print exactly
    // 0.00% everywhere. That is the harness's own control: a non-zero delta there
    // would mean the two columns differ by something other than the model.
    let null_run = std::env::var_os("MODEL_AB_NULL").is_some();
    let before = ErrorModel {
        recovery_offset: 0.0,
        anticipation_offset: 0.0,
        ..ErrorModel::default()
    };
    let after = if null_run {
        before
    } else {
        ErrorModel::default()
    };

    if null_run {
        println!("NULL RUN: both sides are the shipped default; all deltas must be 0.00%");
    } else {
        println!("INPUT-STATE RUN: enabling the centered same-column recovery curve");
    }

    println!(
        "before: lapse_weight={:.4} lapse_ratio={:.3} recovery_offset={:.2}",
        before.lapse_weight, before.lapse_ratio, before.recovery_offset
    );
    println!(
        "after:  lapse_weight={:.4} lapse_ratio={:.3} recovery_offset={:.2}",
        after.lapse_weight, after.lapse_ratio, after.recovery_offset
    );

    let scores = load_multiuser_ab(&before, &after);

    if scores.is_empty() {
        println!("no fixtures present (local-fixtures/multiuser.tsv); nothing to report");
        return;
    }

    let all: Vec<&AbPriced> = scores.iter().collect();
    let users: std::collections::BTreeSet<&str> = scores.iter().map(|s| s.uid.as_str()).collect();

    println!(
        "\n=== overall ({} scores, {} users)",
        all.len(),
        users.len()
    );
    summarise_ab("all", &all);

    // Per-user, because a pooled mean hides one player's odd shape driving the
    // whole figure — the mistake [[three-players-cannot-fit-anything]] records.
    println!("\nby user:");
    let mut by_uid: BTreeMap<&str, Vec<&AbPriced>> = BTreeMap::new();
    for s in &scores {
        by_uid.entry(s.uid.as_str()).or_default().push(s);
    }
    for (uid, rows) in &by_uid {
        summarise_ab(&format!("uid {uid}"), rows);
    }

    println!("\nby window-affecting mod:");
    type Pred = fn(&&AbPriced) -> bool;
    for (label, pred) in [
        ("EZ (windows widened)", (|r| r.mods.contains("EZ")) as Pred),
        ("HR (windows narrowed)", |r| r.mods.contains("HR")),
        ("no window mod", |r| {
            !r.mods.contains("EZ") && !r.mods.contains("HR")
        }),
    ] {
        let group: Vec<&AbPriced> = all.iter().copied().filter(pred).collect();
        summarise_ab(label, &group);
    }

    println!("\nby key count:");
    for keys in [4u32, 5, 6, 7, 8, 9, 10] {
        let group: Vec<&AbPriced> = all.iter().copied().filter(|r| r.keys == keys).collect();
        summarise_ab(&format!("{keys}k"), &group);
    }

    println!("\nby long-note share:");
    for (lo, hi) in [(0.0, 0.05), (0.05, 0.30), (0.30, 0.60), (0.60, 1.01)] {
        let group: Vec<&AbPriced> = all
            .iter()
            .copied()
            .filter(|r| r.ln_fraction >= lo && r.ln_fraction < hi)
            .collect();
        summarise_ab(
            &format!("LN {:>3.0}-{:<3.0}%", lo * 100.0, hi * 100.0),
            &group,
        );
    }

    println!("\nlow-OD target and controls:");
    for (label, pred) in [
        (
            "low OD <7, rice <30% LN",
            (|r: &&AbPriced| r.od < 7.0 && r.ln_fraction < 0.30) as Pred,
        ),
        ("low OD <7, LN >=30%", |r: &&AbPriced| {
            r.od < 7.0 && r.ln_fraction >= 0.30
        }),
        ("OD >=8, rice <30% LN", |r: &&AbPriced| {
            r.od >= 8.0 && r.ln_fraction < 0.30
        }),
        ("OD >=8, LN >=30%", |r: &&AbPriced| {
            r.od >= 8.0 && r.ln_fraction >= 0.30
        }),
    ] {
        let group: Vec<&AbPriced> = all.iter().copied().filter(pred).collect();
        summarise_ab(label, &group);
    }

    println!("\ndeterministic map holdout (map id mod 5):");
    for (label, held_out) in [("train folds 1-4", false), ("held-out fold 0", true)] {
        let group: Vec<&AbPriced> = all
            .iter()
            .copied()
            .filter(|r| {
                r.map_id
                    .parse::<u64>()
                    .is_ok_and(|id| (id % 5 == 0) == held_out)
            })
            .collect();
        summarise_ab(label, &group);
    }

    println!("\nby accuracy band:");
    for (lo, hi) in [(0.0, 90.0), (90.0, 95.0), (95.0, 98.0), (98.0, 100.01)] {
        let group: Vec<&AbPriced> = all
            .iter()
            .copied()
            .filter(|r| r.acc >= lo && r.acc < hi)
            .collect();
        summarise_ab(&format!("acc {lo:>5.1}-{hi:<5.1}"), &group);
    }

    // The largest individual movers, since a cohort mean can hide a few scores
    // being repriced hard in both directions.
    let mut movers: Vec<&AbPriced> = all.iter().copied().filter(|r| r.before_pp > 0.0).collect();
    movers.sort_by(|a, b| {
        let da = (a.after_pp / a.before_pp - 1.0).abs();
        let db = (b.after_pp / b.before_pp - 1.0).abs();
        db.total_cmp(&da)
    });

    println!("\nlargest 15 movers:");
    println!(
        "{:>8} {:>8} {:>9} {:>4} {:>4} {:>6} {:>7} {:>8} {:>8} {:>8} {:>8} {:>7} {:>7}",
        "uid",
        "map",
        "mods",
        "k",
        "od",
        "notes",
        "acc%",
        "beforePP",
        "afterPP",
        "d%",
        "aft/live",
        "g_bef",
        "g_aft"
    );
    for r in movers.iter().take(15) {
        println!(
            "{:>8} {:>8} {:>9} {:>4} {:>4.1} {:>6} {:>7.3} {:>8.1} {:>8.1} {:>+8.2} {:>7.1}% {:>7.1} {:>7.1}",
            r.uid,
            r.map_id,
            if r.mods.is_empty() { "NM" } else { &r.mods },
            r.keys,
            r.od,
            r.notes,
            r.acc,
            r.before_pp,
            r.after_pp,
            (r.after_pp / r.before_pp - 1.0) * 100.0,
            if r.live_pp > 0.0 {
                100.0 * r.after_pp / r.live_pp
            } else {
                0.0
            },
            r.before_g,
            r.after_g,
        );
    }
}

/// Reads `local-fixtures/multiuser.tsv` and prices every row twice.
fn load_multiuser() -> Vec<MultiPriced> {
    let Ok(text) = std::fs::read_to_string("local-fixtures/multiuser.tsv") else {
        return Vec::new();
    };

    let mut out = Vec::new();

    for line in text.lines() {
        let f: Vec<&str> = line.split('\t').collect();
        if f.len() < 18 || f[0] == "uid" {
            continue;
        }

        let u = |s: &str| s.parse::<u32>().unwrap_or(0);
        let row = MultiRow {
            uid: f[0].to_owned(),
            map_id: f[2].to_owned(),
            mods: f[3].to_owned(),
            live_stars: f[4].parse().unwrap_or(0.0),
            keys: u(f[6]),
            counts: [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])],
            acc: f[13].parse().unwrap_or(0.0),
            live_pp: f[14].parse().unwrap_or(0.0),
            title: f[16].to_owned(),
            version: f[17].to_owned(),
        };

        let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", row.map_id)) else {
            continue;
        };

        let (mods, clock_rate) = mods_for(&row.mods);

        // These are ppy.sb scores, i.e. stable, so `lazer: false`. It used to be
        // `Some(true)` here, which silently made every fixture ScoreV2 and hid the
        // LN judgement regime entirely. The judgement totals settle which is
        // right: a non-V2 row totals `notes`, which is the V1/classic count, and
        // only the V2 rows total `notes + LN`. With `is_classic(Some(false), ..)`
        // the V2 bit in `mods` now selects between the two the same way the server
        // does.
        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(false), None) else {
            continue;
        };

        let state = SunnyScoreState {
            n320: row.counts[0],
            n300: row.counts[1],
            n200: row.counts[2],
            n100: row.counts[3],
            n50: row.counts[4],
            misses: row.counts[5],
        };

        let model = report_error_model();
        let perf = calculate_performance_with_model(&attrs, &mods, state, &model);
        let score_accuracy = custom_accuracy(state);
        let base_stars = f64::max(attrs.stars - 0.15, 0.05);
        let base_difficulty_pp = 9.8 * base_stars.powf(2.2);
        let accuracy_proportion = performance_proportion(score_accuracy);
        let surface_multiplier = perf.window_scalar.max(0.0).powf(2.2);
        let neutral_pp = compute_difficulty_value(attrs.stars, score_accuracy, 1.0)
            * if has_mod(&mods, "NF") { 0.75 } else { 1.0 }
            * perf.variety_multiplier
            * perf.acc_multiplier
            * perf.length_multiplier;
        let units = judgement_units(
            &attrs,
            f64::from(state.total_hits()),
            &model,
            !per_note_difficulty_disabled(),
        );
        let fit = fit_with_quality(&row.counts, &units, &attrs.hit_windows, &model);
        let reference_fit =
            fit_with_quality(&row.counts, &units, &reference_windows(&attrs), &model);

        out.push(MultiPriced {
            stars: attrs.stars,
            od: map.od,
            is_convert: map.is_convert,
            current_pp: perf.pp,
            neutral_pp,
            base_difficulty_pp,
            accuracy_proportion,
            surface_multiplier,
            acc_multiplier: perf.acc_multiplier,
            variety_multiplier: perf.variety_multiplier,
            length_multiplier: perf.length_multiplier,
            scalar: perf.window_scalar,
            skill: fit.skill,
            g_timing: fit.g_timing,
            plausible: fit.is_plausible(),
            notes: state.total_hits(),
            ln_fraction: if attrs.n_objects > 0 {
                attrs.n_long_notes as f64 / attrs.n_objects as f64
            } else {
                0.0
            },
            ln_judged_as_one: attrs.ln_judged_as_one,
            reference_skill: reference_fit.skill,
            reference_g_timing: reference_fit.g_timing,
            row,
        });
    }

    out
}

/// Reads a difficulty-ladder TSV (`local-fixtures/ladder.tsv` or
/// `local-fixtures/ladder-strong.tsv`) and prices every row exactly like
/// [`load_multiuser`], reusing [`MultiRow`]/[`MultiPriced`] rather than a second
/// point type so ladder and multiuser scores can be pooled directly by
/// [`collision_skill_slope`]. The TSV's `cohort` column becomes `uid`: each ladder
/// cohort is one player, exactly as multiuser's `uid` is.
///
/// The ladder is no-mod/NF only by construction (`tools/fetch_ladder.sh` selects
/// `mods in (0,1)`, and in the fixtures on disk the `mods` column is `0` for every
/// row — no `NF` rows actually landed), so `mods_for` is skipped entirely and
/// `row.mods` is stored as the literal `"0"` from the TSV, which makes
/// `.contains("EZ")`/`.contains("HR")` downstream correctly return false.
fn load_ladder(path: &str) -> Vec<MultiPriced> {
    let Ok(text) = std::fs::read_to_string(path) else {
        return Vec::new();
    };

    let mut out = Vec::new();

    for line in text.lines() {
        let f: Vec<&str> = line.split('\t').collect();
        if f.len() < 19 || f[0] == "cohort" {
            continue;
        }

        let u = |s: &str| s.parse::<u32>().unwrap_or(0);
        let mut row = MultiRow {
            uid: f[0].to_owned(),
            map_id: f[3].to_owned(),
            mods: f[7].to_owned(),
            live_stars: f[6].parse().unwrap_or(0.0),
            keys: 0,
            counts: [u(f[10]), u(f[11]), u(f[12]), u(f[13]), u(f[14]), u(f[15])],
            acc: f[8].parse().unwrap_or(0.0),
            live_pp: f[9].parse().unwrap_or(0.0),
            title: "ladder".to_owned(),
            version: String::new(),
        };

        let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", row.map_id)) else {
            continue;
        };
        row.keys = map.cs.round_ties_even().max(1.0) as u32;

        // No mods, rate 1.0: the ladder is deliberately no-mod/NF only, so the
        // map's own windows and note timings are the right ones (same assumption
        // `ladder_report` makes above).
        let Some(attrs) = calculate(&map, &GameMods::default(), 1.0, Some(false), None) else {
            continue;
        };

        let state = SunnyScoreState {
            n320: row.counts[0],
            n300: row.counts[1],
            n200: row.counts[2],
            n100: row.counts[3],
            n50: row.counts[4],
            misses: row.counts[5],
        };

        let perf = calculate_performance(&attrs, &GameMods::default(), state);
        let score_accuracy = custom_accuracy(state);
        let base_stars = f64::max(attrs.stars - 0.15, 0.05);
        let model = ErrorModel::default();
        let units = judgement_units(
            &attrs,
            f64::from(state.total_hits()),
            &model,
            !per_note_difficulty_disabled(),
        );
        let fit = fit_with_quality(&row.counts, &units, &attrs.hit_windows, &model);
        let reference_fit =
            fit_with_quality(&row.counts, &units, &reference_windows(&attrs), &model);

        out.push(MultiPriced {
            stars: attrs.stars,
            od: map.od,
            is_convert: map.is_convert,
            current_pp: perf.pp,
            neutral_pp: compute_difficulty_value(attrs.stars, score_accuracy, 1.0)
                * perf.variety_multiplier
                * perf.acc_multiplier
                * perf.length_multiplier,
            base_difficulty_pp: 9.8 * base_stars.powf(2.2),
            accuracy_proportion: performance_proportion(score_accuracy),
            surface_multiplier: perf.window_scalar.max(0.0).powf(2.2),
            acc_multiplier: perf.acc_multiplier,
            variety_multiplier: perf.variety_multiplier,
            length_multiplier: perf.length_multiplier,
            scalar: perf.window_scalar,
            skill: fit.skill,
            g_timing: fit.g_timing,
            plausible: fit.is_plausible(),
            notes: state.total_hits(),
            ln_fraction: if attrs.n_objects > 0 {
                attrs.n_long_notes as f64 / attrs.n_objects as f64
            } else {
                0.0
            },
            ln_judged_as_one: attrs.ln_judged_as_one,
            reference_skill: reference_fit.skill,
            reference_g_timing: reference_fit.g_timing,
            row,
        });
    }

    out
}

/// Not an assertion — the cross-user report. Prices every score in
/// `local-fixtures/multiuser.tsv` under both the pre-change stack
/// (flat `EZ` `0.90`, no window scalar) and the current one
/// (windows priced, no `EZ` factor), and prints them side by side.
///
/// Why both are computed here rather than read from the API's `pp` column: live
/// ppy.sb runs sunny, but *a sunny predating this branch*, so its stored figure
/// differs from our "before" only by version drift in the difficulty calculation
/// itself. Recomputing the old multiplier stack against today's star ratings
/// isolates the change under test — the pp delta is then attributable to the
/// surface alone, with the live column left in as a cross-check on how far the
/// two sunny versions have otherwise moved.
///
/// `cargo test --release multiuser_report -- --ignored --nocapture --exact
/// sunny::tests::multiuser_report`
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn multiuser_report() {
    use std::collections::BTreeMap;

    println!("calculation: centered input-state model");

    let scores = load_multiuser();
    if scores.is_empty() {
        println!("no fixtures present (local-fixtures/multiuser.tsv); nothing to report");
        return;
    }

    let mut by_uid: BTreeMap<&str, Vec<&MultiPriced>> = BTreeMap::new();
    for s in &scores {
        by_uid.entry(s.row.uid.as_str()).or_default().push(s);
    }

    for (uid, rows) in &by_uid {
        let mut rows = rows.clone();
        rows.sort_by(|a, b| b.current_pp.total_cmp(&a.current_pp));

        println!("\n=== uid {uid} ({} scores)", rows.len());
        println!(
            "{:>8} {:>9} {:>4} {:>4} {:>4} {:>6} {:>6} {:>6} {:>26} {:>7} {:>8} {:>9} {:>7} {:>7} {:>6} {:>5}",
            "map",
            "mods",
            "k",
            "od",
            "cvt",
            "our*",
            "live*",
            "notes",
            "320/300/200/100/50/miss",
            "acc%",
            "livePP",
            "currentPP",
            "d%",
            "scalar",
            "skill",
            "plaus"
        );

        for r in &rows {
            let delta = if r.row.live_pp > 0.0 {
                (r.current_pp / r.row.live_pp - 1.0) * 100.0
            } else {
                0.0
            };
            let composition = format!(
                "{}/{}/{}/{}/{}/{}",
                r.row.counts[0],
                r.row.counts[1],
                r.row.counts[2],
                r.row.counts[3],
                r.row.counts[4],
                r.row.counts[5]
            );
            println!(
                "{:>8} {:>9} {:>4} {:>4} {:>4} {:>6.2} {:>6.2} {:>6} {:>26} {:>7.3} {:>8.1} {:>9.1} {:>+7.2} {:>7.4} {:>6.2} {:>5}",
                r.row.map_id,
                r.row.mods,
                r.row.keys,
                r.od,
                r.is_convert,
                r.stars,
                r.row.live_stars,
                r.notes,
                composition,
                r.row.acc,
                r.row.live_pp,
                r.current_pp,
                delta,
                r.scalar,
                r.skill,
                r.plausible
            );
        }

        // Titles are printed separately: they are far too wide for the numeric
        // table but are what makes a row identifiable to a human.
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

    // Split by whether the mod set touches the hit windows. This is the axis the
    // change acts on: EZ/HR scale the windows and so move the scalar, while
    // DT/MR/V2/NF leave them at the map's own values and can only move through
    // OD's distance from the OD-8 reference.
    println!("\nby window-affecting mod:");
    type Pred = fn(&&MultiPriced) -> bool;
    for (label, pred) in [
        (
            "EZ (windows widened)",
            (|r| r.row.mods.contains("EZ")) as Pred,
        ),
        ("HR (windows narrowed)", |r| r.row.mods.contains("HR")),
        ("no window mod", |r| {
            !r.row.mods.contains("EZ") && !r.row.mods.contains("HR")
        }),
    ] {
        let group: Vec<&MultiPriced> = all.iter().copied().filter(pred).collect();
        summarise_group(label, &group);
    }

    // OD bands for no-window-mod scores. OD itself is normalized against each
    // map's natural windows; movement here comes from the structured played units.
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
            .filter(|r| f64::from(r.od) >= lo && f64::from(r.od) < hi)
            .collect();
        if band.is_empty() {
            continue;
        }
        let n = band.len() as f64;
        let scalars: Vec<f64> = band.iter().map(|r| r.scalar).collect();
        println!(
            "  OD {lo:>4.1}-{hi:<4.1} n={:<4} mean scalar {:.4} ({:.4}..{:.4})  mean dPP {:+.2}%",
            band.len(),
            scalars.iter().sum::<f64>() / n,
            scalars.iter().copied().fold(f64::INFINITY, f64::min),
            scalars.iter().copied().fold(f64::NEG_INFINITY, f64::max),
            band.iter()
                .map(|r| (r.current_pp / r.row.live_pp - 1.0) * 100.0)
                .sum::<f64>()
                / n
        );
    }

    // The surface does not read key count directly; this groups the structural
    // populations so their LN/rice and charting differences remain visible.
    println!("\nby key count (reported as a structural cohort, not a model input):");
    for keys in [4u32, 5, 6, 7, 8, 9, 10] {
        let band: Vec<&MultiPriced> = all.iter().copied().filter(|r| r.row.keys == keys).collect();
        if band.is_empty() {
            continue;
        }
        let n = band.len() as f64;
        let mean_od = band.iter().map(|r| f64::from(r.od)).sum::<f64>() / n;
        let mean_ln = band.iter().map(|r| r.ln_fraction).sum::<f64>() / n;
        summarise_group(
            &format!("{keys}k (mean OD {mean_od:.1}, LN {:.0}%)", 100.0 * mean_ln),
            &band,
        );

        // 7k's shortfall could be its low OD or its long notes, which the key-count
        // grouping alone conflates. Splitting the band separates them: the rice-heavy
        // rows carry OD only, the LN-heavy rows carry both.
        if band.len() >= 8 {
            for (sub, lo, hi) in [("  rice <30% LN", 0.0, 0.3), ("  LN >=30%", 0.3, 1.01)] {
                let inner: Vec<&MultiPriced> = band
                    .iter()
                    .copied()
                    .filter(|r| r.ln_fraction >= lo && r.ln_fraction < hi)
                    .collect();
                if inner.len() >= 3 {
                    let mean_inner_od =
                        inner.iter().map(|r| f64::from(r.od)).sum::<f64>() / inner.len() as f64;
                    summarise_group(&format!("{sub} (mean OD {mean_inner_od:.1})"), &inner);
                }
            }
        }
    }

    // Long-note share, which is the axis the LN mixture acts on and the thing key
    // count was standing in for. Under V1 a long note is one judgement over two
    // summed offsets, so an LN-heavy map is a mixture of a narrow and a wide
    // population; fitting a single sigma to that inflates it. Grouping here
    // separates the mechanism from the 4k/7k convention above. Set
    // `SUNNY_NO_LN_SPLIT=1` to price the same rows without the split.
    println!(
        "\nby long-note share (the axis the LN mixture acts on; split {}):",
        if ln_split_disabled() {
            "DISABLED"
        } else {
            "on"
        }
    );
    for (lo, hi) in [(0.0, 0.05), (0.05, 0.3), (0.3, 0.6), (0.6, 1.01)] {
        let band: Vec<&MultiPriced> = all
            .iter()
            .copied()
            .filter(|r| r.ln_fraction >= lo && r.ln_fraction < hi)
            .collect();
        if band.is_empty() {
            continue;
        }
        let v1 = band.iter().filter(|r| r.ln_judged_as_one).count();
        summarise_group(
            &format!(
                "LN {:>3.0}-{:<3.0}% ({v1}/{} judged V1)",
                100.0 * lo,
                100.0 * hi,
                band.len()
            ),
            &band,
        );
    }

    // Surface movement is multiplicative, so its cohort center is a geometric
    // mean. Dividing by it removes a broad level shift and leaves the scores whose
    // movement differs from everybody else's, which is the useful anomaly signal.
    let mean_surface_multiplier = (all
        .iter()
        .map(|r| r.surface_multiplier.max(f64::MIN_POSITIVE).ln())
        .sum::<f64>()
        / all.len() as f64)
        .exp();
    println!(
        "\nsurface pp multiplier: geometric mean {mean_surface_multiplier:.4} ({:+.2}%)",
        (mean_surface_multiplier - 1.0) * 100.0
    );

    // Keep the surface-transfer baseline explicit for the cohorts used when
    // deciding whether to strengthen its contribution to PP. These are all
    // non-EZ so window widening cannot be mistaken for ordinary surface motion.
    println!("\nnon-EZ surface-transfer distributions:");
    println!("  {:<24} {:>5} {:>8} {:>8} {:>8} {:>8} {:>8}",
        "cohort", "n", "mean", "geo", "p10", "median", "p90");
    let print_surface_cohort = |label: &str, rows: Vec<&MultiPriced>| {
        let mut values: Vec<f64> = rows
            .into_iter()
            .filter(|r| !r.row.mods.contains("EZ"))
            .map(|r| r.surface_multiplier.max(f64::MIN_POSITIVE))
            .collect();
        if values.is_empty() {
            println!("  {label:<24} {:>5} (empty)", 0);
            return;
        }
        values.sort_by(f64::total_cmp);
        let n = values.len();
        let mean = values.iter().sum::<f64>() / n as f64;
        let geo = values.iter().map(|v| v.ln()).sum::<f64>().div_euclid(n as f64).exp();
        let quantile = |p: f64| values[((n - 1) as f64 * p).round() as usize];
        println!("  {label:<24} {n:>5} {mean:>8.4} {geo:>8.4} {:>8.4} {:>8.4} {:>8.4}",
            quantile(0.10), quantile(0.50), quantile(0.90));
    };
    print_surface_cohort("all non-EZ", all.iter().copied().collect());
    print_surface_cohort("4K rice LN<30%", all.iter().copied()
        .filter(|r| r.row.keys == 4 && r.ln_fraction < 0.30).collect());
    print_surface_cohort("4K LN>=30%", all.iter().copied()
        .filter(|r| r.row.keys == 4 && r.ln_fraction >= 0.30).collect());
    print_surface_cohort("low-OD 7K LN>30%", all.iter().copied()
        .filter(|r| r.row.keys == 7 && r.od < 6.0 && r.ln_fraction > 0.30).collect());
    print_surface_cohort("all rice LN<5%", all.iter().copied()
        .filter(|r| r.ln_fraction < 0.05).collect());

    println!("\nrice surface transfer by accuracy (non-EZ, LN<5%):");
    for (label, lo, hi) in [("<95%", 0.0, 95.0), ("95-98%", 95.0, 98.0), (">=98%", 98.0, 101.0)] {
        let rows: Vec<&MultiPriced> = all.iter().copied().filter(|r| {
            r.ln_fraction < 0.05 && !r.row.mods.contains("EZ")
                && r.row.acc >= lo && r.row.acc < hi
        }).collect();
        print_surface_cohort(label, rows);
    }

    println!("surface-transfer sensitivity (relative to each exponent's cohort mean):");
    println!(
        "  {:>8} {:>9} {:>9} {:>9} {:>9}",
        "exponent", "p10", "p90", ">+20%", "<-5%"
    );
    for exponent in [0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 2.20] {
        let center = (all
            .iter()
            .map(|r| r.scalar.max(f64::MIN_POSITIVE).ln() * exponent)
            .sum::<f64>()
            / all.len() as f64)
            .exp();
        let mut relative: Vec<f64> = all
            .iter()
            .map(|r| r.scalar.max(0.0).powf(exponent) / center)
            .collect();
        relative.sort_by(f64::total_cmp);
        let high = relative.iter().filter(|&&value| value > 1.20).count();
        let low = relative.iter().filter(|&&value| value < 0.95).count();
        println!(
            "  {exponent:>8.2} {:+8.2}% {:+8.2}% {high:>9} {low:>9}",
            (relative[relative.len() / 10] - 1.0) * 100.0,
            (relative[relative.len() * 9 / 10] - 1.0) * 100.0,
        );
    }

    let report_composition = |label: &str, mut rows: Vec<&MultiPriced>| {
        rows.sort_by(|a, b| {
            let a_delta = a.surface_multiplier / mean_surface_multiplier - 1.0;
            let b_delta = b.surface_multiplier / mean_surface_multiplier - 1.0;
            b_delta.total_cmp(&a_delta)
        });

        println!("\n{label} ({} scores; at most 40 shown)", rows.len());
        println!(
            "{:>6} {:>8} {:>9} {:>4} {:>5} {:>6} {:>8} {:>8} {:>8}  {:>8} x {:>7} x {:>6} = {:>8}  x {:>6} x {:>6} x {:>6}",
            "uid",
            "map",
            "mods",
            "od",
            "LN%",
            "acc%",
            "surface%",
            "relative%",
            "live%",
            "baseDPP",
            "accProp",
            "surf",
            "diffPP",
            "accMul",
            "var",
            "length"
        );

        for r in rows.into_iter().take(40) {
            let surface_delta = (r.current_pp / r.neutral_pp - 1.0) * 100.0;
            let relative_delta = (r.surface_multiplier / mean_surface_multiplier - 1.0) * 100.0;
            let live_delta = (r.current_pp / r.row.live_pp - 1.0) * 100.0;
            let difficulty_pp = r.base_difficulty_pp * r.accuracy_proportion * r.surface_multiplier;
            println!(
                "{:>6} {:>8} {:>9} {:>4.1} {:>5.0} {:>6.2} {:+8.2} {:+8.2} {:+8.2}  {:>8.1} x {:>7.4} x {:>6.3} = {:>8.1}  x {:>6.3} x {:>6.3} x {:>6.3}",
                r.row.uid,
                r.row.map_id,
                r.row.mods,
                r.od,
                100.0 * r.ln_fraction,
                r.row.acc,
                surface_delta,
                relative_delta,
                live_delta,
                r.base_difficulty_pp,
                r.accuracy_proportion,
                r.surface_multiplier,
                difficulty_pp,
                r.acc_multiplier,
                r.variety_multiplier,
                r.length_multiplier,
            );
        }
    };

    report_composition(
        "surface gains above 20% relative to cohort mean",
        all.iter()
            .copied()
            .filter(|r| r.surface_multiplier / mean_surface_multiplier > 1.20)
            .collect(),
    );
    report_composition(
        "non-EZ surface losses below -5% relative to cohort mean",
        all.iter()
            .copied()
            .filter(|r| {
                !r.row.mods.contains("EZ") && r.surface_multiplier / mean_surface_multiplier < 0.95
            })
            .collect(),
    );
    report_composition(
        "uid 3110: low OD or cohort-relative surface movement above 10%",
        all.iter()
            .copied()
            .filter(|r| {
                r.row.uid == "3110"
                    && (r.od < 7.0
                        || (r.surface_multiplier / mean_surface_multiplier - 1.0).abs() > 0.10)
            })
            .collect(),
    );

    // Our star rating against the live server's, which is the other half of the
    // gap between `beforePP` and the `livePP` column: the two sunny versions
    // disagree on difficulty as well as on the multiplier stack.
    let drift: Vec<f64> = all
        .iter()
        .filter(|r| r.row.live_stars > 0.0)
        .map(|r| r.stars / r.row.live_stars)
        .collect();
    if !drift.is_empty() {
        let n = drift.len() as f64;
        let mut sorted = drift.clone();
        sorted.sort_by(f64::total_cmp);
        println!(
            "\nstar rating ours/live: mean {:.4} median {:.4} range {:.3}..{:.3} (n={})",
            drift.iter().sum::<f64>() / n,
            sorted[sorted.len() / 2],
            sorted[0],
            sorted[sorted.len() - 1],
            sorted.len()
        );
        println!(
            "  (live runs a sunny predating this branch, so this is version drift in the \
                 difficulty calc, not the change under test)"
        );
    }

    let mut g: Vec<f64> = all.iter().map(|r| r.g_timing).collect();
    g.sort_by(f64::total_cmp);
    println!(
        "\nfit quality: g_timing median {:.1} p90 {:.1}, plausible {}/{}",
        g[g.len() / 2],
        g[g.len() * 9 / 10],
        all.iter().filter(|r| r.plausible).count(),
        all.len()
    );

    // Fit quality is supplemental evidence, not a pricing gate. Report the full
    // distribution and threshold sensitivity so the arbitrary `g < 30` label does
    // not get mistaken for ground truth, then split it along axes the model actually
    // sees. This is specifically diagnostic: none of these cohorts feed pp.
    let report_fit = |label: &str, rows: &[&MultiPriced]| {
        if rows.is_empty() {
            return;
        }

        let mut values: Vec<f64> = rows
            .iter()
            .map(|row| row.g_timing)
            .filter(|value| value.is_finite())
            .collect();
        values.sort_by(f64::total_cmp);

        if values.is_empty() {
            return;
        }

        let below = |threshold: f64| values.partition_point(|value| *value < threshold);
        println!(
            "  {label:<22} n={:<4} p50 {:>7.1} p75 {:>7.1} p90 {:>7.1}  \
                 g<15 {:>4}  g<30 {:>4}  g<60 {:>4}",
            values.len(),
            values[values.len() / 2],
            values[values.len() * 3 / 4],
            values[values.len() * 9 / 10],
            below(15.0),
            below(30.0),
            below(60.0),
        );
    };

    println!("\nfit-quality diagnostics (supplemental; lower g_timing is better):");
    report_fit("all", &all);

    println!("  by score/window mode:");
    for (label, pred) in [
        ("EZ", (|r: &&MultiPriced| r.row.mods.contains("EZ")) as Pred),
        ("HR", |r: &&MultiPriced| r.row.mods.contains("HR")),
        ("stable V2", |r: &&MultiPriced| r.row.mods.contains("V2")),
        ("stable V1", |r: &&MultiPriced| !r.row.mods.contains("V2")),
        ("DT/NC", |r: &&MultiPriced| {
            r.row.mods.contains("DT") || r.row.mods.contains("NC")
        }),
        ("NM/rate 1", |r: &&MultiPriced| {
            !r.row.mods.contains("DT")
                && !r.row.mods.contains("NC")
                && !r.row.mods.contains("HT")
                && !r.row.mods.contains("EZ")
                && !r.row.mods.contains("HR")
        }),
    ] {
        report_fit(label, &all.iter().copied().filter(pred).collect::<Vec<_>>());
    }

    println!("  by structural and performance bands:");
    for keys in [4_u32, 6, 7] {
        report_fit(
            &format!("{keys}K"),
            &all.iter()
                .copied()
                .filter(|row| row.row.keys == keys)
                .collect::<Vec<_>>(),
        );
    }
    for (label, lo, hi) in [
        ("OD <7", 0.0, 7.0),
        ("OD 7-8", 7.0, 8.0),
        ("OD >=8", 8.0, f64::INFINITY),
    ] {
        report_fit(
            label,
            &all.iter()
                .copied()
                .filter(|row| f64::from(row.od) >= lo && f64::from(row.od) < hi)
                .collect::<Vec<_>>(),
        );
    }
    for (label, lo, hi) in [
        ("LN <5%", 0.0, 0.05),
        ("LN 5-30%", 0.05, 0.30),
        ("LN 30-60%", 0.30, 0.60),
        ("LN >=60%", 0.60, 1.01),
        ("acc <95%", 0.0, 95.0),
        ("acc 95-98%", 95.0, 98.0),
        ("acc >=98%", 98.0, f64::INFINITY),
        ("skill <7", 0.0, 7.0),
        ("skill 7-9", 7.0, 9.0),
        ("skill >=9", 9.0, f64::INFINITY),
    ] {
        report_fit(
            label,
            &all.iter()
                .copied()
                .filter(|row| {
                    let value = if label.starts_with("LN") {
                        row.ln_fraction
                    } else if label.starts_with("acc") {
                        row.row.acc
                    } else {
                        row.skill
                    };
                    value >= lo && value < hi
                })
                .collect::<Vec<_>>(),
        );
    }

    let mut worst = all.clone();
    worst.sort_by(|a, b| b.g_timing.total_cmp(&a.g_timing));
    println!("\nworst 20 timing-shape fits (diagnostic only):");
    println!(
        "  {:>6} {:>8} {:>9} {:>2} {:>4} {:>5} {:>6} {:>6} {:>8}",
        "uid", "map", "mods", "k", "od", "LN%", "acc%", "skill", "g_timing"
    );
    for row in worst.into_iter().take(20) {
        println!(
            "  {:>6} {:>8} {:>9} {:>2} {:>4.1} {:>5.0} {:>6.2} {:>6.2} {:>8.1}",
            row.row.uid,
            row.row.map_id,
            if row.row.mods.is_empty() {
                "NM"
            } else {
                &row.row.mods
            },
            row.row.keys,
            row.od,
            row.ln_fraction * 100.0,
            row.row.acc,
            row.skill,
            row.g_timing,
        );
    }
}

/// Reports how [`ErrorModel::release_mean_offset`] moves pricing under the fixed
/// OD 8 reference, using the same `local-fixtures/multiuser.tsv` fixtures and
/// [`load_multiuser`] harness `multiuser_report` uses.
///
/// Per score: map id, keymode, OD, LN share, live pp, our pp, `window_scalar`,
/// `g_timing` (played side), `g_timing` (reference side), played skill, reference
/// skill.
///
/// Aggregated several ways, all as counts and medians (plus `g_timing` p90):
/// - our-pp/live-pp by keymode (4K vs 7K) and by LN-share bucket (`[0, 0-30%,
///   30-60%, >60%]`)
/// - `g_timing`, played-side and reference-side, by the same two cuts —
///   [`Self::g_timing`]'s doc comment on why the reference side is not a claim
///   about the player: it grades the same observed counts against windows the
///   player never played under
/// - played skill, reference skill, and `window_scalar` for two specific cohorts
///   (the low-OD 7K/LN-heavy target and the 4K/rice control), to separate what the
///   ratio shows from what each fit shows on its own
///
/// `livePP` is itself computed by a live sunny build (`2c2e8a1`-adjacent, not
/// today's `main`), so an our-pp/live-pp ratio close to 1 does not mean the offset
/// is doing nothing — it can equally mean live pricing has drifted to agree with a
/// no-op change. `g_timing` is the only figure here that is not contaminated by
/// that: it is a property of the fit against the *observed judgement counts*
/// alone and does not reference live pp at all. This report does **not** compute
/// the pre-change (`2c2e8a1`) baseline column — only `multiuser_report`
/// does that, and it was not added here; treat any comparison against that
/// baseline as absent, not as implicitly agreeing with it.
///
/// This is the measurement, not the change: [`ErrorModel::release_mean_offset`]'s
/// default has to be edited and the crate rebuilt to see it move — this test only
/// reports whatever value is currently compiled in. Compare runs at 0.0, 4.0, 8.0,
/// 16.0.
///
/// Run with `cargo test --release ln_offset_under_the_fixed_reference -- --ignored
/// --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn ln_offset_under_the_fixed_reference() {
    let scores = load_multiuser();
    if scores.is_empty() {
        println!("no fixtures present (local-fixtures/multiuser.tsv); nothing to report");
        return;
    }

    println!(
        "release_mean_offset = {:.1}",
        ErrorModel::default().release_mean_offset
    );

    println!(
        "\n{:>8} {:>4} {:>5} {:>6} {:>9} {:>9} {:>7} {:>9} {:>9} {:>8} {:>9}",
        "map",
        "keys",
        "od",
        "ln%",
        "livePP",
        "ourPP",
        "scalar",
        "g_played",
        "g_ref",
        "skill",
        "ref_skill"
    );

    let with_live: Vec<&MultiPriced> = scores.iter().filter(|r| r.row.live_pp > 0.0).collect();

    for r in &with_live {
        println!(
            "{:>8} {:>4} {:>5.1} {:>6.1} {:>9.1} {:>9.1} {:>7.4} {:>9.2} {:>9.2} {:>8.3} {:>9.3}",
            r.row.map_id,
            r.row.keys,
            r.od,
            r.ln_fraction * 100.0,
            r.row.live_pp,
            r.current_pp,
            r.scalar,
            r.g_timing,
            r.reference_g_timing,
            r.skill,
            r.reference_skill
        );
    }

    fn median(values: &mut [f64]) -> f64 {
        values.sort_by(f64::total_cmp);
        values[values.len() / 2]
    }

    fn p90(values: &mut [f64]) -> f64 {
        values.sort_by(f64::total_cmp);
        values[values.len() * 9 / 10]
    }

    fn report_ratio(label: &str, rows: &[&MultiPriced]) {
        if rows.is_empty() {
            println!("  {label}: n=0");
            return;
        }
        let mut ratios: Vec<f64> = rows.iter().map(|r| r.current_pp / r.row.live_pp).collect();
        println!(
            "  {label}: n={:<4} median ourPP/livePP {:.4}",
            rows.len(),
            median(&mut ratios)
        );
    }

    fn report_g_timing(label: &str, rows: &[&MultiPriced]) {
        if rows.is_empty() {
            println!("  {label}: n=0");
            return;
        }
        let mut played: Vec<f64> = rows.iter().map(|r| r.g_timing).collect();
        let mut reference: Vec<f64> = rows.iter().map(|r| r.reference_g_timing).collect();
        println!(
            "  {label}: n={:<4} played g_timing median {:>8.2} p90 {:>8.2}  \
                 reference g_timing median {:>8.2} p90 {:>8.2}",
            rows.len(),
            median(&mut played.clone()),
            p90(&mut played),
            median(&mut reference.clone()),
            p90(&mut reference)
        );
    }

    fn report_skills(label: &str, rows: &[&MultiPriced]) {
        if rows.is_empty() {
            println!("  {label}: n=0");
            return;
        }
        let mut played: Vec<f64> = rows.iter().map(|r| r.skill).collect();
        let mut reference: Vec<f64> = rows.iter().map(|r| r.reference_skill).collect();
        let mut scalar: Vec<f64> = rows.iter().map(|r| r.scalar).collect();
        println!(
            "  {label}: n={:<4} median played_skill {:>7.3}  median reference_skill {:>7.3}  \
                 median window_scalar {:>7.4}",
            rows.len(),
            median(&mut played),
            median(&mut reference),
            median(&mut scalar)
        );
    }

    println!("\nby keymode (ourPP/livePP):");
    for keys in [4u32, 7] {
        let band: Vec<&MultiPriced> = with_live
            .iter()
            .copied()
            .filter(|r| r.row.keys == keys)
            .collect();
        report_ratio(&format!("{keys}K"), &band);
    }

    println!("\nby keymode (g_timing, played vs reference):");
    for keys in [4u32, 7] {
        let band: Vec<&MultiPriced> = with_live
            .iter()
            .copied()
            .filter(|r| r.row.keys == keys)
            .collect();
        report_g_timing(&format!("{keys}K"), &band);
    }

    println!("\nby LN share (ourPP/livePP):");
    for (label, lo, hi) in [
        ("0%", 0.0, 0.0),
        ("0-30%", 0.0, 0.3),
        ("30-60%", 0.3, 0.6),
        (">60%", 0.6, 1.01),
    ] {
        let band: Vec<&MultiPriced> = with_live
            .iter()
            .copied()
            .filter(|r| {
                if lo == hi {
                    r.ln_fraction <= 0.0
                } else {
                    r.ln_fraction > lo && r.ln_fraction < hi
                }
            })
            .collect();
        report_ratio(label, &band);
    }

    println!("\nby LN share (g_timing, played vs reference):");
    for (label, lo, hi) in [
        ("0%", 0.0, 0.0),
        ("0-30%", 0.0, 0.3),
        ("30-60%", 0.3, 0.6),
        (">60%", 0.6, 1.01),
    ] {
        let band: Vec<&MultiPriced> = with_live
            .iter()
            .copied()
            .filter(|r| {
                if lo == hi {
                    r.ln_fraction <= 0.0
                } else {
                    r.ln_fraction > lo && r.ln_fraction < hi
                }
            })
            .collect();
        report_g_timing(label, &band);
    }

    // The cohort the offset is meant to move: low-OD 7K, LN-heavy.
    println!("\nlow-OD 7K, LN-heavy (the cohort the offset targets):");
    let low_od_7k_ln: Vec<&MultiPriced> = with_live
        .iter()
        .copied()
        .filter(|r| r.row.keys == 7 && f64::from(r.od) < 6.0 && r.ln_fraction > 0.3)
        .collect();
    report_ratio("OD<6 7K LN>30% (ratio)", &low_od_7k_ln);
    report_g_timing("OD<6 7K LN>30% (g_timing)", &low_od_7k_ln);
    report_skills("OD<6 7K LN>30% (skills+scalar)", &low_od_7k_ln);

    // Rice control: the offset only reaches long-note units, so this cohort must
    // not move at all when the offset changes between runs.
    println!("\n4K rice control (must not move when the offset changes):");
    let rice_4k: Vec<&MultiPriced> = with_live
        .iter()
        .copied()
        .filter(|r| r.row.keys == 4 && r.ln_fraction < 0.05)
        .collect();
    report_ratio("4K LN<5% (ratio)", &rice_4k);
    report_g_timing("4K LN<5% (g_timing)", &rice_4k);
    report_skills("4K LN<5% (skills+scalar)", &rice_4k);

    // Star rating must never move with this parameter — the surface does not feed
    // difficulty. Printed as a hash-free direct dump so a diff across offset runs
    // catches any drift at all, not just drift big enough to change a rounded
    // display value.
    println!("\nstars (must be byte-identical across every release_mean_offset run):");
    let mut by_map: Vec<(&str, f64)> = scores
        .iter()
        .map(|r| (r.row.map_id.as_str(), r.stars))
        .collect();
    by_map.sort_by(|a, b| a.0.cmp(b.0).then(a.1.total_cmp(&b.1)));
    by_map.dedup();
    for (map_id, stars) in &by_map {
        println!("  {map_id:>8}  {stars:.17}");
    }
}

/// Sweeps [`ErrorModel::release_sigma_ratio`] and reports fit quality by long-note
/// share, testing whether a release is harder to place than a press.
///
/// The question this settles: `sqrt(2)` assumes a release lands as precisely as a
/// press, and players say it does not. The sweep prices the same fixtures at
/// ratios from 1.0 (no asymmetry) upward and watches median `g_timing` on the
/// LN-heavy bands, where the parameter is the only thing moving.
///
/// Read the LN 0-5% row as the control: the split cannot touch those maps, so any
/// movement there would mean the sweep is leaking into rice scores and the
/// mechanism is not what it claims to be.
///
/// Unlike `sigma_floor_sweep`, which found its parameter unidentifiable because
/// skill absorbs it exactly, this one changes the *ratio* between two populations
/// inside a single map, which skill cannot reproduce. So it should be visible here
/// or nowhere.
///
/// Run with `cargo test --release ln_release_ratio_sweep -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn ln_release_ratio_sweep() {
    use crate::mania::sunny_accuracy::ln_sigma_scale;

    let cases = load_ln_cases();

    if cases.is_empty() {
        println!("no fixtures present (local-fixtures/multiuser.tsv); nothing to sweep");
        return;
    }

    // The first band is exclusive of zero on purpose. A "0-5% LN" band is *not* a
    // control: only 24 of the 88 fixtures under 5% have no long notes at all, and
    // the other 64 have a handful, so the ratio does reach them and the band moves.
    // The true control is `n_long_notes == 0` (plus every V2 score), reported
    // separately below.
    let bands: [(f64, f64); 4] = [(1e-9, 0.05), (0.05, 0.3), (0.3, 0.6), (0.6, 1.01)];

    println!(
        "{} cases, {} with a V1 long-note population",
        cases.len(),
        cases.iter().filter(|c| c.has_ln_effect()).count()
    );
    print!("{:>6} {:>7}  {:>13}", "ratio", "scale", "CONTROL");
    for (lo, hi) in bands {
        print!(
            "  {:>13}",
            format!("LN{:.0}-{:.0}%", 100.0 * lo, 100.0 * hi)
        );
    }
    println!("  {:>13}  {:>9}", "all V1+LN", "plaus");
    println!("{}", "-".repeat(6 + 7 + 4 * 15 + 15 + 11));

    for ratio in [1.0, 1.1, 1.2, 1.35, 1.5, 1.75, 2.0, 2.5, 3.0] {
        let model = ErrorModel {
            release_sigma_ratio: ratio,
            short_hold_penalty: 0.0,
            ..Default::default()
        };

        // Median g_timing over a subset, refitting each case under `model`.
        let median_g = |subset: &[&LnCase]| -> Option<f64> {
            let mut gs: Vec<f64> = subset
                .iter()
                .map(|c| {
                    let total: u32 = c.counts.iter().sum();
                    let units = ln_units_for(c, f64::from(total), &model);
                    fit_with_quality(&c.counts, &units, &c.windows, &model).g_timing
                })
                .collect();

            if gs.is_empty() {
                return None;
            }

            gs.sort_by(f64::total_cmp);
            Some(gs[gs.len() / 2])
        };

        print!("{ratio:>6.2} {:>7.3}", ln_sigma_scale(ratio));

        // The genuine control: cases the LN split cannot reach at all, either
        // because the map has no long notes or because V2 judged them separately.
        // This column must be constant to the digit, or the parameter is doing
        // something other than what it claims.
        let control: Vec<&LnCase> = cases.iter().filter(|c| !c.has_ln_effect()).collect();
        match median_g(&control) {
            Some(g) => print!("  {:>13}", format!("{g:.3} (n={})", control.len())),
            None => print!("  {:>13}", "-"),
        }

        for (lo, hi) in bands {
            let band: Vec<&LnCase> = cases
                .iter()
                .filter(|c| c.has_ln_effect() && c.ln_fraction() >= lo && c.ln_fraction() < hi)
                .collect();

            match median_g(&band) {
                Some(g) => print!("  {:>13}", format!("{g:.1} (n={})", band.len())),
                None => print!("  {:>13}", "-"),
            }
        }

        // Only the cases the parameter can reach, which is the figure to minimise.
        let affected: Vec<&LnCase> = cases.iter().filter(|c| c.has_ln_effect()).collect();
        let plausible = affected
            .iter()
            .filter(|c| {
                let total: u32 = c.counts.iter().sum();
                let units = ln_units_for(c, f64::from(total), &model);
                fit_with_quality(&c.counts, &units, &c.windows, &model).is_plausible()
            })
            .count();

        match median_g(&affected) {
            Some(g) => print!("  {:>13}", format!("{g:.1} (n={})", affected.len())),
            None => print!("  {:>13}", "-"),
        }
        println!("  {:>9}", format!("{plausible}/{}", affected.len()));
    }

    println!(
        "\nCONTROL is the cases the split cannot reach (no long notes, or V2 judging); \
             it must be constant."
    );
    println!(
        "scale is sqrt(1 + ratio^2), the widening a V1 long note gets; ratio 1.00 is the \
             derived sqrt(2)."
    );

    // ---------------------------------------------------------------
    // Phase two: does making the ratio depend on hold duration help?
    // ---------------------------------------------------------------
    //
    // Phase one wanted two different ratios on two different LN populations, which
    // one number cannot supply. The hypothesis is that duration is the missing axis:
    // a short hold gives the player no time to reset before the release is due, so
    // its release should be wider than a long hold's. If that is right, a nonzero
    // penalty should beat every flat ratio above.
    println!("\n=== short-hold surcharge (penalty x decay scale) ===");
    println!("ratio(t) = release_ratio * (1 + penalty * exp(-t / scale)); penalty 0 is phase one");

    let bands_by_median: [(&str, f64, f64); 3] = [
        ("short", 0.0, 90.0),
        ("mid", 90.0, 160.0),
        ("long", 160.0, 1e9),
    ];

    print!("{:>7} {:>7} {:>6}", "penalty", "scale", "base");
    for (label, _, _) in bands_by_median {
        print!("  {:>13}", format!("medLN {label}"));
    }
    println!("  {:>13}  {:>9}", "all V1+LN", "plaus");

    for &(penalty, scale) in &[
        (0.0, 120.0),
        (0.4, 120.0),
        (0.8, 120.0),
        (0.8, 250.0),
        (1.5, 120.0),
        (1.5, 250.0),
        (2.5, 150.0),
        (4.0, 150.0),
    ] {
        for base in [1.0, 1.5] {
            let model = ErrorModel {
                release_sigma_ratio: base,
                short_hold_penalty: penalty,
                short_hold_scale: scale,
                ..Default::default()
            };

            let median_g = |subset: &[&LnCase]| -> Option<f64> {
                let mut gs: Vec<f64> = subset
                    .iter()
                    .map(|c| {
                        let total: u32 = c.counts.iter().sum();
                        let units = ln_units_for(c, f64::from(total), &model);
                        fit_with_quality(&c.counts, &units, &c.windows, &model).g_timing
                    })
                    .collect();
                if gs.is_empty() {
                    return None;
                }
                gs.sort_by(f64::total_cmp);
                Some(gs[gs.len() / 2])
            };

            print!("{penalty:>7.2} {scale:>7.0} {base:>6.2}");

            // Grouped by the map's *median* hold length, since that is the quantity
            // the surcharge keys off — unlike LN share, which says nothing about
            // whether the holds are taps or half-second presses.
            for (_, lo, hi) in bands_by_median {
                let band: Vec<&LnCase> = cases
                    .iter()
                    .filter(|c| {
                        if !c.has_ln_effect() {
                            return false;
                        }
                        let mut d = c.ln_durations.clone();
                        if d.is_empty() {
                            return false;
                        }
                        d.sort_by(f64::total_cmp);
                        let median = d[d.len() / 2];
                        median >= lo && median < hi
                    })
                    .collect();

                match median_g(&band) {
                    Some(g) => print!("  {:>13}", format!("{g:.1} (n={})", band.len())),
                    None => print!("  {:>13}", "-"),
                }
            }

            let affected: Vec<&LnCase> = cases.iter().filter(|c| c.has_ln_effect()).collect();
            let plausible = affected
                .iter()
                .filter(|c| {
                    let total: u32 = c.counts.iter().sum();
                    let units = ln_units_for(c, f64::from(total), &model);
                    fit_with_quality(&c.counts, &units, &c.windows, &model).is_plausible()
                })
                .count();

            match median_g(&affected) {
                Some(g) => print!("  {:>13}", format!("{g:.1} (n={})", affected.len())),
                None => print!("  {:>13}", "-"),
            }
            println!("  {:>9}", format!("{plausible}/{}", affected.len()));
        }
    }
}

/// `is_classic` must actually see the ScoreV2 mod.
///
/// Regression test for a silent failure: [`has_mod`] *parses* the acronym string, so
/// a wrong one is not a compile error and not a panic — it simply never matches. The
/// code asked for `"V2"` where `rosu_mods::ScoreV2Mania` reports `"SV2"`, so every
/// score was classified as ScoreV1. That was harmless while V2 only changed the score
/// number, and became a real bug the moment long notes were judged differently under
/// the two schemes.
///
/// Asserts against the mod's own acronym rather than a literal, so this cannot drift
/// with the mod crate.
/// Whether the surface infers *less skill* from the same player on long-note charts
/// — the "爆黄" complaint, stated in the only form the model can be wrong about.
///
/// 爆黄 is 320 -> 300: on LN-heavy charts players cannot convert PERFECTs no matter
/// how well they play, and the surplus lands in the yellow 300. This never appears as
/// a *residual*, because skill is free per score and the fit simply answers a lower
/// number — a 320/300 ratio the model finds surprising becomes "this player is worse",
/// not "this score fits badly". So goodness of fit cannot see the complaint at all,
/// and every `g_timing` figure in the other harnesses is silent about it.
///
/// What it does do is move pricing. If a player's fitted skill falls as LN share
/// rises, the surface is charging them for a structural property of the chart, which
/// is exactly what the LN widening exists to undo. The question is whether it undoes
/// enough of it, and the per-player slope answers that: within one player, true skill
/// is roughly constant across their top plays, so any systematic trend against LN
/// share is the model's, not the player's.
///
/// Confounded in one direction worth stating: a player genuinely weaker at LN will
/// show a real negative slope too, and this cannot separate that from a modelling
/// artefact. What it *can* do is show whether the LN split moves the slope toward
/// zero, which is the thing under our control. Run it with and without
/// `SUNNY_NO_LN_SPLIT=1` to see that.
///
/// Run with `cargo test --release ln_skill_slope -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn ln_skill_slope() {
    use std::collections::BTreeMap;

    let Ok(text) = std::fs::read_to_string("local-fixtures/multiuser.tsv") else {
        println!("no fixtures present; nothing to report");
        return;
    };

    struct Point {
        uid: String,
        ln_share: f64,
        median_hold: f64,
        /// Skill as a multiple of the map's difficulty, which is the scale-free form.
        /// Comparing raw skill across maps of different star rating would mostly
        /// measure which maps the player chose.
        skill_ratio: f64,
        perfect_share: f64,
    }

    let model = ErrorModel::default();
    let mut points = Vec::new();

    for line in text.lines() {
        let f: Vec<&str> = line.split('\t').collect();
        if f.len() < 18 || f[0] == "uid" {
            continue;
        }

        let u = |s: &str| s.parse::<u32>().unwrap_or(0);
        let counts = [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])];
        let total: u32 = counts.iter().sum();

        let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", f[2])) else {
            continue;
        };

        let (mods, clock_rate) = mods_for(f[3]);
        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(false), None) else {
            continue;
        };

        // Only V1 scores: under V2 the head and release are separate judgements and
        // the mechanism does not apply, so mixing them in would dilute the slope.
        if !attrs.ln_judged_as_one || total == 0 || attrs.stars <= 0.0 {
            continue;
        }

        let units = judgement_units(
            &attrs,
            f64::from(total),
            &model,
            !per_note_difficulty_disabled(),
        );
        let fit = fit_with_quality(&counts, &units, &attrs.hit_windows, &model);

        let total_columns = map.cs.round_ties_even().max(1.0) as usize;
        let (notes, _) = build_notes(clock_rate, map.hit_objects.iter(), total_columns);
        let mut holds: Vec<f64> = notes
            .iter()
            .filter_map(|n| {
                let d = n.tail_or_head() - n.head;
                (d > 0.0).then_some(d)
            })
            .collect();
        holds.sort_by(f64::total_cmp);

        let timing: f64 = counts[..5].iter().map(|&c| f64::from(c)).sum();

        points.push(Point {
            uid: f[0].to_owned(),
            ln_share: if attrs.n_objects > 0 {
                attrs.n_long_notes as f64 / attrs.n_objects as f64
            } else {
                0.0
            },
            median_hold: if holds.is_empty() {
                0.0
            } else {
                holds[holds.len() / 2]
            },
            skill_ratio: fit.skill / attrs.stars,
            perfect_share: if timing > 0.0 {
                f64::from(counts[0]) / timing
            } else {
                0.0
            },
        });
    }

    if points.is_empty() {
        println!("no V1 scores in the fixture set; nothing to report");
        return;
    }

    println!(
        "LN split is {}. {} V1 scores.",
        if ln_split_disabled() {
            "DISABLED"
        } else {
            "on"
        },
        points.len()
    );
    println!(
        "\n320 share and fitted skill/stars against LN share, per player.\n\
             A negative skill/stars trend means the surface reads an LN chart as the \
             player being worse."
    );

    let mut by_uid: BTreeMap<&str, Vec<&Point>> = BTreeMap::new();
    for point in &points {
        by_uid.entry(point.uid.as_str()).or_default().push(point);
    }

    for (uid, rows) in &by_uid {
        println!("\n=== uid {uid} ({} V1 scores)", rows.len());
        println!(
            "{:>14} {:>5}  {:>13}  {:>13}  {:>13}",
            "LN share", "n", "320 share", "skill/stars", "med hold ms"
        );

        for (lo, hi) in [(0.0, 0.05), (0.05, 0.4), (0.4, 0.75), (0.75, 1.01)] {
            let band: Vec<&&Point> = rows
                .iter()
                .filter(|p| p.ln_share >= lo && p.ln_share < hi)
                .collect();
            if band.is_empty() {
                continue;
            }
            let n = band.len() as f64;
            let mean = |get: &dyn Fn(&Point) -> f64| -> f64 {
                band.iter().map(|p| get(p)).sum::<f64>() / n
            };
            println!(
                "{:>13}% {:>5}  {:>13.4}  {:>13.4}  {:>13.0}",
                format!("{:.0}-{:.0}", 100.0 * lo, 100.0 * hi),
                band.len(),
                mean(&|p| p.perfect_share),
                mean(&|p| p.skill_ratio),
                mean(&|p| p.median_hold),
            );
        }

        // Least-squares slope of skill/stars on LN share, within this player. The
        // sign is the whole point; the magnitude says how much pp is at stake.
        let n = rows.len() as f64;
        let mean_x = rows.iter().map(|p| p.ln_share).sum::<f64>() / n;
        let mean_y = rows.iter().map(|p| p.skill_ratio).sum::<f64>() / n;
        let covariance: f64 = rows
            .iter()
            .map(|p| (p.ln_share - mean_x) * (p.skill_ratio - mean_y))
            .sum();
        let variance: f64 = rows.iter().map(|p| (p.ln_share - mean_x).powi(2)).sum();

        if variance > 1e-9 {
            let slope = covariance / variance;
            println!(
                "  slope d(skill/stars)/d(LN share) = {slope:+.4}  \
                     (mean skill/stars {mean_y:.4}, so {:+.1}% across the full LN range)",
                100.0 * slope / mean_y
            );
        }
    }
}

/// Where the residual misfit on short-hold maps actually lives, judgement by
/// judgement.
///
/// The surcharge sweep says short-hold maps fit worst (median `g_timing` ~40 against
/// ~25 for long-hold maps) but that widening their sigma does not help. That is only
/// consistent with the *shape* being wrong rather than the width, so this prints
/// observed against predicted shares per judgement to see which band the model misses.
///
/// A width error and a shape error look different here: too narrow a sigma
/// underpredicts every band below 320 together, while a shape error misses one band
/// in one direction and another in the other, which no single sigma can fix.
///
/// Run with `cargo test --release ln_shape_residuals -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn ln_shape_residuals() {
    use crate::mania::sunny_accuracy::expected_counts;

    let cases = load_ln_cases();

    if cases.is_empty() {
        println!("no fixtures present; nothing to report");
        return;
    }

    let model = ErrorModel::default();
    let bands: [(&str, f64, f64); 3] = [
        ("short <90ms", 0.0, 90.0),
        ("mid 90-160", 90.0, 160.0),
        ("long >160", 160.0, 1e9),
    ];

    println!("observed / predicted judgement shares, LN maps grouped by median hold length");
    println!(
        "{:>12} {:>5}  {:>15} {:>15} {:>15} {:>15} {:>15}",
        "group", "n", "320", "300", "200", "100", "50"
    );

    for (label, lo, hi) in bands {
        let group: Vec<&LnCase> = cases
            .iter()
            .filter(|c| {
                if !c.has_ln_effect() || c.ln_durations.is_empty() {
                    return false;
                }
                let mut d = c.ln_durations.clone();
                d.sort_by(f64::total_cmp);
                let median = d[d.len() / 2];
                median >= lo && median < hi
            })
            .collect();

        if group.is_empty() {
            continue;
        }

        // Pooled over the group, conditioned on the note having been hit — the same
        // conditioning the fit uses, so the comparison is against what was fitted.
        let mut observed = [0.0; 5];
        let mut predicted = [0.0; 5];

        for case in &group {
            let total: u32 = case.counts.iter().sum();
            let units = ln_units_for(case, f64::from(total), &model);
            let fit = fit_with_quality(&case.counts, &units, &case.windows, &model);
            let expected = expected_counts(&units, &case.windows, &model, fit.skill);

            let obs_timing: f64 = case.counts[..5].iter().map(|&c| f64::from(c)).sum();
            let exp_array = expected.as_array();
            let exp_timing: f64 = exp_array[..5].iter().sum();

            if obs_timing <= 0.0 || exp_timing <= 0.0 {
                continue;
            }

            for judgement in 0..5 {
                observed[judgement] += f64::from(case.counts[judgement]) / obs_timing;
                predicted[judgement] += exp_array[judgement] / exp_timing;
            }
        }

        let n = group.len() as f64;
        print!("{label:>12} {:>5}", group.len());
        for judgement in 0..5 {
            print!(
                "  {:>15}",
                format!(
                    "{:.3}/{:.3}",
                    observed[judgement] / n,
                    predicted[judgement] / n
                )
            );
        }
        println!();
    }

    println!(
        "\nA pure width error misses every sub-320 band the same way; a shape error \
             misses them in opposite directions."
    );
}

/// What the duration binning costs against evaluating every long note at its own
/// duration.
///
/// The bins are a quadrature grid over a continuous function, so the question is not
/// whether they are "correct" but whether the discretisation error is small next to
/// the effect being measured. Asserts rather than prints, because a silent drift here
/// would invalidate every figure the sweep produces.
///
/// Deliberately run at a *large* surcharge, where the function varies most across a
/// bin and the approximation is at its worst. If it holds there it holds everywhere
/// milder.
#[test]
#[ignore = "reads gitignored fixtures"]
fn ln_binning_error_stays_small() {
    let cases = load_ln_cases();

    if cases.is_empty() {
        println!("no fixtures present; nothing to check");
        return;
    }

    let model = ErrorModel {
        release_sigma_ratio: 1.5,
        short_hold_penalty: 2.5,
        short_hold_scale: 150.0,
        ..Default::default()
    };

    let mut worst_skill = 0.0_f64;
    let mut worst_g = 0.0_f64;
    let mut errors: Vec<(f64, usize, f64, f64)> = Vec::new();

    for case in cases.iter().filter(|c| c.has_ln_effect()) {
        let total: u32 = case.counts.iter().sum();
        if total == 0 {
            continue;
        }

        let binned = ln_units_for(case, f64::from(total), &model);
        let exact = ln_units_exact(case, f64::from(total), &model);

        let a = fit_with_quality(&case.counts, &binned, &case.windows, &model);
        let b = fit_with_quality(&case.counts, &exact, &case.windows, &model);

        let error = if b.skill > 0.0 {
            (a.skill / b.skill - 1.0).abs()
        } else {
            0.0
        };

        worst_skill = worst_skill.max(error);
        worst_g = worst_g.max((a.g_timing - b.g_timing).abs());
        errors.push((error, case.n_long_notes, case.ln_fraction(), b.skill));
    }

    let checked = errors.len();
    errors.sort_by(|a, b| b.0.total_cmp(&a.0));

    println!(
        "binning vs exact over {checked} cases: worst skill error {:.3}%, worst g_timing \
             difference {worst_g:.3}",
        100.0 * worst_skill
    );

    // Where the error concentrates matters more than its maximum: a few pathological
    // maps are a different problem from a systematically biased grid.
    let median = errors[checked / 2].0;
    let p90 = errors[checked / 10].0;
    println!(
        "  distribution: median {:.3}%, p90 {:.3}%, over-2% {} of {checked}",
        100.0 * median,
        100.0 * p90,
        errors.iter().filter(|e| e.0 > 0.02).count()
    );
    println!("  worst offenders (error%, nLN, LNshare, skill):");
    for (error, n_ln, share, skill) in errors.iter().take(5) {
        println!(
            "    {:.3}%  nLN={n_ln:<6} share={:.2}  skill={skill:.2}",
            100.0 * error,
            share
        );
    }

    // The typical case is what the grid has to get right, and it does: the median
    // error is ~0.04%, three orders of magnitude under the effect being measured.
    assert!(
        median < 0.005,
        "duration binning must not shift the typical fit at all: median error {:.3}%",
        100.0 * median
    );

    // The tail is bounded but not tiny, and it is bounded for a reason worth stating.
    // Every case above 2% is an LN-saturated map fitted at skill 15-21, i.e. near the
    // saturation ceiling where the likelihood is flat and `skill` is already a lower
    // bound rather than a measurement (see `SKILL_SATURATION_RATIO`). A flat
    // likelihood is exactly where a small change in expected counts moves the argmax
    // a long way, so this is the fit being insensitive, not the grid being wrong.
    // Refining the bins does not help — going from 5 to 8 bins cut the per-bin
    // variation from 32% to under 10% and moved this figure only 4.5% to 3.5%.
    assert!(
        worst_skill < 0.05,
        "duration binning must not shift any fit by more than 5%: got {:.3}%",
        100.0 * worst_skill
    );
    assert!(
        p90 < 0.02,
        "at most a tenth of cases may exceed 2%: p90 is {:.3}%",
        100.0 * p90
    );
}

/// The judgement units for one [`LnCase`], mirroring [`judgement_units`] but
/// driven by a case rather than by live attributes.
fn ln_units_for(case: &LnCase, total: f64, model: &ErrorModel) -> Vec<JudgementUnit> {
    if !case.has_ln_effect() || case.n_objects == 0 {
        return vec![JudgementUnit::repeated(case.stars, total)];
    }

    let per_object = total / case.n_objects as f64;
    let mut units = Vec::with_capacity(LN_DURATION_BUCKETS + 1);
    let mut ln_total = 0.0;

    for (bin, &count) in case.ln_duration_buckets.iter().enumerate() {
        if count == 0 {
            continue;
        }
        let weight = count as f64 * per_object;
        ln_total += weight;
        units.push(JudgementUnit::long_note(
            case.stars,
            weight,
            model,
            LN_DURATION_REPRESENTATIVES[bin],
        ));
    }

    let rice = (total - ln_total).max(0.0);
    if rice > 0.0 {
        units.push(JudgementUnit::repeated(case.stars, rice));
    }
    units
}

/// The exact per-note units for one [`LnCase`]: every long note at its own
/// duration, with no binning.
///
/// The reference the binned approximation is checked against. Too slow to fit with
/// in production — a 5000-note map becomes 5000 units and every likelihood
/// evaluation walks all of them — which is why the shipped path bins.
fn ln_units_exact(case: &LnCase, total: f64, model: &ErrorModel) -> Vec<JudgementUnit> {
    if !case.has_ln_effect() || case.n_objects == 0 {
        return vec![JudgementUnit::repeated(case.stars, total)];
    }

    let per_object = total / case.n_objects as f64;
    let mut units = Vec::with_capacity(case.ln_durations.len() + 1);

    for &duration in &case.ln_durations {
        units.push(JudgementUnit::long_note(
            case.stars, per_object, model, duration,
        ));
    }

    let rice = (total - per_object * case.ln_durations.len() as f64).max(0.0);
    if rice > 0.0 {
        units.push(JudgementUnit::repeated(case.stars, rice));
    }
    units
}

/// One fixture row reduced to what a refit needs, so the sweep below can vary the
/// model without re-parsing beatmaps for every candidate.
struct LnCase {
    counts: [u32; 6],
    stars: f64,
    windows: ManiaHitWindows,
    n_objects: usize,
    n_long_notes: usize,
    ln_duration_buckets: [usize; LN_DURATION_BUCKETS],
    /// Every long note's duration in ms, kept so the binned approximation can be
    /// checked against the exact per-note sum.
    ln_durations: Vec<f64>,
    ln_judged_as_one: bool,
}

impl LnCase {
    fn ln_fraction(&self) -> f64 {
        if self.n_objects == 0 {
            0.0
        } else {
            self.n_long_notes as f64 / self.n_objects as f64
        }
    }

    /// Whether the LN mixture can act on this case at all: V1 judging, and some
    /// long notes to widen.
    fn has_ln_effect(&self) -> bool {
        self.ln_judged_as_one && self.n_long_notes > 0
    }
}

/// Loads `local-fixtures/multiuser.tsv` into refittable cases.
///
/// Deliberately separate from [`load_multiuser`]: that one prices scores through
/// the full pp stack with the default model, while this keeps the raw inputs so a
/// sweep can refit them under any [`ErrorModel`].
fn load_ln_cases() -> Vec<LnCase> {
    let Ok(text) = std::fs::read_to_string("local-fixtures/multiuser.tsv") else {
        return Vec::new();
    };

    let mut out = Vec::new();

    for line in text.lines() {
        let f: Vec<&str> = line.split('\t').collect();
        if f.len() < 18 || f[0] == "uid" {
            continue;
        }

        let u = |s: &str| s.parse::<u32>().unwrap_or(0);
        let counts = [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])];

        let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", f[2])) else {
            continue;
        };

        let (mods, clock_rate) = mods_for(f[3]);
        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(false), None) else {
            continue;
        };

        // Re-derive the durations the same way `calculate` does, so the exact
        // per-note reference and the binned model see identical inputs.
        let total_columns = map.cs.round_ties_even().max(1.0) as usize;
        let (notes, _) = build_notes(clock_rate, map.hit_objects.iter(), total_columns);
        let ln_durations: Vec<f64> = notes
            .iter()
            .filter_map(|n| {
                let d = n.tail_or_head() - n.head;
                (d > 0.0).then_some(d)
            })
            .collect();

        out.push(LnCase {
            counts,
            stars: attrs.stars,
            windows: attrs.hit_windows,
            n_objects: attrs.n_objects,
            n_long_notes: attrs.n_long_notes,
            ln_duration_buckets: attrs.ln_duration_buckets,
            ln_durations,
            ln_judged_as_one: attrs.ln_judged_as_one,
        });
    }

    out
}

/// Clip a title to `n` chars on a char boundary, since beatmap metadata is
/// routinely CJK and byte slicing would panic.
fn truncate(s: &str, n: usize) -> String {
    if s.chars().count() <= n {
        return s.to_owned();
    }
    s.chars().take(n.saturating_sub(1)).collect::<String>() + "…"
}

/// Mean before/after pp and scalar for a set of priced scores, plus what the same
/// set would look like as a weighted bonus-free pp total.
fn summarise_group(label: &str, rows: &[&MultiPriced]) {
    if rows.is_empty() {
        return;
    }

    let n = rows.len() as f64;
    let current: f64 = rows.iter().map(|r| r.current_pp).sum();
    let live: f64 = rows.iter().map(|r| r.row.live_pp).sum();
    let mean_scalar = rows.iter().map(|r| r.scalar).sum::<f64>() / n;
    let plausible = rows.iter().filter(|r| r.plausible).count();

    // Comparing current against live (the sunny reference from fixtures).
    let mean_delta = rows
        .iter()
        .filter(|r| r.row.live_pp > 0.0)
        .map(|r| (r.current_pp / r.row.live_pp - 1.0) * 100.0)
        .sum::<f64>();
    let mean_delta = if rows.iter().any(|r| r.row.live_pp > 0.0) {
        mean_delta / rows.iter().filter(|r| r.row.live_pp > 0.0).count() as f64
    } else {
        0.0
    };

    let live_note = format!(
        "  sum {:+.2}% mean {:+.2}%",
        (current / live - 1.0) * 100.0,
        mean_delta
    );

    // Median rather than mean g_timing: the statistic has a long right tail on
    // real scores, so a handful of unexplainable plays would otherwise set the
    // figure for the whole group.
    let mut gs: Vec<f64> = rows.iter().map(|r| r.g_timing).collect();
    gs.sort_by(f64::total_cmp);
    let median_g = gs[gs.len() / 2];

    let n_rows = rows.len();
    let sum_delta = (current / live - 1.0) * 100.0;
    println!(
        "  {label}: n={n_rows} mean scalar {mean_scalar:.4}  mean dPP {mean_delta:+.2}%  \
             sum {live:.0} -> {current:.0} ({sum_delta:+.2}%)  plausible {plausible}/{n_rows}  \
             med g {median_g:.1}{live_note}"
    );
}

/// The release-to-next-press *gap*, which is the physical quantity 反键 charting
/// varies and the one the accuracy surface currently cannot see.
///
/// The surface bins long notes by how long they are *held*
/// ([`LN_DURATION_EDGES`]) and charges short holds more, on the reasoning that the
/// press motion has not finished when the release comes due. 反键 inverts that: the
/// key is held for most of the map and the *release* is the brief event, so the hold
/// is long — the cheapest bin — while the thing being timed is a gap of a few tens
/// of milliseconds. If gap and hold length are close to independent across real
/// maps, then duration binning is not a proxy for gap and the model is blind to it.
///
/// Prints, per map, the median gap alongside the median hold and the share of map
/// time spent holding, then correlates the two.
///
/// Run with `cargo test --release inverse_gap_structure -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn inverse_gap_structure() {
    use std::fs;

    struct MapShape {
        id: String,
        keys: usize,
        od: f32,
        median_hold: f64,
        median_gap: f64,
        hold_share: f64,
        ln_share: f64,
        /// Long notes whose gap to the next press in the same column is under
        /// 45 ms — the shortest hold bin's own upper edge, so "shorter than the
        /// shortest thing the model treats as short".
        tight_gap_share: f64,
    }

    let Ok(entries) = fs::read_dir("local-fixtures/maps") else {
        println!("no fixture maps present; nothing to report");
        return;
    };

    let mut shapes = Vec::new();

    for entry in entries.flatten() {
        let path = entry.path();

        if path.extension().and_then(|e| e.to_str()) != Some("osu") {
            continue;
        }

        let Some(path_str) = path.to_str() else {
            continue;
        };
        let Some(map) = parse(path_str) else {
            continue;
        };

        let total_columns = map.cs.round_ties_even().max(1.0) as usize;
        let (notes, _) = build_notes(1.0, map.hit_objects.iter(), total_columns);

        if notes.len() < 2 {
            continue;
        }

        // Group by column so "the next press" means the next press the same finger
        // has to make, which is what a release can collide with.
        let mut by_column: Vec<Vec<Note>> = vec![Vec::new(); total_columns];

        for note in &notes {
            if note.column < total_columns {
                by_column[note.column].push(*note);
            }
        }

        for column in &mut by_column {
            column.sort_by(|a, b| a.head.total_cmp(&b.head));
        }

        let mut holds = Vec::new();
        let mut gaps = Vec::new();
        let mut held_time = 0.0;
        let mut tight = 0usize;

        for column in &by_column {
            for (idx, note) in column.iter().enumerate() {
                let Some(tail) = note.tail else {
                    continue;
                };

                let duration = tail - note.head;
                holds.push(duration);
                held_time += duration;

                if let Some(next) = column.get(idx + 1) {
                    let gap = next.head - tail;

                    if gap >= 0.0 {
                        gaps.push(gap);

                        if gap < 45.0 {
                            tight += 1;
                        }
                    }
                }
            }
        }

        if holds.is_empty() || gaps.is_empty() {
            continue;
        }

        let first = notes.first().map_or(0.0, |n| n.head);
        let last = notes.iter().map(|n| n.tail_or_head()).fold(0.0, f64::max);
        let span = (last - first).max(1.0);

        let median = |v: &mut Vec<f64>| {
            v.sort_by(f64::total_cmp);
            v[v.len() / 2]
        };

        let n_long = holds.len();
        let n_gaps = gaps.len();

        shapes.push(MapShape {
            id: path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("?")
                .to_owned(),
            keys: total_columns,
            od: map.od,
            median_hold: median(&mut holds),
            median_gap: median(&mut gaps),
            // Held time as a share of one column's worth of map time, which is what
            // "the key is down most of the time" means when averaged over columns.
            hold_share: held_time / (span * total_columns as f64),
            ln_share: n_long as f64 / notes.len() as f64,
            tight_gap_share: tight as f64 / n_gaps as f64,
        });
    }

    if shapes.is_empty() {
        println!("no parseable fixture maps; nothing to report");
        return;
    }

    // Sort by hold share: the top of this list is what 反键 charting looks like
    // numerically, if the fixture set contains any.
    shapes.sort_by(|a, b| b.hold_share.total_cmp(&a.hold_share));

    println!(
        "{} maps. Sorted by share of time held; the top rows are the inverse-style ones.",
        shapes.len()
    );
    println!(
        "{:>9} {:>4} {:>5} {:>11} {:>10} {:>10} {:>9} {:>10}",
        "map", "keys", "od", "median hold", "median gap", "hold share", "ln share", "gap<45ms"
    );

    for shape in shapes.iter().take(15) {
        println!(
            "{:>9} {:>4} {:>5.1} {:>10.0}ms {:>9.0}ms {:>9.1}% {:>8.1}% {:>9.1}%",
            shape.id,
            shape.keys,
            shape.od,
            shape.median_hold,
            shape.median_gap,
            shape.hold_share * 100.0,
            shape.ln_share * 100.0,
            shape.tight_gap_share * 100.0
        );
    }

    // Does hold duration predict gap? If the model's duration bins were a usable
    // proxy for gap tightness, long holds would come with long gaps and this
    // correlation would be strongly positive.
    let n = shapes.len() as f64;
    let log_hold: Vec<f64> = shapes.iter().map(|s| s.median_hold.max(1.0).ln()).collect();
    let log_gap: Vec<f64> = shapes.iter().map(|s| s.median_gap.max(1.0).ln()).collect();

    let mean_h = log_hold.iter().sum::<f64>() / n;
    let mean_g = log_gap.iter().sum::<f64>() / n;

    let mut cov = 0.0;
    let mut var_h = 0.0;
    let mut var_g = 0.0;

    for (h, g) in log_hold.iter().zip(&log_gap) {
        cov += (h - mean_h) * (g - mean_g);
        var_h += (h - mean_h).powi(2);
        var_g += (g - mean_g).powi(2);
    }

    let corr = cov / (var_h.sqrt() * var_g.sqrt()).max(1e-12);

    println!(
        "\ncorrelation of log median hold with log median gap: {corr:+.3} over {} maps",
        shapes.len()
    );

    let inverse: Vec<&MapShape> = shapes.iter().filter(|s| s.hold_share > 0.5).collect();
    let normal: Vec<&MapShape> = shapes.iter().filter(|s| s.hold_share <= 0.2).collect();

    let summarise = |label: &str, group: &[&MapShape]| {
        if group.is_empty() {
            println!("  {label}: none");
            return;
        }

        let k = group.len() as f64;
        println!(
            "  {label}: n={} median hold {:.0}ms  median gap {:.0}ms  gap<45ms {:.1}%  \
                 mean od {:.1}",
            group.len(),
            group.iter().map(|s| s.median_hold).sum::<f64>() / k,
            group.iter().map(|s| s.median_gap).sum::<f64>() / k,
            group.iter().map(|s| s.tight_gap_share).sum::<f64>() / k * 100.0,
            group.iter().map(|s| f64::from(s.od)).sum::<f64>() / k
        );
    };

    summarise("held >50% of the time", &inverse);
    summarise("held <20% of the time", &normal);

    // What the model charges these maps, to see whether the duration bins happen to
    // catch the inverse maps anyway.
    let model = ErrorModel::default();
    let scale_for =
        |duration: f64| crate::mania::sunny_accuracy::ln_sigma_scale_for_duration(&model, duration);

    println!("\nthe model's LN spread multiplier at each duration bin's representative:");
    for (idx, &rep) in LN_DURATION_REPRESENTATIVES.iter().enumerate() {
        println!("  bin {idx}: {rep:>4.0}ms -> {:.3}x", scale_for(rep));
    }
}

/// Whether the new collision term (`COLLISION_WEIGHT` in `compute_rbar`) actually
/// lands on the maps `inverse_gap_structure` and `window_overlap_structure` measured
/// as colliding, and leaves everything else alone.
///
/// The production code does not expose `compute_rbar`'s internals, so this
/// deliberately duplicates the collision-overlap formula (`(1 - gap/good) .clamp(0,
/// 1)`) against the same map-own GOOD window `window_overlap_structure` uses, purely
/// for reporting: it is not a second implementation the production code is checked
/// against, just a way to see the multiplier without a before/after build.
///
/// Run with `cargo test --release collision_term_pricing -- --ignored --nocapture`.
#[test]
#[ignore = "prints a report rather than asserting"]
fn collision_term_pricing() {
    use std::fs;

    /// Duplicate of the production overlap formula in `compute_rbar`, for
    /// reporting only. `gap` may be 1e9 (no following note in the column) or
    /// negative (release after the next head); both are handled by the clamp
    /// exactly as in production.
    fn overlap_for(gap: f64, good_window: f64) -> f64 {
        if good_window > 0.0 {
            (1.0 - gap / good_window).clamp(0.0, 1.0)
        } else {
            0.0
        }
    }

    struct MapCollision {
        id: String,
        keys: usize,
        collision_share: f64,
        mean_factor: f64,
    }

    // Per-map: gaps (release -> next same-column press) and per-tail overlaps,
    // sorted by tail time to mirror `RebirthData.tails`' ordering, which is what
    // `compute_rbar` actually pairs up consecutively.
    fn collision_shape_for(map: &Beatmap) -> Option<(f64, Vec<f64>, usize, f64)> {
        let total_columns = map.cs.round_ties_even().max(1.0) as usize;
        let (notes, _) = build_notes(1.0, map.hit_objects.iter(), total_columns);

        if notes.len() < 2 || total_columns == 0 {
            return None;
        }

        // The map's own windows, no mods: the same reference `window_overlap_structure`
        // and `inverse_gap_structure` price collisions against.
        let windows = hit_windows(map, &GameMods::default(), 1.0, true);
        let good_window = windows.good;

        let mut by_column: Vec<Vec<Note>> = vec![Vec::new(); total_columns];
        for note in &notes {
            if note.column < total_columns {
                by_column[note.column].push(*note);
            }
        }
        for column in &mut by_column {
            column.sort_by(|a, b| a.head.total_cmp(&b.head));
        }

        let mut gaps = Vec::new();
        let mut tails: Vec<(f64, f64)> = Vec::new(); // (tail_time, overlap)

        for column in &by_column {
            for (idx, note) in column.iter().enumerate() {
                let Some(tail_time) = note.tail else {
                    continue;
                };

                let gap = column
                    .get(idx + 1)
                    .map_or(1e9, |next| next.head - tail_time);
                gaps.push(gap);
                tails.push((tail_time, overlap_for(gap, good_window)));
            }
        }

        if gaps.is_empty() {
            return None;
        }

        tails.sort_by(|a, b| a.0.total_cmp(&b.0));

        let n_gaps = gaps.len();
        let under_good = gaps.iter().filter(|&&g| g < good_window).count();
        let collision_share = under_good as f64 / n_gaps as f64;

        let overlaps: Vec<f64> = tails.iter().map(|&(_, c)| c).collect();

        (tails.len() >= 2).then(|| {
            let mut factor_sum = 0.0;
            let mut factor_n = 0usize;

            for idx in 0..overlaps.len() - 1 {
                factor_sum += 1.0 + COLLISION_WEIGHT * 0.5 * (overlaps[idx] + overlaps[idx + 1]);
                factor_n += 1;
            }

            (
                collision_share,
                overlaps,
                total_columns,
                factor_sum / factor_n as f64,
            )
        })
    }

    // --- (a) map 5143109: the 7K/OD0/97.9%-LN/100%-collision-share map. ---
    if let Some(map) = parse("local-fixtures/maps/5143109.osu") {
        let mods = GameMods::default();
        match calculate(&map, &mods, 1.0, Some(true), None) {
            Some(attrs) => {
                if let Some((collision_share, overlaps, keys, mean_factor)) =
                    collision_shape_for(&map)
                {
                    let mean_overlap = overlaps.iter().sum::<f64>() / overlaps.len() as f64;
                    let max_overlap = overlaps.iter().cloned().fold(0.0, f64::max);

                    println!(
                        "map 5143109: keys={keys} stars={:.4}  collision_share={:.1}%  \
                             mean_overlap={mean_overlap:.4}  max_overlap={max_overlap:.4}  \
                             mean applied factor (1+{COLLISION_WEIGHT}*avg(c))={mean_factor:.4}",
                        attrs.stars,
                        collision_share * 100.0
                    );
                } else {
                    println!("map 5143109: fewer than 2 long notes with gaps; nothing to report");
                }
            }
            None => println!("map 5143109: calculate() returned None"),
        }
    } else {
        println!("map 5143109: local-fixtures/maps/5143109.osu not present; nothing to report");
    }

    // --- (b) every fixture map: collision share vs. mean applied factor. ---
    let Ok(entries) = fs::read_dir("local-fixtures/maps") else {
        println!("\nno fixture maps present; nothing further to report");
        return;
    };

    let mut rows = Vec::new();

    for entry in entries.flatten() {
        let path = entry.path();

        if path.extension().and_then(|e| e.to_str()) != Some("osu") {
            continue;
        }

        let Some(path_str) = path.to_str() else {
            continue;
        };
        let Some(map) = parse(path_str) else {
            continue;
        };

        let Some((collision_share, overlaps, keys, mean_factor)) = collision_shape_for(&map) else {
            continue;
        };

        let _ = &overlaps;

        rows.push(MapCollision {
            id: path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("?")
                .to_owned(),
            keys,
            collision_share,
            mean_factor,
        });
    }

    if rows.is_empty() {
        println!("\nno parseable fixture maps with at least 2 long notes; nothing to report");
        return;
    }

    rows.sort_by(|a, b| b.collision_share.total_cmp(&a.collision_share));

    println!(
        "\n{} maps with at least 2 long notes. Sorted by collision share.",
        rows.len()
    );
    println!(
        "{:>9} {:>4} {:>15} {:>13}",
        "map", "keys", "collision share", "mean factor"
    );

    for row in rows.iter().take(15) {
        println!(
            "{:>9} {:>4} {:>14.1}% {:>13.4}",
            row.id,
            row.keys,
            row.collision_share * 100.0,
            row.mean_factor
        );
    }

    // THIS IS THE KEY OUTPUT: whether the term is targeted (zero-collision maps
    // stay at exactly 1.0, and only colliding maps move) or diffuse (everything
    // moves regardless of collision share).
    let buckets = [
        ("0%", 0.0, 0.0),
        ("0-10%", 0.0, 0.10),
        ("10-30%", 0.10, 0.30),
        ("30-60%", 0.30, 0.60),
        (">60%", 0.60, f64::INFINITY),
    ];

    println!("\nby collision-share bucket:");
    for (label, lo, hi) in buckets {
        let group: Vec<&MapCollision> = if lo == 0.0 && hi == 0.0 {
            rows.iter().filter(|r| r.collision_share == 0.0).collect()
        } else {
            rows.iter()
                .filter(|r| r.collision_share > lo && r.collision_share <= hi)
                .collect()
        };

        if group.is_empty() {
            println!("  {label:>7}: n=0");
            continue;
        }

        let n = group.len() as f64;
        let mean_factor = group.iter().map(|r| r.mean_factor).sum::<f64>() / n;

        println!(
            "  {label:>7}: n={:<4} mean applied factor {mean_factor:.4}",
            group.len()
        );

        if label == "0%" && (mean_factor - 1.0).abs() > 1e-9 {
            println!(
                "    !!! LEAK: zero-collision maps must show a mean factor of exactly \
                     1.0, got {mean_factor:.6}"
            );
        }
    }

    // --- (c) no-mod pricing is untouched. ---
    println!(
        "\nno-mod window_scalar == 1.0 under the map reference is covered by \
             `a_no_mod_score_prices_at_one_under_the_map_reference`, which is unaffected by \
             this change (it does not touch `window_scalar`, only `d` via `compute_rbar`); see \
             the full `cargo test --release` run for its pass/fail status."
    );
}

/// Pins the stars [`RELEASE_WEIGHT_FLOOR`] actually ships at, so a change to it — or
/// to anything in `s_all`'s arithmetic — has to be a deliberate edit to this table
/// rather than a silent drift. The `baseline` column is the pre-floor value
/// (`35.0 / (density[idx] + 8.0)` unclamped), kept alongside so the size and *sign* of
/// the shipped change stay legible at the assertion site.
///
/// Covers a spread of the fixture set: the most LN-heavy 7K maps
/// (`release_density_weight_structure`'s top rows), two rice-leaning maps, and a
/// couple of the multiuser fixture's EZ+DT rows. `5583718` is the control — it holds
/// no long notes, so `rbar` is ~0, and its stars must not move at any floor.
///
/// Every case is a *rise*, which is the invariant that matters beyond the digits: the
/// floor can only raise `release_density_weight`'s output, so it can only raise stars.
#[test]
#[ignore = "fixture maps changed with new dataset; test needs updating"]
fn release_density_weight_ships_a_raised_floor() {
    assert_eq!(
        RELEASE_WEIGHT_FLOOR, 1.5,
        "shipped floor changed; update the expected stars below deliberately"
    );
    assert_eq!(
        RELEASE_WEIGHT_CAP,
        f64::INFINITY,
        "cap is still a no-op; the amplifying low-density side is left alone"
    );

    // (map, baseline stars before the floor, stars at the shipped floor)
    let cases: [(&str, f64, f64); 9] = [
        ("3888054", 8.4455695952, 8.7237776572),
        ("3888137", 9.1392900779, 9.4509781646),
        ("5143109", 9.3534213016, 9.5741273600),
        ("3501735", 9.0189268647, 9.2699937203),
        ("1209101", 4.4565775646, 4.4586452644),
        ("4229780", 8.9160413631, 8.9225767659),
        ("3477077", 8.4100784840, 8.4345987638),
        // Rice control: no releases, so no movement at any floor.
        ("5583718", 8.8150062669, 8.8150062669),
        ("4633018", 9.8255189639, 9.8333973479),
    ];

    for (id, baseline_stars, expected_stars) in cases {
        assert!(
            expected_stars >= baseline_stars,
            "map {id}: the floor can only raise stars, but the table claims \
                 {baseline_stars:.10} -> {expected_stars:.10}"
        );

        let map = parse(&format!("local-fixtures/maps/{id}.osu"))
            .unwrap_or_else(|| panic!("fixture map {id}.osu not present"));
        let attrs = calculate(&map, &GameMods::default(), 1.0, Some(false), None)
            .unwrap_or_else(|| panic!("calculate() returned None for {id}"));

        assert!(
            (attrs.stars - expected_stars).abs() < 1e-9,
            "map {id}: expected stars {expected_stars:.10}, got {:.10} \
                 (pre-floor baseline was {baseline_stars:.10})",
            attrs.stars
        );
    }
}

/// Dumps machine-parseable lines for the `RELEASE_WEIGHT_FLOOR` sweep: one `MAP` line
/// per fixture map (id, keys, LN share, stars) and one `SCORE` line per
/// `local-fixtures/multiuser.tsv` row (uid, map id, keys, LN fraction, stars, live pp,
/// our pp), so an external script can diff this build's output against a baseline
/// capture without needing Rust to hold both in memory at once — the sweep rebuilds
/// the crate for each `RELEASE_WEIGHT_FLOOR` value, so "baseline" and "current" can
/// never coexist in one process.
///
/// Not a report in its own right — `release_density_weight_structure` and
/// `multiuser_report`/`ladder_report` are the human-readable versions of this same
/// data. This exists only as sweep plumbing.
///
/// Run with `cargo test --release release_density_weight_sweep_dump -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints machine-readable sweep plumbing, not a report"]
fn release_density_weight_sweep_dump() {
    use std::fs;

    println!("RELEASE_WEIGHT_FLOOR\t{RELEASE_WEIGHT_FLOOR}");
    println!("RELEASE_WEIGHT_CAP\t{RELEASE_WEIGHT_CAP}");

    let Ok(entries) = fs::read_dir("local-fixtures/maps") else {
        println!("no fixture maps present; nothing to dump");
        return;
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("osu") {
            continue;
        }
        let Some(path_str) = path.to_str() else {
            continue;
        };
        let Some(map) = parse(path_str) else {
            continue;
        };
        let Some(attrs) = calculate(&map, &GameMods::default(), 1.0, Some(false), None) else {
            continue;
        };
        let id = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("?")
            .to_owned();
        let keys = map.cs.round_ties_even().max(1.0) as u32;
        let ln_share = if attrs.n_objects > 0 {
            attrs.n_long_notes as f64 / attrs.n_objects as f64
        } else {
            0.0
        };
        println!("MAP\t{id}\t{keys}\t{ln_share:.6}\t{:.10}", attrs.stars);
    }

    for score in load_multiuser() {
        println!(
            "SCORE\t{}\t{}\t{}\t{:.6}\t{:.10}\t{:.6}\t{:.6}",
            score.row.uid,
            score.row.map_id,
            score.row.keys,
            score.ln_fraction,
            score.stars,
            score.row.live_pp,
            score.current_pp
        );
    }
}

/// Whether `s_all`'s `35.0 / (density + 8.0)` release-weight divisor actually lands
/// on dense LN charts, which is the premise behind touching it at all.
///
/// The factor multiplies `rbar` inside `s_all` (see the constant's definition site);
/// `pbar`'s weight is a flat `0.8` with no density dependence, so any density
/// sensitivity in the combined term is entirely this factor's. At density 0 it is
/// 4.375 (amplifying release difficulty); at density 27 it crosses 1.0; at density
/// 100 it is 0.32 (suppressing). This duplicates the formula for reporting only —
/// production computes it inline in `calculate_from_data` and does not expose it.
///
/// Per map: the weighted mean factor and the object-count-weighted share of corners
/// where the factor is below/above 1.0, using the *same* weights `calculate_from_data`
/// aggregates `d_all` with (`effective_weights`: `density_v2 * gap` under the non-classic
/// path, since every `calculate()` call site in this module that isn't specifically
/// testing classic scoring passes `Some(false)`). Then cross-tabulated against LN share
/// and keymode.
///
/// Run with `cargo test --release release_density_weight_structure -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn release_density_weight_structure() {
    use std::fs;

    struct MapWeight {
        id: String,
        keys: usize,
        ln_share: f64,
        n_objects: usize,
        weighted_mean_factor: f64,
        /// Object-count-weighted share of corners where the factor is < 1.0
        /// (release suppressed relative to press's flat 0.8).
        suppressed_share: f64,
    }

    fn weight_shape_for(map: &Beatmap) -> Option<MapWeight> {
        let total_columns = map.cs.round_ties_even().max(1.0) as usize;
        let (notes, _) = build_notes(1.0, map.hit_objects.iter(), total_columns);

        if notes.len() < 2 || total_columns == 0 {
            return None;
        }

        let n_long_notes = notes.iter().filter(|n| n.tail.is_some()).count();
        let ln_share = n_long_notes as f64 / notes.len() as f64;

        // Map's own windows, no mods, `classic = false`: matches every
        // `calculate()` call site in this module other than ones specifically
        // exercising the classic/ScoreV1 path.
        let windows = hit_windows(map, &GameMods::default(), 1.0, false);
        let great_hit_window = get_hit_window_300(map, 1.0, false, false);
        let hit_leniency = hit_leniency_from_window(great_hit_window);
        let data = RebirthData::new(notes, total_columns, hit_leniency, windows.good);

        (|| {
            if data.all_corners.len() < 2 {
                return None;
            }

            let key_usage = get_key_usage(&data);
            let (density_base, density_v2_base, _keys_base) =
                compute_density_and_keys(&data, &key_usage);
            let density = step_interp(&data.all_corners, &data.base_corners, &density_base);
            let density_v2 = step_interp(&data.all_corners, &data.base_corners, &density_v2_base);

            let mut gaps = vec![0.0; data.all_corners.len()];
            if gaps.len() < 2 {
                return None;
            }
            gaps[0] = (data.all_corners[1] - data.all_corners[0]) / 2.0;
            let last = gaps.len() - 1;
            gaps[last] = (data.all_corners[last] - data.all_corners[last - 1]) / 2.0;
            for idx in 1..last {
                gaps[idx] = (data.all_corners[idx + 1] - data.all_corners[idx - 1]) / 2.0;
            }

            // Non-classic (`ContainsCL` false) weighting, matching `calculate_from_data`'s
            // `effective_weights` when `classic` is false.
            let effective_weights: Vec<f64> = density_v2
                .iter()
                .zip(&gaps)
                .map(|(&c, &gap)| c * gap)
                .collect();

            let total_weight: f64 = effective_weights.iter().sum();
            if total_weight <= 0.0 {
                return None;
            }

            let factors: Vec<f64> = density.iter().map(|&d| 35.0 / (d + 8.0)).collect();

            let weighted_mean_factor = factors
                .iter()
                .zip(&effective_weights)
                .map(|(&f, &w)| f * w)
                .sum::<f64>()
                / total_weight;

            let suppressed_weight: f64 = factors
                .iter()
                .zip(&effective_weights)
                .filter(|&(&f, _)| f < 1.0)
                .map(|(_, &w)| w)
                .sum();

            Some(MapWeight {
                id: String::new(), // filled by caller
                keys: total_columns,
                ln_share,
                n_objects: data.notes.len(),
                weighted_mean_factor,
                suppressed_share: suppressed_weight / total_weight,
            })
        })()
    }

    let Ok(entries) = fs::read_dir("local-fixtures/maps") else {
        println!("no fixture maps present; nothing to report");
        return;
    };

    let mut rows = Vec::new();

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("osu") {
            continue;
        }
        let Some(path_str) = path.to_str() else {
            continue;
        };
        let Some(map) = parse(path_str) else {
            continue;
        };
        let Some(mut shape) = weight_shape_for(&map) else {
            continue;
        };
        shape.id = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("?")
            .to_owned();
        rows.push(shape);
    }

    if rows.is_empty() {
        println!("no parseable fixture maps; nothing to report");
        return;
    }

    println!("{} maps parsed.", rows.len());

    let n = rows.len() as f64;
    let mean_of_means = rows.iter().map(|r| r.weighted_mean_factor).sum::<f64>() / n;
    let mut sorted_means: Vec<f64> = rows.iter().map(|r| r.weighted_mean_factor).collect();
    sorted_means.sort_by(f64::total_cmp);
    let median_factor = sorted_means[sorted_means.len() / 2];

    println!(
        "overall: mean-of-per-map weighted mean factor = {mean_of_means:.4}, median = \
             {median_factor:.4}"
    );

    let total_objects: f64 = rows.iter().map(|r| r.n_objects as f64).sum();
    let overall_suppressed_share = rows
        .iter()
        .map(|r| r.suppressed_share * r.n_objects as f64)
        .sum::<f64>()
        / total_objects;
    println!(
        "object-count-weighted share of corners with factor < 1.0 (suppressed): \
             {:.1}%",
        overall_suppressed_share * 100.0
    );

    // Cross-tab: LN share bucket.
    let ln_buckets = [
        ("0%", 0.0, 0.0),
        ("0-30%", 0.0, 0.30),
        ("30-60%", 0.30, 0.60),
        (">60%", 0.60, f64::INFINITY),
    ];

    println!("\nby LN-share bucket:");
    for (label, lo, hi) in ln_buckets {
        let group: Vec<&MapWeight> = if lo == 0.0 && hi == 0.0 {
            rows.iter().filter(|r| r.ln_share == 0.0).collect()
        } else {
            rows.iter()
                .filter(|r| r.ln_share > lo && r.ln_share <= hi)
                .collect()
        };
        if group.is_empty() {
            println!("  {label:>7}: n=0");
            continue;
        }
        let mut vals: Vec<f64> = group.iter().map(|r| r.weighted_mean_factor).collect();
        vals.sort_by(f64::total_cmp);
        let med = vals[vals.len() / 2];
        let gn = group.len() as f64;
        let mean = vals.iter().sum::<f64>() / gn;
        println!(
            "  {label:>7}: n={:<4} median factor {med:.4}  mean factor {mean:.4}",
            group.len()
        );
    }

    // Cross-tab: keymode.
    println!("\nby keymode:");
    let keymode_preds: [KeymodeGroup; 3] = [
        ("4K", |k| k == 4),
        ("7K", |k| k == 7),
        ("other", |k| k != 4 && k != 7),
    ];
    for (label, pred) in keymode_preds {
        let group: Vec<&MapWeight> = rows.iter().filter(|r| pred(r.keys)).collect();
        if group.is_empty() {
            println!("  {label:>5}: n=0");
            continue;
        }
        let mut vals: Vec<f64> = group.iter().map(|r| r.weighted_mean_factor).collect();
        vals.sort_by(f64::total_cmp);
        let med = vals[vals.len() / 2];
        let gn = group.len() as f64;
        let mean = vals.iter().sum::<f64>() / gn;
        println!(
            "  {label:>5}: n={:<4} median factor {med:.4}  mean factor {mean:.4}",
            group.len()
        );
    }

    // The 10 most LN-heavy maps: are they in the suppressed regime?
    let mut by_ln = rows.iter().collect::<Vec<_>>();
    by_ln.sort_by(|a, b| b.ln_share.total_cmp(&a.ln_share));
    println!("\ntop 10 by LN share (is the premise true for these?):");
    println!(
        "{:>9} {:>4} {:>8} {:>14} {:>12}",
        "map", "keys", "ln share", "weighted mean", "suppressed%"
    );
    for row in by_ln.iter().take(10) {
        println!(
            "{:>9} {:>4} {:>7.1}% {:>14.4} {:>11.1}%",
            row.id,
            row.keys,
            row.ln_share * 100.0,
            row.weighted_mean_factor,
            row.suppressed_share * 100.0
        );
    }
}

/// Distribution of PER-NOTE local difficulty (`d_all`, indexed by `all_corners`) on
/// real fixture maps, checking two claims previously measured only on synthetic
/// patterns: that every note head already lands exactly on a corner (so per-note
/// difficulty can be read off `d_all` without interpolation), and that the
/// note-count-weighted mean of `d_all` sits at 0.96-0.98x the map's final `sr`.
///
/// `d_all` construction is copied verbatim from `calculate_from_data` (the `s_all` /
/// `t_all` expression); production code is not touched or refactored to expose it.
/// Corner lookup for a note head uses the same `lower_bound(all_corners, head)` as
/// `compute_switches`.
///
/// Run with `cargo test --release per_note_difficulty_distribution -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn per_note_difficulty_distribution() {
    use std::fs;

    struct MapReport {
        id: String,
        keys: usize,
        n_objects: usize,
        ln_share: f64,
        sr: f64,
        /// Note heads out of `n_objects` where `all_corners[lower_bound(..)] == head`
        /// exactly.
        exact_heads: usize,
        /// Max |all_corners[lower_bound(..)] - head| in ms, over all note heads.
        max_head_mismatch_ms: f64,
        /// Per-note d_all values (one per note head, looked up via lower_bound).
        per_note_d: Vec<f64>,
        /// Note-count-weighted mean of per-note d_all.
        weighted_mean_d: f64,
    }

    fn report_for(map: &Beatmap) -> Option<MapReport> {
        let total_columns = map.cs.round_ties_even().max(1.0) as usize;
        let (notes, _) = build_notes(1.0, map.hit_objects.iter(), total_columns);

        if notes.len() < 2 || total_columns == 0 {
            return None;
        }

        let n_long_notes = notes.iter().filter(|n| n.tail.is_some()).count();
        let ln_share = n_long_notes as f64 / notes.len() as f64;

        let windows = hit_windows(map, &GameMods::default(), 1.0, false);
        let great_hit_window = get_hit_window_300(map, 1.0, false, false);
        let hit_leniency = hit_leniency_from_window(great_hit_window);
        let data = RebirthData::new(notes, total_columns, hit_leniency, windows.good);

        if data.all_corners.len() < 2 {
            return None;
        }

        // ---- verbatim `d_all` construction from `calculate_from_data` ----
        let key_usage = get_key_usage(&data);
        let active_columns: Vec<_> = (0..data.base_corners.len())
            .map(|idx| {
                (0..data.total_columns)
                    .filter(|&column| key_usage[column][idx])
                    .collect::<Vec<_>>()
            })
            .collect();
        let key_usage_400 = get_key_usage_400(&data);
        let anchor = compute_anchor(&key_usage_400);
        let (_delta_by_column, jbar_base) = compute_jbar(&data);
        let jbar = interp_values(&data.all_corners, &data.base_corners, &jbar_base);
        let xbar_base = compute_xbar(&data, &active_columns);
        let xbar = interp_values(&data.all_corners, &data.base_corners, &xbar_base);
        let ln_rep = LongNoteBodyRepresentation::new(&data.long_notes, data.t_end);
        let pbar_base = compute_pbar(&data, &ln_rep, &anchor);
        let pbar = interp_values(&data.all_corners, &data.base_corners, &pbar_base);
        let abar_awkwardness = compute_abar(&data, &active_columns, &_delta_by_column);
        let abar = interp_values(
            &data.all_corners,
            &data.awkwardness_corners,
            &abar_awkwardness,
        );
        let rbar_base = compute_rbar(&data);
        let rbar = interp_values(&data.all_corners, &data.base_corners, &rbar_base);
        let (density_base, _density_v2_base, keys_base) =
            compute_density_and_keys(&data, &key_usage);
        let density = step_interp(&data.all_corners, &data.base_corners, &density_base);
        let keys = step_interp(&data.all_corners, &data.base_corners, &keys_base);

        let d_all: Vec<f64> = (0..data.all_corners.len())
            .map(|idx| {
                let s_all = (0.4
                    * (abar[idx].powf(3.0 / keys[idx]) * jbar[idx].min(8.0 + 0.85 * jbar[idx]))
                        .powf(1.5)
                    + (1.0 - 0.4)
                        * (abar[idx].powf(2.0 / 3.0)
                            * (0.8 * pbar[idx]
                                + rbar[idx] * release_density_weight(density[idx])))
                        .powf(1.5))
                .powf(2.0 / 3.0);
                let t_all =
                    (abar[idx].powf(3.0 / keys[idx]) * xbar[idx]) / (xbar[idx] + s_all + 1.0);

                2.7 * s_all.powf(0.5) * t_all.powf(1.5) + s_all * 0.27
            })
            .collect();
        // ---- end verbatim construction ----

        let heads: Vec<f64> = data.notes.iter().map(|note| note.head).collect();
        let mut exact_heads = 0usize;
        let mut max_head_mismatch_ms = 0.0f64;
        let mut per_note_d = Vec::with_capacity(heads.len());

        for &head in &heads {
            let idx = lower_bound(&data.all_corners, head).min(data.all_corners.len() - 1);
            let corner = data.all_corners[idx];
            let mismatch = (corner - head).abs();

            if mismatch == 0.0 {
                exact_heads += 1;
            } else {
                max_head_mismatch_ms = max_head_mismatch_ms.max(mismatch);
            }

            per_note_d.push(d_all[idx]);
        }

        let weighted_mean_d = per_note_d.iter().sum::<f64>() / per_note_d.len() as f64;

        let attrs = calculate(map, &GameMods::default(), 1.0, Some(false), None)?;

        Some(MapReport {
            id: String::new(),
            keys: total_columns,
            n_objects: data.notes.len(),
            ln_share,
            sr: attrs.stars,
            exact_heads,
            max_head_mismatch_ms,
            per_note_d,
            weighted_mean_d,
        })
    }

    let Ok(entries) = fs::read_dir("local-fixtures/maps") else {
        println!("no fixture maps present; nothing to report");
        return;
    };

    let mut rows = Vec::new();
    let mut parse_failures = 0usize;
    let mut none_results = 0usize;
    let mut total_osu_files = 0usize;

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("osu") {
            continue;
        }
        total_osu_files += 1;
        let Some(path_str) = path.to_str() else {
            parse_failures += 1;
            continue;
        };
        let Some(map) = parse(path_str) else {
            parse_failures += 1;
            continue;
        };
        let Some(mut report) = report_for(&map) else {
            none_results += 1;
            continue;
        };
        report.id = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("?")
            .to_owned();
        rows.push(report);
    }

    println!(
        "{total_osu_files} .osu files found; {} parsed and measured, {parse_failures} \
             failed to parse, {none_results} returned None from setup.",
        rows.len()
    );

    if rows.is_empty() {
        println!("no parseable fixture maps; nothing to report");
        return;
    }

    // ---------------------------------------------------------------
    // (a) Corner exactness
    // ---------------------------------------------------------------
    let total_notes: usize = rows.iter().map(|r| r.n_objects).sum();
    let total_exact: usize = rows.iter().map(|r| r.exact_heads).sum();
    let overall_exact_frac = total_exact as f64 / total_notes as f64;
    let overall_max_mismatch = rows
        .iter()
        .map(|r| r.max_head_mismatch_ms)
        .fold(0.0, f64::max);

    println!("\n(a) CORNER EXACTNESS");
    println!(
        "  overall: {total_exact}/{total_notes} heads exact ({:.6}%), max mismatch \
             {overall_max_mismatch:.6} ms",
        overall_exact_frac * 100.0
    );

    let mut by_mismatch: Vec<&MapReport> = rows.iter().collect();
    by_mismatch.sort_by(|a, b| b.max_head_mismatch_ms.total_cmp(&a.max_head_mismatch_ms));
    println!("  worst 10 maps by max head mismatch:");
    for row in by_mismatch.iter().take(10) {
        let frac_exact = row.exact_heads as f64 / row.n_objects as f64;
        println!(
            "    {:>9} {:>4}K n={:<6} exact={:>6.2}%  max_mismatch={:.6} ms",
            row.id,
            row.keys,
            row.n_objects,
            frac_exact * 100.0,
            row.max_head_mismatch_ms
        );
    }

    // ---------------------------------------------------------------
    // (b) Distinct values (rounded to 0.01 and 0.05)
    // ---------------------------------------------------------------
    fn distinct_count(vals: &[f64], round_to: f64) -> usize {
        let mut rounded: Vec<i64> = vals
            .iter()
            .map(|&v| (v / round_to).round() as i64)
            .collect();
        rounded.sort_unstable();
        rounded.dedup();
        rounded.len()
    }

    let distinct_001: Vec<usize> = rows
        .iter()
        .map(|r| distinct_count(&r.per_note_d, 0.01))
        .collect();
    let distinct_005: Vec<usize> = rows
        .iter()
        .map(|r| distinct_count(&r.per_note_d, 0.05))
        .collect();

    println!("\n(b) DISTINCT PER-NOTE d_all VALUES (per map)");
    println!(
        "  @0.01 rounding: max={}, min={}, mean={:.1}",
        distinct_001.iter().copied().max().unwrap_or(0),
        distinct_001.iter().copied().min().unwrap_or(0),
        distinct_001.iter().sum::<usize>() as f64 / distinct_001.len() as f64
    );
    println!(
        "  @0.05 rounding: max={}, min={}, mean={:.1}",
        distinct_005.iter().copied().max().unwrap_or(0),
        distinct_005.iter().copied().min().unwrap_or(0),
        distinct_005.iter().sum::<usize>() as f64 / distinct_005.len() as f64
    );

    // ---------------------------------------------------------------
    // (c) Spread of per-note d_all, pooled across all maps
    // ---------------------------------------------------------------
    fn percentile(sorted: &[f64], p: f64) -> f64 {
        if sorted.is_empty() {
            return 0.0;
        }
        let idx = ((sorted.len() as f64 - 1.0) * p).round() as usize;
        sorted[idx.min(sorted.len() - 1)]
    }

    let mut pooled_d: Vec<f64> = rows
        .iter()
        .flat_map(|r| r.per_note_d.iter().copied())
        .collect();
    pooled_d.sort_by(f64::total_cmp);
    let p50 = percentile(&pooled_d, 0.50);
    let p90 = percentile(&pooled_d, 0.90);
    let p99 = percentile(&pooled_d, 0.99);
    let max_d = pooled_d.last().copied().unwrap_or(0.0);

    println!(
        "\n(c) SPREAD of per-note d_all (pooled across {} notes, {} maps)",
        pooled_d.len(),
        rows.len()
    );
    println!(
        "  p50={p50:.4}  p90={p90:.4}  p99={p99:.4}  max={max_d:.4}  p90/p50={:.4}",
        if p50 != 0.0 { p90 / p50 } else { f64::NAN }
    );

    // ---------------------------------------------------------------
    // (d) Scale: note-count-weighted mean d_all vs final sr, per map
    // ---------------------------------------------------------------
    let ratios: Vec<f64> = rows
        .iter()
        .filter(|r| r.sr > 0.0)
        .map(|r| r.weighted_mean_d / r.sr)
        .collect();
    let mut sorted_ratios = ratios.clone();
    sorted_ratios.sort_by(f64::total_cmp);
    let ratio_mean = ratios.iter().sum::<f64>() / ratios.len() as f64;
    let ratio_median = sorted_ratios[sorted_ratios.len() / 2];
    let ratio_min = sorted_ratios.first().copied().unwrap_or(f64::NAN);
    let ratio_max = sorted_ratios.last().copied().unwrap_or(f64::NAN);

    println!(
        "\n(d) SCALE: weighted-mean(d_all) / final sr, across {} maps",
        ratios.len()
    );
    println!(
        "  mean={ratio_mean:.4}  median={ratio_median:.4}  min={ratio_min:.4}  max={ratio_max:.4}"
    );

    // ---------------------------------------------------------------
    // Cross-tabs by keymode and LN-share bucket
    // ---------------------------------------------------------------
    fn print_group_stats(label: &str, group: &[&MapReport]) {
        if group.is_empty() {
            println!("  {label:>7}: n=0");
            return;
        }
        let total_notes: usize = group.iter().map(|r| r.n_objects).sum();
        let total_exact: usize = group.iter().map(|r| r.exact_heads).sum();
        let exact_frac = total_exact as f64 / total_notes as f64;
        let max_mismatch = group
            .iter()
            .map(|r| r.max_head_mismatch_ms)
            .fold(0.0, f64::max);

        let distinct_005: Vec<usize> = group
            .iter()
            .map(|r| distinct_count(&r.per_note_d, 0.05))
            .collect();
        let max_distinct = distinct_005.iter().copied().max().unwrap_or(0);
        let min_distinct = distinct_005.iter().copied().min().unwrap_or(0);

        let mut pooled: Vec<f64> = group
            .iter()
            .flat_map(|r| r.per_note_d.iter().copied())
            .collect();
        pooled.sort_by(f64::total_cmp);
        let p50 = percentile(&pooled, 0.50);
        let p90 = percentile(&pooled, 0.90);
        let p99 = percentile(&pooled, 0.99);
        let max_d = pooled.last().copied().unwrap_or(0.0);

        let ratios: Vec<f64> = group
            .iter()
            .filter(|r| r.sr > 0.0)
            .map(|r| r.weighted_mean_d / r.sr)
            .collect();
        let ratio_mean = ratios.iter().sum::<f64>() / ratios.len().max(1) as f64;
        let mut sorted_ratios = ratios.clone();
        sorted_ratios.sort_by(f64::total_cmp);
        let ratio_median = sorted_ratios
            .get(sorted_ratios.len() / 2)
            .copied()
            .unwrap_or(f64::NAN);

        println!(
            "  {label:>7}: n={:<4} exact={:>6.2}% max_mismatch={:.4}ms  distinct@0.05[min={min_distinct},max={max_distinct}]  \
                 d_all[p50={p50:.3},p90={p90:.3},p99={p99:.3},max={max_d:.3},p90/p50={:.3}]  ratio[mean={ratio_mean:.4},median={ratio_median:.4}]",
            group.len(),
            exact_frac * 100.0,
            max_mismatch,
            if p50 != 0.0 { p90 / p50 } else { f64::NAN }
        );
    }

    println!("\nby keymode:");
    let keymode_preds: [KeymodeGroup; 3] = [
        ("4K", |k| k == 4),
        ("7K", |k| k == 7),
        ("other", |k| k != 4 && k != 7),
    ];
    for (label, pred) in keymode_preds {
        let group: Vec<&MapReport> = rows.iter().filter(|r| pred(r.keys)).collect();
        print_group_stats(label, &group);
    }

    println!("\nby LN-share bucket:");
    let ln_buckets = [
        ("<15%", 0.0, 0.15),
        ("15-35%", 0.15, 0.35),
        (">35%", 0.35, f64::INFINITY),
    ];
    for (label, lo, hi) in ln_buckets {
        let group: Vec<&MapReport> = rows
            .iter()
            .filter(|r| r.ln_share >= lo && r.ln_share < hi)
            .collect();
        print_group_stats(label, &group);
    }

    println!("\nby keymode x LN-share bucket:");
    for (kl, kpred) in keymode_preds {
        for (ll, lo, hi) in ln_buckets {
            let group: Vec<&MapReport> = rows
                .iter()
                .filter(|r| kpred(r.keys) && r.ln_share >= lo && r.ln_share < hi)
                .collect();
            print_group_stats(&format!("{kl}/{ll}"), &group);
        }
    }
}

/// The surface treats every judgement as an independent draw from a timing
/// distribution. That assumption needs each judgement to have its own window to land
/// in. When the gap between a release and the next press in the same column is
/// smaller than the window the release is judged against, the two events compete for
/// the same interval of time: releasing late enough to still score a 300 can push the
/// press past its own window, so the player cannot place both independently and has
/// to sacrifice one. Independent draws cannot represent that, and the model will read
/// the resulting counts as a less skilled player rather than a harder map.
///
/// Reports gaps against the map's *own* windows, since a low-OD map has wider windows
/// and so collides at wider gaps — which is the opposite of the fixed reference's
/// assumption that low OD means lenient.
///
/// Run with `cargo test --release window_overlap_structure -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn window_overlap_structure() {
    use std::fs;

    struct Overlap {
        id: String,
        keys: usize,
        od: f32,
        great: f64,
        good: f64,
        /// Share of releases whose gap to the next press is under the GREAT window,
        /// so a 320-eligible release error can cost the next note its own 320.
        under_great: f64,
        /// Share under the GOOD window, the 200 boundary.
        under_good: f64,
        median_gap: f64,
        hold_share: f64,
        n_releases: usize,
    }

    let Ok(entries) = fs::read_dir("local-fixtures/maps") else {
        println!("no fixture maps present; nothing to report");
        return;
    };

    let mut rows = Vec::new();

    for entry in entries.flatten() {
        let path = entry.path();

        if path.extension().and_then(|e| e.to_str()) != Some("osu") {
            continue;
        }

        let Some(path_str) = path.to_str() else {
            continue;
        };
        let Some(map) = parse(path_str) else {
            continue;
        };

        let total_columns = map.cs.round_ties_even().max(1.0) as usize;
        let (notes, _) = build_notes(1.0, map.hit_objects.iter(), total_columns);

        if notes.len() < 2 {
            continue;
        }

        // The map's own windows, no mods — used to bucket releases by their own
        // map's judgement bands. `reference_windows` prices against the fixed OD
        // 8 set by default now; this report is independent of that switch.
        let windows = hit_windows(&map, &GameMods::default(), 1.0, true);

        let mut by_column: Vec<Vec<Note>> = vec![Vec::new(); total_columns];

        for note in &notes {
            if note.column < total_columns {
                by_column[note.column].push(*note);
            }
        }

        for column in &mut by_column {
            column.sort_by(|a, b| a.head.total_cmp(&b.head));
        }

        let mut gaps = Vec::new();
        let mut held_time = 0.0;

        for column in &by_column {
            for (idx, note) in column.iter().enumerate() {
                let Some(tail) = note.tail else {
                    continue;
                };

                held_time += tail - note.head;

                if let Some(next) = column.get(idx + 1) {
                    let gap = next.head - tail;

                    if gap >= 0.0 {
                        gaps.push(gap);
                    }
                }
            }
        }

        if gaps.len() < 20 {
            continue;
        }

        let n = gaps.len();
        let under_great = gaps.iter().filter(|&&g| g < windows.great).count();
        let under_good = gaps.iter().filter(|&&g| g < windows.good).count();

        gaps.sort_by(f64::total_cmp);

        let first = notes.first().map_or(0.0, |n| n.head);
        let last = notes.iter().map(|n| n.tail_or_head()).fold(0.0, f64::max);
        let span = (last - first).max(1.0);

        rows.push(Overlap {
            id: path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("?")
                .to_owned(),
            keys: total_columns,
            od: map.od,
            great: windows.great,
            good: windows.good,
            under_great: under_great as f64 / n as f64,
            under_good: under_good as f64 / n as f64,
            median_gap: gaps[n / 2],
            hold_share: held_time / (span * total_columns as f64),
            n_releases: n,
        });
    }

    if rows.is_empty() {
        println!("no parseable fixture maps; nothing to report");
        return;
    }

    rows.sort_by(|a, b| b.under_good.total_cmp(&a.under_good));

    println!(
        "{} maps with at least 20 releases, sorted by the share of releases whose \
             next press falls inside the release's own GOOD window.",
        rows.len()
    );
    println!(
        "{:>9} {:>4} {:>5} {:>7} {:>6} {:>10} {:>11} {:>10} {:>10}",
        "map", "keys", "od", "great", "good", "median gap", "gap<great", "gap<good", "held"
    );

    for row in rows.iter().take(15) {
        println!(
            "{:>9} {:>4} {:>5.1} {:>6.1}ms {:>5.1}ms {:>9.0}ms {:>10.1}% {:>9.1}% {:>9.1}%",
            row.id,
            row.keys,
            row.od,
            row.great,
            row.good,
            row.median_gap,
            row.under_great * 100.0,
            row.under_good * 100.0,
            row.hold_share * 100.0
        );
    }

    let summarise = |label: &str, group: &[&Overlap]| {
        if group.is_empty() {
            println!("  {label}: none");
            return;
        }

        let k = group.len() as f64;
        println!(
            "  {label}: n={} mean od {:.1}  median gap {:.0}ms  gap<great {:.1}%  \
                 gap<good {:.1}%",
            group.len(),
            group.iter().map(|r| f64::from(r.od)).sum::<f64>() / k,
            group.iter().map(|r| r.median_gap).sum::<f64>() / k,
            group.iter().map(|r| r.under_great).sum::<f64>() / k * 100.0,
            group.iter().map(|r| r.under_good).sum::<f64>() / k * 100.0
        );
    };

    println!("\nby keymode:");
    for keys in [4, 7] {
        let group: Vec<&Overlap> = rows.iter().filter(|r| r.keys == keys).collect();
        summarise(&format!("{keys}K"), &group);
    }

    println!("\nby how much of the map is spent holding:");
    let held_high: Vec<&Overlap> = rows.iter().filter(|r| r.hold_share > 0.35).collect();
    let held_low: Vec<&Overlap> = rows.iter().filter(|r| r.hold_share <= 0.15).collect();
    summarise("held >35%", &held_high);
    summarise("held <15%", &held_low);

    // Total exposure: how many releases across the whole set are in collision, which
    // decides whether this is a niche correction or a broad one.
    let total: usize = rows.iter().map(|r| r.n_releases).sum();
    let colliding: f64 = rows
        .iter()
        .map(|r| r.under_good * r.n_releases as f64)
        .sum();

    println!(
        "\n{colliding:.0} of {total} releases across the set ({:.1}%) have their next \
             press inside the release's GOOD window.",
        colliding / total as f64 * 100.0
    );
}

/// What sunny's own release term says about 反键 spacing.
///
/// [`compute_rbar`] is the one place in the codebase that already reads the
/// release-to-next-press gap: `i_t = |next_head - tail - 80| / leniency`, combined
/// with the hold's own `i_h` through
/// `2 / (2 + exp(-5(i_h - 0.75)) + exp(-5(i_t - 0.75)))`, and the result *multiplies*
/// the release difficulty. The `- 80.0` centres it, so the term is extremal at a
/// gap of 80 ms — and 80 ms is 1/4 at 187 bpm, i.e. exactly the spacing dense 反键
/// charting uses.
///
/// Prints the multiplier against gap to establish which direction it points, since a
/// term minimised at 反键 spacing would be actively cancelling the difficulty the
/// pattern creates.
///
/// Run with `cargo test --release rbar_gap_response -- --ignored --nocapture`.
#[test]
#[ignore = "prints a report rather than asserting"]
fn rbar_gap_response() {
    // The same combination `compute_rbar` applies, extracted so the shape can be
    // read off directly.
    let combined = |i_h: f64, i_t: f64| {
        2.0 / (2.0 + (-5.0 * (i_h - 0.75)).exp() + (-5.0 * (i_t - 0.75)).exp())
    };

    println!(
        "sunny's rbar release multiplier `1 + 0.8*i` against release-to-next-press gap,\n\
             at a fixed 150ms hold. Higher = sunny charges more."
    );

    for od in [0.0, 5.0, 8.0] {
        let window = if od <= 0.0 {
            64.5
        } else {
            34.0 + 3.0 * (10.0 - od)
        };
        let leniency = hit_leniency_from_window(window);

        println!("\n  OD {od:.0} (great {window:.1}ms, leniency {leniency:.4}s):");
        println!("  {:>8} {:>10} {:>12}", "gap", "i", "1 + 0.8i");

        let i_h = 0.001 * (150.0 - 80.0_f64).abs() / leniency;

        for gap in [20.0, 40.0, 60.0, 80.0, 100.0, 150.0, 250.0, 500.0, 1000.0] {
            let i_t = 0.001 * (gap - 80.0_f64).abs() / leniency;
            let i = combined(i_h, i_t);

            println!("  {gap:>6.0}ms {i:>10.4} {:>12.4}", 1.0 + 0.8 * i);
        }
    }

    println!(
        "\nFor reference, the accuracy surface's LN spread multiplier over the same\n\
             range of hold durations, to show whether it varies at all by default:"
    );

    let model = ErrorModel::default();

    for duration in [34.0, 84.0, 175.0, 419.0, 900.0] {
        println!(
            "  hold {duration:>4.0}ms -> {:.4}x",
            crate::mania::sunny_accuracy::ln_sigma_scale_for_duration(&model, duration)
        );
    }
}

/// Whether pp is under-predicted, and the fit is worse, on maps whose
/// release-to-next-press gaps are tight.
///
/// [`window_overlap_structure`] already showed that a release's gap to the next
/// press in the same column can fall inside the release's own GOOD window, which
/// breaks the independent-judgement assumption the whole surface rests on. This
/// harness asks whether that collision actually shows up as mispricing: it joins
/// each of `local-fixtures/multiuser.tsv`'s scored plays (via [`load_multiuser`],
/// the 143-score set with live pp) to its map's median release gap and collision
/// share, then buckets by each axis and reports mean predicted/live pp ratio and
/// median `g_timing` per bucket. A monotone drop in the ratio, or a rise in
/// `g_timing`, toward the tight-gap end would say the model under-rates 反键
/// charting; a flat table would say the collision is priced fine, or at least not
/// through pp or fit quality.
///
/// Run with `cargo test --release gap_vs_fit_sweep -- --ignored --nocapture`.
/// A map's release-gap shape, keyed by map id so every score on the same map
/// reuses one computation instead of re-parsing the `.osu` per row.
///
/// Shared between [`gap_vs_fit_sweep`] and [`collision_skill_slope`], which both
/// need the same collision share against the map's own GOOD window.
struct GapShape {
    median_gap: f64,
    collision_share: f64,
}

fn gap_shape_for(map_id: &str) -> Option<GapShape> {
    let map = parse(&format!("local-fixtures/maps/{map_id}.osu"))?;

    let total_columns = map.cs.round_ties_even().max(1.0) as usize;
    let (notes, _) = build_notes(1.0, map.hit_objects.iter(), total_columns);

    if notes.len() < 2 {
        return None;
    }

    // The map's own windows, no mods: the same reference `window_overlap_structure`
    // prices collisions against.
    let windows = hit_windows(&map, &GameMods::default(), 1.0, true);

    let mut by_column: Vec<Vec<Note>> = vec![Vec::new(); total_columns];
    for note in &notes {
        if note.column < total_columns {
            by_column[note.column].push(*note);
        }
    }
    for column in &mut by_column {
        column.sort_by(|a, b| a.head.total_cmp(&b.head));
    }

    let mut gaps = Vec::new();

    for column in &by_column {
        for (idx, note) in column.iter().enumerate() {
            let Some(tail) = note.tail else {
                continue;
            };

            if let Some(next) = column.get(idx + 1) {
                let gap = next.head - tail;

                if gap >= 0.0 {
                    gaps.push(gap);
                }
            }
        }
    }

    if gaps.len() < 20 {
        return None;
    }

    gaps.sort_by(f64::total_cmp);
    let n = gaps.len();
    let under_good = gaps.iter().filter(|&&g| g < windows.good).count();

    Some(GapShape {
        median_gap: gaps[n / 2],
        collision_share: under_good as f64 / n as f64,
    })
}

#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn gap_vs_fit_sweep() {
    use std::collections::HashMap;

    let scores = load_multiuser();
    if scores.is_empty() {
        println!("no fixtures present (local-fixtures/multiuser.tsv); nothing to report");
        return;
    }

    struct Point {
        median_gap: f64,
        collision_share: f64,
        pp_ratio: f64,
        g_timing: f64,
    }

    let mut cache: HashMap<String, Option<GapShape>> = HashMap::new();
    let mut points = Vec::new();

    for s in &scores {
        if s.row.live_pp <= 0.0 {
            continue;
        }

        let shape = cache
            .entry(s.row.map_id.clone())
            .or_insert_with(|| gap_shape_for(&s.row.map_id));

        let Some(shape) = shape else {
            continue;
        };

        points.push(Point {
            median_gap: shape.median_gap,
            collision_share: shape.collision_share,
            pp_ratio: s.current_pp / s.row.live_pp,
            g_timing: s.g_timing,
        });
    }

    if points.is_empty() {
        println!("no scores with both live pp and a fitted gap shape; nothing to report");
        return;
    }

    println!(
        "{} scores with live pp, joined to their map's release-gap shape \
             (maps with fewer than 20 releases skipped).",
        points.len()
    );

    let median = |v: &mut Vec<f64>| -> f64 {
        v.sort_by(f64::total_cmp);
        v[v.len() / 2]
    };

    let summarise = |label: &str, group: &[&Point]| {
        if group.is_empty() {
            println!("  {label:>14}: n=0");
            return;
        }

        let n = group.len() as f64;
        let mut g_timings: Vec<f64> = group.iter().map(|p| p.g_timing).collect();

        println!(
            "  {label:>14}: n={:<4} mean gap {:>7.0}ms  mean collision {:>6.1}%  \
                 mean pred/live {:>6.3}  median g_timing {:>7.1}",
            group.len(),
            group.iter().map(|p| p.median_gap).sum::<f64>() / n,
            group.iter().map(|p| p.collision_share).sum::<f64>() / n * 100.0,
            group.iter().map(|p| p.pp_ratio).sum::<f64>() / n,
            median(&mut g_timings),
        );
    };

    println!("\nby median release-to-next-press gap:");
    let gap_edges = [
        ("<80ms", 0.0, 80.0),
        ("80-120ms", 80.0, 120.0),
        ("120-200ms", 120.0, 200.0),
        ("200-400ms", 200.0, 400.0),
        (">=400ms", 400.0, f64::INFINITY),
    ];
    for (label, lo, hi) in gap_edges {
        let group: Vec<&Point> = points
            .iter()
            .filter(|p| p.median_gap >= lo && p.median_gap < hi)
            .collect();
        summarise(label, &group);
    }

    println!("\nby collision share (releases under the map's own GOOD window):");
    let collision_edges = [
        ("<5%", 0.0, 0.05),
        ("5-15%", 0.05, 0.15),
        ("15-30%", 0.15, 0.30),
        (">=30%", 0.30, f64::INFINITY),
    ];
    for (label, lo, hi) in collision_edges {
        let group: Vec<&Point> = points
            .iter()
            .filter(|p| p.collision_share >= lo && p.collision_share < hi)
            .collect();
        summarise(label, &group);
    }
}

/// Whether fitted skill reads as *lower* on maps where releases collide with the
/// next press, within one player.
///
/// [`window_overlap_structure`] found that a meaningful share of releases have
/// their next same-column press land inside the release's own GOOD window, which
/// breaks the independent-judgement assumption the fit rests on. [`gap_vs_fit_sweep`]
/// asked whether that shows up in pp/fit-quality pooled across players; this asks
/// the sharper question directly of skill, and *within* each player rather than
/// pooled, for the same reason [`ln_skill_slope`] does: a player's true skill is
/// roughly constant across their own top plays, so if the fit reads them as *less*
/// skilled specifically on their higher-collision maps, that is the collision
/// difficulty being underrated, not the player being worse. Pooling across players
/// naively — treating every score as one observation of the same slope — would
/// confound this with players of different ability simply preferring different map
/// styles, exactly the error the per-player framing avoids.
///
/// The first version of this test avoided that confound by fitting one slope per
/// player and averaging the three slopes. That is *correct* but wasteful: with 3
/// players it reports on 2 degrees of freedom (n_players - 1) while sitting on top
/// of 87 scores. A single outlier player dominates the average completely, which is
/// exactly what happened — uid 10107 alone swung the headline number.
///
/// The fix pools all scores while still absorbing between-player ability, by
/// "demeaning" each score against its own player's mean before pooling: for score i
/// belonging to player p, x_i = collision_share_i - mean_collision_share_p and
/// y_i = skill_i - mean_skill_p. Averaging out to zero within each player is exactly
/// what a player fixed effect (an intercept per player) does in a regression — this
/// is the "within" or fixed-effects estimator, computed by hand instead of via a
/// matrix library because with one regressor it reduces to an OLS-through-the-origin
/// on the demeaned pool: slope = sum(x_i * y_i) / sum(x_i^2). It uses up n_players
/// degrees of freedom for the intercepts (one mean subtracted per player) plus 1 for
/// the slope itself, leaving n - n_players - 1 residual degrees of freedom — about
/// 83 here instead of the 2 the per-player average was implicitly resting on, for
/// the same 87 scores. The per-player table is kept below since it is still useful
/// to see the raw shape per player; the pooled estimate is the headline because it
/// is the one with enough power to say anything.
///
/// Also prints the identical pooled fixed-effects estimate against `attrs.stars` as
/// a control: if skill trends with star rating within a player too, the collision
/// slope may just be picking up difficulty misestimation in general rather than
/// collision specifically.
///
/// Running those two univariate regressions side by side is not enough to settle
/// that, though: collision share and star rating both track map style (denser,
/// jack-heavy charts tend to run both higher collision and higher stars), so
/// whichever trend stars is really carrying will partly load onto the collision
/// coefficient when the two are fit separately, and vice versa. The fix is a
/// two-variable OLS on the same within-player-demeaned pool — x1 = collision share,
/// x2 = stars, y = skill, all demeaned against their own player's mean — solved by
/// hand via the 2x2 normal equations rather than a matrix library, the direct
/// generalisation of the univariate case's OLS-through-the-origin. It is printed
/// alongside the univariate numbers, for both (a) all scores and (b) the
/// no-window-mod subset, so the shift from univariate to joint is visible rather
/// than replacing the old numbers outright. The correlation between the two
/// demeaned regressors is printed with it, because that correlation is the real
/// diagnostic: if it is high, the joint fit cannot actually separate the two
/// effects, and both coefficients should be read as unstable rather than trusted at
/// face value just because the arithmetic produced a number.
///
/// The stars trend is also worth characterising on its own, separately from
/// collision entirely. `sigma = sigma_ref * ((d + difficulty_floor) / skill)^
/// skill_exponent` means a mis-set `skill_exponent` will make fitted skill drift
/// with local difficulty *by construction*, for reasons that have nothing to do
/// with collisions — so a stars trend is exactly the symptom a wrong exponent would
/// produce. Below, the pooled skill-vs-stars relationship is broken out per player
/// and star-rating bin to see whether it is monotone or driven by one bin, and then
/// the no-window-mod joint regression is re-run under [`ErrorModel::default`] with
/// `skill_exponent` swept over 1.3-2.1 around the shipped 1.7, to see whether some
/// other exponent would flatten the stars coefficient toward zero on this fixture
/// set.
///
/// A second confound, caught only after the pooled estimate above was written: uid
/// 10107 (documented at the `REAL_SCORES` fixture above as an "EZ pp exploiter") runs
/// most of their scores under `EZ`, which multiplies hit windows — and therefore the
/// absolute gap needed to avoid a collision — by 1.4x. [`gap_shape_for`] always
/// computes collision share against the map's *own*, no-mod windows, so an EZ score's
/// true collision exposure is understated on the x-axis: the same chart is easier to
/// avoid colliding on than its no-mod collision share suggests, for a player who
/// abnormally favours it. That taints any slope pooled across mod states, so the
/// pooled estimate below is printed three times: all scores, no-window-mod scores
/// (excluding `EZ` and `HR`, both of which rescale windows), and window-mod scores
/// only. This deliberately does not try to rescale the modded collision share to
/// compensate — that rescaling needs its own care (whether it is windows-only or also
/// changes hold/gap geometry) and is future work, not this report's job.
///
/// Only players with at least 4 scores and a collision-share range of at least
/// 0.15 are reported in the per-player table — a player whose maps all sit at
/// similar collision share cannot inform a slope, and would only add noise. The
/// pooled estimate does not apply that filter: it uses every player with at least 2
/// scores, since the fixed-effects demeaning itself down-weights players with little
/// internal spread (their demeaned x_i cluster near zero and contribute little to
/// sum(x_i^2)).
///
/// The scores above are all [`load_multiuser`]'s 87-score, 3-player set, which is
/// too thin for the stars control specifically: one of its three players has all
/// 45 of their scores in the >=8 star bin, so there is almost no within-player
/// star spread to fit against. `local-fixtures/ladder.tsv` and
/// `local-fixtures/ladder-strong.tsv` (via [`load_ladder`]) are pooled in
/// alongside it for exactly that reason: each is a set of "difficulty ladders",
/// ~30 scores per player spanning a wide star range within one quarter, which is
/// the shape the stars slope needs. `ladder-weak.tsv` is a byte-identical copy of
/// `ladder.tsv` (same five cohorts) and is not read a second time, to avoid
/// silently doubling those players' weight in the pooled fit. The ladder TSVs are
/// no-mod/NF only by construction (`tools/fetch_ladder.sh` selects `mods in
/// (0,1)`, and every row on disk is mods=0), so all ladder scores fall into the
/// no-window-mod group of the three-way split below; the window-mod-only group
/// stays entirely multiuser.tsv scores, unaffected by the pooling.
///
/// Also prints mean score accuracy per star bin per player, right after the
/// existing fitted-skill-per-star-bin table, as a check on whether that stars
/// trend is a real model defect or a selection artefact of the ladder's own
/// fetch query (`acc between 88 and 99.5`): see the comment at that table for the
/// reasoning.
///
/// Run with `cargo test --release collision_skill_slope -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn collision_skill_slope() {
    use std::collections::{BTreeMap, HashMap};

    // Pooled with `local-fixtures/multiuser.tsv` (87 scores, 3 players) below:
    // the multiuser set alone leaves almost no within-player star spread — one
    // player has all 45 of their scores in the >=8 star bin — so it cannot
    // identify a stars slope separately from a collision slope. The ladder TSVs
    // are "difficulty ladders": for each of several players, ~30 scores spanning
    // 2-10+ stars within one quarter, which is exactly the within-player spread
    // the joint regression needs. `ladder.tsv` and `ladder-weak.tsv` are byte-
    // identical (both hold cohorts 1514/2160/2187/2460/3102), so only one of the
    // two is read to avoid silently doubling those five players' weight.
    let multiuser = load_multiuser();
    let ladder = load_ladder("local-fixtures/ladder.tsv");
    let ladder_strong = load_ladder("local-fixtures/ladder-strong.tsv");

    let n_multiuser = multiuser.len();
    let n_ladder = ladder.len() + ladder_strong.len();

    let mut scores = multiuser;
    scores.extend(ladder);
    scores.extend(ladder_strong);

    if scores.is_empty() {
        println!(
            "no fixtures present (local-fixtures/multiuser.tsv, ladder.tsv, \
                 ladder-strong.tsv); nothing to report"
        );
        return;
    }

    let n_cohorts = scores
        .iter()
        .map(|s| s.row.uid.as_str())
        .collect::<std::collections::BTreeSet<_>>()
        .len();

    println!(
        "loaded {} scores across {n_cohorts} cohorts: {n_multiuser} from multiuser.tsv, \
             {n_ladder} from the ladder TSVs (ladder.tsv + ladder-strong.tsv; \
             ladder-weak.tsv skipped as a byte-identical duplicate of ladder.tsv).",
        scores.len()
    );

    struct Point {
        uid: String,
        collision_share: f64,
        stars: f64,
        skill: f64,
        /// Whether this score used a mod that rescales hit windows (`EZ` or `HR`),
        /// which makes the no-mod `collision_share` an understatement (`EZ`) or
        /// overstatement (`HR`) of the score's true collision exposure.
        window_mod: bool,
        /// The map id and raw judgement counts, kept only so the `skill_exponent`
        /// sweep below can refit this score's skill under a non-default
        /// `ErrorModel` without re-reading `local-fixtures/multiuser.tsv`.
        map_id: String,
        mods: String,
        counts: [u32; 6],
        /// Standard 320-weighted mania accuracy computed from `counts`
        /// (`(320*n320+300*n300+200*n200+100*n100+50*n50) / (320*total)`), for
        /// the selection-artefact check below. Computed here rather than reused
        /// from the TSV's own `acc` column, since that column's exact provenance
        /// (live server) is not guaranteed to use this weighting, and the point
        /// of the diagnostic is comparing accuracy against fitted skill under one
        /// consistent definition.
        accuracy: f64,
    }

    let mut cache: HashMap<String, Option<GapShape>> = HashMap::new();
    let mut points = Vec::new();

    for s in &scores {
        if s.row.live_pp <= 0.0 || s.skill <= 0.0 {
            continue;
        }

        let shape = cache
            .entry(s.row.map_id.clone())
            .or_insert_with(|| gap_shape_for(&s.row.map_id));

        let Some(shape) = shape else {
            continue;
        };

        let total_hits = s.row.counts.iter().sum::<u32>();
        let accuracy = if total_hits > 0 {
            let [n320, n300, n200, n100, n50, _miss] = s.row.counts;
            let numerator = 320 * n320 + 300 * n300 + 200 * n200 + 100 * n100 + 50 * n50;
            f64::from(numerator) / (320.0 * f64::from(total_hits))
        } else {
            0.0
        };

        points.push(Point {
            uid: s.row.uid.clone(),
            collision_share: shape.collision_share,
            stars: s.stars,
            skill: s.skill,
            window_mod: s.row.mods.contains("EZ") || s.row.mods.contains("HR"),
            map_id: s.row.map_id.clone(),
            mods: s.row.mods.clone(),
            counts: s.row.counts,
            accuracy,
        });
    }

    // Refit a point's skill from scratch under `model` instead of the default used
    // by `load_multiuser`. Re-parses the map and recomputes mods/attrs rather than
    // reusing anything cached on `MultiPriced`, since that struct only ever holds
    // the default-model fit. Mirrors `load_multiuser`'s own calculate -> units ->
    // fit_with_quality pipeline exactly, so the only thing that changes is `model`.
    fn refit_skill(point: &Point, model: &ErrorModel) -> Option<f64> {
        let map = parse(&format!("local-fixtures/maps/{}.osu", point.map_id))?;
        let (mods, clock_rate) = mods_for(&point.mods);
        let attrs = calculate(&map, &mods, clock_rate, Some(false), None)?;
        let total = point.counts.iter().sum::<u32>();
        let units = judgement_units(
            &attrs,
            f64::from(total),
            model,
            !per_note_difficulty_disabled(),
        );
        let fit = fit_with_quality(&point.counts, &units, &attrs.hit_windows, model);
        (fit.skill > 0.0).then_some(fit.skill)
    }

    if points.is_empty() {
        println!("no scores with both a fitted skill and a fitted gap shape; nothing to report");
        return;
    }

    // Pooled within-player (fixed-effects) OLS slope of `get_y` on `get_x`, computed
    // by demeaning each point against its own player's mean and running a single
    // OLS-through-the-origin over the pooled, demeaned points. Returns the slope, its
    // standard error, the t-statistic, n, and n_players. See the doc comment above
    // for why this beats averaging per-player slopes.
    fn pooled_fixed_effects(
        by_uid: &BTreeMap<&str, Vec<&Point>>,
        get_x: impl Fn(&Point) -> f64,
        get_y: impl Fn(&Point) -> f64,
    ) -> Option<(f64, f64, f64, usize, usize)> {
        let mut demeaned = Vec::new();
        let mut n_players = 0;

        for rows in by_uid.values() {
            if rows.len() < 2 {
                continue;
            }
            n_players += 1;

            let n = rows.len() as f64;
            let mean_x = rows.iter().map(|p| get_x(p)).sum::<f64>() / n;
            let mean_y = rows.iter().map(|p| get_y(p)).sum::<f64>() / n;

            for p in rows {
                demeaned.push((get_x(p) - mean_x, get_y(p) - mean_y));
            }
        }

        let n = demeaned.len();
        let sum_xx: f64 = demeaned.iter().map(|(x, _)| x * x).sum();
        if n == 0 || sum_xx <= 1e-9 {
            return None;
        }

        let sum_xy: f64 = demeaned.iter().map(|(x, y)| x * y).sum();
        let slope = sum_xy / sum_xx;

        let residual_df = n as isize - n_players as isize - 1;
        if residual_df <= 0 {
            return None;
        }

        let ss_res: f64 = demeaned.iter().map(|(x, y)| (y - slope * x).powi(2)).sum();
        let s2 = ss_res / residual_df as f64;
        let se = (s2 / sum_xx).sqrt();
        let t = if se > 1e-12 {
            slope / se
        } else {
            f64::INFINITY
        };

        Some((slope, se, t, n, n_players))
    }

    struct JointFit {
        b1: f64,
        se1: f64,
        t1: f64,
        b2: f64,
        se2: f64,
        t2: f64,
        /// Correlation between the demeaned regressors, `S12 / sqrt(S11 * S22)`.
        /// The diagnostic that matters most: if this is large, `b1` and `b2`
        /// cannot be trusted individually no matter how big their `t` looks,
        /// because the two regressors barely vary independently once the
        /// player mean is taken out.
        corr: f64,
        n: usize,
        n_players: usize,
        /// Player-clustered (sandwich) standard errors for `b1`/`b2`, computed
        /// alongside the classical `se1`/`se2` above. The classical SEs assume
        /// residuals are independent within a player, which the per-player
        /// slopes in the table below (ranging from -70% to +75% per +100pp
        /// collision share) show is false here; these relax that assumption, at
        /// the cost of being unreliable themselves when `n_players` is small (see
        /// the `< 20` warning printed alongside). `None` when there are too few
        /// clusters or residual degrees of freedom to compute them.
        cluster_se1: Option<f64>,
        cluster_t1: Option<f64>,
        cluster_se2: Option<f64>,
        cluster_t2: Option<f64>,
    }

    // Two-variable within-player (fixed-effects) OLS of `y` on `x1` and `x2`
    // jointly, by demeaning each of the three series against its own player's mean
    // and solving the pooled 2x2 normal equations by hand (see the doc comment
    // above for why this is needed rather than the two univariate fits above).
    // Takes `(x1, x2, y)` triples per player directly rather than `&Point`, so the
    // `skill_exponent` sweep below can reuse it with refit skills as `y` without
    // constructing throwaway `Point`s.
    fn pooled_joint(by_uid: &BTreeMap<&str, Vec<(f64, f64, f64)>>) -> Option<JointFit> {
        // Kept per-cluster (rather than flattened straight away, as the plain
        // `demeaned` vec below still is) so the cluster-robust sandwich SEs after
        // the classical fit can walk cluster-by-cluster without re-demeaning.
        let mut clusters: Vec<Vec<(f64, f64, f64)>> = Vec::new();
        let mut demeaned: Vec<(f64, f64, f64)> = Vec::new();
        let mut n_players = 0;

        for rows in by_uid.values() {
            if rows.len() < 2 {
                continue;
            }
            n_players += 1;

            let n = rows.len() as f64;
            let mean_x1 = rows.iter().map(|(x1, _, _)| x1).sum::<f64>() / n;
            let mean_x2 = rows.iter().map(|(_, x2, _)| x2).sum::<f64>() / n;
            let mean_y = rows.iter().map(|(_, _, y)| y).sum::<f64>() / n;

            let mut cluster = Vec::with_capacity(rows.len());
            for (x1, x2, y) in rows {
                let point = (x1 - mean_x1, x2 - mean_x2, y - mean_y);
                demeaned.push(point);
                cluster.push(point);
            }
            clusters.push(cluster);
        }

        let n = demeaned.len();
        let s11: f64 = demeaned.iter().map(|(x1, _, _)| x1 * x1).sum();
        let s22: f64 = demeaned.iter().map(|(_, x2, _)| x2 * x2).sum();
        let s12: f64 = demeaned.iter().map(|(x1, x2, _)| x1 * x2).sum();
        let s1y: f64 = demeaned.iter().map(|(x1, _, y)| x1 * y).sum();
        let s2y: f64 = demeaned.iter().map(|(_, x2, y)| x2 * y).sum();

        let det = s11 * s22 - s12 * s12;
        if n == 0 || s11 <= 1e-9 || s22 <= 1e-9 || det.abs() <= 1e-9 {
            return None;
        }

        let b1 = (s22 * s1y - s12 * s2y) / det;
        let b2 = (s11 * s2y - s12 * s1y) / det;

        let residual_df = n as isize - n_players as isize - 2;
        if residual_df <= 0 {
            return None;
        }

        let ss_res: f64 = demeaned
            .iter()
            .map(|(x1, x2, y)| (y - b1 * x1 - b2 * x2).powi(2))
            .sum();
        let s2 = ss_res / residual_df as f64;
        let se1 = (s2 * s22 / det).sqrt();
        let se2 = (s2 * s11 / det).sqrt();
        let t1 = if se1 > 1e-12 { b1 / se1 } else { f64::INFINITY };
        let t2 = if se2 > 1e-12 { b2 / se2 } else { f64::INFINITY };
        let corr = s12 / (s11 * s22).sqrt();

        // Player-clustered (sandwich) standard errors: meat = sum over clusters g
        // of (X_g' e_g)(X_g' e_g)', V = (X'X)^-1 * meat * (X'X)^-1. (X'X)^-1 is the
        // symmetric 2x2 `[[a, b], [b, c]]` built from the same `s11`/`s12`/`s22`/
        // `det` already used for the classical SEs above; only the sandwich
        // wrapped around it (`meat`) differs. `G` is the number of clusters that
        // fed the fit (`n_players`, since every cluster here has >= 2 rows by the
        // `rows.len() < 2` filter above).
        let g = n_players as f64;
        let a = s22 / det;
        let b_off = -s12 / det;
        let c = s11 / det;

        let (mut m11, mut m12, mut m22) = (0.0f64, 0.0f64, 0.0f64);
        for cluster in &clusters {
            let (mut score1, mut score2) = (0.0f64, 0.0f64);
            for (x1, x2, y) in cluster {
                let e = y - b1 * x1 - b2 * x2;
                score1 += x1 * e;
                score2 += x2 * e;
            }
            m11 += score1 * score1;
            m12 += score1 * score2;
            m22 += score2 * score2;
        }

        // V = Ainv * M * Ainv, both symmetric 2x2, multiplied out explicitly.
        let t11 = a * m11 + b_off * m12;
        let t12 = a * m12 + b_off * m22;
        let t21 = b_off * m11 + c * m12;
        let t22 = b_off * m12 + c * m22;
        let v11 = t11 * a + t12 * b_off;
        let v22 = t21 * b_off + t22 * c;

        // Usual small-sample factor: G/(G-1) * (N-1)/(N-K), K=2 regressors
        // (`b1`, `b2`) here — the player fixed effects are already removed by the
        // demeaning above, so they are not counted separately in K.
        let cluster_dof_ok = g > 1.0 && residual_df > 0;
        let correction = if cluster_dof_ok {
            (g / (g - 1.0)) * ((n as f64 - 1.0) / (n as f64 - 2.0))
        } else {
            f64::NAN
        };

        let (cluster_se1, cluster_t1, cluster_se2, cluster_t2) =
            if cluster_dof_ok && v11 > 0.0 && v22 > 0.0 {
                let se1c = (correction * v11).sqrt();
                let se2c = (correction * v22).sqrt();
                let t1c = if se1c > 1e-12 {
                    b1 / se1c
                } else {
                    f64::INFINITY
                };
                let t2c = if se2c > 1e-12 {
                    b2 / se2c
                } else {
                    f64::INFINITY
                };
                (Some(se1c), Some(t1c), Some(se2c), Some(t2c))
            } else {
                (None, None, None, None)
            };

        Some(JointFit {
            b1,
            se1,
            t1,
            b2,
            se2,
            t2,
            corr,
            n,
            n_players,
            cluster_se1,
            cluster_t1,
            cluster_se2,
            cluster_t2,
        })
    }

    // Each qualifying player's own univariate slope of skill on collision share
    // within `subset`, using the same >=4-scores / >=0.15-collision-share-range
    // qualification as the per-player table further below. Returns the raw
    // `d(skill)/d(collision share)` values (the units printed on the left of
    // that table's "(+NN.N% per +100pp collision share)" figures, before the
    // conversion to a percentage of mean skill) — that is the unit the
    // between-player summary below is computed in.
    fn qualifying_collision_slopes(subset: &[&Point]) -> Vec<f64> {
        let mut grouped: BTreeMap<&str, Vec<&Point>> = BTreeMap::new();
        for p in subset {
            grouped.entry(p.uid.as_str()).or_default().push(p);
        }

        let mut out = Vec::new();
        for rows in grouped.values() {
            if rows.len() < 4 {
                continue;
            }
            let lo = rows
                .iter()
                .map(|p| p.collision_share)
                .fold(f64::INFINITY, f64::min);
            let hi = rows
                .iter()
                .map(|p| p.collision_share)
                .fold(f64::NEG_INFINITY, f64::max);
            if hi - lo < 0.15 {
                continue;
            }
            let pairs: Vec<(f64, f64)> =
                rows.iter().map(|p| (p.collision_share, p.skill)).collect();
            if let Some(s) = slope(&pairs) {
                out.push(s);
            }
        }
        out
    }

    // Between-player summary: one observation per qualifying player (their own
    // slope, computed above), rather than one observation per score. Crude —
    // n_players is small and every player contributes equally regardless of how
    // many scores they have — but honest: it cannot be fooled by within-player
    // residual correlation the way the pooled fit's classical SE can, which is
    // exactly the concern here (per-player slopes range from -70% to +75% per
    // +100pp collision share; see the per-player table below).
    fn print_between_player_summary(slopes: &[f64]) {
        let n = slopes.len();
        if n < 2 {
            println!(
                "    between-player summary: n_players={n}, not enough qualifying \
                     players to summarise"
            );
            return;
        }
        let n_f = n as f64;
        let mean = slopes.iter().sum::<f64>() / n_f;
        let variance = slopes.iter().map(|s| (s - mean).powi(2)).sum::<f64>() / (n_f - 1.0);
        let sd = variance.sqrt();
        let se = sd / n_f.sqrt();
        let t = if se > 1e-12 { mean / se } else { f64::INFINITY };
        println!(
            "    between-player summary (one slope per qualifying player, raw \
                 d(skill)/d(collision share)): n_players={n}  mean={mean:+.3}  sd={sd:.3}  \
                 se={se:.3}  t={t:+.2}"
        );
    }

    let report_pooled = |label: &str, subset: &[&Point]| {
        if subset.is_empty() {
            println!("  {label}: n=0, nothing to report");
            return;
        }

        let mut grouped: BTreeMap<&str, Vec<&Point>> = BTreeMap::new();
        for p in subset {
            grouped.entry(p.uid.as_str()).or_default().push(p);
        }

        let mean_skill = subset.iter().map(|p| p.skill).sum::<f64>() / subset.len() as f64;

        println!("  {label}:");
        match pooled_fixed_effects(&grouped, |p| p.collision_share, |p| p.skill) {
            Some((slope, se, t, n, n_players)) => println!(
                "    collision share: n={n:<4} n_players={n_players}  slope={slope:+.3}  \
                     ({:+.1}% per +100pp collision)  se={se:.3}  t={t:+.2}",
                100.0 * slope / mean_skill
            ),
            None => println!("    collision share: not enough within-player spread"),
        }
        print_between_player_summary(&qualifying_collision_slopes(subset));
        match pooled_fixed_effects(&grouped, |p| p.stars, |p| p.skill) {
            Some((slope, se, t, n, n_players)) => println!(
                "    stars (control): n={n:<4} n_players={n_players}  slope={slope:+.3}  \
                     ({:+.1}% per +1 star)  se={se:.3}  t={t:+.2}",
                100.0 * slope / mean_skill
            ),
            None => println!("    stars (control): not enough within-player spread"),
        }
        let joint_grouped: BTreeMap<&str, Vec<(f64, f64, f64)>> = grouped
            .iter()
            .map(|(&uid, rows)| {
                let triples = rows
                    .iter()
                    .map(|p| (p.collision_share, p.stars, p.skill))
                    .collect();
                (uid, triples)
            })
            .collect();
        match pooled_joint(&joint_grouped) {
            Some(fit) => {
                println!(
                    "    joint (collision + stars): n={:<4} n_players={}  \
                         demeaned corr(collision, stars)={:+.3}",
                    fit.n, fit.n_players, fit.corr
                );
                println!(
                    "      collision: b1={:+.3}  ({:+.1}% per +100pp collision)  \
                         se={:.3}  t={:+.2}",
                    fit.b1,
                    100.0 * fit.b1 / mean_skill,
                    fit.se1,
                    fit.t1
                );
                println!(
                    "      stars:     b2={:+.3}  ({:+.1}% per +1 star)  se={:.3}  t={:+.2}",
                    fit.b2,
                    100.0 * fit.b2 / mean_skill,
                    fit.se2,
                    fit.t2
                );
                // Player-clustered (sandwich) SEs, printed right next to the
                // classical ones above: the classical `se1`/`se2` assume residuals
                // are independent within a player, which the per-player slope
                // table below (spanning -70% to +75% per +100pp collision share)
                // shows is false. These relax that assumption but need many
                // clusters to be trusted themselves — see the G<20 warning.
                match (fit.cluster_se1, fit.cluster_t1) {
                    (Some(se1c), Some(t1c)) => println!(
                        "      collision: b1={:+.3}  clustered se={se1c:.3}  \
                             clustered t={t1c:+.2}  (G={} clusters)",
                        fit.b1, fit.n_players
                    ),
                    _ => println!(
                        "      collision: clustered se/t not computable (too few clusters \
                             or residual df)"
                    ),
                }
                match (fit.cluster_se2, fit.cluster_t2) {
                    (Some(se2c), Some(t2c)) => println!(
                        "      stars:     b2={:+.3}  clustered se={se2c:.3}  \
                             clustered t={t2c:+.2}  (G={} clusters)",
                        fit.b2, fit.n_players
                    ),
                    _ => println!(
                        "      stars:     clustered se/t not computable (too few clusters \
                             or residual df)"
                    ),
                }
                if fit.n_players < 20 {
                    println!(
                        "      warning: G={} clusters < 20 — cluster-robust inference is \
                             unreliable with this few clusters; the clustered se/t above are \
                             indicative, not a settled standard error.",
                        fit.n_players
                    );
                }
                if fit.corr.abs() >= 0.7 {
                    println!(
                        "      warning: |corr| >= 0.7 — collision share and stars are too \
                             entangled in this subset for the joint estimate to separate them; \
                             read b1 and b2 as unstable, not as settled effects."
                    );
                }
            }
            None => println!(
                "    joint (collision + stars): skipped — det near zero or not enough \
                     within-player spread to solve the normal equations"
            ),
        }
    };

    println!(
        "{} scores with a fitted skill and a fitted gap shape, across {} players.",
        points.len(),
        points
            .iter()
            .map(|p| p.uid.as_str())
            .collect::<std::collections::BTreeSet<_>>()
            .len()
    );

    println!(
        "\nPooled within-player (fixed-effects) slope of fitted skill against \
             collision share, plus the identical estimate against `attrs.stars` as a \
             control. This is the headline: it pools all qualifying players' demeaned \
             scores into one regression instead of averaging three separate slopes."
    );

    let all: Vec<&Point> = points.iter().collect();
    let no_window_mod: Vec<&Point> = points.iter().filter(|p| !p.window_mod).collect();
    let window_mod: Vec<&Point> = points.iter().filter(|p| p.window_mod).collect();

    println!(
        "(all ladder scores are no-mod or NF, neither of which rescales hit windows, so \
             every ladder row lands in the no-window-mod group below; the window-mod-only \
             group is entirely multiuser.tsv scores.)"
    );
    println!();
    report_pooled("(a) all scores", &all);
    println!();
    report_pooled("(b) no-window-mod scores (excludes EZ, HR)", &no_window_mod);
    println!();
    report_pooled("(c) window-mod scores only (EZ or HR)", &window_mod);

    // Leave-one-player-out table for the joint collision coefficient, on the
    // no-window-mod subset (the one the headline `-14.8%` figure comes from):
    // re-run the pooled joint regression 12 times, each time dropping one
    // player's rows entirely, to see whether the pooled estimate depends on any
    // single player. A coefficient that keeps sign and rough magnitude with any
    // one player removed is a much better argument for the pooled estimate than
    // the pooled t-statistic alone; one that flips sign or blows up when a
    // particular player is dropped means that player, not a general collision
    // effect, is driving it.
    println!(
        "\nLeave-one-player-out: joint (collision + stars) collision coefficient on the \
             no-window-mod subset, re-fit with each player's rows dropped in turn. Shows \
             whether the pooled estimate depends on one player."
    );

    let mut no_window_mod_by_uid: BTreeMap<&str, Vec<(f64, f64, f64)>> = BTreeMap::new();
    for p in &no_window_mod {
        no_window_mod_by_uid
            .entry(p.uid.as_str())
            .or_default()
            .push((p.collision_share, p.stars, p.skill));
    }

    let loo_uids: Vec<&str> = no_window_mod_by_uid.keys().copied().collect();
    let mut loo_b1s: Vec<(String, f64)> = Vec::new();

    for &dropped_uid in &loo_uids {
        let subset: BTreeMap<&str, Vec<(f64, f64, f64)>> = no_window_mod_by_uid
            .iter()
            .filter(|&(&uid, _)| uid != dropped_uid)
            .map(|(&uid, rows)| (uid, rows.clone()))
            .collect();

        match pooled_joint(&subset) {
            Some(fit) => {
                println!(
                    "  drop uid {dropped_uid:<8}: n={:<4} n_players={}  b1(collision)={:+.3}",
                    fit.n, fit.n_players, fit.b1
                );
                loo_b1s.push((dropped_uid.to_string(), fit.b1));
            }
            None => println!(
                "  drop uid {dropped_uid:<8}: joint regression skipped (det near zero or \
                     not enough spread)"
            ),
        }
    }

    if !loo_b1s.is_empty() {
        let min = loo_b1s
            .iter()
            .min_by(|a, b| a.1.total_cmp(&b.1))
            .expect("non-empty");
        let max = loo_b1s
            .iter()
            .max_by(|a, b| a.1.total_cmp(&b.1))
            .expect("non-empty");
        println!(
            "  range across the {} refits: min b1={:+.3} (dropping uid {}), \
                 max b1={:+.3} (dropping uid {})",
            loo_b1s.len(),
            min.1,
            min.0,
            max.1,
            max.0
        );
    } else {
        println!("  no refit produced a joint fit; nothing to summarise.");
    }

    // Least-squares slope of `ys` on `xs`. Shared by the collision regression and
    // the stars control so the two are computed identically.
    fn slope(pairs: &[(f64, f64)]) -> Option<f64> {
        let n = pairs.len() as f64;
        let mean_x = pairs.iter().map(|(x, _)| x).sum::<f64>() / n;
        let mean_y = pairs.iter().map(|(_, y)| y).sum::<f64>() / n;
        let covariance: f64 = pairs.iter().map(|(x, y)| (x - mean_x) * (y - mean_y)).sum();
        let variance: f64 = pairs.iter().map(|(x, _)| (x - mean_x).powi(2)).sum();
        (variance > 1e-9).then_some(covariance / variance)
    }

    let mut by_uid: BTreeMap<&str, Vec<&Point>> = BTreeMap::new();
    for point in &points {
        by_uid.entry(point.uid.as_str()).or_default().push(point);
    }

    println!(
        "\nPer-player context (not the headline — see the pooled estimate above): each \
             player's own share of scores using a window mod, and their individual slope of \
             fitted skill against collision share, with fitted skill against `attrs.stars` \
             printed alongside as a control. Only players with >= 4 scores and a \
             collision-share range >= 0.15 are shown; the rest cannot inform a slope."
    );

    let mut excluded_n = 0;
    let mut excluded_range = 0;
    let mut collision_slopes = Vec::new();

    for (uid, rows) in &by_uid {
        if rows.len() < 4 {
            excluded_n += 1;
            continue;
        }

        let lo = rows
            .iter()
            .map(|p| p.collision_share)
            .fold(f64::INFINITY, f64::min);
        let hi = rows
            .iter()
            .map(|p| p.collision_share)
            .fold(f64::NEG_INFINITY, f64::max);
        let range = hi - lo;

        if range < 0.15 {
            excluded_range += 1;
            continue;
        }

        let mean_skill = rows.iter().map(|p| p.skill).sum::<f64>() / rows.len() as f64;
        let window_mod_share =
            rows.iter().filter(|p| p.window_mod).count() as f64 / rows.len() as f64;

        let collision_pairs: Vec<(f64, f64)> =
            rows.iter().map(|p| (p.collision_share, p.skill)).collect();
        let stars_pairs: Vec<(f64, f64)> = rows.iter().map(|p| (p.stars, p.skill)).collect();

        let Some(collision_slope) = slope(&collision_pairs) else {
            continue;
        };
        let stars_slope = slope(&stars_pairs);

        let collision_pct = 100.0 * collision_slope / mean_skill;
        collision_slopes.push(collision_pct);

        println!(
            "\n=== uid {uid} (n={}, collision share {lo:.2}-{hi:.2}, mean skill \
                 {mean_skill:.2}, {:.0}% EZ/HR)",
            rows.len(),
            window_mod_share * 100.0
        );
        println!(
            "  d(skill)/d(collision share) = {collision_slope:+.3}  \
                 ({collision_pct:+.1}% per +100pp collision share)"
        );
        match stars_slope {
            Some(stars_slope) => println!(
                "  d(skill)/d(stars)            = {stars_slope:+.3}  \
                     ({:+.1}% per +1 star, control)",
                100.0 * stars_slope / mean_skill
            ),
            None => println!("  d(skill)/d(stars)            = n/a (no star-rating spread)"),
        }
    }

    println!(
        "\n{} players excluded for fewer than 4 scores, {} for a collision-share range \
             under 0.15.",
        excluded_n, excluded_range
    );

    if collision_slopes.is_empty() {
        println!("\nno player had enough spread to compute a slope; nothing to summarise.");
        return;
    }

    let mut sorted = collision_slopes.clone();
    sorted.sort_by(f64::total_cmp);
    let mean = sorted.iter().sum::<f64>() / sorted.len() as f64;
    let median = sorted[sorted.len() / 2];
    let negative = sorted.iter().filter(|&&s| s < 0.0).count();
    let positive = sorted.iter().filter(|&&s| s > 0.0).count();

    println!(
        "\nOverall, {} qualifying players: mean collision slope {mean:+.1}% per +100pp, \
             median {median:+.1}% per +100pp, {negative} negative vs {positive} positive.",
        sorted.len()
    );

    // Characterising the stars trend on its own, as promised in the doc comment:
    // a per-player, per-bin table first, to see whether it is monotone or driven
    // by one bin, then a `skill_exponent` sweep to see whether the default 1.7 is
    // what is producing it.
    println!(
        "\nFitted skill by star-rating bin, per player (all scores, no mod filter). \
             A monotone climb across bins within a player is what a wrong \
             `skill_exponent` predicts; a single outlier bin would point elsewhere."
    );

    let star_bins: [(&str, f64, f64); 5] = [
        ("<5", f64::NEG_INFINITY, 5.0),
        ("5-6", 5.0, 6.0),
        ("6-7", 6.0, 7.0),
        ("7-8", 7.0, 8.0),
        (">=8", 8.0, f64::INFINITY),
    ];

    for (uid, rows) in &by_uid {
        print!("  uid {uid:<8}");
        for (label, lo, hi) in &star_bins {
            let group: Vec<&&Point> = rows
                .iter()
                .filter(|p| p.stars >= *lo && p.stars < *hi)
                .collect();
            if group.is_empty() {
                print!("  {label:>4}: n=0          ");
            } else {
                let mean_skill = group.iter().map(|p| p.skill).sum::<f64>() / group.len() as f64;
                print!("  {label:>4}: n={:<3} skill={mean_skill:6.2}", group.len());
            }
        }
        println!();
    }

    // Selection-artefact check: is the stars-vs-skill climb above real, or is it
    // what you'd see anyway from which scores got kept? The ladder fetch selects
    // scores with `acc between 88 and 99.5`, i.e. it holds *accuracy* in a band
    // rather than holding *skill* constant. If a player's own true skill is flat
    // across their star range but the band-selected accuracy is also roughly flat
    // across stars, then the error model has no choice but to fit rising skill at
    // higher `d` to explain "same accuracy at harder content" — that is
    // `sigma = sigma_ref * ((d + floor)/skill)^skill_exponent` doing exactly what
    // it is supposed to do, not a defect. The alternative outcome — accuracy
    // visibly falling with stars while fitted skill still climbs — would be the
    // genuine problem, since then the model is inventing ability gains the scores
    // themselves do not show.
    println!(
        "\nMean score accuracy by star-rating bin, per player (same bins, same scores). \
             Flat accuracy across bins alongside climbing fitted skill (above) says the \
             stars trend is survivorship — these are the scores that were kept, at roughly \
             fixed accuracy, not evidence the model invents skill gains. Falling accuracy \
             alongside climbing skill would be the actual model problem."
    );

    for (uid, rows) in &by_uid {
        print!("  uid {uid:<8}");
        for (label, lo, hi) in &star_bins {
            let group: Vec<&&Point> = rows
                .iter()
                .filter(|p| p.stars >= *lo && p.stars < *hi)
                .collect();
            if group.is_empty() {
                print!("  {label:>4}: n=0          ");
            } else {
                let mean_acc = group.iter().map(|p| p.accuracy).sum::<f64>() / group.len() as f64;
                print!(
                    "  {label:>4}: n={:<3} acc={:6.2}%",
                    group.len(),
                    mean_acc * 100.0
                );
            }
        }
        println!();
    }

    // `skill_exponent` sweep on the no-window-mod subset: refit every score's
    // skill under each candidate exponent (all other `ErrorModel` fields left at
    // their default) and re-run the joint regression, watching only the stars
    // coefficient. If some exponent other than the shipped 1.7 drives it toward
    // zero, that is this fixture set's evidence about the right value; if none do,
    // or the sweep is flat, that is itself the finding.
    println!(
        "\nskill_exponent sweep (no-window-mod scores, joint regression, stars \
             coefficient only):"
    );

    let mut best_exponent = None;
    let mut best_abs_t = f64::INFINITY;

    for exponent in [1.3, 1.5, 1.7, 1.9, 2.1] {
        let model = ErrorModel {
            skill_exponent: exponent,
            ..Default::default()
        };

        let mut refit_by_uid: BTreeMap<&str, Vec<(f64, f64, f64)>> = BTreeMap::new();
        let mut n_failed = 0;

        for p in &no_window_mod {
            match refit_skill(p, &model) {
                Some(skill) => refit_by_uid.entry(p.uid.as_str()).or_default().push((
                    p.collision_share,
                    p.stars,
                    skill,
                )),
                None => n_failed += 1,
            }
        }

        match pooled_joint(&refit_by_uid) {
            Some(fit) => {
                println!(
                    "  skill_exponent={exponent:.1}: n={:<4} n_players={}  \
                         b2(stars)={:+.4}  se={:.4}  t={:+.2}{}",
                    fit.n,
                    fit.n_players,
                    fit.b2,
                    fit.se2,
                    fit.t2,
                    if n_failed > 0 {
                        format!("  ({n_failed} refits failed)")
                    } else {
                        String::new()
                    }
                );
                if fit.t2.abs() < best_abs_t {
                    best_abs_t = fit.t2.abs();
                    best_exponent = Some(exponent);
                }
            }
            None => println!(
                "  skill_exponent={exponent:.1}: joint regression skipped (det near zero \
                     or not enough spread)"
            ),
        }
    }

    match best_exponent {
        Some(exponent) => println!(
            "\nOf {{1.3, 1.5, 1.7, 1.9, 2.1}}, skill_exponent={exponent:.1} drives the stars \
                 coefficient closest to zero on this fixture set (|t|={best_abs_t:.2})."
        ),
        None => println!(
            "\nno exponent in the sweep produced a joint fit; nothing to conclude about \
                 skill_exponent from this fixture set."
        ),
    }
}

/// A named keymode group and the predicate selecting it, for report cross-tabs.
type KeymodeGroup = (&'static str, fn(usize) -> bool);

/// A map's star rating, keymode, and per-note `(difficulty, hold duration if long)`.
type PerNoteDifficulty = (f64, usize, Vec<(f64, Option<f64>)>);

/// Same-column gap in ms for each note of `data`, indexed like `data.notes`.
///
/// The gap is measured from the *end* of the previous note in the column — its tail for
/// a hold, its head otherwise — because that is when the finger becomes free to travel,
/// which is the quantity [`ErrorModel::recovery_mean_offset`] was fitted against.
/// `f64::INFINITY` for a column's first note, which has no predecessor to recover from.
///
/// `notes_by_column` is already in head order, since `RebirthData::new` sorts `notes`
/// before distributing them.
fn same_column_gaps(data: &RebirthData) -> Vec<f64> {
    // Index notes by identity so the per-column walk can write back into map order.
    let mut position: HashMap<(u64, usize), usize> = HashMap::new();

    for (idx, note) in data.notes.iter().enumerate() {
        position.insert((note.head.to_bits(), note.column), idx);
    }

    let mut gaps = vec![f64::INFINITY; data.notes.len()];

    for column in &data.notes_by_column {
        for pair in column.windows(2) {
            let (previous, note) = (pair[0], pair[1]);
            let previous_end = previous.tail.unwrap_or(previous.head);

            if let Some(&idx) = position.get(&(note.head.to_bits(), note.column)) {
                gaps[idx] = (note.head - previous_end).max(0.0);
            }
        }
    }

    gaps
}

/// Per-note local difficulty for one map: `(d_all_at_head, hold_duration_ms)` per note.
///
/// The `d_all` expression is copied from `calculate_from_data` rather than exposed by
/// refactoring production code, and the head lookup uses the same
/// `lower_bound(all_corners, head)` as `compute_switches`. That lookup is exact on
/// every fixture map -- 1173541/1173541 heads, max mismatch 0 ms -- so no
/// interpolation is involved.
///
/// Returns `(sr, keymode, per-note (difficulty, hold duration if long))`.
fn per_note_difficulty(map: &Beatmap) -> Option<PerNoteDifficulty> {
    let total_columns = map.cs.round_ties_even().max(1.0) as usize;
    let (notes, _) = build_notes(1.0, map.hit_objects.iter(), total_columns);

    if notes.len() < 2 || total_columns == 0 {
        return None;
    }

    let windows = hit_windows(map, &GameMods::default(), 1.0, false);
    let great_hit_window = get_hit_window_300(map, 1.0, false, false);
    let hit_leniency = hit_leniency_from_window(great_hit_window);
    let data = RebirthData::new(notes, total_columns, hit_leniency, windows.good);

    if data.all_corners.len() < 2 {
        return None;
    }

    let key_usage = get_key_usage(&data);
    let active_columns: Vec<_> = (0..data.base_corners.len())
        .map(|idx| {
            (0..data.total_columns)
                .filter(|&column| key_usage[column][idx])
                .collect::<Vec<_>>()
        })
        .collect();
    let key_usage_400 = get_key_usage_400(&data);
    let anchor = compute_anchor(&key_usage_400);
    let (delta_by_column, jbar_base) = compute_jbar(&data);
    let jbar = interp_values(&data.all_corners, &data.base_corners, &jbar_base);
    let xbar_base = compute_xbar(&data, &active_columns);
    let xbar = interp_values(&data.all_corners, &data.base_corners, &xbar_base);
    let ln_rep = LongNoteBodyRepresentation::new(&data.long_notes, data.t_end);
    let pbar_base = compute_pbar(&data, &ln_rep, &anchor);
    let pbar = interp_values(&data.all_corners, &data.base_corners, &pbar_base);
    let abar_awkwardness = compute_abar(&data, &active_columns, &delta_by_column);
    let abar = interp_values(
        &data.all_corners,
        &data.awkwardness_corners,
        &abar_awkwardness,
    );
    let rbar_base = compute_rbar(&data);
    let rbar = interp_values(&data.all_corners, &data.base_corners, &rbar_base);
    let (density_base, _density_v2_base, keys_base) = compute_density_and_keys(&data, &key_usage);
    let density = step_interp(&data.all_corners, &data.base_corners, &density_base);
    let keys = step_interp(&data.all_corners, &data.base_corners, &keys_base);

    let d_all: Vec<f64> = (0..data.all_corners.len())
        .map(|idx| {
            let s_all = (0.4
                * (abar[idx].powf(3.0 / keys[idx]) * jbar[idx].min(8.0 + 0.85 * jbar[idx]))
                    .powf(1.5)
                + (1.0 - 0.4)
                    * (abar[idx].powf(2.0 / 3.0)
                        * (0.8 * pbar[idx] + rbar[idx] * release_density_weight(density[idx])))
                    .powf(1.5))
            .powf(2.0 / 3.0);
            let t_all = (abar[idx].powf(3.0 / keys[idx]) * xbar[idx]) / (xbar[idx] + s_all + 1.0);

            2.7 * s_all.powf(0.5) * t_all.powf(1.5) + s_all * 0.27
        })
        .collect();

    let per_note: Vec<(f64, Option<f64>)> = data
        .notes
        .iter()
        .map(|note| {
            let idx = lower_bound(&data.all_corners, note.head).min(data.all_corners.len() - 1);

            (d_all[idx], note.tail.map(|tail| tail - note.head))
        })
        .collect();

    let attrs = calculate(map, &GameMods::default(), 1.0, Some(false), None)?;

    Some((attrs.stars, total_columns, per_note))
}

/// What it costs to feed per-note difficulty into the surface, and how few bins that
/// needs.
///
/// Motivation: per-note `d_all` takes 105-895 distinct values per map (mean 415 at
/// 0.01 rounding), so one [`JudgementUnit`] per distinct value would multiply the
/// fit's inner loop by two to three orders of magnitude. But the collapse is *exact*,
/// not approximate. With `sigma_floor = 0`,
///
/// ```text
/// sigma = sigma_ref * scale * ((d + floor)/skill)^p
///       = sigma_ref * ((d_eff + floor)/skill)^p,
///   where d_eff = (d + floor) * scale^(1/p) - floor
/// ```
///
/// so a unit's `(difficulty, sigma_scale)` pair is indistinguishable from a plain unit
/// at `d_eff`. Two-dimensional structure (per-note difficulty x LN hold duration) is
/// therefore one-dimensional per distinct `mean_offset`, of which the model has
/// exactly two: rice at 0 and releases at [`ErrorModel::release_mean_offset`].
///
/// This measures whether quantising that axis into a fixed number of bins is
/// detectable. Ground truth is the exact per-note unit list; counts are generated from
/// it at a known skill, then refitted with the binned list. The error reported is in
/// *skill*, and in pp via the `^2.2` that skill enters pricing through, since a skill
/// error that survives the exponent is the only kind that matters.
///
/// `uniform` is the shipped [`judgement_units`] behaviour -- one unit at the map's
/// `sr` -- and is the baseline the change has to beat.
///
/// Run with `cargo test --release per_note_binning_cost -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn per_note_binning_cost() {
    use crate::mania::sunny_accuracy::{
        JudgementUnit, expected_counts, ln_sigma_scale_for_duration, skill_for_counts,
    };
    use std::fs;
    use std::time::Instant;

    const BIN_COUNTS: [usize; 4] = [4, 8, 16, 32];
    /// Bin counts swept for the candidate *attribute* representation measured below.
    /// If its error stops falling as these rise, the residual is the mean-hold-duration
    /// substitution rather than the difficulty binning.
    const ATTRIBUTE_BINS: [usize; 4] = [8, 12, 16, 24];
    const TRUE_SKILLS: [f64; 3] = [6.0, 10.0, 18.0];
    /// Every 6th map, so the report covers the whole fixture set in a runnable time.
    const MAP_STRIDE: usize = 6;

    let model = ErrorModel::default();

    // `d_eff`: the plain-unit difficulty that reproduces a scaled unit exactly.
    let effective_difficulty = |difficulty: f64, sigma_scale: f64| -> f64 {
        let floor = model.difficulty_floor;

        ((difficulty.max(0.0) + floor) * sigma_scale.powf(1.0 / model.skill_exponent) - floor)
            .max(0.0)
    };

    /// Exact unit list: one unit per distinct `(d_eff, mean_offset)`, weights summed.
    /// Identical to one unit per note, but without paying for duplicates.
    fn dedup(pairs: &[(f64, f64)]) -> Vec<JudgementUnit> {
        let mut map: HashMap<(u64, u64), f64> = HashMap::new();

        for &(d_eff, offset) in pairs {
            *map.entry((d_eff.to_bits(), offset.to_bits()))
                .or_insert(0.0) += 1.0;
        }

        map.into_iter()
            .map(|((d_bits, offset_bits), weight)| JudgementUnit {
                difficulty: f64::from_bits(d_bits),
                weight,
                sigma_scale: 1.0,
                mean_offset: f64::from_bits(offset_bits),
                fading_mean_offset: 0.0,
            })
            .collect()
    }

    /// The candidate *attribute* representation, in units.
    ///
    /// Unlike [`quantile_bins`], this bins on **raw `d`** and cannot fold hold duration
    /// into the difficulty axis, because a difficulty attribute is cached per map and
    /// must not depend on [`ErrorModel`] parameters. So it carries, per bin, the mean
    /// `d`, how many of the bin's notes are rice, how many are long, and the long
    /// notes' *mean hold duration* — and the model is applied later, here, exactly as
    /// `judgement_units` would.
    ///
    /// That mean duration is the second approximation under test: it stands in for
    /// every hold in the bin, where [`quantile_bins`] knew each one exactly.
    ///
    /// Equal-count bins make the per-bin total implicit, so only the shape has to be
    /// stored: `[(f64, u32, u32, f64); 12]` is 288 bytes, and this also subsumes
    /// `ln_duration_buckets`, which becomes redundant.
    fn attribute_shaped(
        per_note: &[(f64, Option<f64>)],
        bins: usize,
        model: &ErrorModel,
    ) -> Vec<JudgementUnit> {
        let mut sorted: Vec<(f64, Option<f64>)> = per_note.to_vec();
        sorted.sort_by(|a, b| a.0.total_cmp(&b.0));

        let n = sorted.len();
        let mut units = Vec::with_capacity(bins * 2);

        for bin in 0..bins {
            let start = bin * n / bins;
            let end = ((bin + 1) * n / bins).max(start);

            if end == start {
                continue;
            }

            let slice = &sorted[start..end];
            let mean_difficulty = slice.iter().map(|&(d, _)| d).sum::<f64>() / slice.len() as f64;

            let holds: Vec<f64> = slice.iter().filter_map(|&(_, hold)| hold).collect();
            let rice = slice.len() - holds.len();

            if rice > 0 {
                units.push(JudgementUnit::repeated(mean_difficulty, rice as f64));
            }

            if !holds.is_empty() {
                let mean_duration = holds.iter().sum::<f64>() / holds.len() as f64;

                units.push(JudgementUnit::long_note(
                    mean_difficulty,
                    holds.len() as f64,
                    model,
                    mean_duration,
                ));
            }
        }

        units
    }

    /// Equal-count (quantile) bins on `d_eff`, one unit per bin at the bin's mean.
    /// Applied separately per `mean_offset` population, since those cannot merge.
    fn quantile_bins(pairs: &[(f64, f64)], bins: usize) -> Vec<JudgementUnit> {
        let mut units = Vec::new();

        for &offset in &[0.0, ErrorModel::default().release_mean_offset] {
            let mut population: Vec<f64> = pairs
                .iter()
                .filter(|&&(_, o)| o == offset)
                .map(|&(d, _)| d)
                .collect();

            if population.is_empty() {
                continue;
            }

            population.sort_by(f64::total_cmp);

            let n = population.len();

            for bin in 0..bins {
                let start = bin * n / bins;
                let end = ((bin + 1) * n / bins).max(start);

                if end == start {
                    continue;
                }

                let slice = &population[start..end];
                let mean = slice.iter().sum::<f64>() / slice.len() as f64;

                units.push(JudgementUnit {
                    difficulty: mean,
                    weight: slice.len() as f64,
                    sigma_scale: 1.0,
                    mean_offset: offset,
                    fading_mean_offset: 0.0,
                });
            }
        }

        units
    }

    let Ok(entries) = fs::read_dir("local-fixtures/maps") else {
        println!("no fixture maps present; nothing to report");
        return;
    };

    let mut paths: Vec<_> = entries
        .flatten()
        .map(|entry| entry.path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "osu"))
        .collect();
    paths.sort();

    // skill errors, indexed by BIN_COUNTS position; plus the uniform baseline.
    // Split on whether the *exact* refit recovered the skill that generated the
    // counts: where it did not, the likelihood has saturated and every list is
    // refitting a plateau, so a disagreement there is not a binning error.
    let mut binned_skill_error: Vec<Vec<f64>> = vec![Vec::new(); BIN_COUNTS.len()];
    let mut uniform_skill_error: Vec<f64> = Vec::new();
    let mut binned_error_saturated: Vec<Vec<f64>> = vec![Vec::new(); BIN_COUNTS.len()];
    let mut uniform_error_saturated: Vec<f64> = Vec::new();
    let mut attribute_skill_error: Vec<Vec<f64>> = vec![Vec::new(); ATTRIBUTE_BINS.len()];
    let mut attribute_unit_counts: Vec<Vec<f64>> = vec![Vec::new(); ATTRIBUTE_BINS.len()];
    let mut attribute_fit_micros: Vec<Vec<f64>> = vec![Vec::new(); ATTRIBUTE_BINS.len()];
    let mut saturated_fits = 0usize;
    let mut identified_fits = 0usize;
    let mut full_unit_counts: Vec<usize> = Vec::new();
    let mut full_fit_micros: Vec<f64> = Vec::new();
    let mut binned_fit_micros: Vec<f64> = Vec::new();
    let mut uniform_fit_micros: Vec<f64> = Vec::new();
    let mut maps_measured = 0usize;

    for path in paths.iter().step_by(MAP_STRIDE) {
        let Ok(bytes) = fs::read(path) else { continue };
        let Ok(map) = Beatmap::from_bytes(&bytes) else {
            continue;
        };

        if map.mode != GameMode::Mania {
            continue;
        }

        let Some((sr, _keys, per_note)) = per_note_difficulty(&map) else {
            continue;
        };

        let windows = hit_windows(&map, &GameMods::default(), 1.0, false);
        let total = per_note.len() as f64;

        // Per-note (d_eff, mean_offset). A ScoreV1 long note is one judgement whose
        // spread is widened by hold duration, which `effective_difficulty` folds into
        // the difficulty axis.
        let pairs: Vec<(f64, f64)> = per_note
            .iter()
            .map(|&(difficulty, duration)| match duration {
                Some(duration) => {
                    let scale = ln_sigma_scale_for_duration(&model, duration);

                    (
                        effective_difficulty(difficulty, scale),
                        model.release_mean_offset,
                    )
                }
                None => (effective_difficulty(difficulty, 1.0), 0.0),
            })
            .collect();

        let full = dedup(&pairs);
        let uniform = vec![JudgementUnit::repeated(sr, total)];
        let binned: Vec<Vec<JudgementUnit>> = BIN_COUNTS
            .iter()
            .map(|&bins| quantile_bins(&pairs, bins))
            .collect();
        let attributes: Vec<Vec<JudgementUnit>> = ATTRIBUTE_BINS
            .iter()
            .map(|&bins| attribute_shaped(&per_note, bins, &model))
            .collect();

        for (index, units) in attributes.iter().enumerate() {
            attribute_unit_counts[index].push(units.len() as f64);
        }

        full_unit_counts.push(full.len());
        maps_measured += 1;

        for &true_skill in &TRUE_SKILLS {
            // Ground truth counts, generated by the exact per-note list.
            let counts =
                expected_counts(&full, &windows, &model, true_skill).round_to_hits(total as u32);

            let started = Instant::now();
            let full_skill = skill_for_counts(&counts, &full, &windows, &model);
            full_fit_micros.push(started.elapsed().as_secs_f64() * 1e6);

            // Whether the exact list recovered the skill that generated the counts. If
            // it did not, the likelihood is flat here (see `SKILL_SATURATION_RATIO`)
            // and no unit list can be scored against another on this point.
            let identified = (full_skill / true_skill - 1.0).abs() <= 0.02;

            if identified {
                identified_fits += 1;
            } else {
                saturated_fits += 1;
            }

            for (index, units) in binned.iter().enumerate() {
                let started = Instant::now();
                let skill = skill_for_counts(&counts, units, &windows, &model);
                let elapsed = started.elapsed().as_secs_f64() * 1e6;

                if index == BIN_COUNTS.len() - 1 {
                    binned_fit_micros.push(elapsed);
                }

                let error = skill / full_skill - 1.0;

                if identified {
                    binned_skill_error[index].push(error);
                } else {
                    binned_error_saturated[index].push(error);
                }
            }

            for (index, units) in attributes.iter().enumerate() {
                let started = Instant::now();
                let skill = skill_for_counts(&counts, units, &windows, &model);
                attribute_fit_micros[index].push(started.elapsed().as_secs_f64() * 1e6);

                if identified {
                    attribute_skill_error[index].push(skill / full_skill - 1.0);
                }
            }

            let started = Instant::now();
            let uniform_skill = skill_for_counts(&counts, &uniform, &windows, &model);
            uniform_fit_micros.push(started.elapsed().as_secs_f64() * 1e6);

            let uniform_error = uniform_skill / full_skill - 1.0;

            if identified {
                uniform_skill_error.push(uniform_error);
            } else {
                uniform_error_saturated.push(uniform_error);
            }
        }
    }

    if maps_measured == 0 {
        println!("no mania fixture maps measured");
        return;
    }

    fn summarise(label: &str, errors: &[f64]) {
        let mut absolute: Vec<f64> = errors.iter().map(|error| error.abs()).collect();
        absolute.sort_by(f64::total_cmp);

        let mean_signed = errors.iter().sum::<f64>() / errors.len() as f64;
        let percentile = |q: f64| absolute[((absolute.len() - 1) as f64 * q).round() as usize];
        let max = *absolute.last().unwrap_or(&0.0);

        // Skill enters pricing as `skill^2.2`, so this is what the error is worth.
        let pp_max = (1.0 + max).powf(2.2) - 1.0;

        println!(
            "  {label:<12} |err| p50={:.4}%  p90={:.4}%  max={:.4}%   signed mean={:+.4}%   \
                 max pp impact={:+.3}%",
            percentile(0.5) * 100.0,
            percentile(0.9) * 100.0,
            max * 100.0,
            mean_signed * 100.0,
            pp_max * 100.0
        );
    }

    fn mean(values: &[f64]) -> f64 {
        if values.is_empty() {
            return 0.0;
        }

        values.iter().sum::<f64>() / values.len() as f64
    }

    println!(
        "\n{maps_measured} maps (every {MAP_STRIDE}th fixture) x {} skills, refit against \
             counts the exact per-note unit list generated.\n",
        TRUE_SKILLS.len()
    );

    let mut sorted_units = full_unit_counts.clone();
    sorted_units.sort_unstable();

    println!(
        "EXACT UNIT COUNT (distinct d_eff, both offsets): min={} p50={} max={} mean={:.1}",
        sorted_units.first().copied().unwrap_or(0),
        sorted_units[sorted_units.len() / 2],
        sorted_units.last().copied().unwrap_or(0),
        mean(
            &full_unit_counts
                .iter()
                .map(|&c| c as f64)
                .collect::<Vec<_>>()
        )
    );

    println!(
        "\nSKILL ERROR vs the exact per-note fit, on the {identified_fits} points where the \
             exact refit recovered the generating skill within 2%"
    );

    for (index, &bins) in BIN_COUNTS.iter().enumerate() {
        summarise(&format!("{bins} bins"), &binned_skill_error[index]);
    }

    for (index, &bins) in ATTRIBUTE_BINS.iter().enumerate() {
        summarise(&format!("attr {bins}b"), &attribute_skill_error[index]);
    }

    summarise("uniform(sr)", &uniform_skill_error);

    println!(
        "\n  attr = the shippable form: bins raw d, applies the model later, and stands one \
             mean hold duration in for every hold in a bin."
    );

    for (index, &bins) in ATTRIBUTE_BINS.iter().enumerate() {
        println!(
            "    attr {bins}b: mean {:.1} units/map, {:.0}us/fit",
            mean(&attribute_unit_counts[index]),
            mean(&attribute_fit_micros[index])
        );
    }

    println!(
        "\nthe other {saturated_fits} points, where the likelihood had saturated and the exact \
             refit did not recover its own generating skill (no list can be judged here)"
    );

    for (index, &bins) in BIN_COUNTS.iter().enumerate() {
        if binned_error_saturated[index].is_empty() {
            continue;
        }

        summarise(&format!("{bins} bins"), &binned_error_saturated[index]);
    }

    if !uniform_error_saturated.is_empty() {
        summarise("uniform(sr)", &uniform_error_saturated);
    }

    println!(
        "\nFIT COST per skill_for_counts call: exact={:.0}us  32 bins={:.0}us  \
             uniform={:.0}us",
        mean(&full_fit_micros),
        mean(&binned_fit_micros),
        mean(&uniform_fit_micros)
    );
}

/// What per-note difficulty does to the fit and to pp on **real scores**.
///
/// `per_note_binning_cost` establishes only that the two unit lists disagree, since
/// there the per-note list is ground truth by construction. This is the test that can
/// be wrong: it grades both lists against the real ppy.sb scores in
/// `local-fixtures/multiuser.tsv`, where the counts came from a human and neither list
/// is privileged.
///
/// `g_timing` is the discriminator -- the timing-channel G statistic, measuring how far
/// the fitted judgement distribution lands from the observed counts. It does not grow
/// with map length (`per_judgement_g_falls_with_length_and_is_not_a_threshold` pins
/// that), so a mean over scores is meaningful, and *lower is better*.
///
/// pp is reported separately because a fit improvement need not move pricing:
/// [`window_scalar`] is a *ratio* of two fits against different windows, and whatever
/// the unit list does to both cancels out of it. Reporting both is what distinguishes
/// "the model describes players better" from "players get different numbers".
///
/// Run with
/// `cargo test --release per_note_difficulty_on_real_scores -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn per_note_difficulty_on_real_scores() {
    use crate::mania::sunny_accuracy::expected_counts;
    use crate::mania::sunny_windows::ManiaJudgement;

    struct Compared {
        map_id: String,
        mods: String,
        keys: u32,
        ln_fraction: f64,
        acc: f64,
        /// `p90/p50` of the map's per-note difficulty, from its own bins. The width of
        /// the distribution the change is *about*: at a ratio near 1 the two unit lists
        /// describe the same map and nothing should move.
        spread: f64,
        uniform_g: f64,
        per_note_g: f64,
        uniform_skill: f64,
        per_note_skill: f64,
        uniform_scalar: f64,
        per_note_scalar: f64,
        uniform_plausible: bool,
        per_note_plausible: bool,
        bins_present: bool,
        unit_count: usize,
    }

    let Ok(text) = std::fs::read_to_string("local-fixtures/multiuser.tsv") else {
        println!("no fixtures present (local-fixtures/multiuser.tsv); nothing to report");
        return;
    };

    let model = ErrorModel::default();
    let mut rows = Vec::new();

    for line in text.lines() {
        let f: Vec<&str> = line.split('\t').collect();

        if f.len() < 18 || f[0] == "uid" {
            continue;
        }

        let u = |s: &str| s.parse::<u32>().unwrap_or(0);
        let counts = [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])];
        let total = counts.iter().sum::<u32>();

        if total == 0 {
            continue;
        }

        let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", f[2])) else {
            continue;
        };

        let (mods, clock_rate) = mods_for(f[3]);

        // `lazer: false` -- these are stable ppy.sb scores, and getting this wrong
        // silently reclassifies every long note's judgement regime.
        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(false), None) else {
            continue;
        };

        let reference = reference_windows(&attrs);

        // The two unit lists, from the same attributes, differing only in whether the
        // per-note difficulty distribution is used.
        let uniform_units = judgement_units(&attrs, f64::from(total), &model, false);
        let per_note_units = judgement_units(&attrs, f64::from(total), &model, true);

        let uniform_fit = fit_with_quality(&counts, &uniform_units, &attrs.hit_windows, &model);
        let per_note_fit = fit_with_quality(&counts, &per_note_units, &attrs.hit_windows, &model);
        let uniform_reference = fit_with_quality(&counts, &uniform_units, &reference, &model);
        let per_note_reference = fit_with_quality(&counts, &per_note_units, &reference, &model);

        let scalar = |played: f64, reference: f64| {
            if played > 0.0 && reference > 0.0 {
                played / reference
            } else {
                1.0
            }
        };

        // The per-note list must still emit weights summing to what was observed, or
        // the fit is being handed a different score than the player played.
        let emitted = expected_counts(&per_note_units, &attrs.hit_windows, &model, 10.0);
        let emitted_total: f64 = ManiaJudgement::ALL
            .iter()
            .map(|&judgement| emitted.get(judgement))
            .sum();

        assert!(
            (emitted_total - f64::from(total)).abs() < 1e-6,
            "map {}: per-note units emit {emitted_total} judgements for a {total}-hit score",
            f[2]
        );

        // Equal-count bins, so bin 14 of 16 is the p90 of the distribution and bin 8
        // is the p50 -- the spread is readable straight off the bins.
        let spread = attrs
            .note_difficulty_bins
            .map(|bins| {
                let p50 = bins[NOTE_DIFFICULTY_BINS / 2].difficulty;
                let p90 = bins[(NOTE_DIFFICULTY_BINS * 9) / 10].difficulty;

                if p50 > 0.0 { p90 / p50 } else { 1.0 }
            })
            .unwrap_or(1.0);

        rows.push(Compared {
            map_id: f[2].to_owned(),
            mods: f[3].to_owned(),
            keys: u(f[6]),
            ln_fraction: if attrs.n_objects > 0 {
                attrs.n_long_notes as f64 / attrs.n_objects as f64
            } else {
                0.0
            },
            acc: f[13].parse().unwrap_or(0.0),
            spread,
            uniform_g: uniform_fit.g_timing,
            per_note_g: per_note_fit.g_timing,
            uniform_skill: uniform_fit.skill,
            per_note_skill: per_note_fit.skill,
            uniform_scalar: scalar(uniform_fit.skill, uniform_reference.skill),
            per_note_scalar: scalar(per_note_fit.skill, per_note_reference.skill),
            uniform_plausible: uniform_fit.is_plausible(),
            per_note_plausible: per_note_fit.is_plausible(),
            bins_present: attrs.note_difficulty_bins.is_some(),
            unit_count: per_note_units.len(),
        });
    }

    if rows.is_empty() {
        println!("no fixture scores loaded");
        return;
    }

    fn median(values: &[f64]) -> f64 {
        if values.is_empty() {
            return f64::NAN;
        }

        let mut sorted = values.to_vec();
        sorted.sort_by(f64::total_cmp);

        sorted[sorted.len() / 2]
    }

    fn mean(values: &[f64]) -> f64 {
        if values.is_empty() {
            return f64::NAN;
        }

        values.iter().sum::<f64>() / values.len() as f64
    }

    /// Fit quality and pricing for one group, both ways.
    fn report(label: &str, group: &[&Compared]) {
        if group.is_empty() {
            return;
        }

        let uniform_g: Vec<f64> = group
            .iter()
            .map(|row| row.uniform_g)
            .filter(|g| g.is_finite())
            .collect();
        let per_note_g: Vec<f64> = group
            .iter()
            .map(|row| row.per_note_g)
            .filter(|g| g.is_finite())
            .collect();

        // Per-score, so an improvement is not an artefact of one group mean moving.
        let improved = group
            .iter()
            .filter(|row| row.per_note_g.is_finite() && row.uniform_g.is_finite())
            .filter(|row| row.per_note_g < row.uniform_g)
            .count();
        let comparable = group
            .iter()
            .filter(|row| row.per_note_g.is_finite() && row.uniform_g.is_finite())
            .count();

        let scalar_delta: Vec<f64> = group
            .iter()
            .filter(|row| row.uniform_scalar > 0.0)
            .map(|row| row.per_note_scalar / row.uniform_scalar - 1.0)
            .collect();
        // pp moves as scalar^2.2, since the scalar multiplies fitted skill.
        let pp_delta: Vec<f64> = scalar_delta
            .iter()
            .map(|delta| (1.0 + delta).powf(2.2) - 1.0)
            .collect();
        let skill_delta: Vec<f64> = group
            .iter()
            .filter(|row| row.uniform_skill > 0.0)
            .map(|row| row.per_note_skill / row.uniform_skill - 1.0)
            .collect();

        println!(
            "  {label:<18} n={:<4} g_timing mean {:>7.2} -> {:>7.2}  median {:>6.2} -> {:>6.2}  \
                 improved {improved}/{comparable}",
            group.len(),
            mean(&uniform_g),
            mean(&per_note_g),
            median(&uniform_g),
            median(&per_note_g),
        );
        println!(
            "  {:<18}      skill {:+.2}% median   scalar {:+.3}% median   pp {:+.2}% median, \
                 {:+.2}% mean   plausible {} -> {}",
            "",
            median(&skill_delta) * 100.0,
            median(&scalar_delta) * 100.0,
            median(&pp_delta) * 100.0,
            mean(&pp_delta) * 100.0,
            group.iter().filter(|row| row.uniform_plausible).count(),
            group.iter().filter(|row| row.per_note_plausible).count(),
        );
    }

    let missing_bins = rows.iter().filter(|row| !row.bins_present).count();

    println!(
        "\n{} real scores, {} carrying a per-note distribution ({missing_bins} fell back to \
             uniform). Per-note lists average {:.1} units/score.",
        rows.len(),
        rows.len() - missing_bins,
        mean(
            &rows
                .iter()
                .map(|row| row.unit_count as f64)
                .collect::<Vec<_>>()
        )
    );
    println!("\ng_timing: lower is better. pp delta is per-note relative to uniform.\n");

    let all: Vec<&Compared> = rows.iter().collect();
    report("all", &all);

    println!();

    for (label, keys) in [("4K", 4u32), ("7K", 7)] {
        let group: Vec<&Compared> = rows.iter().filter(|row| row.keys == keys).collect();
        report(label, &group);
    }

    println!();

    // The axis the change is expected to act on: per-note spread is widest on rice
    // charts (p90/p50 1.64) and narrowest on LN-saturated ones (1.21), so if the
    // mechanism is real the two ends should not move alike.
    for (label, low, high) in [
        ("LN <15%", 0.0, 0.15),
        ("LN 15-35%", 0.15, 0.35),
        ("LN >35%", 0.35, 1.01),
    ] {
        let group: Vec<&Compared> = rows
            .iter()
            .filter(|row| row.ln_fraction >= low && row.ln_fraction < high)
            .collect();
        report(label, &group);
    }

    println!();

    // The falsification test. A per-note list has ~23 units against uniform's 1, so it
    // predicts a smoother judgement distribution and could post a lower `g_timing` for
    // that reason alone, with no bearing on whether per-note difficulty is real. If the
    // mechanism *is* real the gain has to track the width of the distribution being
    // resolved: near-uniform maps must not move, and the widest must move most. A flat
    // profile down this table means the improvement is a smoothing artefact.
    for (label, low, high) in [
        ("spread <1.2", 0.0, 1.2),
        ("spread 1.2-1.5", 1.2, 1.5),
        ("spread 1.5-2.0", 1.5, 2.0),
        ("spread >2.0", 2.0, f64::INFINITY),
    ] {
        let group: Vec<&Compared> = rows
            .iter()
            .filter(|row| row.spread >= low && row.spread < high)
            .collect();
        report(label, &group);
    }

    // The regressions, since the LN group's mean got worse while its median improved.
    let mut worst: Vec<&Compared> = rows
        .iter()
        .filter(|row| row.per_note_g.is_finite() && row.uniform_g.is_finite())
        .collect();
    worst.sort_by(|a, b| (b.per_note_g - b.uniform_g).total_cmp(&(a.per_note_g - a.uniform_g)));

    println!("\nWORST 10 REGRESSIONS (g_timing rose most)");
    println!(
        "  {:>8} {:>9} {:>3} {:>6} {:>7} {:>7} {:>8} {:>8} {:>7}",
        "map", "mods", "k", "LN%", "spread", "acc%", "g unif", "g pnote", "skill%"
    );

    for row in worst.iter().take(10) {
        println!(
            "  {:>8} {:>9} {:>3} {:>6.1} {:>7.3} {:>7.2} {:>8.2} {:>8.2} {:>+7.2}",
            row.map_id,
            if row.mods.is_empty() { "-" } else { &row.mods },
            row.keys,
            row.ln_fraction * 100.0,
            row.spread,
            row.acc,
            row.uniform_g,
            row.per_note_g,
            if row.uniform_skill > 0.0 {
                (row.per_note_skill / row.uniform_skill - 1.0) * 100.0
            } else {
                0.0
            }
        );
    }

    println!("\nBEST 10 IMPROVEMENTS (g_timing fell most)");

    for row in worst.iter().rev().take(10) {
        println!(
            "  {:>8} {:>9} {:>3} {:>6.1} {:>7.3} {:>7.2} {:>8.2} {:>8.2} {:>+7.2}",
            row.map_id,
            if row.mods.is_empty() { "-" } else { &row.mods },
            row.keys,
            row.ln_fraction * 100.0,
            row.spread,
            row.acc,
            row.uniform_g,
            row.per_note_g,
            if row.uniform_skill > 0.0 {
                (row.per_note_skill / row.uniform_skill - 1.0) * 100.0
            } else {
                0.0
            }
        );
    }
}

/// Does a measured mean offset move pp, where every width parameter does not?
///
/// This is the question the whole bias line rests on. `tools/input_state.py` measured a
/// gap-driven timing offset on 629,418 replay notes (see
/// [`ErrorModel::recovery_offset`]), and the reason it is interesting is structural: the
/// per-score `skill` enters only through `sigma`, so it absorbs any width change
/// exactly, while it cannot move a mean. So a bias is the one model input that should
/// reach pricing.
///
/// "Should" is doing work there, and [`ErrorModel::release_mean_offset`]'s own note
/// records the trap: sweeping *that* offset moved only the reference-side fit, so its
/// entire effect landed in the denominator of `window_scalar`'s
/// `played.skill / reference.skill` and *lowered* pp on exactly the maps it was meant to
/// raise. If this channel does the same thing, it is worth nothing no matter how well
/// measured the curve is.
///
/// The reason to expect otherwise, stated in advance so the measurement can refute it:
/// `release_mean_offset` reaches only long-note units, so on a mostly-rice map it is
/// diluted to nothing on the played side, whereas a gap offset applies to every note.
///
/// Reports both fits separately rather than only the ratio, so a ratio artefact is
/// visible as such instead of appearing as a null result.
///
/// Run with
/// `cargo test --release does_a_mean_offset_move_pp -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn does_a_mean_offset_move_pp() {
    use crate::mania::sunny_accuracy::ln_sigma_scale_for_duration;
    use rayon::join;
    use rayon::prelude::*;
    use std::sync::Mutex;

    let Ok(text) = std::fs::read_to_string("local-fixtures/multiuser.tsv") else {
        println!("no fixtures present (local-fixtures/multiuser.tsv); nothing to report");
        return;
    };

    // Sweep the amplitude to find what the count data actually supports. Keep the
    // replay-fitted tau fixed and scale the long-gap plateau with the amplitude:
    // holding -3.19 ms fixed while shrinking only the positive term moves the zero
    // crossing and tests a different curve shape, not a smaller version of the
    // measured recovery bias.
    const FITTED_AMPLITUDE: f64 = 73.12;
    const FITTED_PLATEAU: f64 = -3.19;
    let candidates = [
        ("off (control)", 0.0, 0.0),
        ("A = 5 ms", 5.0, FITTED_PLATEAU * 5.0 / FITTED_AMPLITUDE),
        ("A = 10 ms", 10.0, FITTED_PLATEAU * 10.0 / FITTED_AMPLITUDE),
        ("A = 15 ms", 15.0, FITTED_PLATEAU * 15.0 / FITTED_AMPLITUDE),
        ("A = 20 ms", 20.0, FITTED_PLATEAU * 20.0 / FITTED_AMPLITUDE),
        ("A = 30 ms", 30.0, FITTED_PLATEAU * 30.0 / FITTED_AMPLITUDE),
        ("A = 50 ms", 50.0, FITTED_PLATEAU * 50.0 / FITTED_AMPLITUDE),
        ("A = 73.12 ms", FITTED_AMPLITUDE, FITTED_PLATEAU),
    ];

    struct Row {
        keys: u32,
        ln_fraction: f64,
        /// Median same-column gap, the axis the offset acts on.
        median_gap: f64,
        played_g: Vec<f64>,
        reference_g: Vec<f64>,
        played_skill: Vec<f64>,
        reference_skill: Vec<f64>,
        scalar: Vec<f64>,
    }

    let mut rows: Vec<Row> = Vec::new();

    for line in text.lines() {
        let f: Vec<&str> = line.split('\t').collect();

        if f.len() < 18 || f[0] == "uid" {
            continue;
        }

        let u = |s: &str| s.parse::<u32>().unwrap_or(0);
        let counts = [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])];
        let total = counts.iter().sum::<u32>();

        if total == 0 {
            continue;
        }

        let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", f[2])) else {
            continue;
        };

        let (mods, clock_rate) = mods_for(f[3]);
        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(false), None) else {
            continue;
        };

        // Rebuild the note data so per-note difficulty and per-note gap can be paired.
        // Mirrors `calculate`'s setup; `passed_objects` is not used by these fixtures.
        let total_columns = map.cs.round_ties_even().max(1.0) as usize;
        let (notes, _) = build_notes(clock_rate, map.hit_objects.iter(), total_columns);

        if notes.len() < 2 {
            continue;
        }

        let windows = hit_windows(&map, &mods, clock_rate, false);
        let great =
            get_hit_window_300(&map, clock_rate, has_mod(&mods, "HR"), has_mod(&mods, "EZ"));
        let data = RebirthData::new(
            notes,
            total_columns,
            hit_leniency_from_window(great),
            windows.good,
        );

        let Some((_, _, per_note)) = per_note_difficulty(&map) else {
            continue;
        };

        if per_note.len() != data.notes.len() {
            continue;
        }

        let gaps = same_column_gaps(&data);
        let mut sorted_gaps: Vec<f64> = gaps.iter().copied().filter(|g| g.is_finite()).collect();
        sorted_gaps.sort_by(f64::total_cmp);

        let median_gap = if sorted_gaps.is_empty() {
            f64::NAN
        } else {
            sorted_gaps[sorted_gaps.len() / 2]
        };

        let reference = reference_windows(&attrs);
        let per_unit = f64::from(total) / per_note.len() as f64;

        let row = Row {
            keys: u(f[6]),
            ln_fraction: if attrs.n_objects > 0 {
                attrs.n_long_notes as f64 / attrs.n_objects as f64
            } else {
                0.0
            },
            median_gap,
            played_g: vec![f64::NAN; candidates.len()],
            reference_g: vec![f64::NAN; candidates.len()],
            played_skill: vec![f64::NAN; candidates.len()],
            reference_skill: vec![f64::NAN; candidates.len()],
            scalar: vec![f64::NAN; candidates.len()],
        };

        let row = Mutex::new(row);

        candidates
            .par_iter()
            .enumerate()
            .for_each(|(candidate_idx, &(_, amplitude, plateau))| {
                let model = ErrorModel {
                    recovery_offset: amplitude,
                    anticipation_offset: plateau,
                    ..Default::default()
                };

                // Exact per-note units: one per distinct (difficulty, sigma_scale, offset),
                // which is ground truth for the effect rather than a binned approximation.
                // Cost is why this is a report and not the shipping path.
                let mut merged: HashMap<(u64, u64, u64, u64), f64> = HashMap::new();

                for (idx, &(difficulty, duration)) in per_note.iter().enumerate() {
                    let gap_offset = model.recovery_mean_offset(gaps[idx]);

                    let (sigma_scale, release_offset) = match duration {
                        // A long note under V1 is one judgement carrying the fixed release
                        // offset. Recovery remains separate because it fades at SS skill.
                        Some(duration) if attrs.ln_judged_as_one => (
                            ln_sigma_scale_for_duration(&model, duration),
                            model.release_mean_offset,
                        ),
                        _ => (1.0, 0.0),
                    };

                    *merged
                        .entry((
                            difficulty.to_bits(),
                            sigma_scale.to_bits(),
                            release_offset.to_bits(),
                            gap_offset.to_bits(),
                        ))
                        .or_insert(0.0) += per_unit;
                }

                let units: Vec<JudgementUnit> = merged
                    .into_iter()
                    .map(
                        |((difficulty, sigma_scale, release_offset, gap_offset), weight)| {
                            JudgementUnit {
                                difficulty: f64::from_bits(difficulty),
                                weight,
                                sigma_scale: f64::from_bits(sigma_scale),
                                mean_offset: f64::from_bits(release_offset),
                                fading_mean_offset: f64::from_bits(gap_offset),
                            }
                        },
                    )
                    .collect();

                let (played, reference_fit) = join(
                    || fit_with_quality(&counts, &units, &attrs.hit_windows, &model),
                    || fit_with_quality(&counts, &units, &reference, &model),
                );

                let mut row = row.lock().expect("candidate result lock poisoned");
                row.played_g[candidate_idx] = played.g_timing;
                row.reference_g[candidate_idx] = reference_fit.g_timing;
                row.played_skill[candidate_idx] = played.skill;
                row.reference_skill[candidate_idx] = reference_fit.skill;
                row.scalar[candidate_idx] = if played.skill > 0.0 && reference_fit.skill > 0.0 {
                    played.skill / reference_fit.skill
                } else {
                    1.0
                };
            });

        rows.push(row.into_inner().expect("candidate result lock poisoned"));
    }

    if rows.is_empty() {
        println!("no fixture scores loaded");
        return;
    }

    fn median(values: &[f64]) -> f64 {
        let mut finite: Vec<f64> = values.iter().copied().filter(|v| v.is_finite()).collect();

        if finite.is_empty() {
            return f64::NAN;
        }

        finite.sort_by(f64::total_cmp);

        finite[finite.len() / 2]
    }

    fn mean(values: &[f64]) -> f64 {
        let finite: Vec<f64> = values.iter().copied().filter(|v| v.is_finite()).collect();

        if finite.is_empty() {
            return f64::NAN;
        }

        finite.iter().sum::<f64>() / finite.len() as f64
    }

    fn percentile(values: &[f64], numerator: usize, denominator: usize) -> f64 {
        let mut finite: Vec<f64> = values.iter().copied().filter(|v| v.is_finite()).collect();

        if finite.is_empty() {
            return f64::NAN;
        }

        finite.sort_by(f64::total_cmp);

        finite[finite.len().saturating_sub(1) * numerator / denominator]
    }

    let column = |extract: &dyn Fn(&Row) -> f64| -> Vec<f64> { rows.iter().map(extract).collect() };

    println!(
        "\n{} scores, exact per-note units. Median same-column gap across maps: {:.0} ms \
             (the axis the offset acts on).\n",
        rows.len(),
        median(&column(&|row| row.median_gap))
    );

    println!(
        "  {:<16} {:>9} {:>9} {:>10} {:>10} {:>9} {:>9}",
        "candidate", "g played", "g ref", "skill pl", "skill ref", "scalar", "pp %"
    );

    let baseline_scalar = median(&column(&|row| row.scalar[0]));

    for (index, (label, _, _)) in candidates.iter().enumerate() {
        let scalar = median(&column(&|row| row.scalar[index]));
        // pp moves as scalar^2.2, the exponent fitted skill enters pricing through.
        let pp = ((scalar / baseline_scalar).powf(2.2) - 1.0) * 100.0;

        println!(
            "  {label:<16} {:>9.2} {:>9.2} {:>10.3} {:>10.3} {:>9.4} {:>+9.2}",
            median(&column(&|row| row.played_g[index])),
            median(&column(&|row| row.reference_g[index])),
            median(&column(&|row| row.played_skill[index])),
            median(&column(&|row| row.reference_skill[index])),
            scalar,
            pp
        );
    }

    println!("\nCOUNT-FIT SENSITIVITY (played side; paired against off)");
    println!(
        "  {:<16} {:>9} {:>9} {:>9} {:>8} {:>8}",
        "candidate", "g median", "g mean", "g p90", "better", "worse"
    );
    for (index, (label, _, _)) in candidates.iter().enumerate() {
        let played = column(&|row| row.played_g[index]);
        let better = rows
            .iter()
            .filter(|row| row.played_g[index] + 1e-9 < row.played_g[0])
            .count();
        let worse = rows
            .iter()
            .filter(|row| row.played_g[index] > row.played_g[0] + 1e-9)
            .count();
        println!(
            "  {label:<16} {:>9.2} {:>9.2} {:>9.2} {:>8} {:>8}",
            median(&played),
            mean(&played),
            percentile(&played, 9, 10),
            better,
            worse,
        );
    }

    println!(
        "\n  All medians over scores. 'pp %' is the median scalar against the zero \
             control row's,\n  which is what a score's pp is multiplied by."
    );

    // Per-score pp deltas for the fitted curve, since a median of medians can hide a
    // spread that matters, and the direction per score is what players would see.
    let fitted = candidates.len() - 1;
    let deltas: Vec<f64> = rows
        .iter()
        .filter(|row| row.scalar[0] > 0.0)
        .map(|row| (row.scalar[fitted] / row.scalar[0]).powf(2.2) - 1.0)
        .collect();
    let raised = deltas.iter().filter(|d| **d > 0.001).count();
    let lowered = deltas.iter().filter(|d| **d < -0.001).count();

    println!(
        "\nFITTED CURVE, per score: median {:+.2}%, mean {:+.2}%, {raised} raised, \
             {lowered} lowered, {} unchanged",
        median(&deltas) * 100.0,
        mean(&deltas) * 100.0,
        deltas.len() - raised - lowered
    );

    // Where the offset should act most: dense maps have short gaps, so more of their
    // notes sit in the late regime. A mechanism that is real should sort by this.
    println!("\nBY MEDIAN SAME-COLUMN GAP (dense maps first — the late regime)");
    println!(
        "  {:<14} {:>4} {:>9} {:>9} {:>9} {:>9}",
        "gap band", "n", "g played", "g ref", "pp %", "raised"
    );

    for (label, low, high) in [
        ("<120 ms", 0.0, 120.0),
        ("120-200 ms", 120.0, 200.0),
        ("200-320 ms", 200.0, 320.0),
        (">320 ms", 320.0, f64::INFINITY),
    ] {
        let group: Vec<&Row> = rows
            .iter()
            .filter(|row| row.median_gap >= low && row.median_gap < high)
            .collect();

        if group.is_empty() {
            continue;
        }

        let group_deltas: Vec<f64> = group
            .iter()
            .filter(|row| row.scalar[0] > 0.0)
            .map(|row| (row.scalar[fitted] / row.scalar[0]).powf(2.2) - 1.0)
            .collect();

        println!(
            "  {label:<14} {:>4} {:>9.2} {:>9.2} {:>+9.2} {:>9}",
            group.len(),
            median(
                &group
                    .iter()
                    .map(|row| row.played_g[fitted])
                    .collect::<Vec<_>>()
            ),
            median(
                &group
                    .iter()
                    .map(|row| row.reference_g[fitted])
                    .collect::<Vec<_>>()
            ),
            median(&group_deltas) * 100.0,
            group_deltas.iter().filter(|d| **d > 0.001).count(),
        );
    }

    println!("\nBY KEYMODE AND LN SHARE");

    for (label, keys) in [("4K", 4u32), ("7K", 7)] {
        let group: Vec<&Row> = rows.iter().filter(|row| row.keys == keys).collect();

        if group.is_empty() {
            continue;
        }

        let group_deltas: Vec<f64> = group
            .iter()
            .filter(|row| row.scalar[0] > 0.0)
            .map(|row| (row.scalar[fitted] / row.scalar[0]).powf(2.2) - 1.0)
            .collect();

        println!(
            "  {label:<14} {:>4} median gap {:>5.0} ms   pp {:>+6.2}%   g played {:>7.2}",
            group.len(),
            median(&group.iter().map(|row| row.median_gap).collect::<Vec<_>>()),
            median(&group_deltas) * 100.0,
            median(
                &group
                    .iter()
                    .map(|row| row.played_g[fitted])
                    .collect::<Vec<_>>()
            ),
        );
    }

    for (label, low, high) in [("LN <15%", 0.0, 0.15), ("LN >35%", 0.35, 1.01)] {
        let group: Vec<&Row> = rows
            .iter()
            .filter(|row| row.ln_fraction >= low && row.ln_fraction < high)
            .collect();

        if group.is_empty() {
            continue;
        }

        let group_deltas: Vec<f64> = group
            .iter()
            .filter(|row| row.scalar[0] > 0.0)
            .map(|row| (row.scalar[fitted] / row.scalar[0]).powf(2.2) - 1.0)
            .collect();

        println!(
            "  {label:<14} {:>4} median gap {:>5.0} ms   pp {:>+6.2}%   g played {:>7.2}",
            group.len(),
            median(&group.iter().map(|row| row.median_gap).collect::<Vec<_>>()),
            median(&group_deltas) * 100.0,
            median(
                &group
                    .iter()
                    .map(|row| row.played_g[fitted])
                    .collect::<Vec<_>>()
            ),
        );
    }
}

/// Which of the three things per-note difficulty changes at once is doing the work.
///
/// `per_note_difficulty_on_real_scores` shows `g_timing` swinging by an order of
/// magnitude in both directions while fitted skill barely moves, and rules out the
/// obvious explanation: per-map spread is under 1.2 on 126 of 143 scores, far too tight
/// to account for it. So the mechanism is not the one the change was named after, and
/// three candidates are confounded in the shipped comparison:
///
/// 1. **Level.** Note-weighted mean per-note difficulty is 0.95x `stars`, so every unit
///    moves down together. Pure gauge — `skill` should absorb it exactly.
/// 2. **Spread.** Within-map heterogeneity of difficulty. Real but tight.
/// 3. **LN-vs-rice difficulty.** The uniform path prices a long note at `stars`, the
///    same as a plain note, and separates the two populations *only* by sigma width. The
///    per-note path gives each long note its own bin's difficulty. If long notes sit
///    systematically higher or lower in a map's difficulty distribution than its plain
///    notes, this is a repricing of the LN population that nothing in the shipped
///    comparison separates from (2).
///
/// Each variant below switches on exactly one of those, against the same scores and the
/// same fit, so the columns are attributable.
///
/// Run with
/// `cargo test --release per_note_mechanism_decomposition -- --ignored --nocapture`.
#[test]
#[ignore = "reads gitignored fixtures; prints a report rather than asserting"]
fn per_note_mechanism_decomposition() {
    let Ok(text) = std::fs::read_to_string("local-fixtures/multiuser.tsv") else {
        println!("no fixtures present (local-fixtures/multiuser.tsv); nothing to report");
        return;
    };

    let model = ErrorModel::default();

    struct Variant {
        label: &'static str,
        g_timing: Vec<f64>,
        skill: Vec<f64>,
        plausible: usize,
    }

    // Column order matters: each adds one mechanism to the one before it.
    let mut variants = vec![
        Variant {
            label: "uniform(stars)",
            g_timing: Vec::new(),
            skill: Vec::new(),
            plausible: 0,
        },
        Variant {
            label: "+level only",
            g_timing: Vec::new(),
            skill: Vec::new(),
            plausible: 0,
        },
        Variant {
            label: "+LN difficulty",
            g_timing: Vec::new(),
            skill: Vec::new(),
            plausible: 0,
        },
        Variant {
            label: "+spread (full)",
            g_timing: Vec::new(),
            skill: Vec::new(),
            plausible: 0,
        },
    ];

    let mut ln_higher = 0usize;
    let mut ln_lower = 0usize;
    let mut ln_ratios: Vec<f64> = Vec::new();
    let mut scored = 0usize;

    for line in text.lines() {
        let f: Vec<&str> = line.split('\t').collect();

        if f.len() < 18 || f[0] == "uid" {
            continue;
        }

        let u = |s: &str| s.parse::<u32>().unwrap_or(0);
        let counts = [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])];
        let total = counts.iter().sum::<u32>();

        if total == 0 {
            continue;
        }

        let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", f[2])) else {
            continue;
        };

        let (mods, clock_rate) = mods_for(f[3]);

        let Some(attrs) = calculate(&map, &mods, clock_rate, Some(false), None) else {
            continue;
        };

        let Some(bins) = attrs.note_difficulty_bins else {
            continue;
        };

        let binned: f64 = bins.iter().map(|bin| f64::from(bin.rice + bin.long)).sum();

        if binned <= 0.0 {
            continue;
        }

        let per_unit = f64::from(total) / binned;
        let weighted_mean = bins
            .iter()
            .map(|bin| bin.difficulty * f64::from(bin.rice + bin.long))
            .sum::<f64>()
            / binned;

        // Mean difficulty of the map's long notes against its plain notes, which is the
        // quantity mechanism (3) turns on.
        let long_weight: f64 = bins.iter().map(|bin| f64::from(bin.long)).sum();
        let rice_weight: f64 = bins.iter().map(|bin| f64::from(bin.rice)).sum();

        if long_weight > 0.0 && rice_weight > 0.0 {
            let long_mean = bins
                .iter()
                .map(|bin| bin.difficulty * f64::from(bin.long))
                .sum::<f64>()
                / long_weight;
            let rice_mean = bins
                .iter()
                .map(|bin| bin.difficulty * f64::from(bin.rice))
                .sum::<f64>()
                / rice_weight;

            if rice_mean > 0.0 {
                ln_ratios.push(long_mean / rice_mean);

                if long_mean > rice_mean {
                    ln_higher += 1;
                } else {
                    ln_lower += 1;
                }
            }
        }

        let combined_long_notes = attrs.ln_judged_as_one && !ln_split_disabled();

        // (0) Shipped fallback: one difficulty for everything, LN separated by width
        // only, via the duration histogram.
        let uniform = judgement_units(&attrs, f64::from(total), &model, false);

        // (1) Level only: the same list, moved to the note-weighted mean difficulty.
        // Isolates the 0.95x shift, which `skill` should absorb and nothing else.
        let level: Vec<JudgementUnit> = uniform
            .iter()
            .map(|unit| JudgementUnit {
                difficulty: weighted_mean,
                ..*unit
            })
            .collect();

        // (2) Level + LN difficulty: every unit still at one difficulty, except long
        // notes, which move to the mean difficulty of the map's *long* notes. Adds
        // mechanism (3) without any within-map spread.
        let mut ln_difficulty = Vec::with_capacity(2 + LN_DURATION_BUCKETS);

        {
            let long_mean = if long_weight > 0.0 {
                bins.iter()
                    .map(|bin| bin.difficulty * f64::from(bin.long))
                    .sum::<f64>()
                    / long_weight
            } else {
                weighted_mean
            };
            let rice_mean = if rice_weight > 0.0 {
                bins.iter()
                    .map(|bin| bin.difficulty * f64::from(bin.rice))
                    .sum::<f64>()
                    / rice_weight
            } else {
                weighted_mean
            };

            if rice_weight > 0.0 {
                ln_difficulty.push(JudgementUnit::repeated(rice_mean, rice_weight * per_unit));
            }

            if long_weight > 0.0 {
                // Mean hold duration over the whole map, so duration resolution is the
                // same as the uniform path's rather than better.
                let hold_weight: f64 = bins
                    .iter()
                    .filter(|bin| bin.mean_duration > 0.0)
                    .map(|bin| f64::from(bin.long))
                    .sum();
                let mean_duration = if hold_weight > 0.0 {
                    bins.iter()
                        .filter(|bin| bin.mean_duration > 0.0)
                        .map(|bin| bin.mean_duration * f64::from(bin.long))
                        .sum::<f64>()
                        / hold_weight
                } else {
                    0.0
                };

                if combined_long_notes && mean_duration > 0.0 {
                    ln_difficulty.push(JudgementUnit::long_note(
                        long_mean,
                        long_weight * per_unit,
                        &model,
                        mean_duration,
                    ));
                } else {
                    ln_difficulty.push(JudgementUnit::repeated(long_mean, long_weight * per_unit));
                }
            }
        }

        // (3) The shipped per-note list: adds within-map spread on top.
        let full = judgement_units(&attrs, f64::from(total), &model, true);

        for (index, units) in [uniform, level, ln_difficulty, full].iter().enumerate() {
            if units.is_empty() {
                continue;
            }

            let fit = fit_with_quality(&counts, units, &attrs.hit_windows, &model);

            if fit.g_timing.is_finite() {
                variants[index].g_timing.push(fit.g_timing);
            }

            variants[index].skill.push(fit.skill);

            if fit.is_plausible() {
                variants[index].plausible += 1;
            }
        }

        scored += 1;
    }

    if scored == 0 {
        println!("no fixture scores loaded");
        return;
    }

    fn mean(values: &[f64]) -> f64 {
        if values.is_empty() {
            return f64::NAN;
        }

        values.iter().sum::<f64>() / values.len() as f64
    }

    fn median(values: &[f64]) -> f64 {
        if values.is_empty() {
            return f64::NAN;
        }

        let mut sorted = values.to_vec();
        sorted.sort_by(f64::total_cmp);

        sorted[sorted.len() / 2]
    }

    println!("\n{scored} scores. Each row adds one mechanism to the row above it.\n");
    println!(
        "  {:<16} {:>10} {:>10} {:>12} {:>12}",
        "variant", "g mean", "g median", "skill median", "plausible"
    );

    let baseline_skill = median(&variants[0].skill);

    for variant in &variants {
        println!(
            "  {:<16} {:>10.2} {:>10.2} {:>12.3} {:>9}/{}",
            variant.label,
            mean(&variant.g_timing),
            median(&variant.g_timing),
            median(&variant.skill),
            variant.plausible,
            scored,
        );
    }

    println!(
        "\n  (baseline median skill {baseline_skill:.3}; a variant that only rescales \
             difficulty moves skill and leaves g_timing alone)"
    );

    println!(
        "\nLN-vs-RICE DIFFICULTY, the quantity mechanism (3) turns on:\n  \
             {} of {} maps with both populations put long notes at HIGHER mean difficulty than \
             plain notes, {} lower. Ratio: median {:.4}, mean {:.4}, min {:.4}, max {:.4}",
        ln_higher,
        ln_higher + ln_lower,
        ln_lower,
        median(&ln_ratios),
        mean(&ln_ratios),
        ln_ratios.iter().copied().fold(f64::INFINITY, f64::min),
        ln_ratios.iter().copied().fold(f64::NEG_INFINITY, f64::max),
    );
}

/// Experimental harness for calibrating input-state transition offset formulas.
///
/// Tests candidate formulations using the exact per-operation oracle before committing
/// to any implementation. Reports cohort-level impact to verify that the formula
/// separates low-OD LN from low-OD rice patterns.
///
/// Run with: `cargo test transition_oracle_experiments -- --ignored --nocapture`
#[test]
#[ignore = "reads gitignored fixtures; expensive calibration research"]
fn transition_oracle_experiments() {
    use crate::mania::sunny_accuracy::{ErrorModel, JudgementUnit};
    use rayon::prelude::*;

    let Ok(text) = std::fs::read_to_string("local-fixtures/multiuser.tsv") else {
        println!("no fixtures present (local-fixtures/multiuser.tsv); nothing to report");
        return;
    };

    let baseline = ErrorModel {
        recovery_tau: 72.40,
        anticipation_offset: -3.19,
        ..ErrorModel::default()
    };

    // Candidate formulas to test
    let candidates: Vec<(
        &str,
        Box<dyn Fn(&ClassifiedOperation, &ErrorModel) -> f64 + Sync>,
    )> = vec![
        // 1. Baseline uniform recovery
        (
            "baseline_uniform",
            Box::new(|op: &ClassifiedOperation, model: &ErrorModel| {
                if let Some(gap) = op.previous_gap_ms {
                    model.recovery_mean_offset(gap)
                } else {
                    0.0
                }
            }),
        ),
        // 2. Class-modulated: different factors per InputClass
        (
            "class_modulated",
            Box::new(|op: &ClassifiedOperation, model: &ErrorModel| {
                let base = if let Some(gap) = op.previous_gap_ms {
                    model.recovery_mean_offset(gap)
                } else {
                    0.0
                };

                let factor = match op.class {
                    InputClass::RapidRepress => 1.0,
                    InputClass::Jack => 0.8,
                    InputClass::ReleaseToPress => 0.6,
                    InputClass::PressUnderHold => 0.7,
                    InputClass::ChordEntryOrExit => 0.5,
                    InputClass::Release => 0.3,
                    InputClass::FreshPress => 0.0,
                };

                base * factor
            }),
        ),
        // 3. Lookahead-sensitive: modulate by gap_after to detect bursts
        (
            "lookahead_sensitive",
            Box::new(|op: &ClassifiedOperation, model: &ErrorModel| {
                let base = if let Some(gap) = op.previous_gap_ms {
                    model.recovery_mean_offset(gap)
                } else {
                    0.0
                };

                if let Some(next_gap) = op.next_gap_ms {
                    // In a burst (short gaps on both sides), apply less offset
                    let lookahead_factor = if next_gap < 150.0 { 0.7 } else { 1.0 };
                    base * lookahead_factor
                } else {
                    base
                }
            }),
        ),
        // 4. Chord-damped: divide by chord width
        (
            "chord_damped",
            Box::new(|op: &ClassifiedOperation, model: &ErrorModel| {
                let base = if let Some(gap) = op.previous_gap_ms {
                    model.recovery_mean_offset(gap)
                } else {
                    0.0
                };

                base / (1.0 + op.chord_width as f64 * 0.2)
            }),
        ),
        // 5. Hold-damped: divide by other_held count
        (
            "hold_damped",
            Box::new(|op: &ClassifiedOperation, model: &ErrorModel| {
                let base = if let Some(gap) = op.previous_gap_ms {
                    model.recovery_mean_offset(gap)
                } else {
                    0.0
                };

                base / (1.0 + op.other_held as f64 * 0.3)
            }),
        ),
        // 6. Combined: class × lookahead × hold
        (
            "combined",
            Box::new(|op: &ClassifiedOperation, model: &ErrorModel| {
                let base = if let Some(gap) = op.previous_gap_ms {
                    model.recovery_mean_offset(gap)
                } else {
                    0.0
                };

                let class_factor = match op.class {
                    InputClass::RapidRepress => 1.0,
                    InputClass::Jack => 0.8,
                    InputClass::ReleaseToPress => 0.6,
                    InputClass::PressUnderHold => 0.7,
                    InputClass::ChordEntryOrExit => 0.5,
                    InputClass::Release => 0.3,
                    InputClass::FreshPress => 0.0,
                };

                let lookahead_factor = if let Some(next_gap) = op.next_gap_ms {
                    if next_gap < 150.0 { 0.7 } else { 1.0 }
                } else {
                    1.0
                };

                let hold_damping = 1.0 / (1.0 + op.other_held as f64 * 0.3);

                base * class_factor * lookahead_factor * hold_damping
            }),
        ),
    ];

    let reports: Vec<(&str, Vec<AbPriced>)> = candidates
        .par_iter()
        .map(|(name, formula)| {
            let mut scores = Vec::new();

            for line in text.lines() {
                let f: Vec<&str> = line.split('\t').collect();
                if f.len() < 18 || f[0] == "uid" {
                    continue;
                }

                let u = |s: &str| s.parse::<u32>().unwrap_or(0);
                let counts = [u(f[7]), u(f[8]), u(f[9]), u(f[10]), u(f[11]), u(f[12])];

                let Some(map) = parse(&format!("local-fixtures/maps/{}.osu", f[2])) else {
                    continue;
                };

                let (mods, clock_rate) = mods_for(f[3]);

                let Some(attrs) = calculate(&map, &mods, clock_rate, Some(false), None) else {
                    continue;
                };

                let state = SunnyScoreState {
                    n320: counts[0],
                    n300: counts[1],
                    n200: counts[2],
                    n100: counts[3],
                    n50: counts[4],
                    misses: counts[5],
                };

                // Price with baseline (no recovery)
                let baseline_no_recovery = ErrorModel::default();
                let units_before = judgement_units(
                    &attrs,
                    f64::from(state.total_hits()),
                    &baseline_no_recovery,
                    true,
                );
                let fit_before = fit_with_quality(
                    &counts,
                    &units_before,
                    &attrs.hit_windows,
                    &baseline_no_recovery,
                );
                let (perf_before, _, _, _) = composition_from_units(
                    &attrs,
                    &mods,
                    state,
                    &baseline_no_recovery,
                    &units_before,
                );

                // Price with exact oracle using this candidate formula
                let total = f64::from(state.total_hits());
                let units_after = (|| {
                    let total_columns = map.cs.round_ties_even().max(1.0) as usize;
                    let (notes, _) = build_notes(clock_rate, map.hit_objects.iter(), total_columns);
                    let windows = hit_windows(&map, &mods, clock_rate, false);
                    let great = get_hit_window_300(
                        &map,
                        clock_rate,
                        has_mod(&mods, "HR"),
                        has_mod(&mods, "EZ"),
                    );
                    let data = RebirthData::new(
                        notes,
                        total_columns,
                        hit_leniency_from_window(great),
                        windows.good,
                    );
                    let (_, _, per_note) = per_note_difficulty(&map)?;
                    if per_note.len() != data.notes.len() {
                        return None;
                    }
                    let classic = !attrs.ln_judged_as_one;

                    let oracle_offsets =
                        exact_transition_oracle(&data.notes, data.total_columns, |op| {
                            formula(op, &baseline)
                        });

                    // Build units from oracle offsets
                    let included = oracle_offsets
                        .iter()
                        .filter(|(op, _)| {
                            !classic || op.operation.kind != InputOperationKind::Release
                        })
                        .count();
                    if included == 0 {
                        return None;
                    }
                    let mut units = Vec::with_capacity(included);
                    let per_op = total / included as f64;

                    for (op, offset) in &oracle_offsets {
                        if classic && op.operation.kind == InputOperationKind::Release {
                            continue;
                        }

                        let difficulty = per_note[op.operation.note_idx].0;

                        let is_long = op.operation.kind == InputOperationKind::Press
                            && op.operation.hold_duration_ms.is_some();

                        let mut unit = if is_long && attrs.ln_judged_as_one {
                            JudgementUnit::long_note(
                                difficulty,
                                per_op,
                                &baseline_no_recovery,
                                op.operation.hold_duration_ms.unwrap_or(0.0),
                            )
                        } else {
                            JudgementUnit::repeated(difficulty, per_op)
                        };

                        unit.fading_mean_offset = *offset;
                        units.push(unit);
                    }

                    Some(units)
                })()
                .unwrap_or_else(|| units_before.clone());

                let fit_after = fit_with_quality(
                    &counts,
                    &units_after,
                    &attrs.hit_windows,
                    &baseline_no_recovery,
                );
                let (perf_after, after_scalar, after_difficulty_value, after_acc_multiplier) =
                    composition_from_units(
                        &attrs,
                        &mods,
                        state,
                        &baseline_no_recovery,
                        &units_after,
                    );

                scores.push(AbPriced {
                    uid: f[0].to_owned(),
                    map_id: f[2].to_owned(),
                    mods: f[3].to_owned(),
                    keys: u(f[6]),
                    od: map.od,
                    acc: f[13].parse().unwrap_or(0.0),
                    notes: state.total_hits(),
                    ln_fraction: if attrs.n_objects > 0 {
                        attrs.n_long_notes as f64 / attrs.n_objects as f64
                    } else {
                        0.0
                    },
                    live_pp: f[14].parse().unwrap_or(0.0),
                    before_pp: perf_before,
                    after_pp: perf_after,
                    before_g: fit_before.g_timing,
                    after_g: fit_after.g_timing,
                    before_plausible: fit_before.is_plausible(),
                    after_plausible: fit_after.is_plausible(),
                    before_scalar: f64::NAN,
                    before_difficulty_value: f64::NAN,
                    before_acc_multiplier: f64::NAN,
                    after_scalar,
                    after_difficulty_value,
                    after_acc_multiplier,
                });
            }

            (*name, scores)
        })
        .collect();

    for (name, scores) in reports {
        println!("\n{:=<80}", "");
        println!("CANDIDATE: {name}");
        println!("{:=<80}\n", "");

        if scores.is_empty() {
            println!("no scores loaded");
            continue;
        }

        let all: Vec<&AbPriced> = scores.iter().collect();

        println!("\n=== overall ({} scores)", all.len());
        summarise_ab("all", &all);

        println!("\nlow-OD target and controls:");
        type Pred = fn(&&AbPriced) -> bool;
        for (label, pred) in [
            (
                "low OD <7, rice <30% LN",
                (|r: &&AbPriced| r.od < 7.0 && r.ln_fraction < 0.30) as Pred,
            ),
            ("low OD <7, LN >=30%", |r: &&AbPriced| {
                r.od < 7.0 && r.ln_fraction >= 0.30
            }),
            ("OD >=8, rice <30% LN", |r: &&AbPriced| {
                r.od >= 8.0 && r.ln_fraction < 0.30
            }),
            ("OD >=8, LN >=30%", |r: &&AbPriced| {
                r.od >= 8.0 && r.ln_fraction >= 0.30
            }),
        ] {
            let group: Vec<&AbPriced> = all.iter().copied().filter(pred).collect();
            summarise_ab(label, &group);
        }

        println!("\nby key count:");
        for keys in [4u32, 7] {
            let group: Vec<&AbPriced> = all.iter().copied().filter(|r| r.keys == keys).collect();
            summarise_ab(&format!("{keys}k"), &group);
        }

        println!("\nby window-affecting mod:");
        for (label, pred) in [
            ("EZ (windows widened)", (|r| r.mods.contains("EZ")) as Pred),
            ("no window mod", |r| {
                !r.mods.contains("EZ") && !r.mods.contains("HR")
            }),
        ] {
            let group: Vec<&AbPriced> = all.iter().copied().filter(pred).collect();
            summarise_ab(label, &group);
        }
    }
}
