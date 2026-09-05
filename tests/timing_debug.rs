use rosu_pp::mania::sunny::{SunnyManiaDifficultyAttributes, SunnyScoreState};
use rosu_pp::mania::sunny_accuracy::{TIMING_BASELINE_SIGMA, timing_sigma_for_counts, ErrorModel};
use rosu_mods::GameMods;

#[test]
fn debug_timing_modifier() {
    // Simulate an EZDT score with loose timing (from uid 10031's first score)
    // 5124135    EZDTV2    7    9 false   6672   2123/2420/1778/153/4/194  85.732

    let state = SunnyScoreState {
        n320: 2123,
        n300: 2420,
        n200: 1778,
        n100: 153,
        n50: 4,
        misses: 194,
    };

    // Create a mock difficulty attributes
    let mut attrs = SunnyManiaDifficultyAttributes::default();
    attrs.stars = 5.0;
    attrs.n_objects = 6672;
    attrs.n_long_notes = 0;
    attrs.hit_windows = rosu_pp::mania::sunny_windows::ManiaHitWindows {
        perfect: 16.5,
        great: 40.5,
        good: 73.5,
        ok: 103.5,
        meh: 127.5,
        miss: 164.5,
    };

    // Fit sigma from these counts
    let model = ErrorModel::default();
    let total = state.total_hits();
    let counts = [state.n320, state.n300, state.n200, state.n100, state.n50, state.misses];

    // Create a simple unit for testing
    let units = vec![rosu_pp::mania::sunny_accuracy::JudgementUnit::repeated(
        attrs.stars,
        total as f64,
    )];

    let fitted_sigma = timing_sigma_for_counts(&counts, &units, &attrs.hit_windows, &model);

    let timing_ratio = TIMING_BASELINE_SIGMA / fitted_sigma;
    let timing_multiplier = timing_ratio.powf(0.5).clamp(0.7, 1.3);

    println!("=== EZDT Score Debug ===");
    println!("Accuracy: {:.2}%",
        100.0 * (state.n320 + state.n300) as f64 / total as f64);
    println!("Fitted sigma: {:.3}ms", fitted_sigma);
    println!("Baseline sigma: {:.3}ms", TIMING_BASELINE_SIGMA);
    println!("Timing ratio (baseline/fitted): {:.4}", timing_ratio);
    println!("Timing multiplier (ratio^0.5): {:.4}", timing_multiplier);
    println!("Expected: ratio < 1.0 (loose timing) → multiplier < 1.0 (penalty)");
    println!("Actual: {}", if timing_multiplier < 1.0 { "PENALTY ✓" } else { "REWARD ✗" });
}

#[test]
fn debug_timing_modifier_tight() {
    // Simulate a high-accuracy score with tight timing
    let state = SunnyScoreState {
        n320: 1900,
        n300: 200,
        n200: 50,
        n100: 10,
        n50: 5,
        misses: 0,
    };

    let mut attrs = SunnyManiaDifficultyAttributes::default();
    attrs.stars = 5.0;
    attrs.n_objects = 2165;
    attrs.n_long_notes = 0;
    attrs.hit_windows = rosu_pp::mania::sunny_windows::ManiaHitWindows {
        perfect: 16.5,
        great: 40.5,
        good: 73.5,
        ok: 103.5,
        meh: 127.5,
        miss: 164.5,
    };

    let model = ErrorModel::default();
    let total = state.total_hits();
    let counts = [state.n320, state.n300, state.n200, state.n100, state.n50, state.misses];

    let units = vec![rosu_pp::mania::sunny_accuracy::JudgementUnit::repeated(
        attrs.stars,
        total as f64,
    )];

    let fitted_sigma = timing_sigma_for_counts(&counts, &units, &attrs.hit_windows, &model);

    let timing_ratio = TIMING_BASELINE_SIGMA / fitted_sigma;
    let timing_multiplier = timing_ratio.powf(0.5).clamp(0.7, 1.3);

    println!("\n=== High Accuracy Score Debug ===");
    println!("Accuracy: {:.2}%",
        100.0 * (state.n320 + state.n300) as f64 / total as f64);
    println!("Fitted sigma: {:.3}ms", fitted_sigma);
    println!("Baseline sigma: {:.3}ms", TIMING_BASELINE_SIGMA);
    println!("Timing ratio (baseline/fitted): {:.4}", timing_ratio);
    println!("Timing multiplier (ratio^0.5): {:.4}", timing_multiplier);
    println!("Expected: ratio > 1.0 (tight timing) → multiplier > 1.0 (reward)");
    println!("Actual: {}", if timing_multiplier > 1.0 { "REWARD ✓" } else { "PENALTY ✗" });
}
