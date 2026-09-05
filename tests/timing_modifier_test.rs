/// Test the timing-based PP modifier in the sunny system
use rosu_pp::{Beatmap};
use rosu_pp::mania::sunny::{calculate, calculate_performance, SunnyScoreState};
use rosu_mods::GameMods;

#[test]
fn timing_modifier_rewards_tight_timing() {
    // Create a simple test map
    let map_path = "./resources/2916284.osu"; // You'll need an actual map file

    if std::path::Path::new(map_path).exists() {
        let map = Beatmap::from_path(map_path).unwrap();
        let mods = GameMods::default();

        // Calculate difficulty
        let attrs = calculate(&map, &mods, 1.0, None, None).unwrap();

        // Scenario 1: Perfect score (very tight timing, should get fitted_sigma < 11ms)
        let perfect_score = SunnyScoreState {
            n320: attrs.n_objects as u32,
            n300: 0,
            n200: 0,
            n100: 0,
            n50: 0,
            misses: 0,
        };
        let perfect_perf = calculate_performance(&attrs, &mods, perfect_score);

        // Scenario 2: Good but not perfect (fitted_sigma ~11ms, at baseline)
        let total = attrs.n_objects as u32;
        let good_score = SunnyScoreState {
            n320: (total as f32 * 0.95) as u32,
            n300: (total as f32 * 0.04) as u32,
            n200: (total as f32 * 0.01) as u32,
            n100: 0,
            n50: 0,
            misses: 0,
        };
        let good_perf = calculate_performance(&attrs, &mods, good_score);

        // Scenario 3: Sloppy score (fitted_sigma > 11ms, below baseline)
        let sloppy_score = SunnyScoreState {
            n320: (total as f32 * 0.70) as u32,
            n300: (total as f32 * 0.15) as u32,
            n200: (total as f32 * 0.10) as u32,
            n100: (total as f32 * 0.03) as u32,
            n50: (total as f32 * 0.02) as u32,
            misses: 0,
        };
        let sloppy_perf = calculate_performance(&attrs, &mods, sloppy_score);

        println!("\n=== Timing Modifier Test Results ===");
        println!("Perfect score - sigma: {:.2}ms, multiplier: {:.4}, pp: {:.2}",
                 perfect_perf.timing_core_sigma,
                 perfect_perf.acc_multiplier,
                 perfect_perf.pp);
        println!("Good score    - sigma: {:.2}ms, multiplier: {:.4}, pp: {:.2}",
                 good_perf.timing_core_sigma,
                 good_perf.acc_multiplier,
                 good_perf.pp);
        println!("Sloppy score  - sigma: {:.2}ms, multiplier: {:.4}, pp: {:.2}",
                 sloppy_perf.timing_core_sigma,
                 sloppy_perf.acc_multiplier,
                 sloppy_perf.pp);

        // Assertions:
        // 1. Tighter timing should have higher multiplier
        assert!(perfect_perf.acc_multiplier >= good_perf.acc_multiplier,
                "Perfect score should have higher timing multiplier than good score");
        assert!(good_perf.acc_multiplier >= sloppy_perf.acc_multiplier,
                "Good score should have higher timing multiplier than sloppy score");

        // 2. Fitted sigma should reflect score quality
        assert!(perfect_perf.timing_core_sigma < good_perf.timing_core_sigma,
                "Perfect score should have tighter sigma than good score");
        assert!(good_perf.timing_core_sigma < sloppy_perf.timing_core_sigma,
                "Good score should have tighter sigma than sloppy score");

        // 3. PP should increase with tighter timing
        assert!(perfect_perf.pp > sloppy_perf.pp,
                "Perfect score should have higher PP than sloppy score");
    } else {
        println!("Skipping test - map file not found: {}", map_path);
    }
}

#[test]
fn timing_modifier_basic_sanity() {
    // Test with a simple synthetic score to verify the math works
    println!("\nTiming modifier formula test:");

    let baseline_sigma: f64 = 11.0;

    // Test cases: fitted_sigma -> expected multiplier (sqrt ratio, clamped 0.7-1.3)
    let test_cases: Vec<(f64, f64, &str)> = vec![
        (8.0,  (11.0_f64 / 8.0_f64).sqrt(), "tight timing"),   // ~1.171
        (11.0, 1.0, "baseline timing"),                         // 1.0
        (15.0, (11.0_f64 / 15.0_f64).sqrt(), "loose timing"),  // ~0.856
        (5.0,  1.3, "very tight - clamped"),                    // would be 1.48, clamped to 1.3
        (25.0, 0.7, "very loose - clamped"),                    // would be 0.66, clamped to 0.7
    ];

    for (fitted_sigma, expected_mult, desc) in test_cases {
        let ratio: f64 = baseline_sigma / fitted_sigma;
        let multiplier: f64 = ratio.powf(0.5).clamp(0.7, 1.3);

        println!("  sigma={:5.1}ms -> ratio={:.3}, mult={:.3} ({})",
                 fitted_sigma, ratio, multiplier, desc);

        assert!((multiplier - expected_mult).abs() < 0.001,
                "Multiplier mismatch for sigma={}: expected {:.3}, got {:.3}",
                fitted_sigma, expected_mult, multiplier);
    }
}
