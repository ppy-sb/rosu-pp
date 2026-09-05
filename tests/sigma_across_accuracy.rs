use rosu_pp::mania::sunny_accuracy::{TIMING_BASELINE_SIGMA, timing_sigma_for_counts, ErrorModel, JudgementUnit};
use rosu_pp::mania::sunny_windows::ManiaHitWindows;

#[test]
fn debug_various_accuracy_scores() {
    let windows = ManiaHitWindows {
        perfect: 16.5,
        great: 40.5,
        good: 73.5,
        ok: 103.5,
        meh: 127.5,
        miss: 164.5,
    };

    let model = ErrorModel::default();

    // Test various accuracy levels
    let test_cases = vec![
        ("97.4% (high)", [1235_u32, 566, 56, 16, 0, 19]),
        ("95.8% (good)", [3594, 2797, 509, 38, 9, 36]),
        ("93.6% (decent)", [7330, 4374, 781, 201, 40, 322]),
        ("91.0% (low)", [1075, 916, 295, 61, 15, 65]),
        ("87.2% (very low)", [137, 99, 35, 9, 1, 20]),
    ];

    println!("\n=== Sigma Fitting Across Accuracy Levels ===");

    for (label, counts) in test_cases {
        let total: u32 = counts.iter().sum();
        let units = vec![JudgementUnit::repeated(5.0, total as f64)];

        let fitted_sigma = timing_sigma_for_counts(&counts, &units, &windows, &model);
        let timing_ratio = TIMING_BASELINE_SIGMA / fitted_sigma;
        let timing_multiplier = timing_ratio.powf(0.5).clamp(0.7, 1.3);

        let acc = (counts[0] + counts[1]) as f64 / total as f64 * 100.0;

        println!("{:20} acc={:.1}%, sigma={:.1}ms, ratio={:.3}, mult={:.3}",
                 label, acc, fitted_sigma, timing_ratio, timing_multiplier);
    }

    println!("\n⚠️  If all multipliers are 0.700, the baseline (11ms) is too low!");
    println!("Real players have looser timing than 11ms, so everyone hits the floor.");
}
