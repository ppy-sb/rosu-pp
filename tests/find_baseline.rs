use rosu_pp::mania::sunny_accuracy::{TIMING_BASELINE_SIGMA, timing_sigma_for_counts, ErrorModel, JudgementUnit};
use rosu_pp::mania::sunny_windows::ManiaHitWindows;

#[test]
fn find_reasonable_baseline() {
    let windows = ManiaHitWindows {
        perfect: 16.5,
        great: 40.5,
        good: 73.5,
        ok: 103.5,
        meh: 127.5,
        miss: 164.5,
    };

    let model = ErrorModel::default();

    // Simulate various "good" scores that should be around 1.0× multiplier
    let test_cases = vec![
        ("SS (100%)", [2000_u32, 0, 0, 0, 0, 0]),
        ("99.5%", [1990, 10, 0, 0, 0, 0]),
        ("99%", [1980, 20, 0, 0, 0, 0]),
        ("98%", [1960, 40, 0, 0, 0, 0]),
        ("97%", [1920, 70, 10, 0, 0, 0]),
        ("96%", [1880, 100, 20, 0, 0, 0]),
        ("95%", [1820, 150, 30, 0, 0, 0]),
    ];

    println!("\n=== Finding Reasonable Baseline ===");
    println!("Current baseline: {}ms", TIMING_BASELINE_SIGMA);
    println!();

    for (label, counts) in test_cases {
        let total: u32 = counts.iter().sum();
        let units = vec![JudgementUnit::repeated(5.0, total as f64)];

        let fitted_sigma = timing_sigma_for_counts(&counts, &units, &windows, &model);
        let timing_ratio = TIMING_BASELINE_SIGMA / fitted_sigma;
        let timing_multiplier = timing_ratio.powf(0.5).clamp(0.7, 1.3);

        println!("{:15} sigma={:5.1}ms, ratio={:.3}, mult={:.3}{}",
                 label, fitted_sigma, timing_ratio, timing_multiplier,
                 if (timing_multiplier - 1.0).abs() < 0.05 { " ← neutral" } else { "" });
    }

    println!();
    println!("Suggestion: Set baseline to the sigma of ~96-97% accuracy");
    println!("This makes that accuracy level the 1.0× reference point.");
}
