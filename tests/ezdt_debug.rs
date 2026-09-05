use rosu_pp::mania::sunny_accuracy::{TIMING_BASELINE_SIGMA, timing_sigma_for_counts, ErrorModel, JudgementUnit};
use rosu_pp::mania::sunny_windows::ManiaHitWindows;

#[test]
fn debug_ezdt_windows() {
    // EZDT with OD 9
    // EZ multiplies windows by 1.4
    // So OD 9 with EZ has much wider windows

    let ezdt_windows = ManiaHitWindows {
        perfect: 16.5 * 1.4,  // ~23.1ms
        great: 40.5 * 1.4,    // ~56.7ms
        good: 73.5 * 1.4,     // ~102.9ms
        ok: 103.5 * 1.4,      // ~144.9ms
        meh: 127.5 * 1.4,     // ~178.5ms
        miss: 164.5 * 1.4,    // ~230.3ms
    };

    let normal_windows = ManiaHitWindows {
        perfect: 16.5,
        great: 40.5,
        good: 73.5,
        ok: 103.5,
        meh: 127.5,
        miss: 164.5,
    };

    // EZDT score from uid 10031
    let state_counts = [2123_u32, 2420, 1778, 153, 4, 194];
    let total: u32 = state_counts.iter().sum();

    let model = ErrorModel::default();
    let units = vec![JudgementUnit::repeated(5.0, total as f64)];

    println!("\n=== EZDT Windows Analysis ===");
    println!("Normal windows (OD 9):");
    println!("  perfect: {:.1}ms, great: {:.1}ms", normal_windows.perfect, normal_windows.great);
    println!("EZDT windows (OD 9 + EZ):");
    println!("  perfect: {:.1}ms, great: {:.1}ms", ezdt_windows.perfect, ezdt_windows.great);

    let sigma_normal = timing_sigma_for_counts(&state_counts, &units, &normal_windows, &model);
    let sigma_ezdt = timing_sigma_for_counts(&state_counts, &units, &ezdt_windows, &model);

    println!("\nFitted sigma with normal windows: {:.3}ms", sigma_normal);
    println!("Fitted sigma with EZDT windows: {:.3}ms", sigma_ezdt);
    println!("Baseline sigma: {:.3}ms", TIMING_BASELINE_SIGMA);

    let ratio_normal = TIMING_BASELINE_SIGMA / sigma_normal;
    let ratio_ezdt = TIMING_BASELINE_SIGMA / sigma_ezdt;

    let mult_normal = ratio_normal.powf(0.5).clamp(0.7, 1.3);
    let mult_ezdt = ratio_ezdt.powf(0.5).clamp(0.7, 1.3);

    println!("\nWith normal windows:");
    println!("  ratio: {:.4}, multiplier: {:.4}", ratio_normal, mult_normal);
    println!("With EZDT windows:");
    println!("  ratio: {:.4}, multiplier: {:.4}", ratio_ezdt, mult_ezdt);

    println!("\n❌ PROBLEM: Wide windows make sigma appear smaller relative to windows!");
    println!("The same loose timing (36ms) through wide windows (57ms GREAT)");
    println!("might fit to a smaller sigma than through tight windows (40ms GREAT).");
}
