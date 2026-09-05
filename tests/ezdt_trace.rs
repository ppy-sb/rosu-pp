// Test to understand what's happening with the EZDT calculation

#[test]
fn trace_ezdt_calculation() {
    use rosu_pp::{Beatmap, Performance};
    use rosu_pp::mania::sunny::{SunnyScoreState};

    // Mock calculation similar to EZDT score
    // Let's assume xxy_pp_pattern is around 2000-2500 for a high SR map
    let xxy_pp_pattern = 2453.7;  // This is the accuracy-neutral pattern base

    // With Sunny's accuracy system (85.7% accuracy)
    let score_accuracy = 0.85732;

    // Sunny's accuracy multiplier for 85.7% would be quite low
    // Let's say xxy_acc_multiplier ≈ 0.6 and performance_proportion ≈ 0.7
    let performance_proportion = 0.7;
    let acc_multiplier = 0.6;
    let xxy_pp_accuracy = xxy_pp_pattern * (performance_proportion * acc_multiplier - 1.0);
    let sunny_total = xxy_pp_pattern + xxy_pp_accuracy;

    println!("\n=== Tracing EZDT Calculation ===");
    println!("xxy_pp_pattern (accuracy-neutral): {:.1}", xxy_pp_pattern);
    println!("xxy_pp_accuracy (Sunny's accuracy penalty): {:.1}", xxy_pp_accuracy);
    println!("Sunny total (pattern + accuracy): {:.1}", sunny_total);

    // With our timing modifier
    let timing_multiplier = 0.7;  // 0.7× penalty for loose timing
    let our_total = xxy_pp_pattern * timing_multiplier;

    println!("\nOur timing modifier: {:.2}×", timing_multiplier);
    println!("Our total (pattern × timing): {:.1}", our_total);

    println!("\n⚠️  If xxy_pp_pattern is TOO HIGH to begin with,");
    println!("    even a 0.7× penalty won't bring it down enough!");

    println!("\nThe issue: We're multiplying the ACCURACY-NEUTRAL base,");
    println!("which is the SR-derived pattern difficulty WITHOUT any accuracy penalty.");
    println!("For EZDT, this base is HUGE because the star rating is inflated by DT.");
}
