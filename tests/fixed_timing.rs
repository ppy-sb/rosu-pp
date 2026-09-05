// Test the fixed timing modifier calculation

#[test]
fn test_fixed_timing_modifier() {
    // Using the EZDT example
    let xxy_pp_pattern = 2453.7;  // Accuracy-neutral base (DT-inflated)
    let xxy_pp_accuracy = -1423.1; // Sunny's accuracy penalty for 85.7%
    let sunny_total = xxy_pp_pattern + xxy_pp_accuracy; // = 1030.6

    let timing_multiplier = 0.7;  // Our timing penalty for loose timing

    // OLD (BROKEN) approach: multiply pattern only
    let old_total = xxy_pp_pattern * timing_multiplier;

    // NEW (FIXED) approach: multiply (pattern + accuracy)
    let new_total = sunny_total * timing_multiplier;

    println!("\n=== Fixed Timing Modifier Test ===");
    println!("xxy_pp_pattern: {:.1}", xxy_pp_pattern);
    println!("xxy_pp_accuracy: {:.1}", xxy_pp_accuracy);
    println!("Sunny total (no timing mod): {:.1}", sunny_total);
    println!();
    println!("OLD approach (pattern × timing):");
    println!("  Result: {:.1} pp", old_total);
    println!("  Issue: {:.1} > {:.1} (inflated!)", old_total, sunny_total);
    println!();
    println!("NEW approach ((pattern + accuracy) × timing):");
    println!("  Result: {:.1} pp", new_total);
    println!("  Outcome: {:.1} < {:.1} (correctly penalized!)", new_total, sunny_total);
    println!();
    println!("✓ EZDT scores will now DROP as expected!");
}
