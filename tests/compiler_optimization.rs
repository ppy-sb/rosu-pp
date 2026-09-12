use rosu_pp::mania::sunny_windows::effective_windows;
use rosu_pp::model::mods::GameMods;
use std::time::Instant;

type RosuMods = rosu_mods::GameMods;

#[test]
fn dce_only_great() {
    // 10,000,000 iterations to make the performance gap clearly measurable
    const ITERATIONS: usize = 10_000_000;

    // Initialize required test configurations
    let mods = GameMods::from(RosuMods::new());

    // =========================================================================
    // Case 1: Experimental Group — OD changes every iteration, accesses ONLY the `great` field
    // =========================================================================
    let start_great = Instant::now();
    let mut sum_great = 0.0;

    for i in 0..ITERATIONS {
        // CRITICAL: Make the input depend on loop variable `i` and wrap it with `black_box`.
        // This breaks Constant Folding and Loop Invariant Code Motion (LICM).
        let dynamic_od = std::hint::black_box((i % 10) as f64);

        let res = effective_windows(dynamic_od, false, &mods, 1.0, false).great;
        sum_great += res;
    }
    // Inform the compiler that `sum_great` is used, preventing the entire loop from being optimized away
    std::hint::black_box(sum_great);
    let duration_great = start_great.elapsed();

    // =========================================================================
    // Case 2: Control Group — OD changes every iteration, forces full calculation of all fields
    // =========================================================================
    let start_full = Instant::now();
    let mut sum_all = 0.0;

    for i in 0..ITERATIONS {
        let dynamic_od = std::hint::black_box((i % 10) as f64);

        let res = effective_windows(dynamic_od, false, &mods, 1.0, false);
        // By summing all fields explicitly, LLVM is forced to compute every single field
        sum_all += res.perfect + res.great + res.good + res.ok + res.meh + res.miss;
    }
    // Consume the final sum with a black box as well
    std::hint::black_box(sum_all);
    let duration_full = start_full.elapsed();

    // =========================================================================
    // Results Logging and Regression Assertions
    // =========================================================================
    println!("\n--- Results (Constant Folding Disabled) ---");
    println!("Only Great Duration : {:?}", duration_great);
    println!("Full Calc Duration  : {:?}", duration_full);

    // Performance regression check: If the inline pruning optimization is working,
    // accessing a single field MUST be significantly faster than forcing a full calculation.
    assert!(
        duration_great < duration_full,
        "Optimization failure! 'Only Great' duration ({:?}) should be less than 'Full Calc' duration ({:?}). Please verify that the #[inline] chain is unbroken.",
        duration_great,
        duration_full
    );
}
