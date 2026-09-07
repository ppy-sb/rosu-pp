// Test the count-based timing penalty approach

#[test]
fn test_count_based_penalty() {
    let test_cases = vec![
        ("All 320s (perfect)", [2000_u32, 0, 0, 0, 0, 0]),
        ("99% (SS-tier)", [1980, 20, 0, 0, 0, 0]),
        ("97% (high)", [1920, 70, 10, 0, 0, 0]),
        ("95% (good)", [1820, 150, 30, 0, 0, 0]),
        ("90% (decent)", [1600, 300, 100, 0, 0, 0]),
        ("85% (EZDT-like)", [1400, 400, 200, 0, 0, 0]),
        ("70% (very loose)", [1000, 600, 300, 100, 0, 0]),
        ("50% (terrible)", [500, 500, 500, 300, 200, 0]),
    ];

    // Penalty weights
    let w300 = 0.10;
    let w200 = 0.50;
    let w100 = 1.20;
    let w50 = 2.00;
    let wmiss = 3.00;

    println!("\n=== Count-Based Timing Penalty ===");
    println!(
        "Penalty weights: 300={}, 200={}, 100={}, 50={}, miss={}",
        w300, w200, w100, w50, wmiss
    );
    println!();

    for (label, counts) in test_cases {
        let total: u32 = counts.iter().sum();
        let acc = (counts[0] + counts[1]) as f64 / total as f64 * 100.0;

        let penalty_score = (counts[1] as f64 * w300
            + counts[2] as f64 * w200
            + counts[3] as f64 * w100
            + counts[4] as f64 * w50
            + counts[5] as f64 * wmiss)
            / total as f64;

        let timing_multiplier = (1.15 - penalty_score * 0.3).clamp(0.7, 1.15);

        println!(
            "{:25} acc={:5.1}%, penalty={:.3}, mult={:.3}",
            label, acc, penalty_score, timing_multiplier
        );
    }

    println!();
    println!("Expected behavior:");
    println!("  - All 320s: 1.2× reward (best timing)");
    println!("  - 95-99%: 1.0-1.1× (slight reward for good timing)");
    println!("  - EZDT (85%): ~0.9× (small penalty)");
    println!("  - Very loose (50%): 0.7× floor (strong penalty)");
}
