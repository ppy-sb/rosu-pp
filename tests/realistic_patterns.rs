// Test with realistic score patterns from the actual report

#[test]
fn test_realistic_score_patterns() {
    // Penalty weights
    let w300 = 0.10;
    let w200 = 0.50;
    let w100 = 1.20;
    let w50 = 2.00;
    let wmiss = 3.00;

    let test_cases = vec![
        ("EZDT uid 10031 #1", [2123_u32, 2420, 1778, 153, 4, 194], 85.7),
        ("EZDT uid 10031 #2", [1754, 2050, 1334, 119, 1, 160], 86.5),
        ("EZDT uid 10107 #1", [1359, 1458, 783, 52, 3, 65], 89.4),
        ("Normal uid 10158 #1", [3987, 1469, 49, 6, 1, 11], 99.4),
        ("High acc uid 10246", [1671, 377, 33, 1, 1, 6], 99.1),
        ("Perfect uid 10889", [1235, 149, 0, 0, 0, 0], 100.0),
        ("Mixed uid 9540 #1", [3594, 2797, 509, 38, 9, 36], 95.8),
    ];

    println!("\n=== Realistic Score Patterns ===");
    println!("Weights: 300={}, 200={}, 100={}, 50={}, miss={}",
             w300, w200, w100, w50, wmiss);
    println!();

    for (label, counts, expected_acc) in test_cases {
        let total: u32 = counts.iter().sum();
        let acc = (counts[0] + counts[1]) as f64 / total as f64 * 100.0;

        let penalty_score = (
            counts[1] as f64 * w300 +
            counts[2] as f64 * w200 +
            counts[3] as f64 * w100 +
            counts[4] as f64 * w50 +
            counts[5] as f64 * wmiss
        ) / total as f64;

        let timing_multiplier = (1.15 - penalty_score * 0.3).clamp(0.7, 1.15);

        println!("{:25} acc={:5.1}%, penalty={:.3}, mult={:.3}",
                 label, acc, penalty_score, timing_multiplier);
    }

    println!();
    println!("Goal: EZDT should hit penalties around 0.8-0.9×");
    println!("      High acc should be around 1.1×");
    println!("      Perfect should be 1.15×");
}
