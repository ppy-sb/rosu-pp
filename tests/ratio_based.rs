// Test the 320-ratio based approach

#[test]
fn test_ratio_based_approach() {
    let test_cases = vec![
        (
            "EZDT uid 10031 #1",
            [2123_u32, 2420, 1778, 153, 4, 194],
            85.7,
        ),
        ("EZDT uid 10031 #2", [1754, 2050, 1334, 119, 1, 160], 86.5),
        ("EZDT uid 10107 #1", [1359, 1458, 783, 52, 3, 65], 89.4),
        ("Normal uid 10158 #1", [3987, 1469, 49, 6, 1, 11], 99.4),
        ("High acc uid 10246", [1671, 377, 33, 1, 1, 6], 99.1),
        ("Perfect uid 10889", [1235, 149, 0, 0, 0, 0], 100.0),
        ("Mixed uid 9540 #1", [3594, 2797, 509, 38, 9, 36], 95.8),
        ("All 320s", [2000, 0, 0, 0, 0, 0], 100.0),
    ];

    println!("\n=== 320-Ratio Based Approach ===");
    println!();

    for (label, counts, _expected_acc) in test_cases {
        let total: u32 = counts.iter().sum();
        let acc = (counts[0] + counts[1]) as f64 / total as f64 * 100.0;

        let ratio_320 = counts[0] as f64 / total as f64;
        let bad_ratio = (counts[2] + counts[3] + counts[4] + counts[5]) as f64 / total as f64;
        let quality_score = ratio_320 - bad_ratio * 0.5;
        let timing_multiplier = (0.7 + quality_score * 0.45).clamp(0.7, 1.15);

        println!(
            "{:25} acc={:5.1}%, 320%={:5.1}%, quality={:.3}, mult={:.3}",
            label,
            acc,
            ratio_320 * 100.0,
            quality_score,
            timing_multiplier
        );
    }

    println!();
    println!("Goal: EZDT (~30% 320s) should get 0.8-0.9× penalties");
    println!("      High acc (~70% 320s) should get 1.05-1.1×");
    println!("      Perfect (90%+ 320s) should get 1.1-1.15×");
}
