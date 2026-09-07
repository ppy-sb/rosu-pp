// Test the refined count-based approach with all judgement types

#[test]
fn test_full_distribution_approach() {
    let test_cases = vec![
        ("EZDT uid 10031 #1", [2123_u32, 2420, 1778, 153, 4, 194]),
        ("EZDT uid 10031 #2", [1754, 2050, 1334, 119, 1, 160]),
        ("EZDT uid 10107 #1", [1359, 1458, 783, 52, 3, 65]),
        ("Normal uid 10158 #1", [3987, 1469, 49, 6, 1, 11]),
        ("High acc uid 10246", [1671, 377, 33, 1, 1, 6]),
        ("Perfect uid 10889", [1235, 149, 0, 0, 0, 0]),
        ("Mixed uid 9540 #1", [3594, 2797, 509, 38, 9, 36]),
        ("All 320s", [2000, 0, 0, 0, 0, 0]),
        ("All 300s", [0, 2000, 0, 0, 0, 0]),
        ("All 200s", [0, 0, 2000, 0, 0, 0]),
    ];

    // Quality weights
    let w320 = 1.0;
    let w300 = 0.6;
    let w200 = 0.3;
    let w100 = 0.1;
    let w50 = 0.0;
    let wmiss = -0.2;

    println!("\n=== Full Distribution Approach ===");
    println!(
        "Weights: 320={}, 300={}, 200={}, 100={}, 50={}, miss={}",
        w320, w300, w200, w100, w50, wmiss
    );
    println!();

    for (label, counts) in test_cases {
        let total: u32 = counts.iter().sum();
        let acc = (counts[0] + counts[1]) as f64 / total as f64 * 100.0;

        let r320 = counts[0] as f64 / total as f64;
        let r300 = counts[1] as f64 / total as f64;
        let r200 = counts[2] as f64 / total as f64;
        let r100 = counts[3] as f64 / total as f64;
        let r50 = counts[4] as f64 / total as f64;
        let rmiss = counts[5] as f64 / total as f64;

        let quality_score =
            r320 * w320 + r300 * w300 + r200 * w200 + r100 * w100 + r50 * w50 + rmiss * wmiss;

        let timing_multiplier = (0.55 + quality_score * 0.6).clamp(0.7, 1.15);

        println!(
            "{:25} acc={:5.1}%, quality={:.3}, mult={:.3}  [{:.0}/{:.0}/{:.0}]",
            label,
            acc,
            quality_score,
            timing_multiplier,
            r320 * 100.0,
            r300 * 100.0,
            r200 * 100.0
        );
    }

    println!();
    println!("Expected behavior:");
    println!("  - All 320s (quality=1.0): 1.15× reward");
    println!("  - Mostly 300s (quality=0.6): 0.91× neutral");
    println!("  - EZDT mix (quality=0.4-0.5): 0.8-0.85× penalty");
    println!("  - Mostly 200s (quality=0.3): 0.73× penalty");
}
