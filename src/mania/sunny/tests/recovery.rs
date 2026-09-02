use rayon::prelude::*;

#[derive(Clone, Copy, Debug)]
pub(super) struct FitPoint {
    pub(super) gap: f64,
    pub(super) offset: f64,
    pub(super) weight: u64,
}

#[derive(Clone, Copy, Debug)]
pub(super) struct RecoveryFit {
    pub(super) amplitude: f64,
    pub(super) tau: f64,
    pub(super) plateau: f64,
    pub(super) rmse: f64,
}

/// Fit `amplitude * exp(-gap / tau) + plateau` by deterministic grid refinement.
pub(super) fn fit_recovery_curve(
    points: &[FitPoint],
    refinements: usize,
) -> Result<RecoveryFit, &'static str> {
    if points.len() < 3 {
        return Err("recovery fit requires at least three populated bins");
    }
    if points.iter().any(|point| point.weight == 0) {
        return Err("recovery fit weights must be positive");
    }

    let (mut amp_lo, mut amp_hi) = (5.0, 400.0);
    let (mut tau_lo, mut tau_hi) = (20.0, 300.0);
    let (mut base_lo, mut base_hi) = (-8.0, 2.0);
    let mut best = (f64::INFINITY, 0.0, 0.0, 0.0);

    for _ in 0..refinements {
        best = (0..40)
            .into_par_iter()
            .map(|i| {
                let amplitude = lerp(amp_lo, amp_hi, i);
                let mut local = (f64::INFINITY, 0.0, 0.0, 0.0);

                for j in 0..40 {
                    let tau = lerp(tau_lo, tau_hi, j);
                    for k in 0..40 {
                        let plateau = lerp(base_lo, base_hi, k);
                        let error: f64 = points
                            .iter()
                            .map(|point| {
                                let predicted = amplitude * (-point.gap / tau).exp() + plateau;
                                point.weight as f64 * (predicted - point.offset).powi(2)
                            })
                            .sum();
                        let candidate = (error, amplitude, tau, plateau);
                        if candidate.0.total_cmp(&local.0).is_lt() {
                            local = candidate;
                        }
                    }
                }

                local
            })
            .min_by(|left, right| left.0.total_cmp(&right.0))
            .unwrap();

        let (_, amplitude, tau, plateau) = best;
        let amp_span = (amp_hi - amp_lo) / 8.0;
        let tau_span = (tau_hi - tau_lo) / 8.0;
        let base_span = (base_hi - base_lo) / 8.0;
        (amp_lo, amp_hi) = (amplitude - amp_span, amplitude + amp_span);
        (tau_lo, tau_hi) = ((tau - tau_span).max(1.0), tau + tau_span);
        (base_lo, base_hi) = (plateau - base_span, plateau + base_span);
    }

    let (error, amplitude, tau, plateau) = best;
    let total_weight: u64 = points.iter().map(|point| point.weight).sum();

    Ok(RecoveryFit {
        amplitude,
        tau,
        plateau,
        rmse: (error / total_weight as f64).sqrt(),
    })
}

fn lerp(low: f64, high: f64, index: usize) -> f64 {
    low + (high - low) * index as f64 / 39.0
}

#[test]
fn reproduces_original_285_replay_fit() {
    let points = [
        (115.0, 13.49, 17097),
        (145.0, 5.52, 59522),
        (175.0, 3.55, 59728),
        (210.0, 1.05, 56494),
        (255.0, 0.10, 65080),
        (310.0, -2.82, 82518),
        (380.0, -3.05, 55439),
        (470.0, -2.95, 39808),
        (585.0, -3.27, 26597),
        (750.0, -2.96, 19162),
    ]
    .map(|(gap, offset, weight)| FitPoint {
        gap,
        offset,
        weight,
    });
    let fit = fit_recovery_curve(&points, 6).unwrap();

    assert!((fit.amplitude - 73.12).abs() <= 0.02, "{fit:?}");
    assert!((fit.tau - 72.40).abs() <= 0.02, "{fit:?}");
    assert!((fit.plateau - -3.19).abs() <= 0.01, "{fit:?}");
    assert!((fit.rmse - 0.73).abs() <= 0.01, "{fit:?}");
}

#[test]
fn rejects_underidentified_fit() {
    let points = [
        FitPoint {
            gap: 100.0,
            offset: 1.0,
            weight: 20,
        },
        FitPoint {
            gap: 200.0,
            offset: 0.0,
            weight: 20,
        },
    ];

    assert_eq!(
        fit_recovery_curve(&points, 6).unwrap_err(),
        "recovery fit requires at least three populated bins"
    );
}
