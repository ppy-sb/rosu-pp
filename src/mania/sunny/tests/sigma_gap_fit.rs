//! Fit a parametric function to the measured sigma vs gap curve.

// /// Measured sigma values at representative gap times from replay analysis.
// const MEASURED_SIGMA_POINTS: [(f64, f64); 11] = [
//     (50.0, 19.35),
//     (70.0, 23.97),
//     (90.0, 22.43),
//     (115.0, 25.97),
//     (150.0, 22.42),
//     (195.0, 17.56),
//     (260.0, 15.76),
//     (370.0, 15.52),
//     (570.0, 15.16),
//     (1000.0, 16.78),
//     (f64::INFINITY, 17.98), // no predecessor
// ];

/// Result of sigma curve fitting.
#[derive(Clone, Copy, Debug)]
pub struct SigmaFit {
    /// Baseline sigma at very sparse gaps (ms)
    pub baseline: f64,
    /// Peak additional sigma above baseline (ms)
    pub peak_amplitude: f64,
    /// Gap time where sigma peaks (ms)
    pub peak_gap: f64,
    /// Width parameter controlling how fast sigma decays from peak (ms)
    pub width: f64,
    /// Root mean square error of the fit
    pub rmse: f64,
}

/// Evaluate a Gaussian-like sigma curve.
///
/// `sigma(gap) = baseline + amplitude * exp(-(gap - peak)^2 / (2 * width^2))`
fn eval_sigma_curve(gap: f64, baseline: f64, amplitude: f64, peak: f64, width: f64) -> f64 {
    if !gap.is_finite() {
        return baseline; // No predecessor uses baseline
    }

    let deviation = gap - peak;
    let exponent = -(deviation * deviation) / (2.0 * width * width);
    baseline + amplitude * exponent.exp()
}

/// Fit a Gaussian-like curve to measured sigma vs gap data.
///
/// Uses grid search similar to recovery curve fitting.
pub fn fit_sigma_curve(
    points: &[(f64, f64, u64)], // (gap, sigma, weight)
    refinements: usize,
) -> Result<SigmaFit, &'static str> {
    if points.len() < 4 {
        return Err("sigma fit requires at least four populated points");
    }

    // Initial search ranges based on observed data
    let (mut base_lo, mut base_hi) = (12.0, 20.0); // baseline sigma
    let (mut amp_lo, mut amp_hi) = (4.0, 12.0); // peak amplitude
    let (mut peak_lo, mut peak_hi) = (80.0, 150.0); // peak gap location
    let (mut width_lo, mut width_hi) = (50.0, 200.0); // width parameter

    let mut best = (f64::INFINITY, 0.0, 0.0, 0.0, 0.0);

    for _ in 0..refinements {
        best = (0..20)
            .flat_map(|i| {
                let baseline = lerp(base_lo, base_hi, i, 19);
                (0..20).flat_map(move |j| {
                    let amplitude = lerp(amp_lo, amp_hi, j, 19);
                    (0..20).flat_map(move |k| {
                        let peak = lerp(peak_lo, peak_hi, k, 19);
                        (0..20).map(move |m| {
                            let width = lerp(width_lo, width_hi, m, 19);

                            let error: f64 = points
                                .iter()
                                .filter(|(gap, _, _)| gap.is_finite()) // Skip infinity point
                                .map(|&(gap, observed_sigma, weight)| {
                                    let predicted =
                                        eval_sigma_curve(gap, baseline, amplitude, peak, width);
                                    weight as f64 * (predicted - observed_sigma).powi(2)
                                })
                                .sum();

                            (error, baseline, amplitude, peak, width)
                        })
                    })
                })
            })
            .min_by(|a, b| a.0.total_cmp(&b.0))
            .unwrap();

        let (_, baseline, amplitude, peak, width) = best;
        let base_span = (base_hi - base_lo) / 8.0;
        let amp_span = (amp_hi - amp_lo) / 8.0;
        let peak_span = (peak_hi - peak_lo) / 8.0;
        let width_span = (width_hi - width_lo) / 8.0;

        (base_lo, base_hi) = (baseline - base_span, baseline + base_span);
        (amp_lo, amp_hi) = ((amplitude - amp_span).max(0.1), amplitude + amp_span);
        (peak_lo, peak_hi) = ((peak - peak_span).max(10.0), peak + peak_span);
        (width_lo, width_hi) = ((width - width_span).max(10.0), width + width_span);
    }

    let (error, baseline, amplitude, peak, width) = best;
    let total_weight: u64 = points.iter().map(|(_, _, w)| w).sum();

    Ok(SigmaFit {
        baseline,
        peak_amplitude: amplitude,
        peak_gap: peak,
        width,
        rmse: (error / total_weight as f64).sqrt(),
    })
}

fn lerp(low: f64, high: f64, index: usize, max_index: usize) -> f64 {
    low + (high - low) * index as f64 / max_index as f64
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_sigma_curve_works() {
        let baseline = 15.0;
        let amplitude = 8.0;
        let peak = 100.0;
        let width = 80.0;

        // At peak gap, should be baseline + amplitude
        let at_peak = eval_sigma_curve(100.0, baseline, amplitude, peak, width);
        assert!((at_peak - 23.0).abs() < 0.1);

        // Far from peak, should approach baseline
        let far_away = eval_sigma_curve(500.0, baseline, amplitude, peak, width);
        assert!(far_away < baseline + 1.0);
    }

    #[test]
    fn fit_sigma_curve_works() {
        let points = vec![
            (50.0, 19.35, 461872),
            (70.0, 23.97, 31905),
            (90.0, 22.43, 101516),
            (115.0, 25.97, 124734),
            (150.0, 22.42, 98309),
            (195.0, 17.56, 140435),
            (260.0, 15.76, 120108),
            (370.0, 15.52, 43747),
            (570.0, 15.16, 40924),
            (1000.0, 16.78, 7353),
        ];

        let fit = fit_sigma_curve(&points, 5).unwrap();

        println!(
            "Sigma fit: baseline={:.2} peak_amplitude={:.2} peak_gap={:.2} width={:.2} rmse={:.2}",
            fit.baseline, fit.peak_amplitude, fit.peak_gap, fit.width, fit.rmse
        );

        // Check that fit is reasonable
        assert!(
            fit.baseline > 10.0 && fit.baseline < 20.0,
            "baseline={}",
            fit.baseline
        );
        assert!(
            fit.peak_amplitude > 2.0 && fit.peak_amplitude < 15.0,
            "amplitude={}",
            fit.peak_amplitude
        );
        assert!(
            fit.peak_gap > 80.0 && fit.peak_gap < 150.0,
            "peak_gap={}",
            fit.peak_gap
        );
        assert!(fit.width > 40.0 && fit.width < 250.0, "width={}", fit.width);
        assert!(fit.rmse < 3.0, "rmse={}", fit.rmse); // Should fit reasonably well
    }
}
