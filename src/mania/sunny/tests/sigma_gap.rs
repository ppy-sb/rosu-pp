//! Measure how timing spread (sigma) varies with gap time between notes.
//!
//! This complements the recovery offset measurement by quantifying not just
//! the mean shift but also the spread widening under density pressure.

use super::replay::ReplayAnalysis;
use std::collections::HashMap;

/// A bin collecting timing errors for notes within a gap range.
#[derive(Clone, Debug, Default)]
pub struct SigmaBin {
    /// Sum of timing errors (for computing mean)
    pub sum: f64,
    /// Sum of squared timing errors (for computing variance)
    pub sum_sq: f64,
    /// Number of notes in this bin
    pub count: u64,
}

impl SigmaBin {
    pub fn add(&mut self, error: f64) {
        self.sum += error;
        self.sum_sq += error * error;
        self.count += 1;
    }

    pub fn mean(&self) -> f64 {
        if self.count == 0 {
            0.0
        } else {
            self.sum / self.count as f64
        }
    }

    pub fn sigma(&self) -> f64 {
        if self.count < 2 {
            return 0.0;
        }
        let mean = self.mean();
        let variance = self.sum_sq / self.count as f64 - mean * mean;
        variance.max(0.0).sqrt()
    }
}

/// Bin edges in milliseconds for gap-based sigma measurement.
///
/// Log-spaced to match the actual distribution of gaps in mania charts.
/// From very dense (jacks/streams) to sparse (breaks).
pub const GAP_BIN_EDGES: [f64; 11] = [
    0.0, 40.0, 60.0, 80.0, 100.0, 130.0, 170.0, 220.0, 300.0, 450.0, 700.0,
];

/// Representative gap for each bin (geometric midpoint).
pub const GAP_BIN_REPRESENTATIVES: [f64; 12] = [
    20.0,          // 0-40ms: very dense (jacks)
    50.0,          // 40-60ms: dense streams
    70.0,          // 60-80ms: medium-dense
    90.0,          // 80-100ms: normal streams
    115.0,         // 100-130ms: moderate
    150.0,         // 130-170ms: relaxed
    195.0,         // 170-220ms: slow
    260.0,         // 220-300ms: sparse
    370.0,         // 300-450ms: very sparse
    570.0,         // 450-700ms: extremely sparse
    1000.0,        // 700ms+: breaks
    f64::INFINITY, // no predecessor (first note)
];

/// Which gap bin a given gap_ms belongs to.
pub fn gap_bin_index(gap_ms: f64) -> usize {
    if !gap_ms.is_finite() || gap_ms < 0.0 {
        return GAP_BIN_EDGES.len(); // No predecessor bin
    }

    GAP_BIN_EDGES
        .iter()
        .position(|&edge| gap_ms < edge)
        .unwrap_or(GAP_BIN_EDGES.len())
}

/// Extract timing errors with their corresponding gap times from replay analysis.
///
/// For each error, computes the gap from the previous note (or infinity for the first note).
pub fn extract_errors_with_gaps(analysis: &ReplayAnalysis) -> Vec<(f64, f64)> {
    let mut result = Vec::with_capacity(analysis.errors.len());

    for &(note_idx, error) in &analysis.errors {
        let gap_ms = if note_idx == 0 {
            f64::INFINITY
        } else {
            let current_time = analysis.notes[note_idx].time as f64;
            let prev_time = analysis.notes[note_idx - 1].time as f64;
            current_time - prev_time
        };

        result.push((gap_ms, error));
    }

    result
}

/// Collect timing errors into bins by gap time.
///
/// Returns a map from bin index to SigmaBin containing aggregated statistics.
pub fn bin_errors_by_gap(errors_with_gaps: &[(f64, f64)]) -> HashMap<usize, SigmaBin> {
    let mut bins: HashMap<usize, SigmaBin> = HashMap::new();

    for &(gap_ms, error_ms) in errors_with_gaps {
        let bin_idx = gap_bin_index(gap_ms);
        bins.entry(bin_idx).or_default().add(error_ms);
    }

    bins
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gap_bin_index_works() {
        assert_eq!(gap_bin_index(20.0), 1, "0-40ms"); // 0-40ms
        assert_eq!(gap_bin_index(50.0), 2, "40-60ms"); // 40-60ms
        assert_eq!(gap_bin_index(90.0), 4, "80-100ms"); // 80-100ms
        assert_eq!(gap_bin_index(150.0), 6, "130-170ms"); // 130-170ms
        assert_eq!(gap_bin_index(800.0), 11, "700ms+"); // 700ms+
        assert_eq!(gap_bin_index(f64::INFINITY), 11); // No predecessor
        assert_eq!(gap_bin_index(f64::NAN), 11); // Invalid
    }

    #[test]
    fn sigma_bin_computes_statistics() {
        let mut bin = SigmaBin::default();
        bin.add(0.0);
        bin.add(2.0);
        bin.add(4.0);

        assert_eq!(bin.count, 3);
        assert!((bin.mean() - 2.0).abs() < 1e-9);
        // sigma = sqrt(((0-2)^2 + (2-2)^2 + (4-2)^2) / 3) = sqrt(8/3) ≈ 1.633
        assert!((bin.sigma() - 1.633).abs() < 0.001);
    }
}
