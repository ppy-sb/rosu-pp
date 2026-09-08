# Reports and Analysis Tools

This document describes the report generation tools for analyzing the mania difficulty calculation.

## Overview

Report-specific functionality is gated behind the `reports` feature flag. This keeps the production algorithm lean while making comprehensive analysis tools available when needed.

## Feature Flag

The `reports` feature flag enables:
- Additional struct fields needed for detailed reporting (map data, attributes, performance data)
- Access to report generation functions
- Standalone report binaries

### Production builds (default)
```bash
cargo build --release
```

Production builds exclude report-specific fields, keeping struct sizes minimal.

### Report builds
```bash
cargo build --release --features reports
cargo test --release --features reports
```

Report builds include all diagnostic fields and enable the report tools.

## Available Reports

### 1. Multiuser Report

Analyzes score data from real players to validate the algorithm against production data.

**Input**: TSV file with columns: uid, map_id, mods, stars, keys, counts (320/300/200/100/50/miss), accuracy, live_pp, title, version

**Output**: Console table comparing:
- Live PP (from production)
- Rebirth PP (historical)
- Current PP (experimental)

**Run as test**:
```bash
cargo test --release --lib --features reports \
  mania::sunny::tests::multiuser_report -- \
  --ignored --nocapture --exact
```

**Environment variables**:
- `SUNNY_MULTIUSER_TSV`: Path to multiuser TSV file (default: `local-fixtures/multiuser.tsv`)
- `SUNNY_MAPS`: Directory containing .osu map files (default: `local-fixtures/maps`)

**Binary wrapper**:
```bash
cargo run --release --features reports --bin multiuser_report
```

### 2. Surface Dump (2D Visualization)

Generates CSV data for visualizing the accuracy surface over (difficulty, sigma) space.

**Outputs** (to `target/surface/`):
- `grid.csv`: Accuracy and miss rate over difficulty × sigma grid
- `bands.csv`: Judgment band distributions vs sigma at fixed difficulty
- `windows.csv`: Accuracy vs sigma for different window sets (NM/EZ/HR)
- `surface_2d_meta.csv`: Metadata about the slice
- `input_state_bins.csv`: Input state bin statistics (if map provided)
- `per_note_difficulty.csv`: Per-note difficulty values (if map provided)
- `per_note_expected_counts.csv`: Expected judgments per note (if map provided)

**Run as test**:
```bash
cargo test --release --lib --features reports \
  mania::sunny::tests::surface_dump -- \
  --ignored --nocapture --exact
```

**Environment variables**:
- `SURFACE_MAP`: Path to .osu file (optional, defaults to synthetic Decoy slice)
- `SURFACE_CLOCK_RATE`: Clock rate multiplier (default: 1.0)
- `SURFACE_CORE_SIGMA`: Core timing spread for per-note overlay (default: TIMING_BASELINE_SIGMA)
- `SURFACE_SIGMAS`: Comma-separated sigma values to sample (optional)

**Binary wrapper**:
```bash
cargo run --release --features reports --bin surface_dump
```

**Example with custom map**:
```bash
SURFACE_MAP=local-fixtures/maps/12345.osu \
SURFACE_CLOCK_RATE=1.5 \
cargo test --release --lib --features reports \
  mania::sunny::tests::surface_dump -- \
  --ignored --nocapture --exact
```

## Struct Fields Affected

### `MultiPriced`

Fields only available with `reports` feature:
```rust
#[cfg(any(test, feature = "reports"))]
map: Arc<Beatmap>,

#[cfg(any(test, feature = "reports"))]
attrs: Arc<SunnyManiaDifficultyAttributes>,

#[cfg(any(test, feature = "reports"))]
perf: SunnyManiaPerformanceAttributes,
```

These fields provide full context for report generation but are not needed by the production algorithm.

## Why This Design?

1. **Production efficiency**: The core algorithm doesn't need map data, detailed attributes, or performance breakdowns after calculation completes. Excluding these fields reduces memory usage.

2. **Analysis completeness**: Reports need full context to compare different approaches, debug issues, and visualize the algorithm's behavior.

3. **Feature flag convention**: Using `#[cfg(any(test, feature = "reports"))]` means:
   - Tests always have access (can validate both paths)
   - Report binaries explicitly opt in
   - Production builds stay lean

## Running All Report Tests

```bash
cargo test --release --lib --features reports mania::sunny::tests -- --ignored --nocapture
```

This runs all ignored tests in the mania::sunny::tests module, including both reports above.
