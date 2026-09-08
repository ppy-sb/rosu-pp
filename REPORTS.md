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

TSV/CSV rows include beatmap metadata (`map`, `artist`, `title`, `diff`) followed by mods, score state, and calculated PP fields.

**Run**:
```bash
cargo run --release --features reports --bin multiuser_report -- \
  --tsv local-fixtures/multiuser.tsv \
  --maps local-fixtures/maps
```

**Arguments**:
- `--tsv`: Path to multiuser TSV file (default: `local-fixtures/multiuser.tsv`)
- `--maps`: Directory containing .osu map files (default: `local-fixtures/maps`)
- `--output-format`: `text` (default), `tsv`, or `csv`. TSV/CSV emit machine-readable score rows only.

Both arguments are optional and will use defaults if not specified.

Example machine-readable output:
```bash
cargo run --release --features reports --bin multiuser_report -- \
  --output-format=csv > report.csv
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

**Run**:
```bash
cargo run --release --features reports --bin surface_dump
```

**Arguments**:
- `--map`: Path to .osu file (optional, defaults to synthetic Decoy slice)
- `--clock-rate`: Clock rate multiplier (default: 1.0)
- `--core-sigma`: Core timing spread for per-note overlay in ms (optional, defaults to TIMING_BASELINE_SIGMA)
- `--sigmas`: Comma-separated sigma values to sample (optional)

**Example with custom map**:
```bash
cargo run --release --features reports --bin surface_dump -- \
  --map local-fixtures/maps/12345.osu \
  --clock-rate 1.5 \
  --core-sigma 15.0
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
