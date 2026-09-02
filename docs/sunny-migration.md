# Sunny Mania Migration

## Purpose

The improved Star-Rating-Rebirth (Sunny) mania algorithm was developed and
validated in `rosu-pp-js`. It now lives in `rosu-pp` so the algorithm and its
measurement assets can evolve with the Rust implementation, while
`rosu-pp-js` remains a WASM wrapper and integration-test repository.

## Rust API

The implementation is available alongside the existing mania algorithm:

- `rosu_pp::mania::sunny`
- `rosu_pp::mania::SunnyManiaDifficultyAttributes`
- `rosu_pp::mania::SunnyManiaPerformanceAttributes`
- `rosu_pp::mania::SunnyScoreState`
- `rosu_pp::mania::sunny_accuracy`
- `rosu_pp::mania::sunny_windows`

The Sunny module exposes `calculate` and `calculate_performance`, plus the
input-state and long-note metadata types used by the accuracy model. Existing
mania APIs remain available; the regular mania difficulty path currently uses
Sunny-derived stars, variety, and accuracy scalar values.

## Migrated Assets

The following were moved from `rosu-pp-js` into this repository:

- `tools/`: visualisers, diagnostics, data builders, fetchers, and Python test
  harnesses
- `local-fixtures/`: local maps, replays, reports, and tabular fixtures
- Sunny Rust unit tests, carried with `src/mania/sunny.rs`

The compositional harness now runs from the `rosu-pp` checkout and imports
`rosu_pp::mania::sunny`.

## Wrapper Boundary

`rosu-pp-js` retains the WASM-facing wrapper and its end-to-end integration
test (`test-sunny.mjs`). Algorithm calibration, fixture generation, and
Rust-level behavioral tests belong here in `rosu-pp`.

## Validation

- Migrated Python harnesses pass `py_compile` syntax checks.
- The Sunny integration has no Sunny-specific Rust type errors.
- Full `cargo check` is currently limited by pre-existing toolchain errors in
  unrelated `rosu-pp` modules that use unstable `let` chains and an existing
  const-evaluation issue.

## Follow-up

The remaining architectural step is to decide whether and when Sunny's full
timing metadata should flow through the canonical mania performance attribute
type. Until that is deliberately migrated, keep the standalone Sunny API
additive and do not interpret legacy cached mania attributes as Sunny data.
