//! Multiuser report tool for analyzing score data.
//!
//! Prices every score in `local-fixtures/multiuser.tsv` and prints live Sunny,
//! locally reconstructed Sunny, and experimental surface totals side by side.
//!
//! Environment variables:
//! - `SUNNY_MULTIUSER_TSV`: Path to multiuser TSV file (default: local-fixtures/multiuser.tsv)
//! - `SUNNY_MAPS`: Directory containing .osu map files (default: local-fixtures/maps)
//!
//! Run with: `cargo run --release --features reports --bin multiuser_report`

fn main() {
    eprintln!("This binary requires the test code to be available.");
    eprintln!("Run the multiuser report test directly instead:");
    eprintln!("  cargo test --release --lib --features reports mania::sunny::tests::multiuser_report -- --ignored --nocapture --exact");
    std::process::exit(1);
}
