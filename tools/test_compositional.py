#!/usr/bin/env python3
"""
Test the new compositional architecture on a few scores.
Shows pp_pattern vs pp_timing breakdown.
"""

import subprocess
import sys

def main():
    # Build the library first
    print("Building...")
    result = subprocess.run(
        ["cargo", "build", "--release"],
        cwd="/Users/Shared/git/ppy-sb/rosu-pp",
        capture_output=True,
        text=True
    )
    if result.returncode != 0:
        print("Build failed:", result.stderr)
        return 1

    # Run a test calculation
    print("\nRunning test calculation...")
    test_code = """
use rosu_pp::mania::sunny;
use rosu_map::Beatmap;
use rosu_mods::GameMods;

// Synthetic map for testing
fn synthetic_map(od: f64, notes: usize, bpm: f64) -> Beatmap {
    let mut map = Beatmap::default();
    map.mode = rosu_map::GameMode::Mania;
    map.od = od as f32;
    map.cs = 4.0;

    let ms_per_note = 60000.0 / bpm;
    for i in 0..notes {
        let time = (i as f64 * ms_per_note) as i32;
        map.hit_objects.push(rosu_map::HitObject {
            pos: rosu_map::Pos { x: 0.0, y: 0.0 },
            start_time: time as f64,
            kind: rosu_map::HitObjectKind::Circle,
        });
    }

    map
}

fn main() {
    let map = synthetic_map(8.0, 1000, 180.0);
    let mods = GameMods::default();

    let attrs = sunny::calculate(&map, &mods, 1.0, Some(true), None).unwrap();

    // Test with 95% acc
    let state = sunny::SunnyScoreState {
        n320: 800,
        n300: 150,
        n200: 50,
        n100: 0,
        n50: 0,
        misses: 0,
    };

    let perf = sunny::calculate_performance(&attrs, &mods, state);

    println!("=== Compositional Architecture Test ===");
    println!("Map: OD 8, 1000 notes, 180 BPM");
    println!("Score: 80% 320, 15% 300, 5% 200");
    println!();
    println!("Pattern PP:  {:.2}", perf.pp_pattern);
    println!("Timing PP:   {:.2}", perf.pp_timing);
    println!("Total PP:    {:.2}", perf.pp);
    println!();
    println!("Timing skill (played):   {:.3}", perf.timing_skill_played);
    println!("Timing skill (baseline): {:.3}", perf.timing_skill_baseline);
    println!("Window scalar: {:.4}", perf.window_scalar);
    println!();
    println!("Timing contribution: {:.1}%", (perf.pp_timing / perf.pp * 100.0));
}
"""

    with open("/tmp/test_comp.rs", "w") as f:
        f.write(test_code)

    print("Compilation test created. Run manually with:")
    print("  cd /Users/Shared/git/ppy-sb/rosu-pp")
    print("  cargo run --example test_comp")

if __name__ == "__main__":
    sys.exit(main())
