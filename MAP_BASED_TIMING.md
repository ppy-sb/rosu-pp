Map-Based Timing Difficulty - Hybrid Approach
==============================================

CONCEPT:
Two-factor timing adjustment that separates map difficulty from player performance:

1. MAP TIMING FACTOR (computed at SR, constant for map+mods)
   - Based on expected_accuracy at baseline sigma (12ms, ~8.5ms player skill)
   - Formula: 1.0 + (0.994 - expected_acc) * 15.0
   - EZ maps (0.998 exp acc) → 0.94 factor (easier to acc)
   - NM maps (0.994 exp acc) → 1.0 factor (baseline)
   - HR maps (0.986 exp acc) → 1.12 factor (harder to acc)
   - Also considers LN structure (反键-like patterns get modest boost)

2. SCORE TIMING ADJUSTMENT (computed per score)
   - Compares player's actual acc to map's expected_acc
   - Formula: sqrt(score_acc / expected_acc), clamped [0.85, 1.15]
   - Uses sqrt for dampening (don't want huge swings)
   - 97% on map expecting 95% → 1.01 adjustment (+1%)
   - 95% on map expecting 97% → 0.99 adjustment (-1%)

COMBINED EFFECT:
timing_multiplier = map_timing_factor × score_timing_adjustment

RESULTS (1204 scores):
- EZDT: map factor 0.94, scores 86-88% vs 99.7% expected → total -11% to -23%
- Clean NM: map factor ~1.0, scores 97-100% vs 99.4% expected → total -0.1% to +0.2%
- Good performance on hard maps: up to +10% bonus
- Tests: 23/23 passing

ADVANTAGES:
✓ Map difficulty stable across all scores (map factor doesn't change)
✓ Still rewards exceptional timing within same map/mods (score adjustment)
✓ EZ/HR properly priced via window width affecting expected_acc
✓ 反键 LN patterns can get structural boost
✓ Uses existing expected_acc from input-state model (305-based)

FILES MODIFIED:
- src/mania/sunny.rs:
  - Added timing_difficulty_factor field to SunnyManiaDifficultyAttributes
  - compute_map_timing_difficulty() computes map factor from expected_acc
  - calculate_performance_inner() applies both map factor and score adjustment
