# Mania Accuracy Surface Discussion - 2026-09-03

## Context

We are working on the osu!mania PP algorithm (sunnyxxy alternative system). The goal is to:
1. **Reduce EZ mod produced PP** (currently overrewarded)
2. **Reward fairly for low OD long note (LN) maps** (currently underrewarded, especially 反键/anti-key patterns)

## Previous Failed Approach

From `docs/mania-acc-surface-research.md`:
- Attempted to fix by adjusting the "accuracy surface" (OD-based hit window function)
- **Failed** because the surface is input-agnostic - it doesn't know about mods, note types, or gameplay context
- Trying to fix mod-specific or pattern-specific issues with surface adjustments creates artificial discontinuities

## Current Implementation Architecture

### The "Curve" (Judgment Probability Model) ✅ WORKS

**Location**: `src/mania/sunny_accuracy.rs`

**What it does**:
```rust
judgement_probabilities(windows, model, difficulty, skill) -> JudgementProbabilities
```

**Inputs**:
- `windows`: Hit windows (16.5ms for 320, 40.5ms for 300, etc.) - mod-aware
- `model`: ErrorModel parameters (sigma_ref, skill_exponent, lapse_weight, etc.)
- `difficulty`: Per-note local difficulty (from SR calculation)
- `skill`: Player skill level in star-rating units

**Output**:
- Probability distribution: P(320), P(300), P(200), P(100), P(50), P(miss)

**Key function**:
```rust
pub fn expected_counts(
    units: &[JudgementUnit],
    windows: &ManiaHitWindows,
    model: &ErrorModel,
    skill: f64,
) -> ExpectedCounts
```

For a given skill level, predicts the expected judgment counts across all notes.

**Why it works**:
- Correctly models per-note accuracy probability based on:
  - Hit window size (wider windows = easier to hit)
  - Pattern difficulty
  - LN vs rice note differences (LN has wider sigma via `sigma_scale`)
  - Input state (recovery offset, release offset, etc.)
- Produces sensible probability distributions
- Good visualizations (see tools/mania_surface_2d.py)

### The "Surface" (Skill-Based Translation) ❌ BROKEN

**Location**: `src/mania/sunny.rs` lines 747-826, 1338-1407

**What it tries to do**:
```rust
// Invert: judgment counts → skill level
let played_skill = skill_for_counts(&counts, &units, &hit_windows, model);
let baseline_skill = skill_for_counts(&counts, &units, &hit_windows, &baseline_model);

// Compute ratio
let window_scalar = played_skill / baseline_skill;

// Use ratio in PP calculation
let surface_power = surface_transfer.powf(2.2);
let accuracy_reward = accuracy_proportion * acc_multiplier * surface_power;
```

**Why it's broken**:

1. **Ratio collapses to ~1.00**:
   - Both `played_skill` and `baseline_skill` are fitted against the **same hit windows**
   - The only difference is `recovery_offset` being enabled/disabled
   - The recovery offset effect is small, so ratio ≈ 1.00
   - Result: the surface contributes nothing to PP

2. **No absolute anchor**:
   - From `sunny_accuracy.rs` line 207-215 comments:
     > "`sigma_ref` is a gauge parameter and cannot be calibrated. It sets the unit skill is expressed in, and skill is refit for every score, so a change in `sigma_ref` is absorbed exactly by the fitted skill and no observable moves at all"
   - Both skills scale with the same gauge
   - The ratio becomes invariant to the actual skill values
   - It's measuring "played vs baseline" but both are fitted from the same counts

3. **Wrong comparison**:
   - Current: `played_skill (EZ windows) / baseline_skill (EZ windows)` ≈ 1.00
   - The EZ mod effect isn't captured because both sides use EZ windows
   - Low OD isn't addressed because both sides see the same OD

**Code Evidence**:
```rust
// From compute_timing_pp() line 1365-1383
let played = fit_with_quality(&counts, &units, &attrs.hit_windows, model);

let baseline_model = ErrorModel {
    recovery_offset: 0.0,
    anticipation_offset: 0.0,
    ..*model
};
let baseline = fit_with_quality(
    &counts,
    &baseline_units,
    &attrs.hit_windows,  // SAME WINDOWS!
    &baseline_model,
);

// Ratio of two skills fitted from same data through same windows
// Only difference: recovery offset on/off
played.skill / baseline.skill  // ≈ 1.00
```

## The Fundamental Problem

The **curve** (probability model) requires a `skill` parameter as input to produce judgment probabilities.

The **surface** tries to provide that skill by:
1. Inverting the curve: `skill_for_counts()` finds what skill would produce the observed counts
2. Computing a ratio of two fitted skills
3. Using that ratio as a performance measure

**This inversion is the dead end**:
- Any skill-fitting approach loses the absolute anchor
- Comparing two fitted skills from the same score data produces meaningless ratios
- The gauge parameter problem makes all fitted skills relative, not absolute

## What the Curve Can and Cannot Do

### CAN DO ✅
- Model per-note judgment probability given:
  - Known difficulty
  - Known hit windows (mod-aware)
  - Known skill level
- Account for LN vs rice differences
- Account for input state (recovery, release timing)
- Produce expected judgment distributions

### CANNOT DO ❌
- Determine player skill from judgment counts (inversion is broken)
- Provide an absolute performance measure without external skill input
- Compare "difficulty" across different windows/ODs without skill fitting

## Current PP Formula

From `src/mania/sunny.rs` line 768-809:

```rust
// Pattern difficulty (accuracy-neutral)
let pp_pattern = 9.8 * pattern_difficulty.powf(2.2) 
    * variety_multiplier * length_multiplier * multiplier;

// Timing difficulty (from surface - currently broken)
let timing_result = compute_timing_pp(attrs, state, model);
let surface_transfer = (played_skill / baseline_skill).max(0.0);
let surface_power = surface_transfer.powf(2.2);

// Merged accuracy reward
let accuracy_reward = accuracy_proportion * acc_multiplier * surface_power;
let pp_timing = pp_pattern * (accuracy_reward - 1.0);

// Total
let pp = pp_pattern + pp_timing;
```

The `surface_power` term (from fitted skill ratio) is what's broken and contributing ~nothing.

## Two Paths Forward

### Path A: Find New Way to Use Curve
**Challenge**: The curve needs a skill input. How do we get it without skill fitting?

**Attempted ideas** (all blocked):
1. Use `attrs.stars` directly as skill
   - Problem: Not clear if stars are in the same units as curve's skill parameter
   - Problem: Still doesn't solve how to compare across different windows/ODs
2. Compare actual vs expected judgment distributions
   - Problem: Expected at what skill? We're back to needing skill fitting
3. Use the curve without skill parameter
   - Problem: The curve fundamentally requires skill as input

**Conclusion**: Path A appears blocked unless we find a way to determine skill without the broken fitting mechanism.

### Path B: Fix at SR Calculation Level ✅ VIABLE

**Approach**: Adjust strain/difficulty calculation during SR computation where we have full context.

**For EZ PP reduction**:
- During strain calculation, apply EZ-specific difficulty adjustments
- EZ makes patterns genuinely easier beyond just wider windows
- Adjust strain values when EZ mod is active

**For Low OD LN fair reward**:
- Adjust LN strain calculation based on actual OD
- Low OD LN patterns (especially 反键) should have higher strain
- Current algorithm may be undervaluing LN difficulty at low OD

**Advantages**:
- Direct: fixes root cause rather than patching afterwards
- Has context: SR calculation knows about mods, pattern types, OD
- No translation problem: difficulty IS the output, no need to convert
- Sunnyxxy already does this: uses fixed OD8 for SR, fixed acc curve for PP

**Next Steps**:
- Examine current strain calculation for EZ mod
- Examine current LN difficulty calculation at different ODs
- Identify specific levers to adjust

## Decision Point

**Should the curve stay or go?**

The curve itself (judgment probability model) is well-designed and works correctly. However:

1. **It requires skill fitting to be useful** for PP calculation
2. **Skill fitting is fundamentally broken** (ratio collapse, no anchor)
3. **We have no alternative way** to use the curve without skill fitting
4. **The visualizations are valuable** but don't translate to working PP

**Recommendation**: **Abandon the curve/surface approach for PP calculation.**

The curve represents significant work, but it's solving the wrong problem. We don't need a sophisticated probability model if we can't translate it to PP. Path B (fixing SR calculation) is more direct and doesn't have the translation problem.

## Questions for Next Session

1. **Confirm decision**: Abandon curve/surface, pursue Path B (SR-level fixes)?
2. **EZ mod in SR**: How does current rebirth algorithm handle EZ during strain calculation?
3. **LN at low OD**: How is LN difficulty currently calculated? Does it account for OD?
4. **Sunnyxxy comparison**: How does sunnyxxy handle these issues?
5. **Implementation**: What specific changes to make in strain calculation?

## Key Terminology for Future Reference

- **Curve**: The judgment probability model (`judgement_probabilities()`, `expected_counts()`)
  - Input: `(windows, model_parameters, difficulty, skill)`
  - Output: Probability distribution over judgments
  - Status: **Works correctly, but requires skill parameter**
  
- **Surface**: The skill-fitting and ratio-based translation mechanism
  - Inverts curve: counts → skill, then compares played_skill / baseline_skill
  - Status: **Broken - ratio collapses to ~1.00, no absolute anchor**
  
- **Skill fitting**: Using `skill_for_counts()` to invert the curve
  - Status: **Fundamentally flawed - gauge parameter problem, produces meaningless ratios**
  
- **SR/Stars**: Star rating from difficulty calculation (pattern difficulty, accuracy-neutral)
- **PP**: Performance points (SR + accuracy + mods)
- **Judgment units**: Per-note difficulty bins with LN sigma scaling
- **Window scalar**: The broken ratio of played_skill / baseline_skill (currently ≈1.00)

## The Critical Insight

**The curve cannot exist without the surface, and the surface is broken.**

The curve is a probability model that requires `skill` as an input parameter. The only way to get that skill parameter from a real score is through skill fitting (the surface). Since skill fitting is broken, the curve has no way to receive the input it needs.

**This is why the curve must be abandoned along with the surface.** It's not that the curve is bad - it's that it's part of an inseparable system where the other half (surface/skill fitting) is fundamentally broken.
