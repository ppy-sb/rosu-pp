# Mania Accuracy Surface Research

## Problem Statement

The accuracy surface has minimal impact on PP for rice (non-LN) patterns, with surface_transfer staying near 1.0 even as accuracy varies significantly (e.g., 0.2-0.35% PP impact for large accuracy differences).

## Root Cause Analysis

The dual-fit architecture compares:
- `played_skill`: fitted from actual judgement counts through played windows
- `baseline_skill`: fitted from same counts through natural windows

For rice patterns, both fits see nearly identical judgement distributions, causing the ratio to normalize to ~1.0 regardless of accuracy level.

### Why LN Patterns Work Differently

LN patterns show stronger surface response because:
- LN judgements involve both head press and release timing
- This creates structural distributional differences between the two fits
- More degrees of freedom in the judgement distribution

## Attempted Solutions

### 1. Amplification (FAILED)
**Approach**: Amplify deviations from 1.0 with a multiplier
```rust
let amplified = 1.0 + (surface_transfer - 1.0) * AMPLIFICATION_FACTOR;
```
**Result**: EZ penalties amplified catastrophically (0.82 → 0.10 → near-zero PP after ^2.2), while rice barely moved (1.00 → 1.01)
**Why it failed**: Amplifies both signal AND noise equally; destroys EZ balance

### 2. Prediction-based (FAILED)
**Approach**: Compare actual distribution against expected distribution at map difficulty
**Result**: 100% accuracy scores penalized (-33% PP), exceptional distributions penalized
**Why it failed**: Doesn't align with PP philosophy of rewarding achieved 320s generously

### 3. Bounded Skill Ratio (FAILED)
**Approach**: Use `attrs.stars` as baseline_skill directly instead of fitting
**Result**: SS scores got 220-947% PP increases, unbounded growth
**Why it failed**: Skill grows unbounded as accuracy approaches 100%; ratio isn't naturally bounded

### 4. Composition Comparison (TESTED)
**Approach**: Compare judgement counts directly without skill-fitting
```rust
played_acc = sum(played_counts[i] * weights[i]) / total
expected_acc = sum(expected_counts[i] * weights[i]) / total
surface_transfer = played_acc / expected_acc
```
**Result**: Works identically to Sunny's acc_multiplier (just compares overall accuracy %)
**Why**: Without knowing which notes got which judgements, can't extract distributional information beyond overall accuracy

## Fundamental Limitation

A score only reports aggregate judgement counts:
- `{320: X, 300: Y, 200: Z, 100: W, 50: V, miss: M}`
- We don't know **which specific notes** got which judgements

Even though we know the map has notes at different difficulty levels, we can't distinguish:
- Hit hard notes with 320s + missed easy notes
- Hit easy notes with 320s + missed hard notes

**Hypothesis** (not proven): For rice patterns, judgement counts alone may not contain enough information to meaningfully improve upon a simple accuracy-percentage-based multiplier. The acc surface's value is in:
1. Pattern-type awareness (LN release timing difficulty)
2. Mod adjustments (EZ/HR window effects)
3. Input-state modeling (recovery/anticipation)

Rather than extracting finer timing precision from rice judgement distributions.

## Current Status

The previous natural-window comparison was identified as a semantic bug. EZ scores are
already judged under their wider windows, so comparing their counts against unmodded
windows charged the same mod twice. The production baseline now uses the score's actual
windows and the same structural units as the played fit.

The corrected multiuser report (1204 scores) shows:

- EZ surface scalar: `1.0000`, with total PP change falling from roughly `-38%` to
  roughly `-5%` (the remaining SR/difficulty effect).
- Non-EZ surface scalar: `1.0000` and approximately `0%` surface contribution.
- Rice, LN-light, and LN-heavy cohorts: no measurable surface PP contribution.

This does **not** show that the underlying map-conditioned model is blind to structure.
The visualization and earlier recovery sweeps showed that changing recovery parameters
can substantially change the predicted composition and surface curves across maps. The
failure is in turning that response into a reliable score-level PP signal.

### Revised Diagnosis: Skill Unit Is the Suspect

The current implementation has one fitted scalar skill. Map structure changes the
`skill -> expected judgement composition` curve, but the score still supplies only one
aggregate judgement-count vector. Fitting that same vector twice lets the free skill
parameter absorb recovery/structure changes, so the ratio can collapse to `1.0` even
when the fixed-skill expected distributions differ.

This distinguishes two claims:

1. The map-conditioned error model can respond to OD, density, LN/rice structure, and
   recovery in synthetic/fixed-skill predictions.
2. The current one-dimensional fitted `skill` ratio is not a validated second accuracy
   dimension for PP.

The current dead end is therefore specifically:

```text
same observed counts -> fit skill with model A
same observed counts -> fit skill with model B
surface = skill_A / skill_B
```

It is not evidence that every possible accuracy surface is useless. It is evidence that
this skill-unit/ratio construction has no independent anchor in the available score
state.

**Open questions for future research:**
- Does LN surface response remain strong with composition-based comparison?
- Can we verify the hypothesis with synthetic data (known difficulty × judgement mappings)?
- Are there alternative formulations that extract distributional information without skill-fitting?
- Would per-note replay data (hit offsets) enable better accuracy modeling?
- Can a skill unit be rooted to an externally calibrated quantity instead of being fit
  freely from the same counts used by the surface?
- Can a second identifiable dimension (for example consistency/variance) be introduced
  without inventing per-note judgements?
- Which recovery sweep results are genuine structure response, and which are changes in
  the fitted skill gauge?

## Technical Details

### Expected Counts Calculation
```rust
let expected = expected_counts(&units, &attrs.hit_windows, model, attrs.stars);
```
Uses `attrs.stars` as reference skill: "what distribution would we expect at this SR?"

### Judgement Units
Contains per-difficulty-bin information, but when summed into total expected counts, the structural information is lost. Individual bin comparisons would require knowing which notes the player actually hit.
