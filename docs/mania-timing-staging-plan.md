# Mania Timing Model Staging Plan

Date: 2026-09-07

## Current Context

Sunny/Rebirth already supplies the mania star rating and its existing
star-rating-related performance value. The millisecond timing model is an
additional layer, not a replacement for Sunny in the first release.

The input-state model is active: it changes the judgement composition by
state, including lower judgements and misses, rather than acting as a single
accuracy multiplier. `core_sigma` controls the player's baseline timing spread;
`sigma_scale` and the input-state parameters vary that spread by note/state.

The 11 ms reference currently used by the surface is a fixed reference player,
not a guarantee that every map is comfortably playable at that spread. On a
high-difficulty map, the same 11 ms core can legitimately produce many misses.
A smaller value such as 4 ms represents a substantially more precise player and
can produce a good composition on that map.

The correct terminology for the concern is that the fixed core sigma and local
difficulty are being *coupled* or *conflated*. The model is not flat; its
reference-player calibration is the issue.

## Agreed Stage 1 Scope

For the first release, keep Sunny/Rebirth responsible for SR and its existing
SR-related PP. Use the timing surface only for timing-related adjustments:

- EZ penalty through the already-modified judgement windows
- LN timing treatment and any LN-specific reward
- general accuracy PP updates
- input-state-dependent judgement composition and timing variation

Do not add separate EZ, HR, or DT branches to the input-state model. Window
affecting mods are already folded into `hit_windows` before the SR/PP stages,
and the timing model should consume those windows as data.

The input-state simulation and the score-reward policy must remain separate
questions. A low expected accuracy at 11 ms may be a valid statement about the
reference player's difficulty. It must not automatically become a large PP
bonus merely because a submitted score beats that reference.

## Reward Attribution

When investigating an EZDT result, split the calculation into:

```text
sunny_base = xxy_pp_pattern + xxy_pp_accuracy
final_pp   = sunny_base * timing_multiplier
```

If `sunny_base` is already higher, the behavior belongs to Sunny/Rebirth and
should be recorded separately. If only `timing_multiplier` moves the result,
the behavior belongs to the added timing layer. The timing layer is exposed by
`timing_map_factor` and `timing_score_adjustment` in the Sunny performance
attributes.

The current discussion is intentionally not changing this reward policy. The
first priority is to validate and calibrate input-state simulation.

## Player-Dependent Measured Parameters

The measured constants were fitted from a mid/high-level player population.
They should therefore be treated as population averages, not immutable human
facts for every player tier.

Likely dependencies include:

- `core_sigma`: strongly player-dependent; stronger players should have lower
  baseline spread.
- `sigma_baseline` and `sigma_peak_amplitude`: likely scale with player
  precision, hardware, and fatigue.
- `sigma_peak_gap` and `sigma_width`: primarily describe the density response,
  but may vary by keymode, speed, and play style.
- `recovery_offset` and `recovery_tau`: likely player/style-dependent recovery
  behavior.
- `anticipation_offset`: likely a player-specific timing bias rather than a
  universal constant.

A useful eventual form is a hierarchical model with global shape parameters and
player-specific effects:

```text
sigma(player, gap) = core_sigma(player)
                     * [1 + amplitude(player) * density_shape(gap)]

offset(player, gap) = player_bias
                      + recovery_amplitude(player) * recovery_shape(gap)
                      + anticipation(player)
```

The current sparse-gap normalization is compatible with this direction:
`sigma_scale_from_gap` returns 1 at sparse gaps, so player level can primarily
be represented by `core_sigma` instead of changing the absolute measured curve.

## Mapping SR To Core Sigma

Do not scale `core_sigma` independently to each map in production; that would
erase map difficulty. Instead, keep map/local difficulty in the judgement
units and make core sigma a player/session parameter.

For calibration and visualization, expose separate quantities:

1. Map expected accuracy at a fixed reference sigma (currently 11 ms).
2. Expected accuracy for a selected player sigma (for example 4 ms, 8.5 ms,
   or 11 ms).
3. Fitted player sigma from an observed score.

This permits a mapping from Sunny SR to reference player sigmas without
pretending that one fixed sigma is equally viable on all maps. The mapping
needs data: scores or replay measurements across SR bands and player levels.

## Follow-up Work (Separate From Stage 1)

- Validate whether measured input-state parameters change with player level.
- Collect or fit player-conditioned recovery and sigma curves.
- Decide whether the gap curve should be evaluated per operation instead of at
  a bin mean.
- Define a stable mapping between Sunny SR and reference core-sigma bands.
- Revisit score-side positive timing rewards only after input-state calibration
  is settled.

## Non-Goals For This Discussion

- Replacing Sunny/Rebirth SR immediately.
- Adding mod-specific input-state formulas.
- Forcing every map to produce a comfortable accuracy at 11 ms.
- Treating the current population-average measured constants as final for all
  player skill levels.
