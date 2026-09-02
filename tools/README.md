# tools

Visualisers and measurement harnesses for the osu!mania accuracy surface. None are part
of the build. The visualisers read CSVs produced by `#[ignore]`d tests in `src/sunny.rs`
and write into `target/surface/`, which is gitignored.

## Fixture fetchers

All write into `local-fixtures/` (gitignored — not redistributable, not needed to
build) and need the bancho.py MySQL container reachable.

- **`fetch_cohorts.sh` — use this one.** Deep per-player cohorts with no accuracy filter.
  Supersedes `fetch_ladder.sh`; see the section below for why the other two mislead.
- `fetch_batch.sh` — scores ranked by pp across players, with an EZ cohort and a no-mod
  control. The set the mod response was measured on.
- `fetch_ladder.sh` — *difficulty ladders*: many scores from one player inside one
  quarter, stratified across star rating. `fetch_batch.sh` cannot fit sigma's difficulty
  response because it returns roughly one score per player, and the error model has one
  free skill each; a ladder holds skill roughly fixed while difficulty sweeps.
- `build_full_osr.py` / `build_multiuser_tsv.py` — rebuild replays/`multiuser.tsv` from
  bancho.py's on-disk partial replay blob + a row read via `docker exec` MySQL. Only
  correct when the local docker-mysql mirror actually has the rows you need.
- `build_osr_from_csv.py` / `build_multiuser_from_csv.py` / `compute_stars.mjs` — the
  MySQL-free versions of the two above. Use these when the local mirror is a stale
  backup (it will silently miss recent users/scores/maps rather than error). They take
  a raw CSV export (scoreid, bid, score, pp, acc, max_combo, n300, n100, n50, nmiss,
  ngeki, nkatu, grade, play_time, userid, username, bid, key_count, od, stars, mods) —
  map_md5 is computed locally from the `.osu` file instead of trusted from `maps.md5`,
  and star rating is computed with the vendored sunny wasm build
  (`refactor/osu-server-ts/node_modules/rosu-pp-js-sunny`) instead of read from
  `maps.diff`, which is bancho's own (different, often stale) calculation.

### `fetch_cohorts.sh`, and what the older two get wrong

```sh
tools/fetch_cohorts.sh survey 200                    # eligible cohorts, fetches nothing
tools/fetch_cohorts.sh fetch 2057:2020:3 1326:2020:2 # scores + beatmaps
tools/fetch_cohorts.sh fetch --replays 2768:2021:1   # also .osr, for a per-note fit
```

Three defects it fixes, each of which cost a round of bad fitting:

- **`status = 2` hides the entire low-accuracy population.** It is best-per-map, and it
  contains *zero* mania scores below 80% acc — HP drain censors them, so they survive only
  as non-best submissions. Including `status = 1` restores 5178 scores at 80-88%, 2490 at
  60-80% and 281 below 60%. The share of NF rises 0.2% → 4.0% → 17.1% → 72.6% across those
  bands, which is the drain mechanism showing up directly in the data.
- **`status = 0` looks like the low-acc tail but is not.** Failed plays average **31.6%**
  of the map's hit count; only 3632 of 121k reach 98%. A part-map play is a different map
  at a different difficulty. Filter on `hits / max_hits_for_that_map > 0.98`, never on
  `status` alone.
- **The `acc between 88 and 99.5` bound in `fetch_ladder.sh` was the binding constraint**,
  not sample size. Across 152 cohorts of 150+ scores, 12022 of 38439 rows sit below 94%.
  `fetch_cohorts.sh` applies no accuracy filter.

Also: `fetch_ladder.sh` always wrote `local-fixtures/ladder.tsv`, so a second cohort
destroyed the first. `fetch_cohorts.sh` writes `local-fixtures/cohorts/<uid>-<year>Q<q>.tsv`
and is idempotent. Replays are opt-in (`--replays`) because the counts-side fit needs only
beatmaps, and replays are one HTTP request per score.

Scale, for calibration of expectations: 1.18M mania rows total; ~171k ranked full-map
scores over 1940 players; 528 EZ scores over 118 players.

```sh
tools/fetch_ladder.sh 30                            # 4 default cohorts x 30 scores
tools/fetch_ladder.sh 30 4616:2023:3                # userid:year:quarter
tools/parse_replay.py --batch local-fixtures/ladder.tsv --json local-fixtures/ladder-errors.json
```

`fetch_ladder.sh` always writes `local-fixtures/ladder.tsv`, so **fetching a second
cohort overwrites the first** — rename the file after each run if you want more than one
skill band. The TSV is reconstructable from a `--json` dump if it is lost, since that
carries the count vector and the DB columns.

Note that a ladder deliberately excludes saturating scores (`acc between 88 and 99.5`),
which makes it the wrong set for anything about the clean end of the surface: see
`ErrorModel::sigma_floor`, where a replay-measured floor turned out to be contradicted by
the judgement counts of near-perfect scores the ladder never sampled.

`parse_replay.py` turns `.osr` replays into per-note hit errors, which measures a
player's timing sigma directly instead of inferring it from judgement counts. Its
`--verify` mode recomputes judgements and diffs them against the server's stored counts;
that is the correctness check on the whole pipeline. See the module docstring, which
records which pairing rules were tested and rejected.

Fit against sunny's own star ratings, not bancho's stored `maps.diff` — the two are
different calculations (`log`-`log` slope 0.78), and an exponent is only meaningful in
the units its difficulty is expressed in:

```sh
cut -f4 local-fixtures/ladder.tsv | tail -n +2 | sort -u \
  | cargo test --release ladder_stars -- --ignored --nocapture --exact sunny::tests::ladder_stars
```

To see what the surface makes of a ladder — fitted skill, window scalar, fit quality,
grouped per player and summarised by star band:

```sh
cat local-fixtures/ladder-*.tsv \
  | cargo test --release ladder_report -- --ignored --nocapture --exact sunny::tests::ladder_report
```

## Where each number comes from

Every fixture mixes columns of different provenance, and getting this wrong has already
cost one round of bad reasoning. Treat this table as the answer; do not re-derive it from
the fetch scripts.

| Column | Source | Algorithm |
| --- | --- | --- |
| `pp` in `ladder*.tsv` | ppy.sb | **live sunny pp**, as deployed, before our changes |
| `pp` in the tRPC fixtures (`multiuser.tsv`, `bp1.json`, `bp2.json`, `batch.tsv`) | ppy.sb tRPC endpoint | **live sunny pp**, same as above |
| star column in `ladder*.tsv` | bancho.py `maps.diff` | **bancho**, a different calculation (`log`-`log` slope 0.78) |
| `.osu` files under `local-fixtures/maps/` | `osu.ppy.sh/osu/$mapid` | n/a |

So a single ladder row carries sunny pp next to bancho stars. Both pp sources are the
algorithm family this crate computes, which makes a pp ratio against them a measure of
**our own changes** rather than the gap to a foreign algorithm. That is the calibration
anchor for anything needing an absolute pp scale — pricing from fitted skill instead of a
window ratio, most of all.

The star column is the one to distrust, per the warning above: fit exponents against
sunny's own `d`, never against `maps.diff`. A ratio of our stars to that column running
~1.2 is the two calculations disagreeing, not a defect in ours.

To see what a candidate `sigma_floor` would do — to fit quality and to pricing, which
turn out to be different questions:

```sh
cargo test --release sigma_floor_sweep -- --ignored --nocapture --exact sunny::tests::sigma_floor_sweep
```

Read the two halves against each other. `mean_g` is bit-identical across the whole
0-10 ms sweep, because the counts pin sigma and the fit absorbs any small floor into
skill; the window scalar moves anyway, because it is a ratio of skills at two different
sigmas and quadrature is nonlinear. The lower table is the binding constraint: a
1506-note all-320 score allows about 2 ms and rules out 5.

## Setup

```sh
uv venv .venv && uv pip install --python .venv/bin/python plotly matplotlib numpy
```

Then use `.venv/bin/python` in place of `python3` below.

## `mania_surface.py` — interactive, 3D

Browsable HTML: judgement-band wireframes over an (OD, skill) grid, at one map
difficulty. A dropdown switches between judgement bands, 305-weighted accuracy, and
the EZ gain, for each of classic/lazer scoring and each mod state.

```sh
tools/mania_surface.py                                          # default 13.77* slice
tools/mania_surface.py --map path/to.osu --clock-rate 1.5 --open
tools/mania_surface.py --stars 8.5                              # no beatmap needed
tools/mania_surface.py --no-dump                                # reuse the last CSV
```

Only one mod state draws at a time, and everything is wireframe rather than solid.
Both are deliberate: the bands overlap heavily, EZ's surfaces sit directly above
NM's, and an opaque sheet hides whatever is beneath it.

The OD axis is the reason this view exists. Under classic scoring the 320 ridge is
*flat* in OD — PERFECT is pinned at 16 ms while everything below it shifts by
`3 * (10 - od)` — so OD moves the 300/200 boundary and never the 320 rate. Switch to
a `lazer` entry and that ridge tilts, because lazer interpolates PERFECT over OD too.
EZ scales every window including PERFECT, which is why it is the only thing that
moves classic's 320 count, and why it prices as strongly as it does.

Converts are not swept: their classic windows key off a single `round(od) > 4`
threshold, so an OD axis would be two flat plateaus rather than a surface.

## `mania_surface_2d.py` — static overview, PNG

Four panels over the whole (difficulty, skill) plane at fixed windows. Usually the
better starting point, since the 3D view is one difficulty slice.

```sh
tools/mania_surface_2d.py --fit-skill 10.305 --target-accuracy 0.91672
```

1. Accuracy shortfall `1 - acc`, log-scaled — accuracy itself is flat above ~95% over
   most of the plane and hides the structure.
2. Judgement composition against skill at one difficulty, with implied sigma.
3. The same difficulty under several window sets; `window_scalar` is the horizontal
   gap between the curves.
4. Miss rate, which is just the mass beyond the last window rather than its own term.

## Reading these honestly

The straight, parallel contours in panels 1 and 4 are an *assumption*, not a
measurement. They follow from `sigma(d, skill) = sigma_ref * ((d + floor)/skill)^exp`
with `skill_exponent` fixed at 1.7 and `difficulty_floor` at 0.6 — neither of which
has been fitted, because the calibration set is one player across a narrow
0.96–1.72x star ratio. Straightness claims a 20-star map at skill 20 grades exactly
like a 2-star map at skill 2; that is the thing new data needs to test.

Local difficulty is also still uniform: every note carries the map's whole star
rating, which is why the mod response barely varies between maps.

`input_state.py` measures the same-column recovery curve and now performs the complete
deterministic fit used by `ErrorModel`: median within-score offsets in fixed gap bins are
fit to `amplitude * exp(-gap / tau) + plateau` by note-count-weighted least squares.
To refit against every locally indexed replay, deduplicating score IDs across inputs:

```sh
tools/input_state.py --batch local-fixtures/multiuser.tsv local-fixtures/cohorts/*.tsv
```

The report prints the fitted parameters, weighted RMSE, score count, paired-note count,
populated-bin count, and input TSV paths. `python3 tools/test_input_state.py` reproduces
the historical 285-replay fit from its recorded bin inputs.
