#!/usr/bin/env python3
"""Does a column's input state actually change how accurately its next note is hit?

The accuracy surface currently models a note by its local difficulty and, if it is a
long note, its hold duration. A proposed alternative models the *input state* per
column -- lifted / pressed / holding / idle -- on the theory that the state a column is
left in changes the timing of whatever comes next. That is a claim about real players,
so it can be measured rather than assumed, and this measures it.

Method: re-run the validated pairing from `parse_replay.py` (imported, not copied, so
the judging rules stay in one place), keep the note index alongside each head error,
then compute each note's input state from the *beatmap alone* and group the measured
errors by it. Nothing here is fitted; the states are structural.

The confound this is built around: every state that sounds hard also occurs in denser
music. A note arriving while three other columns are held is in a chord-heavy section,
which is harder for reasons that have nothing to do with input state, and a naive
grouping would credit the state with all of it. So the report also stratifies by
same-column gap and compares within maps, and the raw table should be read as an upper
bound rather than as the effect.

Usage:
    tools/input_state.py --batch local-fixtures/batch.tsv
    tools/input_state.py --batch local-fixtures/multiuser.tsv local-fixtures/cohorts/*.tsv
    tools/input_state.py --batch local-fixtures/ladder.tsv --json out.json
"""
from __future__ import annotations

import argparse
import json
import math
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from parse_replay import (  # noqa: E402
    J_100,
    MODE_MANIA,
    Action,
    Beatmap,
    Note,
    actions_from_frames,
    mania_windows,
    match_column,
    mod_label,
    parse_osr,
    parse_osu,
    scale_time,
)

# How long after a release the column still counts as "just lifted" rather than idle.
# 150 ms is roughly the point where a release stops overlapping the next press motion
# at the densities these maps run at; the report sweeps it so the choice is visible
# rather than load-bearing.
LIFTED_MS = 150.0

# Same-column gap strata, in ms. The primary confound control: the input state a note
# arrives in is strongly determined by how soon it follows the last note in its column,
# and so is its difficulty. Comparing states only within a stratum holds that roughly
# fixed.
GAP_EDGES = (0.0, 60.0, 100.0, 160.0, 250.0, 400.0, 700.0, float("inf"))

# Bins used to measure and fit the recovery curve. They intentionally start at 100 ms
# and exclude notes whose predecessor is still inside the replay pairing window.
RECOVERY_EDGES = (
    100.0, 130.0, 160.0, 190.0, 230.0, 280.0, 340.0, 420.0, 520.0, 650.0, 850.0,
)


def gap_stratum(gap: float) -> int:
    for idx in range(len(GAP_EDGES) - 1):
        if GAP_EDGES[idx] <= gap < GAP_EDGES[idx + 1]:
            return idx
    return len(GAP_EDGES) - 2


def note_states(bm: Beatmap, lifted_ms: float = LIFTED_MS) -> list[dict]:
    """The input state each note arrives into, derived from the beatmap only.

    Per note, in its own column:

      - `prev_gap`      ms since the previous note in this column ended (its tail for a
                        hold, its head otherwise). `inf` for a column's first note.
      - `own_state`     what this column was doing when the note arrived:
                        `idle`, `lifted` (a hold released within `lifted_ms`),
                        `tapped` (a plain note within `lifted_ms`).
      - `holding`       how many *other* columns are mid-hold at this note's time. The
                        "按住" load: fingers already committed elsewhere.
      - `concurrent`    how many other columns have a note within 10 ms, i.e. the chord
                        width this note is part of.
      - `is_hold`       whether this note is itself a hold.
      - `releases_into` for a hold, ms from its own tail to the next note in the same
                        column; `inf` if none. The 反键 case the collision term charges.

    Deterministic and map-only, so it can be computed at difficulty time for every
    score on the map -- which is what would let it feed the surface, and what stops it
    from being fitted to how anyone played.
    """
    by_col: dict[int, list[int]] = {}
    for idx, note in enumerate(bm.notes):
        by_col.setdefault(note.column, []).append(idx)

    for indices in by_col.values():
        indices.sort(key=lambda i: bm.notes[i].time)

    # Hold intervals per column, for the "other columns are held" count.
    holds = [
        (note.time, note.time + note.duration, note.column)
        for note in bm.notes
        if note.is_hold and note.duration > 0
    ]
    holds.sort()

    # Note times per column for the chord-width count.
    times_by_col = {
        col: sorted(bm.notes[i].time for i in indices) for col, indices in by_col.items()
    }

    out: list[dict] = [{} for _ in bm.notes]

    for col, indices in by_col.items():
        for position, idx in enumerate(indices):
            note = bm.notes[idx]

            if position == 0:
                prev_gap = float("inf")
                own_state = "idle"
                prev_was_hold = False
            else:
                prev = bm.notes[indices[position - 1]]
                prev_was_hold = bool(prev.is_hold and prev.duration > 0)
                prev_end = prev.time + (prev.duration if prev.is_hold else 0)
                prev_gap = float(note.time - prev_end)

                if prev_gap > lifted_ms:
                    own_state = "idle"
                elif prev_was_hold:
                    own_state = "lifted"
                else:
                    own_state = "tapped"

            if note.is_hold and position + 1 < len(indices):
                nxt = bm.notes[indices[position + 1]]
                releases_into = float(nxt.time - (note.time + note.duration))
            else:
                releases_into = float("inf")

            # Other columns mid-hold at this note's head. Strictly inside the hold, so a
            # chord of simultaneous hold heads does not count itself as "already held".
            holding = sum(
                1
                for start, end, hold_col in holds
                if hold_col != col and start < note.time < end
            )

            concurrent = sum(
                1
                for other_col, times in times_by_col.items()
                if other_col != col
                and any(abs(t - note.time) <= 10 for t in times)
            )

            out[idx] = {
                "column": col,
                "time": note.time,
                "prev_gap": prev_gap,
                "own_state": own_state,
                "prev_was_hold": prev_was_hold,
                "holding": holding,
                "concurrent": concurrent,
                "is_hold": bool(note.is_hold and note.duration > 0),
                "releases_into": releases_into,
            }

    return out


def paired_errors(
    osr_path: Path, osu_path: Path
) -> tuple[dict, list[tuple[int, float]], Beatmap]:
    """`(meta, [(note_index, head_error_ms), ...], beatmap)` for one replay.

    The beatmap comes back *already mirrored and rate-scaled*, because the note indices
    in the pairs refer to it. Recomputing either transform in the caller would risk the
    two drifting apart, and a mirrored map with unmirrored states silently attributes
    every error to the wrong column.

    Deliberately reuses `parse_replay.match_column` rather than reimplementing the
    pairing: that function's rules were each measured against server-side judgement
    counts, and a second copy would drift from them silently. The only thing added here
    is keeping the note index, which `judge` discards.
    """
    rep = parse_osr(osr_path)
    bm = parse_osu(osu_path)

    if rep.mode != MODE_MANIA or bm.mode != MODE_MANIA:
        raise ValueError(f"{osr_path.name}: not a mania replay/map pair")

    if rep.is_mirror:
        for note in bm.notes:
            note.column = bm.keys - note.column - 1

    windows = mania_windows(bm.od, rep.mods)
    actions = actions_from_frames(rep.frames, bm.keys)

    rate = rep.clock_rate
    if rate != 1.0:
        for note in bm.notes:
            note.duration = scale_time(note.duration, rate)
            note.time = scale_time(note.time, rate)
        actions = [
            Action(
                time=scale_time(a.time, rate),
                column=a.column,
                duration=scale_time(a.duration, rate),
            )
            for a in actions
        ]

    by_col_notes: dict[int, list[tuple[int, Note]]] = {}
    for ni, note in enumerate(bm.notes):
        by_col_notes.setdefault(note.column, []).append((ni, note))

    by_col_press: dict[int, list[tuple[int, Action]]] = {}
    for ai, action in enumerate(actions):
        by_col_press.setdefault(action.column, []).append((ai, action))

    claimed: dict[int, int] = {}
    for col, notes in by_col_notes.items():
        claimed.update(
            match_column(notes, by_col_press.get(col, []), windows, rep.is_score_v2)
        )

    pairs = [
        (ni, float(actions[ai].time - bm.notes[ni].time)) for ni, ai in claimed.items()
    ]
    pairs.sort()

    meta = {
        "score_id": int(osr_path.stem),
        "map_id": int(osu_path.stem),
        "player": rep.player,
        "mods": mod_label(rep.mods),
        "keys": bm.keys,
        "od": bm.od,
        "clock_rate": rep.clock_rate,
        "notes": len(bm.notes),
        "matched": len(pairs),
        "hold_share": bm.holds / len(bm.notes) if bm.notes else 0.0,
        "window_100": windows[J_100],
    }

    return meta, pairs, bm


def sd(values: list[float]) -> float:
    n = len(values)
    if n < 2:
        return float("nan")
    mean = sum(values) / n
    return math.sqrt(sum((v - mean) ** 2 for v in values) / (n - 1))


def mean(values: list[float]) -> float:
    return sum(values) / len(values) if values else float("nan")


def median(values: list[float]) -> float:
    if not values:
        return float("nan")
    ordered = sorted(values)
    mid = len(ordered) // 2
    if len(ordered) % 2:
        return ordered[mid]
    return (ordered[mid - 1] + ordered[mid]) / 2


class Bucket:
    """Errors for one state group, kept per score so within-score ratios are possible."""

    def __init__(self) -> None:
        self.pooled: list[float] = []
        # score_id -> errors, so each score can be compared against its own baseline.
        self.by_score: dict[int, list[float]] = {}

    def add(self, score_id: int, error: float) -> None:
        self.pooled.append(error)
        self.by_score.setdefault(score_id, []).append(error)

    def ratio_stats(self, baseline: dict[int, list[float]], min_n: int = 30):
        """Per-score `sd(this group) / sd(that score's own errors)`.

        The confound control that matters. A group's pooled sigma mixes in which
        *players* and which *maps* happen to populate it -- a state common on hard 7K
        charts inherits those players' sigma wholesale. Dividing by the same score's
        overall sigma removes the score entirely, so what is left is whether the state
        is harder *for that player on that map*. A ratio near 1.0 across scores means
        the state carries no information the rest of the model does not already have.
        """
        ratios = []
        for score_id, errors in self.by_score.items():
            if len(errors) < min_n:
                continue
            whole = baseline.get(score_id)
            if not whole or len(whole) < min_n:
                continue
            here, there = sd(errors), sd(whole)
            if there and there == there and here == here and there > 0:
                ratios.append(here / there)
        return ratios

    def offset_stats(self, baseline: dict[int, list[float]], min_n: int = 30):
        """Per-score `mean(this group) - mean(that score's own errors)`, in ms.

        The same control applied to *bias* rather than width, and the reason it is
        reported separately: a width difference is absorbed exactly by the surface's
        fitted skill parameter and so cannot change what a score is worth, whereas a
        mean offset cannot be absorbed by anything the model currently fits. Subtracting
        the score's own mean removes each player's constant lateness, which is large and
        varies between players, leaving only how much *this state* shifts them.
        """
        offsets = []
        for score_id, errors in self.by_score.items():
            if len(errors) < min_n:
                continue
            whole = baseline.get(score_id)
            if not whole or len(whole) < min_n:
                continue
            offsets.append(mean(errors) - mean(whole))
        return offsets


def recovery_fit_points(
    recovery: dict[int, Bucket], baseline: dict[int, list[float]]
) -> list[tuple[float, float, int]]:
    """Return `(bin centre, median per-score offset, note count)` fit points."""
    points = []

    for idx in range(len(RECOVERY_EDGES) - 1):
        bucket = recovery.get(idx)
        if not bucket:
            continue

        offsets = bucket.offset_stats(baseline)
        if offsets:
            centre = (RECOVERY_EDGES[idx] + RECOVERY_EDGES[idx + 1]) / 2.0
            points.append((centre, median(offsets), len(bucket.pooled)))

    return points


def fit_recovery_curve(
    points: list[tuple[float, float, int]], refinements: int = 6
) -> tuple[float, float, float, float]:
    """Fit `amplitude * exp(-gap / tau) + plateau` by weighted least squares.

    This is the original fitter that produced 73.12/72.40/-3.19, moved from its
    temporary session file into the repository. Note count is the weight of each bin;
    the measured value is its median within-score offset. The bounded grid and fixed
    refinement schedule make the result deterministic without a scipy dependency.
    """
    if len(points) < 3:
        raise ValueError("recovery fit requires at least three populated bins")
    if any(weight <= 0 for _, _, weight in points):
        raise ValueError("recovery fit weights must be positive")

    def wsse(amplitude: float, tau: float, plateau: float) -> float:
        return sum(
            weight
            * (amplitude * math.exp(-gap / tau) + plateau - measured) ** 2
            for gap, measured, weight in points
        )

    amp_lo, amp_hi = 5.0, 400.0
    tau_lo, tau_hi = 20.0, 300.0
    base_lo, base_hi = -8.0, 2.0
    best = (float("inf"), 0.0, 0.0, 0.0)

    for _ in range(refinements):
        current = (float("inf"), 0.0, 0.0, 0.0)
        for i in range(40):
            amplitude = amp_lo + (amp_hi - amp_lo) * i / 39
            for j in range(40):
                tau = tau_lo + (tau_hi - tau_lo) * j / 39
                for k in range(40):
                    plateau = base_lo + (base_hi - base_lo) * k / 39
                    error = wsse(amplitude, tau, plateau)
                    if error < current[0]:
                        current = (error, amplitude, tau, plateau)

        best = current
        _, amplitude, tau, plateau = current
        amp_span = (amp_hi - amp_lo) / 8
        tau_span = (tau_hi - tau_lo) / 8
        base_span = (base_hi - base_lo) / 8
        amp_lo, amp_hi = amplitude - amp_span, amplitude + amp_span
        tau_lo, tau_hi = max(1.0, tau - tau_span), tau + tau_span
        base_lo, base_hi = plateau - base_span, plateau + base_span

    error, amplitude, tau, plateau = best
    total_weight = sum(weight for _, _, weight in points)
    return amplitude, tau, plateau, math.sqrt(error / total_weight)


def report_group(label: str, bucket: Bucket, baseline: dict[int, list[float]]) -> None:
    pooled = bucket.pooled

    if not pooled:
        return

    ratios = bucket.ratio_stats(baseline)
    offsets = bucket.offset_stats(baseline)

    # A sign test on the per-score offsets: how many of the scores that populate this
    # group shift in the same direction as the median. With one offset per score these
    # are independent observations in a way the pooled notes are not, so this is the
    # figure to believe about whether the shift is a property of the state rather than
    # of whichever scores happen to be in it.
    if offsets:
        agree = sum(1 for value in offsets if value > 0)
        agree = max(agree, len(offsets) - agree)
        sign_text = f"{agree}/{len(offsets)}"
    else:
        sign_text = "-"

    ratio_text = f"{median(ratios):>6.3f}" if ratios else f"{'-':>6}"
    offset_text = f"{median(offsets):>+7.2f}" if offsets else f"{'-':>7}"

    print(
        f"  {label:<22} {len(pooled):>8} {sd(pooled):>7.2f} {ratio_text} "
        f"{mean(pooled):>+8.2f} {offset_text} {sign_text:>7}"
    )


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    ap.add_argument(
        "--batch",
        type=Path,
        nargs="+",
        required=True,
        help="one or more TSVs with id/mapid columns; duplicate score IDs are ignored",
    )
    ap.add_argument("--replays", type=Path, default=Path("local-fixtures/replays"))
    ap.add_argument("--maps", type=Path, default=Path("local-fixtures/maps"))
    ap.add_argument("--json", type=Path, help="write per-note records here")
    ap.add_argument(
        "--lifted-ms",
        type=float,
        default=LIFTED_MS,
        help=f"release-to-next-press cutoff for 'lifted' (default {LIFTED_MS})",
    )
    args = ap.parse_args()

    rows = []
    seen_score_ids = set()

    for batch in args.batch:
        header: list[str] = []
        for line_no, line in enumerate(batch.read_text().splitlines()):
            fields = line.split("\t")
            if not header:
                if "id" in fields and ("mapid" in fields or "mapId" in fields):
                    header = fields
                    continue

                # `local-fixtures/multiuser.tsv` is the compact, headerless report input:
                # uid, score id, map id, ... . Only these identifiers are needed here.
                if len(fields) >= 3 and all(field.isdigit() for field in fields[:3]):
                    header = ["userid", "id", "mapid"]
                else:
                    print(
                        f"unrecognized batch layout in {batch} on line {line_no + 1}",
                        file=sys.stderr,
                    )
                    return 1
            if len(fields) < len(header):
                continue

            row = dict(zip(header, fields[: len(header)]))
            score_id = row.get("id") or row.get("scoreId")
            if score_id and score_id not in seen_score_ids:
                rows.append(row)
                seen_score_ids.add(score_id)

    if not rows:
        print(f"no rows in {args.batch}", file=sys.stderr)
        return 1

    # State buckets. Each is a hypothesis about what changes a note's timing.
    own_state = {name: Bucket() for name in ("idle", "lifted", "tapped")}
    holding_load = {n: Bucket() for n in range(4)}
    hold_kind = {"rice": Bucket(), "hold": Bucket()}
    inverse = {"collides": Bucket(), "clear": Bucket()}
    by_gap = {idx: Bucket() for idx in range(len(GAP_EDGES) - 1)}
    # own_state within a gap stratum: the same comparison with density held roughly fixed.
    state_in_gap: dict[tuple[int, str], Bucket] = {}
    # The pairing-artefact control, and the reason it is needed:
    #
    # `match_column` gives a press the frontmost note whose 100 window is still open. In a
    # pattern tighter than that window, a press meant for note N+1 can still be inside note
    # N's window, so a consistently late player has their error booked against the earlier
    # note. That manufactures a late bias out of nothing but the pairing rule -- precisely
    # the signal being measured here.
    #
    # Where the same-column gap *exceeds* the 100 window the ambiguity cannot arise: a
    # press late enough to belong to the next note is already outside this one's window and
    # is dropped rather than mispaired. So a bias that survives this split is a property of
    # the player, and one that does not is a property of the parser.
    safe_state = {name: Bucket() for name in ("idle", "lifted", "tapped")}
    unsafe_state = {name: Bucket() for name in ("idle", "lifted", "tapped")}
    # The unambiguous `tapped` offset, split by things that could be producing it for
    # reasons other than the player's hand: a clock rate (which rescales every window and
    # so moves where the ambiguity boundary sits), keymode, and how far past the boundary
    # the note actually is. A real motor effect should survive all three; an artefact of
    # the boundary should decay as the gap moves away from it.
    safe_by_rate: dict[str, Bucket] = {}
    safe_by_keys: dict[int, Bucket] = {}
    safe_by_margin: dict[str, Bucket] = {}
    MARGIN_EDGES = ((0.0, 20.0), (20.0, 50.0), (50.0, 100.0), (100.0, float("inf")))
    # The recovery curve: offset as a function of same-column gap, over a range wide
    # enough to see it return to zero. This is the shape a model would actually need, and
    # it is also what separates a motor limit from a boundary artefact -- an artefact is
    # pinned to the window and cannot produce a smooth decay that continues well past it,
    # whereas a finger that needs time to lift and re-press must.
    #
    # Restricted to unambiguous notes throughout, and the boundary moves with the clock
    # rate, so the first bins are thin under DT and are reported with their counts.
    recovery: dict[int, Bucket] = {}

    baseline: dict[int, list[float]] = {}
    records = []
    scored = 0
    failed = 0

    for row in rows:
        score_id = row.get("id") or row.get("scoreId")
        map_id = row.get("mapid") or row.get("mapId")

        if not score_id or not map_id:
            continue

        osr = args.replays / f"{score_id}.osr"
        osu = args.maps / f"{map_id}.osu"

        if not osr.exists() or not osu.exists():
            continue

        try:
            meta, pairs, bm = paired_errors(osr, osu)
        except Exception as exc:  # noqa: BLE001 - a bad replay must not stop the batch
            print(f"  skip {score_id}: {exc}", file=sys.stderr)
            failed += 1
            continue

        # Computed on the same map object the pairs index into, so mirroring and rate
        # scaling cannot disagree between the two.
        states = note_states(bm, args.lifted_ms)

        sid = meta["score_id"]
        scored += 1

        for note_index, error in pairs:
            state = states[note_index]
            baseline.setdefault(sid, []).append(error)

            own_state[state["own_state"]].add(sid, error)
            holding_load[min(state["holding"], 3)].add(sid, error)
            hold_kind["hold" if state["is_hold"] else "rice"].add(sid, error)

            stratum = gap_stratum(state["prev_gap"]) if state["prev_gap"] < float("inf") else len(GAP_EDGES) - 2
            by_gap[stratum].add(sid, error)

            key = (stratum, state["own_state"])
            state_in_gap.setdefault(key, Bucket()).add(sid, error)

            # Unambiguous when the previous note in this column is further away than the
            # window a press could reach back across.
            if state["prev_gap"] > meta["window_100"]:
                safe_state[state["own_state"]].add(sid, error)

                # The recovery curve, over presses only: a hold's release is a different
                # motion and would mix two mechanisms into one curve.
                if not state.get("prev_was_hold") and state["prev_gap"] < float("inf"):
                    for idx in range(len(RECOVERY_EDGES) - 1):
                        if RECOVERY_EDGES[idx] <= state["prev_gap"] < RECOVERY_EDGES[idx + 1]:
                            recovery.setdefault(idx, Bucket()).add(sid, error)
                            break

                if state["own_state"] == "tapped":
                    rate_label = f"{meta['clock_rate']:.2f}x"
                    safe_by_rate.setdefault(rate_label, Bucket()).add(sid, error)
                    safe_by_keys.setdefault(meta["keys"], Bucket()).add(sid, error)

                    margin = state["prev_gap"] - meta["window_100"]
                    for lo, hi in MARGIN_EDGES:
                        if lo <= margin < hi:
                            label = (
                                f"+{lo:.0f}-{hi:.0f} ms"
                                if hi < float("inf")
                                else f">+{lo:.0f} ms"
                            )
                            safe_by_margin.setdefault(label, Bucket()).add(sid, error)
                            break
            else:
                unsafe_state[state["own_state"]].add(sid, error)

            if state["is_hold"]:
                collides = state["releases_into"] <= meta["window_100"]
                inverse["collides" if collides else "clear"].add(sid, error)

            if args.json:
                records.append(
                    {
                        "score_id": sid,
                        "map_id": meta["map_id"],
                        "note_index": note_index,
                        "error": error,
                        **{k: v for k, v in state.items() if k != "time"},
                    }
                )

    if scored == 0:
        print("no replay/map pairs found on disk", file=sys.stderr)
        return 1

    total = sum(len(v) for v in baseline.values())
    print(
        f"\n{scored} scores, {total} paired notes"
        f"{f', {failed} skipped' if failed else ''}. lifted cutoff {args.lifted_ms:.0f} ms."
    )
    print(
        "\nBoth controlled columns are computed per score and then pooled, so the player "
        "and the map divide out;\nthe raw sd and mean columns do not control for that and "
        "are shown only for scale."
    )
    print(
        "  ratio  = median over scores of sd(group) / sd(score). 1.000 = no width effect."
    )
    print(
        "  offset = median over scores of mean(group) - mean(score), ms. 0 = no bias "
        "effect."
    )
    print(
        "  sign   = how many of those scores agree on the offset's direction "
        "(one vote per score).\n"
    )
    header_line = (
        f"  {'group':<22} {'notes':>8} {'raw sd':>7} {'ratio':>6} {'raw mean':>8} "
        f"{'offset':>7} {'sign':>7}"
    )
    print(header_line)

    print("\nOWN COLUMN'S PRIOR STATE")
    for name in ("idle", "tapped", "lifted"):
        report_group(name, own_state[name], baseline)

    print("\nOTHER COLUMNS HELD (按住 load)")
    for load in range(4):
        label = f"{load} held" if load < 3 else "3+ held"
        report_group(label, holding_load[load], baseline)

    print("\nNOTE KIND")
    for name in ("rice", "hold"):
        report_group(name, hold_kind[name], baseline)

    print("\nHOLD RELEASE vs NEXT PRESS (反键)")
    for name in ("clear", "collides"):
        report_group(name, inverse[name], baseline)

    print("\nSAME-COLUMN GAP (the confound: state is largely determined by this)")
    for idx in range(len(GAP_EDGES) - 1):
        lo, hi = GAP_EDGES[idx], GAP_EDGES[idx + 1]
        label = f"{lo:.0f}-{hi:.0f} ms" if hi < float("inf") else f">{lo:.0f} ms"
        report_group(label, by_gap[idx], baseline)

    print("\nOWN STATE WITHIN A GAP STRATUM (state's effect with density held fixed)")
    for idx in range(len(GAP_EDGES) - 1):
        present = [
            (name, state_in_gap[(idx, name)])
            for name in ("idle", "tapped", "lifted")
            if (idx, name) in state_in_gap and len(state_in_gap[(idx, name)].pooled) >= 200
        ]
        if len(present) < 2:
            continue
        lo, hi = GAP_EDGES[idx], GAP_EDGES[idx + 1]
        span = f"{lo:.0f}-{hi:.0f}" if hi < float("inf") else f">{lo:.0f}"
        print(f"  gap {span} ms:")
        for name, bucket in present:
            report_group(f"    {name}", bucket, baseline)

    print(
        "\nPAIRING-ARTEFACT CONTROL: same split, restricted by whether a press could have\n"
        "been mispaired at all. Where the previous same-column note is *further* than the\n"
        "100 window, the queue cannot book one note's press against another, so an offset\n"
        "surviving there is the player's. An offset only present in the ambiguous half is\n"
        "the parser's."
    )

    print("\n  unambiguous (prev same-column note beyond the 100 window)")
    for name in ("idle", "tapped", "lifted"):
        report_group(name, safe_state[name], baseline)

    print("\n  ambiguous (prev same-column note within the 100 window)")
    for name in ("idle", "tapped", "lifted"):
        report_group(name, unsafe_state[name], baseline)

    print(
        "\nIS THE UNAMBIGUOUS 'tapped' OFFSET REAL? Same notes, split by things that would\n"
        "produce it for reasons other than the hand. The margin split is the sharp test: an\n"
        "artefact of the window boundary must fade as notes sit further past it, a motor\n"
        "limit must not."
    )

    print("\n  by clock rate")
    for label in sorted(safe_by_rate):
        report_group(label, safe_by_rate[label], baseline)

    print("\n  by keymode")
    for keys in sorted(safe_by_keys):
        report_group(f"{keys}K", safe_by_keys[keys], baseline)

    print("\n  by margin past the 100 window")
    for lo, hi in MARGIN_EDGES:
        label = f"+{lo:.0f}-{hi:.0f} ms" if hi < float("inf") else f">+{lo:.0f} ms"
        if label in safe_by_margin:
            report_group(label, safe_by_margin[label], baseline)

    print(
        "\nRECOVERY CURVE: offset against same-column gap, presses only, unambiguous notes\n"
        "only. The shape a model would need. A window artefact is pinned near the boundary;\n"
        "a motor limit decays smoothly to zero over a timescale the hand sets."
    )

    for idx in range(len(RECOVERY_EDGES) - 1):
        if idx not in recovery:
            continue
        lo, hi = RECOVERY_EDGES[idx], RECOVERY_EDGES[idx + 1]
        report_group(f"{lo:.0f}-{hi:.0f} ms", recovery[idx], baseline)

    points = recovery_fit_points(recovery, baseline)
    try:
        amplitude, tau, plateau, rmse = fit_recovery_curve(points)
    except ValueError as exc:
        print(f"\nRECOVERY FIT unavailable: {exc}")
    else:
        print(
            "\nRECOVERY FIT (note-count-weighted least squares over median "
            "within-score offsets)"
        )
        print(
            f"  offset(gap) = {amplitude:.3f} * exp(-gap / {tau:.2f}) "
            f"{plateau:+.3f} ms"
        )
        print(f"  weighted RMSE = {rmse:.4f} ms")
        print(f"  inputs = {scored} scores, {total} paired notes, {len(points)} bins")
        print(f"  batches = {', '.join(str(batch) for batch in args.batch)}")

    if args.json:
        args.json.write_text(json.dumps(records))
        print(f"\nwrote {len(records)} per-note records to {args.json}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
