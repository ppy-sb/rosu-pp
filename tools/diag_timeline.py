#!/usr/bin/env python3
"""Diagnose the mapping between replay frame times and beatmap note times.

Two failure modes this separates, both of which look like "bad UR" downstream:

  - **wrong scale** (clock-rate handling): the residual drifts linearly across the
    map, so a regression of error on note time has a non-zero slope. The implied
    rate is `rate_used * (1 + slope)`.
  - **wrong offset** (a constant lead/lag): the residual has a non-zero intercept
    but no drift.

Pairing here is deliberately nearest-note with no window cut, unlike the judging in
parse_replay.py: a miss window would hide exactly the mispaired notes that reveal
the problem.

The scan then reports the (offset, rate) that minimises the spread of hit errors,
which is a direct read on whether the assumed rate is right.
"""
from __future__ import annotations

import math
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from parse_replay import (  # noqa: E402
    actions_from_frames, mod_label, parse_osr, parse_osu,
)


def nearest_errors(notes, actions, rate: float, offset: float) -> list[float]:
    """(press - note) in real ms for the nearest press in the same column, no cut."""
    by_note: dict[int, list[float]] = {}
    for n in notes:
        by_note.setdefault(n.column, []).append(n.time / rate)
    by_press: dict[int, list[float]] = {}
    for a in actions:
        by_press.setdefault(a.column, []).append(a.time / rate + offset)

    out: list[float] = []
    for col, ns in by_note.items():
        ps = by_press.get(col)
        if not ps:
            continue
        j = 0
        for n in ns:
            while j + 1 < len(ps) and abs(ps[j + 1] - n) <= abs(ps[j] - n):
                j += 1
            out.append(ps[j] - n)
    return out


def regress(xs: list[float], ys: list[float]) -> tuple[float, float]:
    n = len(xs)
    mx, my = sum(xs) / n, sum(ys) / n
    sxx = sum((x - mx) ** 2 for x in xs)
    sxy = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
    slope = sxy / sxx if sxx else 0.0
    return slope, my - slope * mx


def robust_spread(errors: list[float]) -> float:
    """Interquartile range: insensitive to the mispaired tail, unlike stdev."""
    s = sorted(errors)
    n = len(s)
    if n < 4:
        return float("inf")
    return s[int(0.75 * (n - 1))] - s[int(0.25 * (n - 1))]


def main() -> int:
    batch = Path(sys.argv[1] if len(sys.argv) > 1 else "local-fixtures/batch.tsv")
    root = batch.parent
    rows = batch.read_text().splitlines()
    header = rows[0].split("\t")

    print(f"{'score':>8} {'mods':>10} {'rate':>5} {'slope':>9} {'implied':>8} "
          f"{'median':>7} {'IQR':>6} {'best_off':>8} {'best_rate':>9} {'IQR@best':>8}")
    print("-" * 95)
    for line in rows[1:]:
        f = dict(zip(header, line.split("\t")))
        osr = root / "replays" / f"{f['id']}.osr"
        osu = root / "maps" / f"{f['mapid']}.osu"
        if not osr.exists() or not osu.exists():
            continue
        rep = parse_osr(osr)
        bm = parse_osu(osu)
        if rep.is_mirror:
            for n in bm.notes:
                n.column = bm.keys - n.column - 1
        actions = actions_from_frames(rep.frames, bm.keys)
        if not actions:
            print(f"{f['id']:>8} no presses")
            continue

        rate = rep.clock_rate
        errs = nearest_errors(bm.notes, actions, rate, 0.0)
        # Regress against note time to detect drift.
        by_note: dict[int, list[float]] = {}
        for n in bm.notes:
            by_note.setdefault(n.column, []).append(n.time / rate)
        xs: list[float] = []
        for col, ns in by_note.items():
            xs.extend(ns[:0])  # placeholder; rebuilt below
        # Rebuild aligned (x, y) pairs so the regression uses matching note times.
        xs, ys = [], []
        for col, ns in by_note.items():
            sub = nearest_errors([n for n in bm.notes if n.column == col],
                                 [a for a in actions if a.column == col], rate, 0.0)
            xs.extend(ns[:len(sub)])
            ys.extend(sub)
        slope, _ = regress(xs, ys) if xs else (0.0, 0.0)
        implied = rate * (1.0 + slope)
        med = sorted(errs)[len(errs) // 2]
        iqr = robust_spread(errs)

        # Scan offsets and candidate rates for the tightest error distribution.
        best = (float("inf"), 0.0, rate)
        for cand_rate in (rate, 1.0, 1.5, 0.75, rate * (1 + slope)):
            for off in range(-60, 61, 2):
                e = nearest_errors(bm.notes, actions, cand_rate, float(off))
                s = robust_spread(e)
                if s < best[0]:
                    best = (s, float(off), cand_rate)

        print(f"{f['id']:>8} {mod_label(rep.mods):>10} {rate:>5.2f} {slope:>9.5f} "
              f"{implied:>8.4f} {med:>7.2f} {iqr:>6.2f} {best[1]:>8.0f} "
              f"{best[2]:>9.4f} {best[0]:>8.2f}")

    print("\nslope ~0 means the rate is right; best_off far from 0 means a constant lag.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
