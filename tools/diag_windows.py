#!/usr/bin/env python3
"""Invert the server's judgement counts into implied hit windows.

This is the decisive test for *which timeline hit windows are compared in*, which the
drift regression in diag_timeline.py cannot answer: if replay and beatmap share a
timeline, scaling both by any constant leaves the drift at zero, so the divisor is
still free.

Method. Pair every note to its nearest same-column press with no window cut, giving a
raw |error| per note. The server's counts say what fraction of notes fell inside each
window, so the |error| quantile at that fraction *is* the window stable used. Compare
against the window this tool computes:

  ratio ~ 1.0   -> stable compares errors in the same units the notes are stored in
  ratio ~ rate  -> a clock-rate division is missing or spurious
  ratio varies across bands -> the errors themselves are wrong (mispairing), not the
                               window convention

No-mod scores act as the control: their ratio must come out 1.0, since their counts
already reproduce exactly.
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from parse_replay import (  # noqa: E402
    BAND_NAMES, actions_from_frames, mania_windows, mod_label, parse_osr, parse_osu,
)

INF = float("inf")


def nearest_abs_errors(notes, actions) -> list[float]:
    """|press - note| in stored units for the nearest same-column press, no cut.

    Notes with no press in their column at all get infinity, so they count as beyond
    every window rather than being dropped from the denominator.
    """
    by_press: dict[int, list[int]] = {}
    for a in actions:
        by_press.setdefault(a.column, []).append(a.time)
    for v in by_press.values():
        v.sort()

    out: list[float] = []
    for n in notes:
        ps = by_press.get(n.column)
        if not ps:
            out.append(INF)
            continue
        # Binary search for the closest press.
        lo, hi = 0, len(ps) - 1
        while lo < hi:
            mid = (lo + hi) // 2
            if ps[mid] < n.time:
                lo = mid + 1
            else:
                hi = mid
        best = abs(ps[lo] - n.time)
        if lo > 0:
            best = min(best, abs(ps[lo - 1] - n.time))
        out.append(float(best))
    return out


def main() -> int:
    batch = Path(sys.argv[1] if len(sys.argv) > 1 else "local-fixtures/batch.tsv")
    root = batch.parent
    rows = batch.read_text().splitlines()
    header = rows[0].split("\t")

    print("implied window = |error| quantile at the server's cumulative share")
    print("ratio = implied / computed; 'rate' column is the clock rate for reference\n")
    print(f"{'score':>8} {'mods':>10} {'rate':>5}  {'band':>6} {'share':>7} "
          f"{'computed':>9} {'implied':>8} {'ratio':>6}")
    print("-" * 74)

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
        windows = mania_windows(bm.od, rep.mods)
        actions = actions_from_frames(rep.frames, bm.keys)
        errs = sorted(nearest_abs_errors(bm.notes, actions))
        n = len(errs)

        server = [rep.ngeki, rep.n300, rep.nkatu, rep.n100, rep.n50, rep.nmiss]
        total = sum(server)
        if total == 0 or n == 0:
            continue

        cum = 0
        ratios = []
        first = True
        for bi in range(5):  # the miss band has no upper window
            cum += server[bi]
            share = cum / total
            idx = min(n - 1, max(0, int(round(share * n)) - 1))
            implied = errs[idx]
            computed = windows[bi]
            ratio = implied / computed if computed else float("nan")
            if implied != INF:
                ratios.append(ratio)
            label = f"{rep.__class__.__name__}"  # unused; keep row compact
            print(f"{f['id'] if first else '':>8} "
                  f"{mod_label(rep.mods) if first else '':>10} "
                  f"{rep.clock_rate if first else '':>5} "
                  f" {BAND_NAMES[bi]:>6} {share:>7.3f} {computed:>9.1f} "
                  f"{implied:>8.1f} {ratio:>6.2f}")
            first = False
        if ratios:
            spread = max(ratios) / min(ratios)
            print(f"{'':>8} {'':>10} {'':>5}  {'mean ratio':>14} "
                  f"{sum(ratios) / len(ratios):>18.2f}   spread {spread:.2f}")
        print()

    print("constant ratio ~1.0 = right convention; constant ~rate = a rate division "
          "is off;\nvarying ratio = the errors are mispaired, not the windows wrong.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
