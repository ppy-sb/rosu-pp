#!/usr/bin/env python3
"""Compare press-to-note pairing strategies against the server's judgement counts.

The server counts are ground truth: stable produced them from the same replay, so a
pairing that reproduces them is measuring what stable measured. This scores several
candidate rules on total absolute band error, summed over a batch.

The variants differ in two ways:

  early_cut   how early a press may be and still claim a note. The reference uses the
              *miss* window and then judges the resulting error as a miss. If stable
              instead ignores presses that early, a spurious press would not consume
              an upcoming note -- which matters a lot, because a stolen note cascades:
              the real press finds its note gone, claims the next one, and so on until
              a gap in that column exceeds the window.
  select      earliest eligible note (reference/queue semantics) vs nearest eligible.
              Nearest is robust to spurious presses by construction; if it wins big,
              the extra presses are the story.
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from parse_replay import (  # noqa: E402
    BAND_NAMES, J_50, J_100, J_MISS, Action, Beatmap, Note,
    actions_from_frames, counts_from_bands, judgement_for, ln_judged_with,
    mania_windows, mod_label, parse_osr, parse_osu, stats,
)


def judge_variant(
    beatmap: Beatmap,
    actions: list[Action],
    windows: list[float],
    score_v2: bool,
    early_cut: int,
    select: str,
) -> tuple[list[float], list[int]]:
    remaining = list(range(len(beatmap.notes)))
    claimed: dict[int, int] = {}
    ln_bands: dict[int, int] = {}
    early = windows[early_cut]

    for ai, action in enumerate(actions):
        head = 0
        target = None
        best_d = None
        while head < len(remaining):
            ni = remaining[head]
            note = beatmap.notes[ni]
            if note.column != action.column:
                head += 1
                continue
            diff = action.time - note.time
            if -diff > early:
                break
            if note.is_hold:
                too_late = action.end - note.end > windows[J_100] * (1.5 if score_v2 else 1.0)
            else:
                too_late = diff >= windows[J_100]
            if too_late:
                remaining.pop(head)
                continue
            if select == "earliest":
                target = ni
                remaining.pop(head)
                break
            d = abs(diff)
            if best_d is None or d < best_d:
                best_d, target = d, ni
            head += 1
        if select == "nearest" and target is not None:
            remaining.remove(target)

        if target is None:
            continue
        claimed[target] = ai
        note = beatmap.notes[target]

        if note.is_hold and not score_v2:
            end_diff = abs(action.end - note.end)
            start_diff = abs(action.time - note.time)
            if note.time - windows[J_50] > action.time:
                start_diff = abs(note.time - (note.end - 1))
            total_diff = start_diff + end_diff
            if action.end - note.end < -windows[J_50]:
                continue
            for band, rate in ((0, 1.2), (1, 1.1), (2, 1.0), (3, 1.0)):
                if ln_judged_with(start_diff, total_diff, band, rate, windows):
                    ln_bands[target] = band
                    break
            else:
                ln_bands[target] = J_50

    errors: list[float] = []
    bands: list[int] = []
    for ni, note in enumerate(beatmap.notes):
        ai = claimed.get(ni)
        if ai is None:
            bands.append(J_MISS)
            continue
        err = float(actions[ai].time - note.time)
        errors.append(err)
        bands.append(ln_bands.get(ni, judgement_for(abs(err), windows)))
    return errors, bands


def load(sid: int, mid: int, root: Path):
    rep = parse_osr(root / "replays" / f"{sid}.osr")
    bm = parse_osu(root / "maps" / f"{mid}.osu")
    if rep.is_mirror:
        for n in bm.notes:
            n.column = bm.keys - n.column - 1
    windows = mania_windows(bm.od, rep.mods)
    actions = actions_from_frames(rep.frames, bm.keys)
    r = rep.clock_rate
    if r != 1.0:
        notes = [Note(int(n.time / r), n.column, int(n.duration / r)) for n in bm.notes]
        bm = Beatmap(bm.mode, bm.keys, bm.od, notes)
        actions = [Action(int(a.time / r), a.column, int(a.duration / r)) for a in actions]
    return rep, bm, windows, actions


VARIANTS = [
    ("ref: earliest, early=miss", J_MISS, "earliest"),
    ("earliest, early=50", J_50, "earliest"),
    ("earliest, early=100", J_100, "earliest"),
    ("nearest, early=miss", J_MISS, "nearest"),
    ("nearest, early=50", J_50, "nearest"),
]


def main() -> int:
    batch = Path(sys.argv[1] if len(sys.argv) > 1 else "local-fixtures/batch.tsv")
    root = batch.parent
    rows = batch.read_text().splitlines()
    header = rows[0].split("\t")

    cases = []
    for line in rows[1:]:
        f = dict(zip(header, line.split("\t")))
        if not (root / "replays" / f"{f['id']}.osr").exists():
            continue
        if not (root / "maps" / f"{f['mapid']}.osu").exists():
            continue
        cases.append((int(f["id"]), int(f["mapid"])))

    totals = {name: 0 for name, _, _ in VARIANTS}
    print(f"{'score':>8} {'mods':>10} {'acts-notes':>10}  " +
          "".join(f"{name.split(':')[-1].strip():>22}" for name, _, _ in VARIANTS))
    print("-" * 130)
    for sid, mid in cases:
        rep, bm, windows, actions = load(sid, mid, root)
        server = {"n320": rep.ngeki, "n300": rep.n300, "n200": rep.nkatu,
                  "n100": rep.n100, "n50": rep.n50, "miss": rep.nmiss}
        cells = []
        for name, cut, select in VARIANTS:
            _, bands = judge_variant(bm, actions, windows, rep.is_score_v2, cut, select)
            got = counts_from_bands(bands)
            err = sum(abs(got[k] - server[k]) for k in BAND_NAMES)
            totals[name] += err
            cells.append(f"{err:>22}")
        print(f"{sid:>8} {mod_label(rep.mods):>10} "
              f"{len(actions) - len(bm.notes):>+10}  " + "".join(cells))

    print("\ntotal absolute band error, summed over the batch (lower is better):")
    for name, _, _ in VARIANTS:
        print(f"  {name:<28} {totals[name]}")

    best = min(totals, key=totals.get)
    print(f"\nbest: {best}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
