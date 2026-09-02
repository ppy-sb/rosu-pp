#!/usr/bin/env python3
"""Parse osu!mania .osr replays into per-note hit errors.

Why this exists: the accuracy surface infers a player's timing sigma from judgement
counts, which is an inverse problem with a broad likelihood. A replay carries the
actual keypress times, so pairing them against the beatmap's note times measures
sigma (and therefore Unstable Rate) *directly*. That turns the surface's central
assumption into something testable against ground truth rather than against its own
residuals -- see `sunny::tests::unstable_rate_check`, which otherwise has exactly one
screenshot-derived UR to work with.

The judging logic here follows Mania-Replay-Master (Apache-2.0), which is a validated
implementation of what osu!stable actually does:
https://github.com/Mania-Visualization-Project/Mania-Replay-Master

Cross-checked against abraker95/ultimate_osu_analyzer, whose mania scorer is an
independent per-column state machine. It cannot verify band counts (fixed 100/200 ms
windows, no 320/300/200 bands, releases skipped via `lazy_sliders`) but its pairing
rules are a real second opinion: its distinctive one, that an early press in the miss
band consumes the note as a miss, was tested here and is worse (1224 vs 912), so it
is deliberately not adopted.

Neither reference is authoritative. Every rule below that could be measured against
the server's own judgement counts was, and where a reference disagreed with that
measurement the measurement won.

Points where a naive reading gets it wrong, all of them confirmed against server-side
judgement counts by `--verify`:

  - **Replay frame times share the beatmap's timeline, not the player's wall clock.**
    Both are divided by the clock rate together, so hit errors come out in real ms,
    which is the space hit windows live in. Dividing only the press side drifts the
    residual linearly across the map -- under DT that is a slope of -1/3, which is
    how this was caught. Judging map-time errors against real-time windows is the
    same bug wearing a different hat.
  - **Pairing is a per-column queue, and both of its cuts are the 100 window.** Stable
    burns notes the press has passed and *drops* presses that arrive before the
    frontmost note's window opens. Widening either cut to the miss window costs
    accuracy against the server (total band error 912 -> 2232 early, -> 2074 late).
    A globally optimal min-cost matching is worse still (2434): it wins on its own
    cost function while agreeing with the server less, because it pairs each press to
    its nearest note and so invents a tighter error distribution than the player
    produced. See `match_column`.
  - **Windows can be checked, not just trusted.** Sorting |error| and cutting at the
    server's cumulative counts inverts the score into the windows it implies. No-mod
    lands on 16.0 for a 16.0 ms 320 window -- exact -- and EZ+DT on 1.00-1.09x, which
    is what confirmed the 1.4x-on-every-window convention (including a flat-16 ms
    PERFECT) and localised every remaining discrepancy to pairing.
  - **Under ScoreV1, a hold note's judgement is recomputed from head and tail error
    together** (`judgeRelease`), with widened windows: MAX allows 1.2x on the head and
    2.4x on head+tail combined. Judging heads alone systematically over-awards 320s --
    it was worth +9 320s on a map only 2% long notes, and several hundred on LN maps.
  - **Windows are truncated to whole ms** after the OD and mod scaling.
  - **Mirror (mod bit 30) flips the map's columns**, and is common in this data.
  - **ScoreV2 (bit 29) uses a different window formula entirely**: it maps OD through
    a min/mid/max range where EZ halves the *OD* rather than widening the window by
    1.4x. Reading EZ as a flat 1.4x on a ScoreV2 replay is wrong in both directions.

Usage:
    tools/parse_replay.py local-fixtures/replays/448457.osr local-fixtures/maps/1540691.osu
    tools/parse_replay.py --batch local-fixtures/batch.tsv --verify
    tools/parse_replay.py --batch local-fixtures/batch.tsv --json local-fixtures/errors.json
"""
from __future__ import annotations

import argparse
import json
import lzma
import math
import struct
import sys
from dataclasses import dataclass, field
from pathlib import Path

MODE_MANIA = 3

MOD_EZ = 1 << 1
MOD_HT = 1 << 8
MOD_HR = 1 << 4
MOD_DT = 1 << 6
MOD_NC = 1 << 9
MOD_SCOREV2 = 1 << 29
MOD_MIRROR = 1 << 30

# Judgement band indices, matching the reference's J_* constants.
J_MAX, J_300, J_200, J_100, J_50, J_MISS = range(6)
BAND_NAMES = ("n320", "n300", "n200", "n100", "n50", "miss")


# --------------------------------------------------------------------------- osr

class Reader:
    """Byte reader for osu!'s binary serialisation format."""

    def __init__(self, data: bytes) -> None:
        self.d = data
        self.i = 0

    def u8(self) -> int:
        v = self.d[self.i]
        self.i += 1
        return v

    def u16(self) -> int:
        v = struct.unpack_from("<H", self.d, self.i)[0]
        self.i += 2
        return v

    def u32(self) -> int:
        v = struct.unpack_from("<I", self.d, self.i)[0]
        self.i += 4
        return v

    def u64(self) -> int:
        v = struct.unpack_from("<Q", self.d, self.i)[0]
        self.i += 8
        return v

    def i32(self) -> int:
        v = struct.unpack_from("<i", self.d, self.i)[0]
        self.i += 4
        return v

    def uleb(self) -> int:
        result = shift = 0
        while True:
            b = self.u8()
            result |= (b & 0x7F) << shift
            if not b & 0x80:
                return result
            shift += 7

    def string(self) -> str:
        kind = self.u8()
        if kind == 0x00:
            return ""
        if kind != 0x0B:
            raise ValueError(f"bad string marker 0x{kind:02x} at {self.i - 1}")
        n = self.uleb()
        s = self.d[self.i:self.i + n].decode("utf-8", errors="replace")
        self.i += n
        return s

    def blob(self, n: int) -> bytes:
        b = self.d[self.i:self.i + n]
        self.i += n
        return b


@dataclass
class Replay:
    mode: int
    version: int
    map_md5: str
    player: str
    replay_md5: str
    n300: int
    n100: int
    n50: int
    ngeki: int   # mania: 320 / rainbow 300
    nkatu: int   # mania: 200
    nmiss: int
    score: int
    max_combo: int
    perfect: bool
    mods: int
    life_bar: str
    timestamp: int
    frames: list[tuple[int, int]] = field(default_factory=list)  # (map_ms, key_mask)

    @property
    def total_notes(self) -> int:
        return self.ngeki + self.n300 + self.nkatu + self.n100 + self.n50 + self.nmiss

    @property
    def is_score_v2(self) -> bool:
        return bool(self.mods & MOD_SCOREV2)

    @property
    def is_mirror(self) -> bool:
        return bool(self.mods & MOD_MIRROR)

    @property
    def clock_rate(self) -> float:
        if self.mods & (MOD_DT | MOD_NC):
            return 1.5
        if self.mods & MOD_HT:
            return 0.75
        return 1.0


def parse_osr(path: Path) -> Replay:
    r = Reader(path.read_bytes())
    mode = r.u8()
    version = r.u32()
    map_md5 = r.string()
    player = r.string()
    replay_md5 = r.string()
    n300 = r.u16()   # mania: plain 300
    n100 = r.u16()
    n50 = r.u16()
    ngeki = r.u16()  # mania: 320
    nkatu = r.u16()  # mania: 200
    nmiss = r.u16()
    score = r.i32()
    max_combo = r.u16()
    perfect = bool(r.u8())
    mods = r.u32()
    life_bar = r.string()
    timestamp = r.u64()
    length = r.u32()
    compressed = r.blob(length)

    frames: list[tuple[int, int]] = []
    if length:
        raw = lzma.decompress(compressed).decode("ascii", errors="replace")
        t = 0
        for part in raw.split(","):
            if not part:
                continue
            bits = part.split("|")
            if len(bits) != 4:
                continue
            try:
                dt = int(bits[0])
                keys = int(float(bits[1]))
            except ValueError:
                continue
            # The trailing -12345 frame carries the RNG seed, not a time delta;
            # accumulating it corrupts every timestamp after it.
            if dt == -12345:
                continue
            t += dt
            frames.append((t, keys))

    return Replay(
        mode=mode, version=version, map_md5=map_md5, player=player,
        replay_md5=replay_md5, n300=n300, n100=n100, n50=n50, ngeki=ngeki,
        nkatu=nkatu, nmiss=nmiss, score=score, max_combo=max_combo,
        perfect=perfect, mods=mods, life_bar=life_bar, timestamp=timestamp,
        frames=frames,
    )


# --------------------------------------------------------------------------- osu

@dataclass
class Note:
    time: int
    column: int
    duration: int = 0

    @property
    def end(self) -> int:
        return self.time + self.duration

    @property
    def is_hold(self) -> bool:
        return self.duration != 0


@dataclass
class Beatmap:
    mode: int
    keys: int
    od: float
    notes: list[Note]

    @property
    def holds(self) -> int:
        return sum(1 for n in self.notes if n.is_hold)


def parse_osu(path: Path) -> Beatmap:
    mode = 0
    cs = 4.0
    od = 8.0
    notes: list[Note] = []
    section = ""

    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        line = line.strip()
        if not line or line.startswith("//"):
            continue
        if line.startswith("[") and line.endswith("]"):
            section = line[1:-1]
            continue
        if section in ("General", "Difficulty") and ":" in line:
            k, _, v = line.partition(":")
            k, v = k.strip(), v.strip()
            try:
                if k == "Mode":
                    mode = int(v)
                elif k == "CircleSize":
                    cs = float(v)
                elif k == "OverallDifficulty":
                    od = float(v)
            except ValueError:
                pass
        elif section == "HitObjects":
            f = line.split(",")
            if len(f) < 5:
                continue
            try:
                x = int(float(f[0]))
                time = int(float(f[2]))
                otype = int(f[3])
            except ValueError:
                continue
            keys = max(1, int(cs))
            # Mania maps column from x over the playfield's 512 width.
            col = min(keys - 1, max(0, x * keys // 512))
            duration = 0
            if otype & 128:  # hold note; endTime is the first ':'-delimited extra
                extra = f[5].split(":")[0] if len(f) > 5 else ""
                try:
                    duration = max(0, int(float(extra)) - time)
                except ValueError:
                    duration = 0
            notes.append(Note(time=time, column=col, duration=duration))

    notes.sort(key=lambda n: (n.time, n.column))
    return Beatmap(mode=mode, keys=max(1, int(cs)), od=od, notes=notes)


# ------------------------------------------------------------------------ windows

def map_difficulty_range(od: float, lo: float, mid: float, hi: float, mods: int) -> float:
    """ScoreV2 / lazer-style OD -> window interpolation.

    Note the mod handling: HR multiplies the *OD* by 1.4 and EZ halves it, rather
    than scaling the resulting window. Truncated to whole ms, as the reference does.
    """
    if mods & MOD_HR:
        difficulty = min(10.0, od * 1.4)
    elif mods & MOD_EZ:
        difficulty = max(0.0, od / 2.0)
    else:
        difficulty = od

    if difficulty > 5:
        result = mid + (hi - mid) * (difficulty - 5) / 5
    elif difficulty < 5:
        result = mid - (mid - lo) * (5 - difficulty) / 5
    else:
        result = mid
    return float(int(result))


def mania_windows(od: float, mods: int) -> list[float]:
    """osu!mania hit windows in real (unrated) ms, indexed by the J_* bands.

    Classic scoring pins PERFECT at 16 ms whatever the OD -- OD moves the 300/200
    boundary and never the 320 rate -- which is why EZ, scaling *every* window
    including PERFECT, is the only thing that shifts the 320 count. ScoreV2 instead
    interpolates every band over OD, so its EZ response is a different shape.
    """
    if mods & MOD_SCOREV2:
        return [
            map_difficulty_range(od, 22.4, 19.4, 13.9, mods),
            map_difficulty_range(od, 64.0, 49.0, 34.0, mods),
            map_difficulty_range(od, 97.0, 82.0, 67.0, mods),
            map_difficulty_range(od, 127.0, 112.0, 97.0, mods),
            map_difficulty_range(od, 151.0, 136.0, 121.0, mods),
            map_difficulty_range(od, 188.0, 173.0, 158.0, mods),
        ]

    if mods & MOD_HR:
        mod_rate = 1.0 / 1.4
    elif mods & MOD_EZ:
        mod_rate = 1.4
    else:
        mod_rate = 1.0

    base = [16.0, 34.0, 67.0, 97.0, 121.0, 158.0]
    out = []
    for i, d in enumerate(base):
        r = d if i == 0 else d + 3.0 * (10.0 - od)
        out.append(float(int(r * mod_rate)))
    return out


# ------------------------------------------------------------------------ pairing

@dataclass
class Action:
    time: int
    column: int
    duration: int

    @property
    def end(self) -> int:
        return self.time + self.duration


def scale_time(value: int, rate: float) -> int:
    """Divide a timestamp by the clock rate, truncating as the reference does.

    Applied to notes and actions alike, so everything below works in real ms.
    """
    return int(value / rate)


def actions_from_frames(frames: list[tuple[int, int]], keys: int) -> list[Action]:
    """Frames -> press/release pairs per column, as the reference builds them.

    A column's bit going 0->1 opens an action; going 1->0 closes it and fixes its
    duration. Unreleased holds at the end of the replay are dropped, matching the
    reference's `0..size-2` loop.
    """
    hold_start: dict[int, int | None] = {c: None for c in range(keys)}
    out: list[Action] = []
    for t, mask in frames:
        for col in range(keys):
            pressed = bool(mask & (1 << col))
            if pressed:
                if hold_start[col] is None:
                    hold_start[col] = t
            else:
                if hold_start[col] is not None:
                    start = hold_start[col]
                    out.append(Action(time=start, column=col, duration=t - start))
                    hold_start[col] = None
    out.sort(key=lambda a: (a.time, a.column))
    return out


def judgement_for(diff: float, windows: list[float]) -> int:
    """Band for an absolute error, or J_MISS past the 50 window."""
    for i in range(len(windows) - 1):
        if diff <= windows[i]:
            return i
    return J_MISS


def ln_judged_with(start_diff: float, total_diff: float, band: int,
                   rate: float, windows: list[float]) -> bool:
    """Stable's combined head+tail test for one band of a hold note.

    The head must land inside `window * rate` and head+tail together inside twice
    that. The per-band rates (1.2 for MAX, 1.1 for 300, 1.0 below) are stable's own
    leniency for holds.
    """
    w = windows[band] * rate
    return start_diff <= w and total_diff <= w * 2


def match_column(
    notes: list[tuple[int, Note]],
    presses: list[tuple[int, Action]],
    windows: list[float],
    score_v2: bool,
) -> dict[int, int]:
    """Claim one column's notes with its presses, in time order.

    Stable holds a per-column queue: each press takes the frontmost note still live,
    notes that fall behind the press are burned as misses, and a press that arrives
    before the frontmost note's window opens is dropped without consuming anything.
    That last asymmetry is what stops an early stray from cascading.

    Both cuts are the 100 window (`windows[J_100]`), which is measured, not assumed:
    the batch's total band error against the server is 912 with 100/100 versus 2232
    with a miss-window early cut and 2074 with a miss-window late cut. Widening either
    cut lets presses reach notes stable would not have let them reach.

    A five-zone variant (ignore | miss | hit | miss | ignore, where a press inside the
    miss band consumes the note *and* itself) is also worse: 3372 and 3318 with wide
    miss bands, because consuming the press is exactly what lets one stray desync a
    whole dense section. Discarding it instead keeps the queue in sync. Only a miss
    band as narrow as the 50-100 sliver ties this (920 vs 912), which is not worth the
    extra state.

    A global min-cost matching was tried here and is worse -- 2434 on the same metric.
    It scored well on its own cost function while disagreeing with the server more,
    because optimal play is not what a human did: given slack it pairs every press to
    its nearest note and manufactures a tighter error distribution than really
    occurred. The inverted windows show it directly, EZ+DT implying a 320 window at
    0.82x of the real one under the DP and 1.05x under this queue.
    """
    out: dict[int, int] = {}
    ptr = 0
    for ai, action in presses:
        while ptr < len(notes):
            ni, note = notes[ptr]
            diff = action.time - note.time
            if diff >= windows[J_100]:
                ptr += 1       # note's window has closed; it is a miss
                continue
            if -diff > windows[J_100]:
                break          # press is too early for this note; ignore the press
            out[ni] = ai
            ptr += 1
            break
        else:
            break              # out of notes; remaining presses are strays
    return out


def judge(
    beatmap: Beatmap,
    actions: list[Action],
    windows: list[float],
    score_v2: bool,
) -> tuple[list[float], list[int], int]:
    """Pair actions to notes, then judge; return (head_errors, bands, unmatched).

    Errors are signed real ms of the *head* press, which is the quantity the accuracy
    surface models. The returned bands are the full stable judgement, which for hold
    notes also folds in tail error -- so a band can be worse than the head error alone
    implies. Keep the two separate: fitting sigma wants head errors, checking against
    server counts wants bands.
    """
    by_col_notes: dict[int, list[tuple[int, Note]]] = {}
    for ni, note in enumerate(beatmap.notes):
        by_col_notes.setdefault(note.column, []).append((ni, note))
    by_col_press: dict[int, list[tuple[int, Action]]] = {}
    for ai, action in enumerate(actions):
        by_col_press.setdefault(action.column, []).append((ai, action))

    claimed: dict[int, int] = {}  # note index -> action index
    ln_bands: dict[int, int] = {}  # note index -> combined LN band
    for col, notes in by_col_notes.items():
        claimed.update(
            match_column(notes, by_col_press.get(col, []), windows, score_v2)
        )

    for target, ai in claimed.items():
        action = actions[ai]
        note = beatmap.notes[target]

        if note.is_hold and not score_v2:
            # ScoreV1 re-judges the whole hold from head and tail error combined.
            end_diff = abs(action.end - note.end)
            start_diff = abs(action.time - note.time)
            # stable quirk the reference flags: a press that starts before the head's
            # 50 window measures its "start" from the tail instead.
            if note.time - windows[J_50] > action.time:
                start_diff = abs(note.time - (note.end - 1))
            total_diff = start_diff + end_diff

            if action.end - note.end < -windows[J_50]:
                # Released far too early; stable leaves it open to be hit again.
                continue
            for band, rate in ((J_MAX, 1.2), (J_300, 1.1), (J_200, 1.0), (J_100, 1.0)):
                if ln_judged_with(start_diff, total_diff, band, rate, windows):
                    ln_bands[target] = band
                    break
            else:
                ln_bands[target] = J_50

    head_errors: list[float] = []
    bands: list[int] = []
    for ni, note in enumerate(beatmap.notes):
        ai = claimed.get(ni)
        if ai is None:
            bands.append(J_MISS)
            continue
        err = float(actions[ai].time - note.time)
        head_errors.append(err)
        bands.append(ln_bands.get(ni, judgement_for(abs(err), windows)))

    unmatched_presses = len(actions) - len(claimed)
    return head_errors, bands, unmatched_presses


def counts_from_bands(bands: list[int]) -> dict[str, int]:
    out = dict.fromkeys(BAND_NAMES, 0)
    for b in bands:
        out[BAND_NAMES[b]] += 1
    return out


# ------------------------------------------------------------------------ stats

def stats(errors: list[float]) -> dict[str, float]:
    """Summary of the error distribution. Input is already in real ms.

    Unstable Rate is 10x the standard deviation of hit errors, which is the exact
    quantity the surface calls sigma. Excess kurtosis is reported because it is the
    direct test of the mixture tail: a single normal has excess 0, and the surface
    added a lapse component precisely because real errors do not.
    """
    n = len(errors)
    if n == 0:
        return {"n": 0}
    real = sorted(errors)
    mean = sum(real) / n
    var = sum((e - mean) ** 2 for e in real) / n
    sd = math.sqrt(var)

    def q(p: float) -> float:
        if n == 1:
            return real[0]
        idx = p * (n - 1)
        lo = int(math.floor(idx))
        hi = min(n - 1, lo + 1)
        return real[lo] + (real[hi] - real[lo]) * (idx - lo)

    return {
        "n": n,
        "mean": mean,
        "sd": sd,
        "ur": 10.0 * sd,
        "abs_mean": sum(abs(e) for e in real) / n,
        "p01": q(0.01), "p05": q(0.05), "p25": q(0.25), "median": q(0.50),
        "p75": q(0.75), "p95": q(0.95), "p99": q(0.99),
        "min": real[0], "max": real[-1],
        "excess_kurtosis": (
            sum((e - mean) ** 4 for e in real) / n / (var ** 2) - 3.0 if var > 0 else 0.0
        ),
    }


def analyse(osr_path: Path, osu_path: Path) -> dict:
    rep = parse_osr(osr_path)
    bm = parse_osu(osu_path)

    if rep.mode != MODE_MANIA:
        raise ValueError(f"{osr_path.name}: replay mode {rep.mode}, expected mania")
    if bm.mode != MODE_MANIA:
        raise ValueError(f"{osu_path.name}: beatmap mode {bm.mode}, expected mania")

    if rep.is_mirror:
        for n in bm.notes:
            n.column = bm.keys - n.column - 1

    windows = mania_windows(bm.od, rep.mods)
    actions = actions_from_frames(rep.frames, bm.keys)

    # Normalise both timelines by the clock rate together, so every comparison
    # below is in real ms -- the space the windows are defined in.
    rate = rep.clock_rate
    if rate != 1.0:
        for n in bm.notes:
            n.duration = scale_time(n.duration, rate)
            n.time = scale_time(n.time, rate)
        actions = [Action(time=scale_time(a.time, rate), column=a.column,
                          duration=scale_time(a.duration, rate)) for a in actions]

    errors, bands, unmatched_presses = judge(bm, actions, windows, rep.is_score_v2)

    return {
        "score_id": int(osr_path.stem),
        "map_id": int(osu_path.stem),
        "player": rep.player,
        "mods": rep.mods,
        "score_v2": rep.is_score_v2,
        "mirror": rep.is_mirror,
        "clock_rate": rep.clock_rate,
        "od": bm.od,
        "keys": bm.keys,
        "map_notes": len(bm.notes),
        "hold_notes": bm.holds,
        "replay_md5": rep.map_md5,
        "frames": len(rep.frames),
        "actions": len(actions),
        "unmatched_presses": unmatched_presses,
        "reported": {
            "n320": rep.ngeki, "n300": rep.n300, "n200": rep.nkatu,
            "n100": rep.n100, "n50": rep.n50, "miss": rep.nmiss,
            "total": rep.total_notes, "score": rep.score,
            "max_combo": rep.max_combo,
        },
        "from_replay": counts_from_bands(bands),
        "windows": dict(zip(BAND_NAMES, windows)),
        "stats": stats(errors),
        "errors": errors,
    }


# ---------------------------------------------------------------------------- cli

def mod_label(mods: int) -> str:
    names = [(MOD_EZ, "EZ"), (MOD_HR, "HR"), (MOD_DT, "DT"), (MOD_NC, "NC"),
             (MOD_HT, "HT"), (1, "NF"), (8, "HD"), (4, "SD"),
             (MOD_SCOREV2, "V2"), (MOD_MIRROR, "MI")]
    got = [n for bit, n in names if mods & bit]
    return "".join(got) or "NM"


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("osr", nargs="?", type=Path)
    ap.add_argument("osu", nargs="?", type=Path)
    ap.add_argument("--batch", type=Path,
                    help="batch.tsv from tools/fetch_batch.sh; analyses every row")
    ap.add_argument("--verify", action="store_true",
                    help="compare recomputed judgements against the server's counts")
    ap.add_argument("--json", type=Path, help="write full results (incl. errors) here")
    args = ap.parse_args()

    results = []
    if args.batch:
        root = args.batch.parent
        rows = args.batch.read_text().splitlines()
        header = rows[0].split("\t")
        for line in rows[1:]:
            f = dict(zip(header, line.split("\t")))
            osr = root / "replays" / f"{f['id']}.osr"
            osu = root / "maps" / f"{f['mapid']}.osu"
            if not osr.exists() or not osu.exists():
                missing = "replay" if not osr.exists() else "beatmap"
                print(f"skip {f['id']}: missing {missing}", file=sys.stderr)
                continue
            try:
                res = analyse(osr, osu)
            except Exception as e:  # one bad replay should not sink the batch
                print(f"skip {f['id']}: {e}", file=sys.stderr)
                continue
            res["cohort"] = f.get("cohort", "?")
            res["db"] = {
                "acc": float(f["acc"]), "pp": float(f["pp"]), "diff": float(f["diff"]),
                "md5": f["md5"], "userid": int(f["userid"]),
            }
            res["md5_match"] = res["replay_md5"].lower() == f["md5"].lower()
            results.append(res)
    elif args.osr and args.osu:
        results.append(analyse(args.osr, args.osu))
    else:
        ap.error("give an .osr and .osu, or --batch")

    report(results)
    if args.verify:
        verify(results)

    if args.json:
        args.json.write_text(json.dumps(results, indent=1))
        print(f"\nwrote {args.json}")
    return 0


def report(results: list[dict]) -> None:
    print(f"{'score':>8} {'coh':>3} {'mods':>10} {'od':>4} {'k':>2} {'notes':>6} "
          f"{'LN%':>4} {'paired':>6} {'UR':>7} {'mean':>6} {'kurt':>6} {'md5':>4}")
    print("-" * 88)
    for r in results:
        s = r["stats"]
        if not s.get("n"):
            print(f"{r['score_id']:>8} no paired notes")
            continue
        ln_pct = 100.0 * r["hold_notes"] / max(1, r["map_notes"])
        print(f"{r['score_id']:>8} {r.get('cohort','?'):>3} {mod_label(r['mods']):>10} "
              f"{r['od']:>4.1f} {r['keys']:>2} {r['map_notes']:>6} {ln_pct:>4.0f} "
              f"{s['n']:>6} {s['ur']:>7.1f} {s['mean']:>6.2f} "
              f"{s['excess_kurtosis']:>6.2f} "
              f"{'ok' if r.get('md5_match', True) else 'BAD':>4}")


def verify(results: list[dict]) -> None:
    """Recomputed vs server judgement counts -- the correctness check on all of this.

    A clean match on rice-heavy maps means the pairing, windows, mirror and rate
    handling are all right. Long note maps are expected to drift, since tails are
    not judged here.
    """
    print("\nverify: recomputed judgements vs the server's stored counts")
    print(f"{'score':>8} {'mods':>10} {'LN%':>4}  {'320':>7} {'300':>7} {'200':>6} "
          f"{'100':>5} {'50':>5} {'miss':>5}  {'total_err':>9} {'note':>s}".replace("{'note':>s}", "note"))
    print("-" * 92)
    clean = 0
    for r in results:
        rep, got = r["reported"], r["from_replay"]
        deltas = {k: got[k] - rep[k] for k in BAND_NAMES}
        total_abs = sum(abs(v) for v in deltas.values())
        ln_pct = 100.0 * r["hold_notes"] / max(1, r["map_notes"])
        note = ""
        if total_abs == 0:
            note = "exact"
            clean += 1
        elif ln_pct > 5:
            note = "LN tails unjudged"
        elif total_abs <= 0.005 * max(1, rep["total"]):
            note = "<0.5% of notes"
            clean += 1
        print(f"{r['score_id']:>8} {mod_label(r['mods']):>10} {ln_pct:>4.0f}  "
              f"{deltas['n320']:>+7} {deltas['n300']:>+7} {deltas['n200']:>+6} "
              f"{deltas['n100']:>+5} {deltas['n50']:>+5} {deltas['miss']:>+5}  "
              f"{total_abs:>9} {note}")
    print(f"\n{clean}/{len(results)} within 0.5% of notes on every band")


if __name__ == "__main__":
    sys.exit(main())
