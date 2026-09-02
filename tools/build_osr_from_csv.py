#!/usr/bin/env python3
"""Rebuild full .osr files from a raw CSV export (no MySQL dependency).

Use this instead of build_full_osr.py when the local docker-mysql mirror is
a stale backup that predates the scores/users/maps you need — it reads the
score row straight from the CSV and computes map_md5 locally from the
already-fetched .osu file (verified to match the DB's stored md5 for maps
present in both). `perfect` isn't in typical CSV exports and only feeds an
internal consistency hash, not anything checked server-side, so it's
approximated as `nmiss == 0`.

CSV columns expected (header row, in this order):
  scoreid,bid_dup,score,pp,acc,max_combo,n300,n100,n50,nmiss,ngeki,nkatu,
  grade,play_time,userid,username,bid,key_count,od,stars,mods

Usage:
  tools/build_osr_from_csv.py <csv> <partial_osr_dir> <maps_dir> <out_dir>
"""
import csv
import datetime
import hashlib
import struct
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from build_full_osr import DATETIME_OFFSET, VANILLA_MODE, write_string  # noqa: E402


def build_replay(score_id: int, raw_replay_data: bytes, row: dict) -> bytes:
    n300, n100, n50 = int(row["n300"]), int(row["n100"]), int(row["n50"])
    ngeki, nkatu, nmiss = int(row["ngeki"]), int(row["nkatu"]), int(row["nmiss"])
    score, max_combo = int(row["score"]), int(row["max_combo"])
    perfect = 1 if nmiss == 0 else 0
    mods = int(row["mods"])
    map_md5 = row["map_md5"]
    username = row["username"]

    replay_md5 = hashlib.md5(
        "{}p{}o{}o{}t{}a{}r{}e{}y{}o{}u{}{}{}".format(
            n100 + n300, n50, ngeki, nkatu, nmiss, map_md5, max_combo,
            str(perfect == 1), username, score, 0, mods, "True",
        ).encode(),
    ).hexdigest()

    buf = bytearray()
    buf += struct.pack("<Bi", VANILLA_MODE[3], 20200207)  # mode 3 = mania
    buf += write_string(map_md5)
    buf += write_string(username)
    buf += write_string(replay_md5)
    buf += struct.pack(
        "<hhhhhhihBi",
        n300, n100, n50, ngeki, nkatu, nmiss, score, max_combo, perfect, mods,
    )
    buf += b"\x00"  # life graph, not stored

    play_time = datetime.datetime.strptime(row["play_time"], "%Y-%m-%d %H:%M:%S")
    timestamp = int(play_time.timestamp() * 1e7)
    buf += struct.pack("<q", timestamp + DATETIME_OFFSET)

    buf += struct.pack("<i", len(raw_replay_data))
    buf += raw_replay_data
    buf += struct.pack("<q", score_id)
    return bytes(buf)


def main(argv: list[str]) -> int:
    if len(argv) != 5:
        print(__doc__)
        return 2
    csv_path, partial_dir, maps_dir, out_dir = (
        Path(argv[1]), Path(argv[2]), Path(argv[3]), Path(argv[4]),
    )
    out_dir.mkdir(parents=True, exist_ok=True)

    md5_cache: dict[str, str] = {}
    ok = skipped = 0
    with open(csv_path, newline="") as f:
        reader = csv.reader(f)
        next(reader)  # header
        for cols in reader:
            (scoreid, _bid_dup, score, pp, acc, max_combo, n300, n100, n50, nmiss,
             ngeki, nkatu, grade, play_time, userid, username, bid, key_count,
             od, stale_stars, mods) = cols

            partial = partial_dir / f"{scoreid}.osr"
            if not partial.exists() or partial.stat().st_size == 0:
                print(f"  ! {scoreid}: no partial replay on disk")
                skipped += 1
                continue

            osu_path = maps_dir / f"{bid}.osu"
            if not osu_path.exists():
                print(f"  ! {scoreid}: no map {bid}.osu on disk")
                skipped += 1
                continue

            map_md5 = md5_cache.get(bid)
            if map_md5 is None:
                map_md5 = hashlib.md5(osu_path.read_bytes()).hexdigest()
                md5_cache[bid] = map_md5

            row = {
                "n300": n300, "n100": n100, "n50": n50, "ngeki": ngeki,
                "nkatu": nkatu, "nmiss": nmiss, "score": score,
                "max_combo": max_combo, "mods": mods, "map_md5": map_md5,
                "username": username, "play_time": play_time,
            }
            raw = partial.read_bytes()
            full = build_replay(int(scoreid), raw, row)
            (out_dir / f"{scoreid}.osr").write_bytes(full)
            ok += 1

    print(f"rebuilt {ok} replays -> {out_dir} (skipped {skipped})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
