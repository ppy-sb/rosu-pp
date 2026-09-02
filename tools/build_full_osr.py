#!/usr/bin/env python3
"""Rebuild full .osr files from bancho.py's partial on-disk replay + SQL row.

bancho.py only stores the LZMA-compressed frame blob at
.data/osr/{scoreid}.osr; the full .osr (headers + blob + trailing score id) is
built on demand by the /get_replay endpoint. This reproduces that endpoint's
packing exactly (refactor/bancho.py/app/api/v1/api.py:api_get_replay), reading
partial files scp'd from prod and score/map/user rows from the local
docker-mysql mirror.

Usage:
  tools/build_full_osr.py <partial_dir> <out_dir> <score_id> [<score_id> ...]
  tools/build_full_osr.py <partial_dir> <out_dir> --ids-file <path>

Reads {partial_dir}/{score_id}.osr for each id, looks up the matching row via
`docker exec ppysb-docker-mysql-1 mysql`, and writes {out_dir}/{score_id}.osr.
"""
import hashlib
import struct
import subprocess
import sys
from pathlib import Path

DATETIME_OFFSET = 0x89F7FF5F7B58000
CONTAINER = "ppysb-docker-mysql-1"

# vanilla mode: bancho.py's GameMode.as_vanilla collapses relax/autopilot
# variants (4-8) back onto 0-3; scores tables here are already mode 3 (mania).
VANILLA_MODE = {0: 0, 1: 1, 2: 2, 3: 3, 4: 0, 5: 1, 6: 2, 7: 3, 8: 0}


def write_uleb128(num: int) -> bytes:
    if num == 0:
        return b"\x00"
    out = bytearray()
    while num != 0:
        b = num & 0x7F
        num >>= 7
        if num != 0:
            b |= 0x80
        out.append(b)
    return bytes(out)


def write_string(s: str) -> bytes:
    if s:
        enc = s.encode()
        return b"\x0b" + write_uleb128(len(enc)) + enc
    return b"\x00"


def fetch_row(score_id: int) -> dict | None:
    query = (
        "SELECT u.name username, u.id uid, m.md5 map_md5, "
        "m.artist, m.title, m.version, "
        "s.mode, s.n300, s.n100, s.n50, s.ngeki, "
        "s.nkatu, s.nmiss, s.score, s.max_combo, "
        "s.perfect, s.mods, s.play_time "
        "FROM scores s "
        "INNER JOIN users u ON u.id = s.userid "
        "INNER JOIN maps m ON m.md5 = s.map_md5 "
        f"WHERE s.id = {score_id}"
    )
    out = subprocess.run(
        ["docker", "exec", CONTAINER, "mysql", "-uroot", "banchopy", "-B", "-e", query],
        capture_output=True, text=True, check=True,
    ).stdout
    lines = out.splitlines()
    if len(lines) < 2:
        return None
    cols = lines[0].split("\t")
    vals = lines[1].split("\t")
    return dict(zip(cols, vals))


def build_replay(score_id: int, raw_replay_data: bytes, row: dict) -> bytes:
    n300 = int(row["n300"])
    n100 = int(row["n100"])
    n50 = int(row["n50"])
    ngeki = int(row["ngeki"])
    nkatu = int(row["nkatu"])
    nmiss = int(row["nmiss"])
    score = int(row["score"])
    max_combo = int(row["max_combo"])
    perfect = int(row["perfect"])
    mods = int(row["mods"])
    mode = int(row["mode"])
    map_md5 = row["map_md5"]
    username = row["username"]

    replay_md5 = hashlib.md5(
        "{}p{}o{}o{}t{}a{}r{}e{}y{}o{}u{}{}{}".format(
            n100 + n300,
            n50,
            ngeki,
            nkatu,
            nmiss,
            map_md5,
            max_combo,
            str(perfect == 1),
            username,
            score,
            0,  # rank, unused by consumers
            mods,
            "True",
        ).encode(),
    ).hexdigest()

    buf = bytearray()
    buf += struct.pack("<Bi", VANILLA_MODE[mode], 20200207)
    buf += write_string(map_md5)
    buf += write_string(username)
    buf += write_string(replay_md5)
    buf += struct.pack(
        "<hhhhhhihBi",
        n300, n100, n50, ngeki, nkatu, nmiss, score, max_combo, perfect, mods,
    )
    buf += b"\x00"  # life graph, not stored

    # play_time comes back as "YYYY-MM-DD HH:MM:SS"
    import datetime
    play_time = datetime.datetime.strptime(row["play_time"], "%Y-%m-%d %H:%M:%S")
    timestamp = int(play_time.timestamp() * 1e7)
    buf += struct.pack("<q", timestamp + DATETIME_OFFSET)

    buf += struct.pack("<i", len(raw_replay_data))
    buf += raw_replay_data
    buf += struct.pack("<q", score_id)
    return bytes(buf)


def main(argv: list[str]) -> int:
    if len(argv) < 4:
        print(__doc__)
        return 2
    partial_dir = Path(argv[1])
    out_dir = Path(argv[2])
    out_dir.mkdir(parents=True, exist_ok=True)
    if argv[3] == "--ids-file":
        score_ids = [int(x) for x in Path(argv[4]).read_text().split()]
    else:
        score_ids = [int(x) for x in argv[3:]]

    ok = 0
    for sid in score_ids:
        partial = partial_dir / f"{sid}.osr"
        if not partial.exists() or partial.stat().st_size == 0:
            print(f"  ! {sid}: no partial replay on disk")
            continue
        row = fetch_row(sid)
        if row is None:
            print(f"  ! {sid}: no matching sql row")
            continue
        raw = partial.read_bytes()
        full = build_replay(sid, raw, row)
        (out_dir / f"{sid}.osr").write_bytes(full)
        ok += 1
    print(f"rebuilt {ok}/{len(score_ids)} replays -> {out_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
