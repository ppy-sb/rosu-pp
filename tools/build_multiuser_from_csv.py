#!/usr/bin/env python3
"""Build local-fixtures/multiuser.tsv from a raw phpMyAdmin/adminer-style CSV
export plus locally-computed sunny stars, with no MySQL dependency.

Use this instead of build_multiuser_tsv.py when the local docker-mysql
mirror is a stale backup that predates the users/scores/maps you need (it
can't supply mods/stars/username for rows it doesn't have). The star rating
here is computed by tools/compute_stars.mjs rather than trusted from the
`maps` table, since bancho.py's stored `diff` is a different (and often
stale) calculation from sunny's own.

CSV columns expected (header row, in this order):
  scoreid,bid_dup,score,pp,acc,max_combo,n300,n100,n50,nmiss,ngeki,nkatu,
  grade,play_time,userid,username,bid,key_count,od,stars(bancho,ignored),mods

Output columns (matches sunny.rs::load_multiuser, 18 cols):
  uid scoreId mapId mods liveStars od keys n320 n300 n200 n100 n50 miss acc livePp artist title version
"""
import csv
import sys
from pathlib import Path

MOD_BITS = [
    (1 << 0, "NF"),
    (1 << 1, "EZ"),
    (1 << 4, "HR"),
    (1 << 6, "DT"),
    (1 << 8, "HT"),
    (1 << 9, "NC"),
    (1 << 29, "V2"),
]


def mods_string(mods: int) -> str:
    names = [name for bit, name in MOD_BITS if mods & bit]
    return "".join(names) if names else "NM"


def load_stars(path: str) -> dict[tuple[str, str], str]:
    out = {}
    for line in Path(path).read_text().splitlines():
        mapid, mods, stars = line.split("\t")
        out[(mapid, mods)] = stars
    return out


def osu_metadata(path: Path) -> tuple[str, str, str]:
    artist = title = version = ""
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        if line.startswith("Artist:"):
            artist = line.split(":", 1)[1].strip()
        elif line.startswith("Title:"):
            title = line.split(":", 1)[1].strip()
        elif line.startswith("Version:"):
            version = line.split(":", 1)[1].strip()
        elif line.startswith("["):
            if artist and title and version:
                break
    return artist, title, version


def main(argv: list[str]) -> int:
    if len(argv) != 4:
        print(f"usage: {argv[0]} <csv> <stars_tsv> <out_multiuser.tsv>")
        return 2

    stars = load_stars(argv[2])
    maps_dir = Path("local-fixtures/maps")

    out_lines = []
    skipped = 0
    with open(argv[1], newline="") as f:
        reader = csv.reader(f)
        next(reader)  # header
        for row in reader:
            (scoreid, _bid_dup, score, pp, acc, max_combo, n300, n100, n50, nmiss,
             ngeki, nkatu, grade, play_time, userid, username, bid, key_count,
             od, _stale_stars, mods) = row

            if not (maps_dir / f"{bid}.osu").exists():
                skipped += 1
                continue

            sr = stars.get((bid, mods))
            if sr is None:
                skipped += 1
                continue

            artist, title, version = osu_metadata(maps_dir / f"{bid}.osu")
            keys_int = str(int(round(float(key_count))))
            out_lines.append("\t".join([
                userid,
                scoreid,
                bid,
                mods_string(int(mods)),
                sr,
                od,
                keys_int,
                ngeki, n300, nkatu, n100, n50, nmiss,
                acc,
                pp,
                artist,
                title,
                version,
            ]))

    Path(argv[3]).write_text("\n".join(out_lines) + "\n")
    print(f"wrote {len(out_lines)} rows -> {argv[3]} (skipped {skipped})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
