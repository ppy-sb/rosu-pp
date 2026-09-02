#!/usr/bin/env python3
"""Turn a raw SQL dump (scores+maps joined) into multiuser.tsv's 18-column
schema, and fetch the CS (key count) for each map from docker-mysql.

Input columns (tab-separated, header row):
  userid id mapid mods diff od acc pp n320 n300 n200 n100 n50 nmiss artist title version

Output columns (matches sunny.rs::load_multiuser):
  uid scoreId mapId mods liveStars od keys n320 n300 n200 n100 n50 miss acc livePp artist title version
"""
import subprocess
import sys
from pathlib import Path

CONTAINER = "ppysb-docker-mysql-1"

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


def fetch_keys(map_ids: set[str]) -> dict[str, str]:
    ids = ",".join(sorted(map_ids))
    query = f"select id, cs from maps where id in ({ids})"
    out = subprocess.run(
        ["docker", "exec", CONTAINER, "mysql", "-uroot", "banchopy", "-B", "-e", query],
        capture_output=True, text=True, check=True,
    ).stdout
    keys = {}
    for line in out.splitlines()[1:]:
        mid, cs = line.split("\t")
        keys[mid] = str(int(float(cs)))
    return keys


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print(f"usage: {argv[0]} <raw_dump.tsv> <out_multiuser.tsv>")
        return 2

    lines = Path(argv[1]).read_text().splitlines()
    header = lines[0].split("\t")
    rows = [line.split("\t") for line in lines[1:] if line.strip()]

    idx = {name: i for i, name in enumerate(header)}
    map_ids = {r[idx["mapid"]] for r in rows}
    keys_by_map = fetch_keys(map_ids)

    out_lines = []
    for r in rows:
        mapid = r[idx["mapid"]]
        out_lines.append("\t".join([
            r[idx["userid"]],
            r[idx["id"]],
            mapid,
            mods_string(int(r[idx["mods"]])),
            r[idx["diff"]],
            r[idx["od"]],
            keys_by_map.get(mapid, "4"),
            r[idx["n320"]], r[idx["n300"]], r[idx["n200"]], r[idx["n100"]], r[idx["n50"]], r[idx["nmiss"]],
            r[idx["acc"]],
            r[idx["pp"]],
            r[idx["artist"]],
            r[idx["title"]],
            r[idx["version"]],
        ]))

    Path(argv[2]).write_text("\n".join(out_lines) + "\n")
    print(f"wrote {len(out_lines)} rows -> {argv[2]}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
