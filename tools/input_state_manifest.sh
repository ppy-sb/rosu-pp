#!/bin/sh
set -eu

fixture_root=${1:-local-fixtures}

if [ -L "$fixture_root" ] || [ -L "$fixture_root/maps" ] || [ -L "$fixture_root/multiuser.tsv" ]; then
    echo "refusing symlinked fixture input: $fixture_root" >&2
    exit 1
fi

echo "input-state fixture manifest v1"
echo "commit $(git rev-parse HEAD)"
echo "loader cargo test --release model_ab_report -- --ignored --nocapture"
echo "cohorts overall,user,mods,keys,ln-share,accuracy,largest-movers"
echo "fixture_files $(find "$fixture_root" -maxdepth 2 -type f | wc -l | tr -d ' ')"
echo "multiuser_lines $(wc -l < "$fixture_root/multiuser.tsv" | tr -d ' ')"
echo "multiuser_sha256 $(shasum -a 256 "$fixture_root/multiuser.tsv" | awk '{print $1}')"
echo "maps_begin"
find "$fixture_root/maps" -maxdepth 1 -type f -print | LC_ALL=C sort | while IFS= read -r map; do
    shasum -a 256 "$map"
done
echo "maps_end"
