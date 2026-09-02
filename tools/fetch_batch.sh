#!/usr/bin/env bash
# Pull a batch of real osu!mania scores out of the local bancho.py database, then
# fetch the matching .osr replay and .osu beatmap for each.
#
# Replays come from the bancho.py v1 API directly (https://api.ppy.sb/v1/get_replay);
# the guccho /replay/{id}/download route in front of it requires a login session.
# Beatmaps come from https://osu.ppy.sh/osu/{mapId}, unauthenticated.
#
# Everything lands in local-fixtures/, which is gitignored: this data is not
# redistributable and is not needed to build.
#
# Usage: tools/fetch_batch.sh [limit]
set -euo pipefail

cd "$(dirname "$0")/.."

LIMIT="${1:-16}"
CONTAINER="${MYSQL_CONTAINER:-ppysb-docker-mysql-1}"
V1="${BANCHO_V1:-https://api.ppy.sb/v1}"
OUT=local-fixtures
mkdir -p "$OUT/replays" "$OUT/maps"

# Score selection, mirroring guccho's getBests (see bancho.py/server/user.ts:259):
#   scores.status = 2      BanchoPyScoreStatus.Pick, i.e. a user's best on the map
#   maps.status in (2,5)   BanchoPyRankedStatus Ranked and Loved
#   scores.mode = 3        mania, vanilla ruleset
# Restricted further to osu!-sourced maps so the .osu is fetchable, and to a note
# count range that keeps a fit meaningful without being a 10k-note marathon.
#
# EZ scores first (they are the point of the exercise and are rare), then no-mod
# controls at a comparable pp so the mod response has something to sit against.
read -r -d '' QUERY <<SQL || true
select 'ez' cohort, s.id, s.userid, m.id mapid, m.md5, m.od, m.diff, s.mods,
       s.acc, s.pp, s.ngeki n320, s.n300, s.nkatu n200, s.n100, s.n50, s.nmiss,
       s.max_combo, s.score, s.play_time
from scores s join maps m on m.md5 = s.map_md5
where s.mode = 3 and s.status = 2 and m.mode = 3 and m.status in (2,5)
  and m.server = 'osu!' and (s.mods & 2)
  and (s.ngeki+s.n300+s.nkatu+s.n100+s.n50+s.nmiss) between 1000 and 6000
order by s.pp desc
limit $LIMIT;
SQL

read -r -d '' QUERY_NM <<SQL || true
select 'nm' cohort, s.id, s.userid, m.id mapid, m.md5, m.od, m.diff, s.mods,
       s.acc, s.pp, s.ngeki n320, s.n300, s.nkatu n200, s.n100, s.n50, s.nmiss,
       s.max_combo, s.score, s.play_time
from scores s join maps m on m.md5 = s.map_md5
where s.mode = 3 and s.status = 2 and m.mode = 3 and m.status in (2,5)
  and m.server = 'osu!' and s.mods = 0 and m.id <> 1795016
  and (s.ngeki+s.n300+s.nkatu+s.n100+s.n50+s.nmiss) between 1000 and 6000
  and s.pp between 200 and 700
order by rand(7)
limit $LIMIT;
SQL

echo "querying $CONTAINER for up to $((LIMIT * 2)) scores..."
{
  docker exec "$CONTAINER" mysql -uroot banchopy -B -e "$QUERY"
  # Drop the header row of the second query so the TSV has exactly one.
  docker exec "$CONTAINER" mysql -uroot banchopy -B -e "$QUERY_NM" | tail -n +2
} > "$OUT/batch.tsv"

rows=$(($(wc -l < "$OUT/batch.tsv") - 1))
echo "selected $rows scores -> $OUT/batch.tsv"

# Fetch replays and beatmaps. Both are skipped when already on disk, so re-running
# after adding rows only pulls what is new. Sequential with a small delay: this is
# someone else's server and the batch is small.
fetched_r=0 fetched_m=0 failed_r=0 failed_m=0
while IFS=$'\t' read -r cohort id userid mapid rest; do
  osr="$OUT/replays/$id.osr"
  if [[ ! -s "$osr" ]]; then
    if curl -sSf -o "$osr" "$V1/get_replay?id=$id" 2>/dev/null && [[ -s "$osr" ]]; then
      fetched_r=$((fetched_r + 1))
    else
      rm -f "$osr"
      echo "  ! replay $id ($cohort) unavailable"
      failed_r=$((failed_r + 1))
    fi
    sleep 0.3
  fi

  osu="$OUT/maps/$mapid.osu"
  if [[ ! -s "$osu" ]]; then
    if curl -sSf -o "$osu" "https://osu.ppy.sh/osu/$mapid" 2>/dev/null && [[ -s "$osu" ]]; then
      fetched_m=$((fetched_m + 1))
    else
      rm -f "$osu"
      echo "  ! beatmap $mapid unavailable"
      failed_m=$((failed_m + 1))
    fi
    sleep 0.3
  fi
done < <(tail -n +2 "$OUT/batch.tsv")

echo "replays: +$fetched_r fetched, $failed_r failed"
echo "beatmaps: +$fetched_m fetched, $failed_m failed"
echo "on disk: $(ls "$OUT/replays" | wc -l | tr -d ' ') replays, $(ls "$OUT/maps" | wc -l | tr -d ' ') beatmaps"
