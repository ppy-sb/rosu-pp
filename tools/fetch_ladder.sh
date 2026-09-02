#!/usr/bin/env bash
# Pull "difficulty ladders": for each of a few players, many ranked osu!mania scores
# spanning a wide star range inside one quarter. Companion to fetch_batch.sh, which
# selects by pp across players and therefore cannot fit `skill_exponent`.
#
# Why the shape of this query matters. The error model is
#     sigma = sigma_ref * ((d + floor) / skill)^skill_exponent
# with one free `skill` per player. fetch_batch.sh returned 15 scores across 13
# players, which leaves 2 degrees of freedom for the shape and identifies nothing --
# only one player had depth, and its within-player slopes came out non-monotone.
# Identifying an exponent needs the *same* player measured across a wide difficulty
# range, so skill is held while `d` sweeps.
#
# Three deliberate restrictions, each removing a confound found in the first batch:
#
#   - One quarter per player. Depth alone is not enough: several players span 8+ stars
#     but over 2020-2024, and skill is not constant across four years while the model
#     insists on a single value. A quarter keeps the ladder near-simultaneous. Ample
#     depth survives -- uid 4616 has 132 scores over 8.6 stars in 2023 Q3.
#   - `mods in (0,1)` -- no-mod or NF only. Any window-scaling mod would move the
#     windows sigma is measured against, and DT/HT would move the note timings too.
#     NF is allowed because it only removes the fail condition.
#   - Accuracy bounded on both sides. Above ~99.5% the fit saturates and returns a
#     lower bound on skill rather than a value (see mania_accuracy's
#     SKILL_SATURATION_RATIO); below ~88% adjacent notes' hit windows overlap enough
#     that replay pairing is genuinely ambiguous. Neither end constrains an exponent.
#
# Everything lands in local-fixtures/, which is gitignored: this data is not
# redistributable and is not needed to build.
#
# Usage: tools/fetch_ladder.sh [scores_per_player] [userid:year:quarter ...]
set -euo pipefail

cd "$(dirname "$0")/.."

PER_PLAYER="${1:-30}"
shift || true

# Cohorts chosen by the survey in this script's header: greatest
# span * min(n, 40) among (player, quarter) groups that also carry sub-97% scores, so
# the ladder is not all saturated. Override by passing your own userid:year:quarter.
COHORTS=("$@")
if [[ ${#COHORTS[@]} -eq 0 ]]; then
  COHORTS=(4616:2023:3 5534:2023:2 4255:2023:1 6192:2024:1)
fi

CONTAINER="${MYSQL_CONTAINER:-ppysb-docker-mysql-1}"
V1="${BANCHO_V1:-https://api.ppy.sb/v1}"
OUT=local-fixtures
TSV="$OUT/ladder.tsv"
mkdir -p "$OUT/replays" "$OUT/maps"

# Stratify across difficulty rather than taking the top N by pp, which would bunch at
# the hard end and waste the span the cohort was picked for. `ntile` buckets the
# player's eligible scores into PER_PLAYER difficulty bands; one score per band, the
# median-accuracy pick within it, avoids both a fluke choke and a saturated near-SS.
build_query() {
  local uid="$1" yr="$2" qs="$3"
  cat <<SQL
select '$uid' cohort, id, userid, mapid, md5, od, diff, mods, acc, pp,
       n320, n300, n200, n100, n50, nmiss, max_combo, score, play_time
from (
  select b.*, row_number() over (partition by b.band order by b.acc) rn,
         count(*) over (partition by b.band) cnt
  from (
    select s.id, s.userid, m.id mapid, m.md5, m.od, m.diff, s.mods, s.acc, s.pp,
           s.ngeki n320, s.n300, s.nkatu n200, s.n100, s.n50, s.nmiss,
           s.max_combo, s.score, s.play_time,
           ntile($PER_PLAYER) over (order by m.diff) band
    from scores s join maps m on m.md5 = s.map_md5
    where s.mode = 3 and s.status = 2 and m.mode = 3 and m.status in (2,5)
      and m.server = 'osu!' and s.mods in (0,1)
      and s.userid = $uid
      and year(s.play_time) = $yr and quarter(s.play_time) = $qs
      and s.acc between 88.0 and 99.5
      and (s.ngeki+s.n300+s.nkatu+s.n100+s.n50+s.nmiss) between 800 and 6000
  ) b
) c
where rn = (cnt + 1) div 2
order by diff;
SQL
}

echo "querying $CONTAINER: ${#COHORTS[@]} cohorts, up to $PER_PLAYER scores each"

: > "$TSV.tmp"
header_written=0
for cohort in "${COHORTS[@]}"; do
  IFS=: read -r uid yr qs <<<"$cohort"
  out=$(docker exec "$CONTAINER" mysql -uroot banchopy -B -e "$(build_query "$uid" "$yr" "$qs")")
  rows=$(($(printf '%s\n' "$out" | wc -l) - 1))
  echo "  uid $uid ${yr}Q${qs}: $rows scores"
  if [[ $header_written -eq 0 ]]; then
    printf '%s\n' "$out" >> "$TSV.tmp"
    header_written=1
  else
    printf '%s\n' "$out" | tail -n +2 >> "$TSV.tmp"
  fi
done
mv "$TSV.tmp" "$TSV"

total=$(($(wc -l < "$TSV") - 1))
echo "selected $total scores -> $TSV"

# Fetch replays and beatmaps. Both are skipped when already on disk, so re-running
# after adding cohorts only pulls what is new. Sequential with a small delay: this is
# someone else's server.
fetched_r=0 fetched_m=0 failed_r=0 failed_m=0
while IFS=$'\t' read -r cohort id userid mapid rest; do
  osr="$OUT/replays/$id.osr"
  if [[ ! -s "$osr" ]]; then
    if curl -sSf -o "$osr" "$V1/get_replay?id=$id" 2>/dev/null && [[ -s "$osr" ]]; then
      fetched_r=$((fetched_r + 1))
    else
      rm -f "$osr"
      echo "  ! replay $id (uid $cohort) unavailable"
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
done < <(tail -n +2 "$TSV")

echo "replays: +$fetched_r fetched, $failed_r failed"
echo "beatmaps: +$fetched_m fetched, $failed_m failed"
echo "on disk: $(ls "$OUT/replays" | wc -l | tr -d ' ') replays, $(ls "$OUT/maps" | wc -l | tr -d ' ') beatmaps"
