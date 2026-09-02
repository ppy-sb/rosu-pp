#!/usr/bin/env bash
# Pull deep per-player score cohorts for fitting the mania error model.
#
# Supersedes fetch_ladder.sh. Three differences, each removing a defect that cost a
# round of bad fitting:
#
#   - **Per-cohort output files.** fetch_ladder.sh always wrote local-fixtures/ladder.tsv,
#     so fetching a second cohort destroyed the first. Here each cohort lands in
#     local-fixtures/cohorts/<uid>-<year>Q<q>.tsv and re-running is idempotent.
#   - **No accuracy filter.** fetch_ladder.sh bounded acc to 88-99.5, which turned out to
#     be the binding constraint on fitting sigma's difficulty response rather than sample
#     size. The low-acc tail is the informative end and it is kept.
#   - **status in (1,2), with a real full-map test.** status=2 is best-per-map and contains
#     *zero* scores below 80% acc: HP drain censors them, so they only survive as
#     non-best submissions (status=1). status=0 is fail — mean 31.6% of the map's hit
#     count, a part-map play that must not be read as a low-acc full-map score. Rather
#     than trusting status alone, every row is required to cover >98% of the largest hit
#     count any score on that map achieved.
#
# Usage:
#   tools/fetch_cohorts.sh survey [min_scores]        # list eligible cohorts, fetch nothing
#   tools/fetch_cohorts.sh fetch <uid:year:q> ...     # pull scores + beatmaps
#   tools/fetch_cohorts.sh fetch --replays <uid:year:q> ...   # also pull .osr replays
#
# Replays are opt-in because they are only needed for the small subset that gets a
# per-note timing fit; the counts-side fit needs beatmaps only.
#
# Everything lands in local-fixtures/, which is gitignored: not redistributable, not
# needed to build.
set -euo pipefail

cd "$(dirname "$0")/.."

CONTAINER="${MYSQL_CONTAINER:-ppysb-docker-mysql-1}"
V1="${BANCHO_V1:-https://api.ppy.sb/v1}"
OUT=local-fixtures
mkdir -p "$OUT/cohorts" "$OUT/replays" "$OUT/maps"

sql() { docker exec "$CONTAINER" mysql -uroot banchopy -B -e "$1"; }

# Shared row filter. `mx` is the largest hit count any mania score reached on that map,
# which stands in for the object count without needing to parse the .osu.
FULL_MAP_FROM="
from scores s
join maps m on m.md5 = s.map_md5
join (select map_md5, max(ngeki+n300+nkatu+n100+n50+nmiss) mh
      from scores where mode = 3 group by map_md5) mx on mx.map_md5 = s.map_md5
where s.mode = 3 and s.status in (1,2)
  and m.mode = 3 and m.status in (2,5) and m.server = 'osu!'
  and (s.ngeki+s.n300+s.nkatu+s.n100+s.n50+s.nmiss) / nullif(mx.mh,0) > 0.98
  and (s.ngeki+s.n300+s.nkatu+s.n100+s.n50+s.nmiss) between 500 and 8000"

cmd="${1:-survey}"
shift || true

if [[ "$cmd" == "survey" ]]; then
  MIN="${1:-150}"
  echo "eligible (player, quarter) cohorts with >= $MIN full-map no-mod/NF scores:"
  sql "
select concat(uid,':',y,':',q) cohort, n, star_lo, star_hi, acc_lo, acc_hi,
       below94, below88
from (
  select s.userid uid, year(s.play_time) y, quarter(s.play_time) q,
         count(*) n,
         round(min(m.diff),2) star_lo, round(max(m.diff),2) star_hi,
         round(min(s.acc),1) acc_lo, round(max(s.acc),1) acc_hi,
         sum(s.acc < 94) below94, sum(s.acc < 88) below88
  $FULL_MAP_FROM
    and s.mods in (0,1)
  group by s.userid, year(s.play_time), quarter(s.play_time)
  having n >= $MIN
) t
order by n desc;"
  echo
  echo "pick cohorts with a wide star span AND a populated below94 column: the low-acc"
  echo "end is what identifies sigma's difficulty response."
  exit 0
fi

if [[ "$cmd" != "fetch" ]]; then
  echo "usage: $0 survey [min_scores] | $0 fetch [--replays] <uid:year:q> ..." >&2
  exit 2
fi

WANT_REPLAYS=0
if [[ "${1:-}" == "--replays" ]]; then
  WANT_REPLAYS=1
  shift
fi

if [[ $# -eq 0 ]]; then
  echo "no cohorts given; run '$0 survey' first" >&2
  exit 2
fi

# Every score for the cohort, no stratification and no acc bound: the fit wants the whole
# distribution, and sampling by difficulty band was a workaround for having too few rows.
build_query() {
  local uid="$1" yr="$2" qs="$3"
  cat <<SQL
select '$uid' cohort, s.id, s.userid, m.id mapid, m.md5, m.od, m.diff, s.mods, s.acc, s.pp,
       s.ngeki n320, s.n300, s.nkatu n200, s.n100, s.n50, s.nmiss,
       s.max_combo, s.score, s.status, s.play_time
$FULL_MAP_FROM
  and s.mods in (0,1)
  and s.userid = $uid
  and year(s.play_time) = $yr and quarter(s.play_time) = $qs
order by m.diff;
SQL
}

total=0
for cohort in "$@"; do
  IFS=: read -r uid yr qs <<<"$cohort"
  tsv="$OUT/cohorts/${uid}-${yr}Q${qs}.tsv"
  sql "$(build_query "$uid" "$yr" "$qs")" > "$tsv.tmp"
  rows=$(($(wc -l < "$tsv.tmp") - 1))
  mv "$tsv.tmp" "$tsv"
  echo "  uid $uid ${yr}Q${qs}: $rows scores -> $tsv"
  total=$((total + rows))
done
echo "selected $total scores across $# cohorts"

# Beatmaps for every row, replays only when asked. Both skip what is already on disk, so
# re-running after adding a cohort pulls only the new files. Sequential with a delay:
# this is someone else's server.
fetched_m=0 failed_m=0 fetched_r=0 failed_r=0
while IFS=$'\t' read -r _cohort id _userid mapid _rest; do
  [[ -z "${mapid:-}" ]] && continue

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

  if [[ $WANT_REPLAYS -eq 1 ]]; then
    osr="$OUT/replays/$id.osr"
    if [[ ! -s "$osr" ]]; then
      if curl -sSf -o "$osr" "$V1/get_replay?id=$id" 2>/dev/null && [[ -s "$osr" ]]; then
        fetched_r=$((fetched_r + 1))
      else
        rm -f "$osr"
        failed_r=$((failed_r + 1))
      fi
      sleep 0.3
    fi
  fi
done < <(cat "$OUT"/cohorts/*.tsv | grep -v '^cohort')

echo "beatmaps: +$fetched_m fetched, $failed_m failed"
[[ $WANT_REPLAYS -eq 1 ]] && echo "replays: +$fetched_r fetched, $failed_r failed"
echo "on disk: $(ls "$OUT/maps" | wc -l | tr -d ' ') beatmaps, $(ls "$OUT/replays" | wc -l | tr -d ' ') replays"
