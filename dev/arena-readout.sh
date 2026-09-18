#!/usr/bin/env bash
# Read the two karamazov-fp21.4 arms off a sweep's recordings.
#
#   dev/arena-readout.sh [recordings-dir]     # default: $TMPDIR/arena-recordings
#
# One line per recorded run, then the two comparisons the bead asks for:
#   force   — on turns a gate forced a tool (turns.forced_via, v32): how many,
#             the cache hit rate on them and on the turn after, and whether
#             the forced call landed (done / give_up recorded), grouped by via.
#   orient  — the :orient-inject note per run, orientation length per branch
#             (turns before the first write, patch, plan or done), turns to
#             the first plan, and the first turn's prompt tokens.
# The arm is in the recording's file name (wt-<task>-<arm>-<k>.sqlite3).
set -euo pipefail
DIR="${1:-${TMPDIR:-/tmp}/arena-recordings}"
ls "$DIR"/*.sqlite3 >/dev/null 2>&1 || { echo "no recordings under $DIR" >&2; exit 1; }

for db in "$DIR"/*.sqlite3; do
  nm=$(basename "$db" .sqlite3)
  echo "=== $nm"
  sqlite3 -header -column "$db" "
    SELECT status, max_turns,
           (SELECT count(*) FROM turns) AS turns,
           (SELECT count(*) FROM turns WHERE tool_name IN ('__no_call__','__parse_error__')) AS nocall_parse,
           (SELECT round(avg(cache_hit_tokens*1.0/prompt_tokens),3) FROM turns
             WHERE prompt_tokens > 0 AND cache_hit_tokens IS NOT NULL) AS hit_rate
      FROM runs;"
  echo "--- force"
  sqlite3 -header -column "$db" "
    WITH f AS (SELECT branch_id, turn, forced_tool, forced_via, tool_name, category,
                      cache_hit_tokens*1.0/prompt_tokens AS hit
                 FROM turns WHERE forced_tool IS NOT NULL),
         n AS (SELECT t.branch_id, t.turn, t.cache_hit_tokens*1.0/t.prompt_tokens AS hit_next
                 FROM turns t JOIN f ON t.branch_id = f.branch_id AND t.turn = f.turn + 1)
    SELECT coalesce(f.forced_via,'native?') AS via, count(*) AS forced,
           round(avg(f.hit),3) AS hit_on_forced, round(avg(n.hit_next),3) AS hit_after,
           sum(f.tool_name = f.forced_tool) AS landed,
           sum(f.tool_name IN ('__no_call__','__parse_error__')) AS nocall_parse
      FROM f LEFT JOIN n ON n.branch_id = f.branch_id AND n.turn = f.turn
     GROUP BY via;"
  echo "--- orient"
  sqlite3 -header -column "$db" "
    SELECT substr(data,1,120) AS orient_note FROM events WHERE kind = 'orient-inject';"
  sqlite3 -header -column "$db" "
    WITH firstact AS (
      SELECT branch_id, min(turn) AS first_act FROM turns
       WHERE tool_name IN ('write_file','edit_file','patch','plan','done','give_up') GROUP BY branch_id),
    firstplan AS (SELECT branch_id, min(turn) AS first_plan FROM turns WHERE tool_name = 'plan' GROUP BY branch_id)
    SELECT t.branch_id,
           coalesce(fa.first_act, max(t.turn)+1) - 1 AS orientation_turns,
           fp.first_plan,
           (SELECT prompt_tokens FROM turns t1 WHERE t1.branch_id = t.branch_id ORDER BY turn LIMIT 1) AS first_prompt_tokens
      FROM turns t
      LEFT JOIN firstact fa ON fa.branch_id = t.branch_id
      LEFT JOIN firstplan fp ON fp.branch_id = t.branch_id
     GROUP BY t.branch_id;"
done
