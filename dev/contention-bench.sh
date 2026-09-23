#!/usr/bin/env bash
# Measure what a turn costs on a local llama-server by how many calls are in
# flight — the table gates.edn :beam-contention :turn-ms-at wants
# (karamazov-vm3w.1). The journal cannot answer this cleanly: a turn's wall
# time there includes tool execution and a prefill that differs turn to turn.
#
# Each request is a fresh ~9k-token prompt (no prompt cache) and a fixed
# 300-token generation (ignore_eos), sized like an opening turn. Run it with
# nothing else on the server — a concurrent generation is exactly what it is
# measuring.
#
#   dev/contention-bench.sh              # 1 2 4 in flight against :8080
#   BENCH_URL=http://host:port BENCH_WIDTHS="1 2 3" dev/contention-bench.sh
#
# Prints one line per width and then the table to paste. Measured 2026-09-23
# on Bonsai 27B (dev/bonsai-server.sh, 4 slots): {1 79000 2 162500 4 309000}.
set -euo pipefail
url="${BENCH_URL:-http://localhost:8080}"
widths="${BENCH_WIDTHS:-1 2 4}"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT

curl -sf -m 5 "$url/health" >/dev/null || { echo "no server at $url" >&2; exit 1; }

one() { # $1 = output file
  local words; words="$(LC_ALL=C awk -v s="$RANDOM$$" 'BEGIN{srand(s); for(i=0;i<900;i++) printf "item%d ", int(rand()*1e9)}')"
  curl -s -m 1800 -o /dev/null -w '%{time_total}\n' "$url/completion" \
       -H 'Content-Type: application/json' \
       -d "{\"prompt\":\"Summarise this list: $words\",\"n_predict\":300,\"ignore_eos\":true,\"cache_prompt\":false}" > "$1"
}

table=""
for k in $widths; do
  for i in $(seq 1 "$k"); do one "$tmp/$k.$i" & done
  wait
  ms="$(cat "$tmp/$k".* | awk '{s+=$1} END {printf "%d", s/NR*1000}')"
  echo "in flight $k: ${ms} ms per request"
  table="$table $k $ms"
done
echo ":turn-ms-at {${table# }}"
