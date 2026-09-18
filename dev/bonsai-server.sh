#!/usr/bin/env bash
# The local model samizdat is tested against: Ternary Bonsai 2 27B on the
# PrismML fork of llama.cpp (the stock llama-server cannot load the PQ2_0
# ternary format). One script so the flags are the vendor's and the same
# every time, not whatever the last terminal remembered.
#
# Flags follow Bonsai-demo's scripts/start_llama_server.sh for the 27B:
#   -fa on --jinja, sampling temp 1.0 / top-p 0.95 / top-k 20 (the base
#   model's own defaults), the Q8_0 vision projector, image tokens capped at
#   1024 on Metal. Thinking stays ON at the server — Bonsai 2 is a thinking
#   model; samizdat decides per call (config :llm :thinking?, the runaway
#   breaker's reasoning_effort "none") — but CAPPED: --reasoning-budget is
#   the vendor's own advice for slow hardware, and on this M1 Max the model
#   decodes at 15-20 tok/s, so the first live turn thought for ~6000 tokens
#   and timed out. Measured 2026-09-18 on a small coding task: unrestricted
#   1200-1340 reasoning tokens (104-117 s); a 2048 cap changed nothing there;
#   a 1024 cap cut it to 760-860 (85 s) and the answer was still right. 2048
#   bounds the long turns without touching the ordinary ones. A per-request
#   reasoning_budget_tokens exists in this fork too (server-common.cpp:1354).
#
# Slots: -np SLOTS with -c CTX per slot, KV NOT unified, so each branch of a
# beam pins its own prefix cache (id_slot / cache_prompt, RFC-005) instead of
# four branches evicting one shared buffer.
#
# Everything is overridable from the environment; extra arguments pass
# straight through to llama-server.
#
#   dev/bonsai-server.sh                 # start, wait for /health, print /props
#   BONSAI_REASONING_BUDGET=-1 dev/bonsai-server.sh      # unrestricted thinking
#   BONSAI_SLOTS=1 BONSAI_CTX=65536 dev/bonsai-server.sh
#   kill "$(lsof -ti TCP:${BONSAI_PORT:-8080})"   # stop
set -euo pipefail

BIN="${BONSAI_LLAMA_SERVER:-/Users/yogthos/src/llama.cpp-prism-ml/build/bin/llama-server}"
MODEL_DIR="${BONSAI_MODEL_DIR:-/Users/yogthos/src/models}"
MODEL="${BONSAI_MODEL:-$MODEL_DIR/Ternary-Bonsai-2-27B-PQ2_0.gguf}"
MMPROJ="${BONSAI_MMPROJ:-$MODEL_DIR/Ternary-Bonsai-2-27B-mmproj-Q8_0.gguf}"
HOST="${BONSAI_HOST:-127.0.0.1}"
PORT="${BONSAI_PORT:-8080}"
NGL="${BONSAI_NGL:-99}"
CTX="${BONSAI_CTX:-32768}"      # per slot
SLOTS="${BONSAI_SLOTS:-4}"
REASONING_BUDGET="${BONSAI_REASONING_BUDGET:-2048}"   # -1 = unrestricted, 0 = no thinking
LOG="${BONSAI_LOG:-/tmp/bonsai-server.log}"

for f in "$BIN" "$MODEL" "$MMPROJ"; do
  [ -e "$f" ] || { echo "missing: $f" >&2; exit 1; }
done
if lsof -ti TCP:"$PORT" >/dev/null 2>&1; then
  echo "port $PORT is busy: $(lsof -ti TCP:"$PORT" | head -1) — stop it first" >&2
  exit 1
fi

# -c is the TOTAL context in llama.cpp; without -kvu it is split evenly
# across the slots, which is what gives every slot its own CTX.
TOTAL=$(( CTX * SLOTS ))

nohup "$BIN" -m "$MODEL" --host "$HOST" --port "$PORT" -ngl "$NGL" -fa on \
  -c "$TOTAL" -np "$SLOTS" -no-kvu \
  --temp 1.0 --top-p 0.95 --top-k 20 \
  --reasoning-budget "$REASONING_BUDGET" \
  --jinja \
  --mmproj "$MMPROJ" --image-max-tokens 1024 \
  "$@" > "$LOG" 2>&1 &
echo "llama-server pid $! — log $LOG"

for _ in $(seq 1 60); do
  if curl -sf -m 2 "http://$HOST:$PORT/health" >/dev/null 2>&1; then
    curl -s -m 5 "http://$HOST:$PORT/props" | python3 -c '
import sys, json
d = json.load(sys.stdin)
g = d.get("default_generation_settings", {})
print("model:", d.get("model_path") or d.get("model_alias"))
print("slots:", d.get("total_slots"), " n_ctx:", g.get("n_ctx"), " modalities:", d.get("modalities"))'
    grep -E "n_slots|n_ctx_slot" "$LOG" | tail -1
    exit 0
  fi
  sleep 2
done
echo "server did not answer /health within 120s — see $LOG" >&2
exit 1
