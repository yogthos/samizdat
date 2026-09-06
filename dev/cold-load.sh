#!/usr/bin/env bash
# Every namespace that touches a SHIMMED library must load ON ITS OWN.
#
# jolt resolves java.time.* only once jolt.time has installed the host shim, so
# a library that touches one of those classes at namespace load throws unless
# jolt.time was required first — and it throws naming itself, not the cause.
# Whether a given path is safe therefore depends on LOAD ORDER, which makes it
# invisible until somebody exercises an entry point that never reaches
# samizdat.system.
#
# That has now happened three times: selmer via samizdat.prompt, malli.transform
# via mycelium.schema (which killed the whole suite at load), and
# clojure.data.json across 26 namespaces (which killed the single-namespace
# inner loop AGENTS.md documents). Each was found by a human hitting a wall.
# The suite passing proves nothing here, because the suite loads everything in
# one process in one order (karamazov-kp7w).
#
# One jolt process per namespace is the only honest test of "loads on its own".
# The list is DERIVED, not written down, so a new requirer is covered the day it
# lands.
set -uo pipefail
cd "$(dirname "$0")/.." || exit 1
JOLT="${JOLT:-jolt}"

# -A:test:gui, never -M:test. Two reasons for the exact form:
#   -A not -M   the :test alias carries :main-opts that RUN the whole suite, so
#               -M spent minutes per namespace running every test instead of
#               loading one namespace. -A adds paths without the main.
#   :gui too    gui/ is on :test's paths but glimmer is only on :gui's deps, so
#               without it samizdat.gui.core fails to load for a reason that has
#               nothing to do with the shim this checks.

# Libraries known to reach for a shimmed JDK class at load. Add one here when a
# jolt release moves another class behind the shim.
SHIMMED='clojure\.data\.json|malli\.|selmer\.'

mapfile -t FILES < <(grep -rlE "\[($SHIMMED)" src gui 2>/dev/null | sort -u)

if [ "${#FILES[@]}" -eq 0 ]; then
  echo "cold-load: found no namespaces requiring a shimmed library — the grep is wrong" >&2
  exit 1
fi

echo "cold-load: ${#FILES[@]} namespace(s) that require a shimmed library"
failed=0
for f in "${FILES[@]}"; do
  ns=$(sed -n 's/^(ns \([a-zA-Z0-9._-]*\).*/\1/p' "$f" | head -1)
  [ -n "$ns" ] || { echo "  SKIP $f (no ns form)"; continue; }
  # </dev/null so a child can never consume the driver's stdin.
  if out=$("$JOLT" -A:test:gui -e "(require (quote $ns))" 2>&1 </dev/null); then
    echo "  ok   $ns"
  else
    echo "  FAIL $ns"
    echo "$out" | head -4 | sed 's/^/         /'
    failed=$((failed + 1))
  fi
done

if [ "$failed" -gt 0 ]; then
  echo "cold-load: $failed namespace(s) cannot load on their own." >&2
  echo "Require [jolt.time] before the shimmed library in each, the way" >&2
  echo "samizdat.store.journal and samizdat.prompt already do." >&2
  exit 1
fi
echo "cold-load: all clear"
