;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Ported from dirge's src/agent/agent_loop/thinking_budget.rs. The derived
;; cap, the overrun factor, the both-signals trigger and the recovery are all
;; that design.

(ns samizdat.agent.thinking
  "THE RUNAWAY-REASONING BREAKER.

  A model that deliberates without converging emits reasoning steadily, so
  nothing keyed on SILENCE ever trips: the request is producing tokens the
  whole time, and they never become an action. Every other guard here keys on
  completed turns — the storm window, the failure streak, the progress gates —
  and this failure happens inside one.

  The harness already sees it happen. `:provider-empty-replies` in the watch
  signals names it exactly: `the model is spending its whole output budget
  thinking and never reaching a tool call`. What it could not do was stop it.

  THE CAP IS DERIVED, NOT PICKED, and dirge learned that the expensive way. A
  flat constant meant a high-effort turn was granted a large thinking budget by
  the request and then cut off below it by the breaker — the harness truncating
  reasoning it had itself just asked for, and then disabling thinking for the
  rest of the task over it. Traces of tens of thousands of tokens are ordinary
  for a reasoning model on a hard turn; that is the feature working, not a
  runaway.

  So the cap is a MULTIPLE of whatever this turn was actually granted. It means
  `the model blew well past its own allocation`, which is the real signal, and
  it cannot contradict the request the harness just sent.

  BOTH SIGNALS, never either alone: the reply was cut off at the token limit,
  AND the reasoning trace is over the derived cap. A truncation on a short
  trace is a budget that was simply too small — the fix there is more tokens,
  and this project's own `:truncated` signal exists to keep those apart."
  (:require [clojure.string :as str]))

(defn derived-cap
  "The reasoning ceiling for a turn granted `granted` thinking tokens:
  `factor` times the grant, floored at `floor`.

  `fallback` when the grant is unknown — no effort resolved, nothing to read.
  Deliberately the most permissive derived value rather than a guess at a
  typical one: a missed runaway costs a turn, and a truncated good turn costs
  the work in it."
  [granted {:keys [factor floor fallback]}]
  (if (and granted (pos? granted))
    (max floor (* factor granted))
    fallback))

(defn runaway?
  "Whether this reply is a model reasoning without converging.

  Cut off at the token limit AND a trace past the derived cap. A reply that
  merely ran out of room, or one that thought hard and then called a tool, is
  neither."
  [{:keys [truncated? reasoning parsed]} cap chars-per-token]
  (boolean (and truncated?
                (nil? parsed)
                (> (quot (count (str reasoning)) (max 1 chars-per-token)) cap))))

(defn recovery
  "What a branch carries after the breaker fires: thinking off for the rest of
  the task, and a count so a second firing is visible.

  OFF FOR THE REST OF THE TASK, not for one turn. A model that has just spent
  a whole budget deliberating will spend the next one the same way — the
  condition is the approach it has taken to the problem, not the turn. The
  branch is the right scope because a task ends and the next one starts fresh."
  [branch]
  (-> branch
      (assoc :thinking-off? true)
      (update :thinking-breaks (fnil inc 0))))

(defn effort-for
  "The reasoning effort this branch's next request should ask for: whatever the
  run configured, unless the breaker has fired, in which case none.

  Read at request time rather than written into the run config, because the
  config is the RUN's and this decision is one branch's."
  [branch configured off-value]
  (if (:thinking-off? branch) off-value configured))
