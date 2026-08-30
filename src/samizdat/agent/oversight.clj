;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.oversight
  "A PARALLEL STREAM over a running run: the mechanism a supervisor is.

  This namespace knows nothing about supervision. It runs a pass function on a
  cadence, against a budget, in a thread whose failures cost the run nothing,
  and it carries one value from each pass to the next. What a pass looks at and
  what it decides is a cell — the harness's policy about when to think has to
  be something the agent can rewrite while it runs, like every other policy
  here.

  WHY A STREAM AND NOT A NODE. A supervisor wired as a node in the workflow it
  supervises can only run where that workflow puts it. `:feature/supervise` is
  node five of six, reached after the implement stage RETURNS — so a run whose
  implementer stalls never reaches its own watchdog. Runs fps5 and fps6 both
  ended with no supervisor turn at all, having stalled inside implement. A peer
  process is not an arrangement of the graph; it is a second stream beside it.

  WHY IT CARRIES CONTEXT. `run-role` opens a fresh branch per call, so the
  supervisor in feature.edn reads the run cold on every revision and cannot
  refer to what it concluded before. A stream that cannot remember its own last
  conclusion cannot distinguish a change it made from one it merely considered,
  which is most of what supervising is. The carry is that memory.

  It is a peer of `samizdat.watch`, not a replacement. The watcher is a REFLEX:
  rule-based, cheap, every few seconds, steering only. This is DELIBERATION:
  a model call, rare, and permitted to tune the harness as well as steer it.
  Different costs, so different evidence bars and different cadences."
  (:require [clojure.tools.logging :as log]))

(defn due?
  "Whether a pass should run now.

  The first pass is due immediately: a supervisor that waits out a full cadence
  before its first look is blind through the opening stretch in which a run
  picks the approach it will then spend its whole budget on.

  The budget is checked FIRST and binds unconditionally — including against a
  signal. A bound a signal can lift is not a bound.

  What the budget counts is passes that SPENT something, not passes that
  happened; see `pass!`."
  [{:keys [last-at passes]} {:keys [now every-ms budget signal?]}]
  (and (or (nil? budget) (< (or passes 0) budget))
       (or (nil? last-at)
           signal?
           (>= (- now last-at) every-ms))))

(defn pass!
  "Run one pass, and never let it out.

  The pass receives the value the previous pass returned under `:carry`. What
  it returns is either a bare value — which becomes the next carry, and counts
  against the budget — or `{:carry v :spent? bool}`, which lets a pass say it
  cost nothing.

  WHY A PASS MAY BE FREE. The budget bounds MODEL CALLS; that is the whole
  reason a supervisor is budgeted at all. A pass that looked at the run and
  decided it was healthy made no model call, and counting it burned the
  allowance during exactly the stretch where nothing was wrong. Live, run
  a3566c73: twelve quiet heartbeats through a healthy first half exhausted a
  budget of twelve, and when the branch later livelocked with five unmet gates
  — against a floor of two — there was nothing left to spend (karamazov-808).

  A THROWING PASS STILL SPENDS. An observer whose failures are free retries a
  broken pass until the run ends, which is the shape of every busy loop that
  ever pretended to be a watchdog. So does a pass that reports nothing: an
  unreported pass is assumed expensive, because guessing the other way is how
  a bound stops binding.

  `:looks` counts every pass, spent or not, so `status` can tell a stream that
  is watching and content from one that is not running. Returns nil always:
  the caller is a thread and has nothing to inspect."
  [ctx state pass-fn]
  (try
    (let [out (pass-fn (assoc ctx :carry (:carry @state)))
          reported? (and (map? out) (contains? out :spent?))
          spent? (if reported? (boolean (:spent? out)) true)
          carry (if reported? (:carry out) out)]
      (swap! state (fn [s]
                     (cond-> (assoc s :carry carry)
                       true    (update :looks (fnil inc 0))
                       spent?  (update :passes (fnil inc 0))))))
    (catch Throwable e
      ;; Logged, not rethrown, and not retried faster. This thread exists to
      ;; help; the one thing it must never do is become the reason a run ends.
      (swap! state (fn [s] (-> s
                               (update :looks (fnil inc 0))
                               (update :passes (fnil inc 0)))))
      (log/warn "oversight pass failed:" (ex-message e))))
  nil)

(defn start!
  "Begin a stream. Returns an idempotent stop function.

  Every cadence number comes from the caller (the `:oversight` policy in
  gates.edn), never from a default here — a fallback in this file would be a
  policy the agent cannot see or change, which is the one thing `src/` may not
  hold.

  Disabled returns a stop function too, so a caller's teardown never has to
  ask whether the stream was ever running."
  [{:keys [enabled? every-ms budget poll-ms now-fn signal-fn] :as ctx} pass-fn]
  (if-not enabled?
    (constantly nil)
    (let [running (atom true)
          now (or now-fn #(System/currentTimeMillis))
          state (atom {:passes 0 :last-at nil :carry nil})
          f (future
              (while @running
                (try
                  (Thread/sleep (long poll-ms))
                  (when (and @running
                             (due? @state {:now (now)
                                           :every-ms every-ms
                                           :budget budget
                                           :signal? (boolean (when signal-fn (signal-fn)))}))
                    (swap! state assoc :last-at (now))
                    (pass! ctx state pass-fn))
                  (catch Throwable e
                    ;; Guarded on @running: stop clears the flag and then
                    ;; cancels, so an ordinary stop unwinds through here and
                    ;; must not log a warning at the end of every clean run.
                    (when @running
                      (log/warn "oversight loop:" (ex-message e)))))))]
      (fn stop []
        (reset! running false)
        (future-cancel f)
        nil))))
