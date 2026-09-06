;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.oversight
  "A PARALLEL STREAM over a running run: the mechanism a supervisor is.

  This namespace knows nothing about supervision. It runs a pass function when
  the run it watches reaches a turn boundary — no more often than a spacing,
  against a budget, in a thread whose failures cost the run nothing — and it
  carries one value from each pass to the next. What a pass looks at and what
  it decides is a cell — the harness's policy about when to think has to be
  something the agent can rewrite while it runs, like every other policy
  here.

  WHY A STREAM AND NOT A NODE. A supervisor wired as a node in the workflow it
  supervises can only run where that workflow puts it. `:feature/supervise`
  used to be node five of six, reached after the implement stage RETURNED — so
  a run whose implementer stalled never reached its own watchdog. Runs fps5
  and fps6 both ended with no supervisor turn at all, having stalled inside
  implement. A peer process is not an arrangement of the graph; it is a second
  stream beside it.

  WHY IT CARRIES CONTEXT. `run-role` opens a fresh branch per call, so a
  supervisor run as a stage reads the run cold on every revision and cannot
  refer to what it concluded before. A stream that cannot remember its own
  last conclusion cannot distinguish a change it made from one it merely
  considered, which is most of what supervising is. The carry is that memory.

  It has TWO PHASES, and `reflex!` below is the first. The reflex is cheap:
  rule-based, every poll, steering only. The pass is DELIBERATION: a model
  call, rare, and permitted to tune the harness as well as steer it.
  Different costs, so different evidence bars and different triggers.

  WHAT TRIGGERS EACH (RFC-012 F2). Both phases read the same event bus,
  drained ONCE per poll. The reflex looks whenever anything of this run
  arrived. The pass is due when a TURN of this run has ended since the last
  pass — a `:turn` journal event, which is the record of a turn being written
  — and at least `:every-ms` have elapsed. The clock is a spacing, not a
  trigger: it used to fire the pass every two minutes whether or not the
  implementer had done anything, which evaluated no turn in particular and
  spent looks on a run that was idle."
  (:require [clojure.tools.logging :as log]
            [samizdat.events :as events]
            [samizdat.session :as session]
            [samizdat.store.interventions :as interventions]
            [samizdat.prompt :as prompt]
            [samizdat.lexicon :as lexicon]
            [samizdat.store.journal :as journal]))

(defn due?
  "Whether a pass should run now.

  The first pass is due immediately: a supervisor that waits out a full
  spacing before its first look is blind through the opening stretch in which
  a run picks the approach it will then spend its whole budget on.

  After that a pass is due when a turn has ENDED since the last one
  (`boundary?`) and the spacing has elapsed. The boundary is what makes there
  be something to evaluate; the spacing is what keeps a beam of five branches
  from buying a pass per turn. Neither alone is enough, and that is the
  difference between evaluating a turn and polling.

  The budget is checked FIRST and binds unconditionally — including on a
  boundary. A bound a signal can lift is not a bound.

  What the budget counts is passes that SPENT something, not passes that
  happened; see `pass!`."
  [{:keys [last-at passes]} {:keys [now every-ms budget boundary?]}]
  (and (or (nil? budget) (< (or passes 0) budget))
       (or (nil? last-at)
           (and boundary?
                (>= (- now last-at) (or every-ms 0))))))

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

;; --- phase 1: the reflex ----------------------------------------------------
;;
;; Cheap, rule-based, steering only. IT STEERS; IT DOES NOT TUNE, and that
;; split is not tidiness: a nudge is wrong for one turn and the branch reads
;; the next one, while a userspace edit is wrong for every run until somebody
;; changes it back. So they have different evidence bars. The reflex may act on
;; what a single run shows; tuning waits for phase 2, which has the run's
;; history and the experiment machinery to judge whether its change helped.

(defn- watch-policy [] (lexicon/policy :watch))

(defn- actionable
  "The findings this pass should react to: severe enough to be worth a turn,
  and not already raised.

  Severity is the filter rather than novelty alone, because most findings are
  worth SEEING and only some are worth interrupting for. A supervisor reading
  a block can weigh a medium finding; a branch mid-task being handed one is
  just distracted."
  [findings seen p]
  (let [wanted (set (map keyword (:severities p)))]
    (remove #(or (contains? seen (:kind %))
                 (not (contains? wanted (keyword (name (:severity %))))))
            findings)))

(defn- react!
  "Say something about one finding, through the one write path.

  A `message` directive, not a `cull` or a `pause`: the reflex observes and
  advises, and deciding that a run should stop is a judgement with a cost that
  belongs to a person or to the supervisor's reasoning pass, not to a
  threshold that fired. The message names the finding and what it rules out —
  a branch told only that turns are being wasted will reword something, which
  is the expensive wrong move."
  [conn run-id {:keys [kind detail evidence]}]
  (interventions/submit!
   conn run-id
   {:kind "message"
    :issued-by "watch"
    :payload {:text (prompt/render "watch-intervention"
                                   {:kind (name kind)
                                    :detail detail
                                    :evidence (pr-str evidence)})}})
  (journal/note! conn run-id :watch-intervention
                 {:data {:finding kind :evidence evidence}})
  (log/info "watch: raised" kind "-" detail))

;; Findings already raised, per run. The reflex must not repeat itself — an
;; observer that says the same thing every few seconds is noise a branch learns
;; to ignore, which is worse than silence — and the stream's driver keeps no
;; per-run state to hang this on. `defonce` so a reload does not re-raise
;; everything a live run has already been told.
(defonce ^:private seen-by-run (atom {}))

(defn forget-run!
  "Drop a finished run's raised-findings set, so a long-lived serve process
  does not accumulate one per run it has ever driven."
  [run-id]
  (swap! seen-by-run dissoc run-id)
  nil)

(defn reflex!
  "PHASE 1 of the supervisor (RFC-012): one cheap, rule-based look at what the
  implementer has just done, and at most a nudge.

  `:arrived`, when the caller supplies it, is this run's share of what the bus
  delivered since the last look — steps from the traced turn manifest, turn
  records from every sub-loop, gate firings. Empty means nothing advanced, so
  there is nothing new to judge: the findings are derived from what turns
  did, and re-deriving them over an unchanged run is the work the bus exists
  to stop doing. Any event counts, not only a step — role sub-loops are
  compiled without the tracer and journal turns instead, and a reflex that
  waited for steps was blind through every implement stage of a feature run.
  A caller that hands nothing (a test, a REPL) gets a look regardless.

  Returns the findings it reacted to. Exposed separately from the stream so a
  test can drive it a step at a time — a supervisor that can only be tested by
  waiting on a thread is one nobody tests.

  This was samizdat.watch, a second supervisory thread of its own. Its argument
  for existing was right — a NODE in the implementer's graph runs only when the
  graph reaches it, which is exactly when a stuck turn does not — but that is
  an argument against being a node, not for being a separate stream. It is the
  supervisor's cheap phase, so it runs on the supervisor's stream."
  ([{:keys [run-id] :as ctx}]
   ;; The stream calls this arity every poll and keeps no per-run state of its
   ;; own, so the already-raised set lives here, keyed by run.
   (reflex! ctx (or (get @seen-by-run run-id)
                    (get (swap! seen-by-run update run-id #(or % (atom #{})))
                         run-id))))
  ([{:keys [conn run-id arrived] :as ctx} seen]
   (let [p (watch-policy)]
     (if (and (contains? ctx :arrived) (empty? arrived))
       []
       (let [;; Evaluated over THIS RUN, not the whole session. A watcher is
             ;; asking `is this going wrong now`, and a rate over every run the
             ;; process has done answers a different question — one that says
             ;; no for a long time after the answer became yes.
             fresh (actionable (session/findings (session/run-window run-id)) @seen p)
             room (- (:max-interventions p) (count @seen))
             raising (take (max 0 room) fresh)]
         (doseq [f raising]
           (react! conn run-id f)
           (swap! seen conj (:kind f)))
         (vec raising))))))

(defn start!
  "Begin a stream. Returns an idempotent stop function.

  Every cadence number comes from the caller (the `:oversight` policy in
  gates.edn), never from a default here — a fallback in this file would be a
  policy the agent cannot see or change, which is the one thing `src/` may not
  hold.

  `:event-ch` is the stream's tap on the bus. With one, the pass is due on the
  run's turn boundaries (see `due?`). Without one there is no way to see a
  boundary, so the clock stands in and the pass runs on the spacing alone —
  the shape a driver with no bus gets, and the old behaviour, kept explicit
  here rather than hidden in `due?`.

  Disabled returns a stop function too, so a caller's teardown never has to
  ask whether the stream was ever running."
  [{:keys [enabled? every-ms budget poll-ms now-fn reflex-fn run-id] ch :event-ch :as ctx}
   pass-fn]
  (if-not enabled?
    (constantly nil)
    (let [running (atom true)
          now (or now-fn #(System/currentTimeMillis))
          state (atom {:passes 0 :last-at nil :carry nil :turns-ended 0})
          f (future
              (while @running
                (try
                  (Thread/sleep (long poll-ms))
                  ;; ONE DRAIN of the bus per poll, shared by both phases. The
                  ;; bus is process-wide and two runs in one process publish
                  ;; onto the same one, so narrowing by run is not tidiness.
                  (let [arrived (when ch
                                  (filterv #(= run-id (:run-id %)) (events/collect ch)))
                        ended (count (filter #(= :turn (:kind %)) arrived))]
                    (when (pos? ended)
                      (swap! state update :turns-ended (fnil + 0) ended))
                    ;; PHASE 1, every poll and unbudgeted (RFC-012). The reflex
                    ;; is rule-based and cheap — it reads what the implementer
                    ;; has done and may nudge — so it is not what :every-ms
                    ;; and :budget exist to ration. Those bound MODEL CALLS,
                    ;; which is phase 2 below. This used to be a second thread
                    ;; of its own (samizdat.watch), which made two supervisors
                    ;; where the design calls for one with two phases.
                    (when (and @running reflex-fn)
                      (try (reflex-fn (cond-> ctx ch (assoc :arrived arrived)))
                           (catch Throwable e
                             ;; The reflex must never cost the stream its
                             ;; reasoning pass, nor the run anything at all.
                             (when @running
                               (log/warn "oversight reflex:" (ex-message e))))))
                    ;; PHASE 2, on the run's turn boundary, no closer together
                    ;; than the spacing, and against the budget.
                    (when (and @running
                               (due? @state {:now (now)
                                             :every-ms every-ms
                                             :budget budget
                                             :boundary? (if ch
                                                          (pos? (or (:turns-ended @state) 0))
                                                          true)}))
                      (swap! state assoc :last-at (now) :turns-ended 0)
                      (pass! ctx state pass-fn)))
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
