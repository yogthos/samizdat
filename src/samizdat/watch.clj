;; samizdat - a self-hosting agentic harness
;; Copyright (C) 2026 Dmitri Sotnikov
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.watch
  "The supervisor as a WATCHER: a thread that observes the run while it runs
  and reacts when something goes wrong, the way a person watching a coding
  harness does.

  WHY NOT A NODE IN THE GRAPH. `:feature/supervise` is a manifest node, so it
  runs between rounds, in sequence, only in the workflows that wire it, and
  only when the round it is waiting on has finished. A run losing every turn to
  empty provider replies reaches no round boundary quickly and the node never
  gets a look. The thing you want is the thing a person does: watch the turns
  go by, notice the pattern forming, and say something WHILE it is forming.

  HOW IT INTERVENES, and this is the design decision that matters: through the
  SAME interventions queue a human uses. It does not touch a branch. RFC-006's
  rule — a directive lands on a turn boundary, because a branch mid-turn holds
  a ledger it read before the change — is not a rule the harness's own
  observer gets to skip. So the watcher submits a directive and the driver
  drains it at the boundary exactly as it drains a person's, with the same
  guards, the same refusals and the same record. `issued-by` says `watch`
  rather than `human`, and that is the only difference.

  Three properties it must have, all of which are about not making things
  worse:

  - It can never wedge the run. Every pass is wrapped; a watcher that throws
    logs and keeps watching, and a watcher that dies does not take the run
    with it.
  - It does not repeat itself. A finding raised once is not raised again for
    the same run — an observer that says the same thing every two seconds is
    noise a branch learns to ignore, which is worse than silence.
  - It is bounded. `gates.edn :watch :max-interventions` caps how often it may
    speak at all, because every directive it submits costs the branch a turn
    reading it.

  IT STEERS; IT DOES NOT TUNE. This watcher only ever nudges a running branch,
  and never edits a cell, a manifest, a prompt or a threshold. That split is
  not tidiness — the two jobs have different costs and therefore different
  evidence bars. A nudge is wrong for one turn and the branch reads the next
  one; a userspace edit is wrong for every run until somebody changes it back.
  So steering may act on what a single run shows, which is all this thread can
  see, while tuning waits for a pattern corroborated across runs and belongs to
  the supervisor role, which has that evidence and the experiment machinery to
  judge whether its change helped."
  (:require [clojure.tools.logging :as log]
            [samizdat.agent.gates :as gates]
            [samizdat.events :as events]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]
            [samizdat.session :as session]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]))

(defn- policy [] (lexicon/policy :watch))

(defn- steps-since
  "The implementer's steps that have arrived on `ch` since the last look,
  narrowed to this run.

  The bus is process-wide and two runs in one process publish onto the same
  one, so filtering by run is not tidiness. `:kind :step` excludes the journal
  appends that have always been published there — a watcher wants the graph
  advancing, not the record of it being written."
  [ch run-id]
  (when ch
    (filterv #(and (= :step (:kind %)) (= run-id (:run-id %)))
             (events/collect ch))))

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
  "Say something about one finding, through the human channel.

  A `message` directive, not a `cull` or a `pause`: the watcher observes and
  advises, and deciding that a run should stop is a judgement with a cost that
  belongs to a person or to the supervisor role, not to a threshold that fired.
  The message names the finding and what it rules out — a branch told only
  that turns are being wasted will reword something, which is the expensive
  wrong move."
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

(defn pass!
  "One observation. Returns the findings it reacted to.

  Exposed separately from the loop so a test can drive it a step at a time —
  a watcher that can only be tested by waiting on a thread is a watcher nobody
  tests."
  ;; `ch` rather than destructuring into a local named `events` — that would
  ;; shadow the samizdat.events alias and every events/… call here would
  ;; resolve against the local instead.
  [{:keys [conn run-id] ch :event-ch} seen]
  (let [p (policy)
        ;; THE IMPLEMENTER'S STEPS, pushed (RFC-012). mycelium hands every
        ;; completed cell to :on-trace and the tracer publishes it; this reads
        ;; what has arrived. The supervisor now looks BECAUSE the state graph
        ;; advanced, rather than re-deriving session/findings on a clock
        ;; whether or not the implementer had moved.
        steps (steps-since ch run-id)]
    (if (and ch (empty? steps))
      ;; Nothing advanced, so there is nothing new to judge. The findings are
      ;; derived from what turns did; re-deriving them over an unchanged run
      ;; is the work this change exists to stop doing.
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
        (vec raising)))))

(defn start!
  "Begin watching `run-id`. Returns a function that stops the watcher.

  A plain daemon-ish future rather than anything cleverer: it sleeps, looks,
  and occasionally speaks. The stop function is idempotent and is called from
  the driver's `finally`, so a run that crashes still stops its watcher."
  [{:keys [conn run-id] :as ctx}]
  (if-not (and conn run-id (:enabled? (policy)))
    (constantly nil)
    (let [running (atom true)
          seen (atom #{})
          ;; Subscribed BEFORE the loop starts, so no step that arrives while
          ;; the watcher is getting going is missed. The hub's tap has a
          ;; sliding buffer, so a watcher that falls behind loses the oldest
          ;; steps rather than applying backpressure to the turn producing
          ;; them — which is the right trade here for the same reason the bus
          ;; makes it everywhere else.
          ch (events/subscribe)
          ctx (assoc ctx :event-ch ch)
          f (future
              (while @running
                (try
                  ;; The interval the CONSUMER wakes on, not a poll of the
                  ;; implementer: the steps are already queued, pushed by
                  ;; :on-trace as each cell completed. This only bounds how
                  ;; long a step waits to be seen. It is its own thread
                  ;; because :on-trace runs synchronously inside the turn, so
                  ;; thinking about an event there would stall the turn being
                  ;; watched.
                  (Thread/sleep (:poll-ms (policy)))
                  (when @running (pass! ctx seen))
                  (catch Throwable e
                    ;; A watcher that throws keeps watching. It is an observer;
                    ;; its failure must cost the run nothing.
                    ;;
                    ;; Guarded on @running because `stop` clears the flag and
                    ;; THEN cancels, so a watcher parked in its sleep unwinds
                    ;; through here on the ordinary stop path. Logging that put
                    ;; a warning at the end of every clean run — seven of them
                    ;; in one suite — which is how a real warning becomes
                    ;; invisible.
                    (when @running
                      (log/warn "watch pass failed:" (ex-message e)))))))]
      (fn stop []
        (reset! running false)
        (future-cancel f)
        ;; Release the tap. A subscription nobody reads keeps consuming a
        ;; slot on the hub's mult for the life of the process, which on a
        ;; serve process is every run that ever ran.
        (try (events/unsubscribe! ch) (catch Throwable _ nil))
        nil))))
