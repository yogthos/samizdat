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

(ns samizdat.steps
  "The manifest-state trace, held where an HTTP client can read it.

  `samizdat.events` publishes a STEP for every cell the implementer
  completes — the state graph being walked, live: :start, :measure, :infer,
  :parse, :dispatch, :journal, :settle, :arbiter, :route. That bus is
  in-process, and both front ends are strict HTTP clients with no bus to
  subscribe to, so the trace they most want to draw was the one thing they
  could not see.

  This is the buffer between the two. It subscribes to the bus once, keeps a
  bounded ring per run, and hands out everything after a cursor — the same
  read the journal tail already gives a polling UI, so a client needs no new
  machinery to consume it.

  DELIBERATELY NOT DURABLE. A step is worth a frame of a UI and nothing
  after that; what a turn actually DID is the turns table, which is a
  different question with a different answer. Writing fourteen rows a turn
  to record the shape of the graph the manifest already states would be
  paying storage for a redraw.

  Three properties the ring holds, all of them bounded:

  - A SLOW CLIENT LOSES THE OLDEST, and is told. `:dropped` counts what fell
    off before the client's cursor, so a UI can say it fell behind instead of
    drawing a trace with an invisible hole in it. This is the bus's own
    contract, kept one layer up.
  - A RUN THAT ENDS FREES ITS RING, through `forget!`.
  - A PROCESS NEVER TOLD still does not grow: only the newest `max-runs`
    rings are held, oldest evicted.

  Mechanism only. The numbers are `:step-ring` in gates.edn, and nothing here
  decides when a step is interesting."
  (:require [clojure.tools.logging :as log]
            [samizdat.agent.gates :as gates]
            [samizdat.events :as events]))

;; No fallback literals. gates.edn is the source for every one of these, the
;; way beam.clj reads :pause-poll-ms — a default spelled here would be a
;; second answer to a question resources already answer, and the one nobody
;; edits when they tune the file.
(defn- policy [] (gates/threshold :step-ring))

(defn capacity
  "Steps kept per run."
  []
  (:capacity (policy)))

(defn max-runs
  "How many runs' rings are held at once."
  []
  (:max-runs (policy)))

;; {run-id {:seq n :dropped n :buf [step ...]}}, plus ::order — the run ids
;; oldest-first, so eviction has an answer that does not depend on map order.
(defonce ^:private rings (atom {::order []}))

(defn reset!
  "Drop every ring. For tests and for a process reusing the namespace."
  []
  (clojure.core/reset! rings {::order []})
  nil)

(defn run-count []
  (count (::order @rings)))

(defn- evict-runs
  "`m` with the oldest rings dropped down to `max-runs`."
  [m cap]
  (let [order (::order m)]
    (if (<= (count order) cap)
      m
      (let [drop-n (- (count order) cap)
            gone (subvec order 0 drop-n)]
        (assoc (apply dissoc m gone) ::order (subvec order drop-n))))))

(defn- append
  "Fold one step into a run's ring: a sequence number, and the oldest dropped
  past the cap. `:dropped` counts evictions for the life of the run, so a
  client's shortfall is the difference between it and what the client last
  saw."
  [{:keys [seq dropped buf] :or {seq 0 dropped 0 buf []}} step cap]
  (let [seq (inc seq)
        buf (conj buf (assoc step :seq seq))
        over (max 0 (- (count buf) cap))]
    {:seq seq
     :dropped (+ dropped over)
     ;; A fresh vector rather than a subvec, whose base would keep every step
     ;; the run ever took — the same reason events/slide copies.
     :buf (if (pos? over) (vec (drop over buf)) buf)}))

(defn record!
  "Take one bus event. Anything that is not a step with a run behind it is
  dropped: the subscription is to the WHOLE bus, which carries every journal
  append too, and a ring that took those would evict the trace it exists to
  hold. A step with no run id is real — events/tracer derefs a promise for it,
  and the turn manifest is compiled before the run row exists — and filing
  those under nil would make a ring no client polling a real run ever reads."
  [{:keys [kind run-id] :as event}]
  (when (and (= :step kind) (some? run-id))
    (let [cap (capacity)
          runs (max-runs)]
      (swap! rings
             (fn [m]
               (-> m
                   (update run-id append event cap)
                   (update ::order (fn [o] (if (contains? m run-id) o (conj o run-id))))
                   (evict-runs runs))))))
  nil)

(defn since
  "Everything recorded for `run-id` after `cursor`, at most `limit`.

  Returns {:steps [...] :cursor n :dropped n}. `:cursor` is the sequence
  number of the last step HANDED OVER, not the last recorded, so a limited
  response resumes exactly where it stopped. `:dropped` is how many steps
  were evicted before this client's cursor — 0 when it is keeping up."
  ([run-id cursor] (since run-id cursor 200))
  ([run-id cursor limit]
   (let [{:keys [seq dropped buf] :or {seq 0 dropped 0 buf []}} (get @rings run-id)
         cursor (or cursor 0)
         fresh (into [] (comp (filter #(> (:seq %) cursor)) (take limit)) buf)
         ;; What this client missed: evictions it had not already read past.
         ;; A client at cursor 0 on a ring that has evicted 5 missed all 5;
         ;; one that has kept up missed none.
         missed (max 0 (- dropped cursor))]
     {:steps fresh
      :cursor (if (clojure.core/seq fresh) (:seq (peek fresh)) (min cursor seq))
      :dropped missed})))

(defn forget!
  "Drop a run's ring.

  NOT called when a run finishes, on purpose: the moment a run ends is
  exactly when someone opens it to see how it went, and a trace deleted at
  completion would be empty for every finished run. The `max-runs` bound is
  what keeps the process honest instead. This is here for a caller that
  knows it is done with one."
  [run-id]
  (swap! rings (fn [m] (-> (dissoc m run-id)
                           (update ::order (fn [o] (vec (remove #{run-id} o)))))))
  nil)

;; --- the bus seam ------------------------------------------------------------
;;
;; Split into subscribe!/drain! rather than hidden inside a thread, because
;; the same two calls are how a test drives it deterministically and how the
;; server's pump drives it on a timer. A pump that owned its own loop would
;; make the wiring untestable without sleeping.

(defn pump-ms
  "How often the bus is drained into the rings."
  []
  (:pump-ms (policy)))

(defn subscribe!
  "A bus subscription feeding this ring. Close it with `unsubscribe!`.

  Its window is the ring's own capacity rather than the bus default, so the
  bus cannot drop a step the ring had room for — `:dropped` would then
  under-report, and a UI would draw a hole it was never told about."
  []
  (events/subscribe (max (capacity) events/buffer-size)))

(defn unsubscribe! [sub]
  (events/unsubscribe! sub))

(defn drain!
  "Record everything `sub` has collected since the last drain. Returns how
  many events were taken (steps and all), so a caller can tell a quiet bus
  from a stalled one."
  [sub]
  (let [batch (events/collect sub)]
    (run! record! batch)
    (count batch)))

;; --- the pump ----------------------------------------------------------------

(defonce ^:private pump (atom nil))

(defn pumping? [] (some? @pump))

(defn start-pump!
  "Drain the bus into the rings on a timer, until `stop-pump!`.

  Idempotent: a second call while one is running is a no-op rather than a
  second pump. Two pumps on one bus would each take half the events — the
  bus hands a batch to whoever collects it first — and the rings would carry
  a trace with gaps that appears only after a restart! that ran start! twice.

  Never throws out of the loop. This runs beside a live run and a pump that
  died on one malformed event would take the whole trace down with it, which
  is the failure the tracer's own catch already refuses."
  []
  (when-not @pump
    (let [sub (subscribe!)
          running (atom true)
          t (Thread.
             (fn []
               (while @running
                 (try (drain! sub)
                      (catch Throwable e
                        (log/warn "steps: drain failed:" (ex-message e))))
                 (Thread/sleep (pump-ms))))
             "samizdat-steps-pump")]
      (.setDaemon t true)
      (clojure.core/reset! pump {:sub sub :running running :thread t})
      (.start t)))
  nil)

(defn stop-pump!
  "Stop the pump and close its subscription. A no-op when none is running —
  system/stop! runs best effort over every resource and must not throw here."
  []
  (when-let [{:keys [sub running]} @pump]
    (clojure.core/reset! running false)
    (unsubscribe! sub)
    (clojure.core/reset! pump nil))
  nil)
