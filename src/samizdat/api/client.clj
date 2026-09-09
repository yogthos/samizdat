;; samizdat - a claim-first verification harness
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

(ns samizdat.api.client
  "The client over the run API, plus the journal poll loop.

  Mechanism, so it lives in the base: how to reach the server is the same
  question for every front end, and the GUI and the TUI had no business
  answering it twice. `samizdat.gui.api` is now this namespace under its old
  names; the TUI calls it directly.

  Deliberately toolkit-free — http-client and JSON only — so the headless
  suite covers it without loading GTK or a terminal, and so the model/view
  split stays honest: everything a front end knows arrives through these
  functions, and everything it does goes back through them. The server
  neither knows nor cares that a front end exists.

  Every call returns {:ok true :body ...} or {:ok false :error ...} — a dead
  or absent server is a value the UI renders, never a throw that takes the
  window down."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [jolt.http-client :as http]))

(def ^:private opts
  {:socket-timeout 10000 :conn-timeout 3000 :throw-exceptions false})

(defn- decode [s]
  (try (json/read-str (str s) :key-fn keyword) (catch Throwable _ nil)))

(defn- result [r]
  (if (<= 200 (:status r 0) 299)
    {:ok true :body (decode (:body r))}
    {:ok false :error (str "HTTP " (:status r)
                           (some->> (:body r) decode :error :message (str ": ")))}))

(defn- GET [base path]
  (try (result (http/get (str base path) opts))
       (catch Throwable e {:ok false :error (ex-message e)})))

(defn- POST
  ([base path body] (POST base path body nil))
  ([base path body socket-timeout-ms]
   (try (result (http/post (str base path)
                           (cond-> (assoc opts
                                          :headers {"Content-Type" "application/json"}
                                          :body (json/write-str (or body {})))
                             socket-timeout-ms (assoc :socket-timeout socket-timeout-ms))))
        (catch Throwable e {:ok false :error (ex-message e)}))))

(defn list-runs [base] (GET base "/v1/runs"))

(defn run-detail
  "One run in full: the row, its branches, artifacts, gates, interventions,
  the board and the paths it has changed."
  [base run-id]
  (GET base (str "/v1/runs/" run-id)))

(defn models
  "{:current \"...\" :models [...]} — what the provider serves, for the picker.
  Not /v1/models, which answers what this harness serves and is a different
  question."
  [base]
  (GET base "/v1/harness/models"))

(defn journal-since
  "Everything after `cursor`, the polling UI's one read."
  ([base run-id cursor] (journal-since base run-id cursor 200))
  ([base run-id cursor limit]
   (GET base (str "/v1/runs/" run-id "/journal?since=" (or cursor 0)
                  "&limit=" limit))))

(defn steps-since
  "The live manifest-state trace after `cursor` — the implementer walking its
  state graph.

  Same cursor contract as `journal-since`, which is the point: a front end
  runs one poll loop over both feeds. Held in memory by the server and
  bounded, so the response carries `dropped` when this client fell behind
  (see samizdat.steps)."
  ([base run-id cursor] (steps-since base run-id cursor 200))
  ([base run-id cursor limit]
   (GET base (str "/v1/runs/" run-id "/steps?since=" (or cursor 0)
                  "&limit=" limit))))

(defn approvals
  "Questions this run is waiting on a person to answer — the permission gate
  and ask_human, which share one queue."
  [base run-id]
  (GET base (str "/v1/runs/" run-id "/approvals")))

(defn decide!
  "Answer one of them. `decision` is :allow / :deny for a permission
  question, :answer with `answers` for a questionnaire."
  [base approval-id {:keys [decision note answers]}]
  (POST base (str "/v1/approvals/" approval-id)
        (cond-> {:decision (name (or decision :deny))}
          note (assoc :note note)
          answers (assoc :answers answers))))

(defn turn-detail
  "One turn, whole — including the model's prose, which `branch-detail`
  leaves out because it is the bulk of a long run. Fetched per turn, for the
  turns actually on screen."
  [base run-id branch-id turn]
  (GET base (str "/v1/runs/" run-id "/branches/" branch-id "/turns/" turn)))

(defn branch-detail
  "Every turn and artifact for a branch, in full. Deliberately given a
  longer socket timeout than the rest: the response carries every result
  string and every encoding, which on a long run is hundreds of kilobytes
  and takes seconds. A front end renders live activity from the event stream
  meanwhile, so this arriving late costs nothing."
  [base run-id branch-id]
  (try (result (http/get (str base "/v1/runs/" run-id "/branches/" branch-id)
                         (assoc opts :socket-timeout 45000)))
       (catch Throwable e {:ok false :error (ex-message e)})))

(def start-timeout-ms
  "How long to wait for POST /v1/runs.

  Deliberately longer than the 30s api.control/start-run! itself waits on
  (deref promised 30000). That endpoint does not answer when the run row is
  written — beam/run! opens every branch in the beam first and only then
  calls on-start — so a start can legitimately take tens of seconds, and how
  long depends on the beam width and on what else the machine is doing.

  Under the shared 10s default this reported a failure for a run that was
  starting normally: the row was already committed, so the user was told the
  run had failed AND left with a live run consuming provider spend, with the
  same request succeeding under curl. Any client bound tighter than the
  server's own budget has that bug; this one has to outlast it."
  40000)

(defn start-run!
  "Start a fresh run and return its id in the body.

  The server answers 503 when the beam does not come up inside its 30s window,
  which `result` already turns into {:ok false}. It used to answer 200 with an
  `{:error ...}` body instead, so a plain 2xx was not proof a run existed; the
  unwrap below is kept as a belt-and-braces check on that older shape, since a
  caller that mistakes a refusal for a started run goes on to poll a run id
  that is nil."
  [base body]
  (let [r (POST base "/v1/runs" body start-timeout-ms)]
    (if (and (:ok r) (get-in r [:body :error]))
      {:ok false :error (get-in r [:body :error])}
      r)))

(defn intervene!
  "A human directive, applied at the branch's next turn boundary. `branch-id`
  nil targets the whole run."
  [base run-id {:keys [branch-id kind payload]}]
  (POST base (str "/v1/runs/" run-id "/interventions")
        (cond-> {:kind (or kind "message") :payload payload}
          branch-id (assoc :branch_id branch-id))))

(defn abort! [base run-id]
  (POST base (str "/v1/runs/" run-id "/abort") {}))

(defn resume!
  "Resume a crashed run; with `max-turns`, extend an exhausted one's budget."
  ([base run-id] (resume! base run-id nil))
  ([base run-id max-turns]
   (POST base (str "/v1/runs/" run-id "/resume")
         (if max-turns {:max_turns max-turns} {}))))

;; --- the poll loop -----------------------------------------------------------

(def base-interval-ms 1500)
(def max-backoff-ms 30000)

(defn poll-step
  "Fold one journal fetch into the loop state {:cursor :interval-ms}.

  Pure, so the loop's whole policy is testable offline: events advance the
  cursor and reset the interval; an empty batch keeps both; a failure marks
  the state disconnected and doubles the interval up to the cap — the
  cursor survives an outage, so nothing is missed when the server returns."
  [{:keys [cursor interval-ms] :as state} {:keys [ok body]}]
  (if ok
    (let [events (vec (:events body))]
      (assoc state
             :cursor (or (:id (peek events)) cursor 0)
             :interval-ms base-interval-ms
             :connected? true
             :events events))
    (assoc state
           :events []
           :connected? false
           :interval-ms (min max-backoff-ms
                             (* 2 (max base-interval-ms
                                       (or interval-ms base-interval-ms)))))))

(defn start-poller!
  "Tail `run-id`'s journal on a background thread.

  Calls (on-events events) for every non-empty batch and (on-status state)
  after every step. Callbacks are guarded — one throw used to kill the
  future silently, leaving the header saying \"tailing\" over a frozen
  graph (code-review-2026-08 #5). @running is rechecked after the fetch
  too: a batch landing after stop! belongs to a run the UI has already
  left, and delivering it folded the old run's events into the new one.
  glimmer marshals ratom writes made off the main thread onto the GTK
  loop itself, so callbacks may swap! UI state directly; ftxui's refresh!
  is thread-safe for the same reason.
  Returns {:stop! (fn [])}; stopping is cooperative at the next wakeup."
  [{:keys [base run-id on-events on-status]}]
  (let [running (atom true)
        guard (fn [f]
                (when f
                  (fn [& args]
                    (try (apply f args)
                         (catch Throwable e
                           (println "[api.client] poller callback failed:"
                                    (ex-message e)))))))]
    (let [on-events (guard on-events)
          on-status (guard on-status)]
      (future
        (loop [state {:cursor 0 :interval-ms base-interval-ms}]
          (when @running
            (let [state (poll-step state (journal-since base run-id (:cursor state)))]
              (when (and @running on-events (seq (:events state)))
                (on-events (:events state)))
              (when (and @running on-status)
                (on-status (dissoc state :events)))
              (Thread/sleep (:interval-ms state))
              (recur state)))))
      {:stop! (fn [] (reset! running false))})))
