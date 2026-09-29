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

(ns samizdat.heldout.child
  "The process one held-out case replays in (samizdat.heldout). Started in a
  staged project directory — the case's fixture with the userspace under test
  in its .samizdat/ — it brings the harness up on that directory with an
  in-memory database, replays the recorded conversation through the real
  driver, checks the case's expectations against the run it produced, and
  writes the result to $HELDOUT_OUT.

  Required by nothing: it is only ever the entry point of a child process,
  and it needs the driver, which the tool layer that starts a gate cannot
  require without a cycle.

  NO PROVIDER. Every model turn is served from the recording. A side call a
  cell makes on its own (a critic, a judge) was never recorded, so it is
  refused the same way on both sides of a comparison — the verdict compares
  like with like, and nothing is spent."
  (:require [clojure.edn :as edn]
            [samizdat.agent.beam :as beam]
            [samizdat.battery :as battery]
            [samizdat.llm.client :as llm]
            [samizdat.replay :as replay]
            [samizdat.server :as server]
            [samizdat.store.db :as db]
            [samizdat.store.tasks :as tasks]
            [samizdat.system :as system]))

(defn- trail
  "The end of what the replayed run did — branch, turn, tool, category and
  the head of each result — so a target that fails can be read without the
  database, which dies with this process."
  [conn run-id n width]
  (vec (reverse
        (for [r (db/fetch conn ["SELECT branch_id, turn, tool_name, category, result FROM turns
                                  WHERE run_id = ? ORDER BY id DESC LIMIT ?" run-id n])]
          [(:branch_id r) (:turn r) (:tool_name r) (:category r)
           (let [s (str (:result r))] (subs s 0 (min width (count s))))]))))

(defn- turns-cap
  "The turn cap a replay runs under: the recording's longest branch plus the
  policy's slack, so a candidate that takes more turns exhausts the recording
  (a named result) rather than being cut off first."
  [c slack]
  (+ (long (or slack 0))
     (reduce max 0 (map count (vals (get-in c [:replay :replies]))))))

(defn- recorded-ids
  "A new-id that hands out the recorded run's task ids, in the order it made
  them, then fresh ones: a reply naming a task means the same task under
  replay (heldout/draft!)."
  [ids fresh]
  (let [ids (atom (seq ids))]
    (fn []
      (if-let [id (first @ids)]
        (do (swap! ids next) id)
        (fresh)))))

(defn run
  "Replay case `c` in the harness started on `root`. Returns
  {:run-id :status :result :trail}."
  [{:keys [case root http-port slack trail-turns trail-chars]}]
  (system/start! #'server/handler
                 {:run (cond-> {:root root}
                         (:loop case) (assoc :loop (:loop case)))
                  :db {:path ":memory:"}
                  :http {:port http-port}})
  (try
    ;; new-id is private, hence the -fn form.
    (with-redefs-fn {#'llm/chat (fn [& _]
                                  (throw (ex-info "held-out replay: no provider" {:heldout true})))
                     #'tasks/new-id (recorded-ids (:task-ids case) @#'tasks/new-id)}
      (fn []
        (let [cfg (system/config)
              conn (system/conn)
              rep (:replay case)
              result (beam/run! {:conn conn :config cfg
                                 :llm-adapter (system/adapter)
                                 :llm-config (:llm cfg)
                                 :root root
                                 :problem (:problem rep)
                                 :max-turns (turns-cap case slack)
                                 :beam-width 1
                                 :complete (replay/case-complete-fn rep)
                                 ;; The same computation every time: one turn
                                 ;; at a time, no clock-driven supervisor
                                 ;; (karamazov-x0dx).
                                 :serial-turns? true
                                 :oversight? false})
              run-id (:run-id result)]
          {:run-id run-id
           :status (:status result)
           :result (battery/check conn run-id (:expect case))
           :trail (trail conn run-id (or trail-turns 0) (or trail-chars 0))})))
    (finally (try (system/stop!) (catch Throwable _ nil)))))

(defn -main [& _]
  (let [in (edn/read-string (slurp (System/getenv "HELDOUT_IN")))
        out (System/getenv "HELDOUT_OUT")
        row (try (run in)
                 (catch Throwable e {:error :child :detail (or (ex-message e) (str e))}))]
    (spit out (pr-str row))
    (System/exit 0)))
