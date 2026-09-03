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

(ns samizdat.tasks-test
  "The task board: dirge's issues schema generalized (epic_id -> parent_id +
  a type column, session scoping -> run scoping) plus the contract fields
  that make a task a delegable unit, and the model-facing `task` tool."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [jolt.fs :as fs]
            [samizdat.agent.loop :as aloop]
            [samizdat.workflow :as wf]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools :as tools]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.store.runs :as runs]
            [samizdat.store.tasks :as tasks]))

(defmacro with-db [[binding] & body]
  `(let [~binding (db/open! ":memory:")]
     (try ~@body (finally (db/close ~binding)))))

;; --- store ------------------------------------------------------------------

(deftest create-and-get-roundtrip
  (with-db [c]
    (let [id (tasks/create! c {:title "wire the loop"
                               :body "the loop must load from a manifest"
                               :contract "loop compiles from db manifest at boot"
                               :tests "test/samizdat/loop_manifest_test.clj"})]
      (is (str/starts-with? id "sz-"))
      (let [t (tasks/get-task c id)]
        (is (= "wire the loop" (:title t)))
        (is (= "task" (:type t)))
        (is (= "open" (:status t)))
        (is (= "normal" (:priority t)))
        (is (nil? (:run_id t)) "unclaimed tasks are backlog")
        (is (nil? (:parent_id t)))
        (is (= "loop compiles from db manifest at boot" (:contract t)))
        (is (= "test/samizdat/loop_manifest_test.clj" (:tests t)))
        (is (some? (:created_at t)))
        (is (nil? (:closed_at t)))))))

(deftest status-aliases-normalize
  ;; dirge's vocabulary: models say todo/wip/completed/wontfix and the board
  ;; must not fork into synonym lanes.
  (with-db [c]
    (doseq [[alias canonical] {"todo" "open" "backlog" "open" "pending" "open"
                               "wip" "in_progress" "doing" "in_progress"
                               "completed" "done" "finished" "done"
                               "wontfix" "cancelled"}]
      (let [id (tasks/create! c {:title alias :status alias})]
        (is (= canonical (:status (tasks/get-task c id)))
            (str alias " should normalize to " canonical))))
    (is (thrown? Exception (tasks/create! c {:title "bad" :status "resting"}))
        "an unknown status is an error, not a new lane")))

(deftest priority-aliases-normalize
  (with-db [c]
    (doseq [[alias canonical] {"p0" "high" "p1" "high" "urgent" "high"
                               "p2" "normal"
                               "p3" "low" "p4" "low" "minor" "low"
                               "high" "high" "normal" "normal" "low" "low"
                               ;; bare 0-4, what a model actually writes
                               "0" "high" "1" "high" "2" "normal"
                               "3" "low" "4" "low"
                               ;; and the number as JSON sends it
                               2 "normal"}]
      (let [id (tasks/create! c {:title (str alias) :priority alias})]
        (is (= canonical (:priority (tasks/get-task c id)))
            (str alias " should normalize to " canonical))))))

(deftest terminal-status-stamps-closed-at
  (with-db [c]
    (let [id (tasks/create! c {:title "t"})]
      (tasks/update! c id {:status "done"})
      (is (some? (:closed_at (tasks/get-task c id))))
      ;; Reopening clears the stamp: a closed_at on an open task lies.
      (tasks/update! c id {:status "open"})
      (is (nil? (:closed_at (tasks/get-task c id)))))))

(deftest parent-child-hierarchy
  ;; An epic is a TYPE of task, so epics can belong to epics and the model
  ;; decides how many levels it wants.
  (with-db [c]
    (let [epic (tasks/create! c {:title "harness v1" :type "epic"})
          child (tasks/create! c {:title "step 1" :parent-id epic})
          grand (tasks/create! c {:title "step 1a" :parent-id child})]
      (is (= epic (:parent_id (tasks/get-task c child))))
      (is (= [child] (mapv :id (tasks/children-of c epic))))
      (is (= [grand] (mapv :id (tasks/children-of c child)))))
    (is (thrown? Exception (tasks/create! c {:title "orphan" :parent-id "sz-nope"}))
        "a parent that does not exist is an error")))

(deftest board-ordering-and-terminal-exclusion
  ;; dirge's ordering: status (in_progress, blocked, open) then priority then
  ;; recency. Terminal tasks are history, not board.
  (with-db [c]
    (let [open-lo (tasks/create! c {:title "open-low" :priority "low"})
          open-hi (tasks/create! c {:title "open-high" :priority "high"})
          blocked (tasks/create! c {:title "blocked" :status "blocked"})
          wip     (tasks/create! c {:title "wip" :status "in_progress" :priority "low"})
          done    (tasks/create! c {:title "done"})]
      (tasks/update! c done {:status "done"})
      (is (= [wip blocked open-hi open-lo]
             (mapv :id (tasks/board c {})))
          "in_progress before blocked before open; priority within status")
      (is (not-any? #{done} (map :id (tasks/board c {})))
          "terminal tasks stay off the board"))))

(deftest backlog-claim-and-run-scope
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          other (runs/start-run! c {:problem "q"})
          t1 (tasks/create! c {:title "for anyone"})
          t2 (tasks/create! c {:title "already mine" :run-id rid :status "in_progress"})]
      (is (= [t1] (mapv :id (tasks/backlog c))))
      (testing "claim! assigns the run and marks in_progress"
        (is (some? (tasks/claim! c t1 rid "B1")))
        (let [t (tasks/get-task c t1)]
          (is (= rid (:run_id t)))
          (is (= "in_progress" (:status t))))
        (is (empty? (tasks/backlog c))))
      (testing "a task claimed by one run refuses another"
        (is (nil? (tasks/claim! c t1 other "B1")))
        (is (= rid (:run_id (tasks/get-task c t1)))))
      (testing "the board scopes to a run plus the backlog"
        (let [t3 (tasks/create! c {:title "unclaimed"})
              board (tasks/board c {:run-id rid})]
          (is (= #{t1 t2 t3} (set (map :id board)))
              "a run sees its own tasks and the open backlog")
          (is (not-any? #(= other (:run_id %)) board)
              "another run's claimed tasks are not on this run's board"))))))

(deftest a-claim-race-is-decided-by-the-row
  ;; a#4 (docs/provenance.md): claim! used to read-then-write with no guard
  ;; on the write, so two branches whose reads both saw the unclaimed row
  ;; could both "win" — the second silently stealing the task. Simulate the
  ;; interleaved read: the second claim's get-task returns the stale
  ;; unclaimed row, and its write must still lose to the first.
  (with-db [c]
    (let [id (tasks/create! c {:title "race"})
          stale (tasks/get-task c id)]
      (is (= "run-1" (:run_id (tasks/claim! c id "run-1" "B1"))))
      (with-redefs [tasks/get-task (fn [_ _] stale)]
        (is (nil? (tasks/claim! c id "run-2" "B1"))
            "a stale read must not let the second writer steal the claim"))
      (is (= "run-1" (:run_id (tasks/get-task c id)))))))

(deftest update-bumps-updated-at
  (with-db [c]
    (let [id (tasks/create! c {:title "t"})]
      ;; Distinct timestamps without sleeping: pin created_at into the past.
      (db/execute! c ["UPDATE tasks SET updated_at = '2020-01-01T00:00:00Z' WHERE id = ?" id])
      (tasks/update! c id {:body "new body"})
      (is (not= "2020-01-01T00:00:00Z" (:updated_at (tasks/get-task c id)))))))

(deftest board-survives-reopen
  ;; The board is durable state, not context state: kill the process, reopen
  ;; the file, the board is still there.
  (let [path (str "/tmp/samizdat-tasks-test-" (random-uuid) ".sqlite3")]
    (try
      (let [c (db/open! path)
            id (tasks/create! c {:title "durable" :contract "still here"})]
        (db/close c)
        (let [c2 (db/open! path)]
          (try
            (is (= "durable" (:title (tasks/get-task c2 id))))
            (is (= [id] (mapv :id (tasks/board c2 {}))))
            (finally (db/close c2)))))
      (finally (fs/delete-if-exists path)))))

;; --- the task tool ----------------------------------------------------------

(defn- run-tool [c rid tool-name args]
  (tools/run-tool {:tool-name tool-name :args args
                   :branch (state/new-branch {:id "B1" :problem "p"})
                   :conn c :run-id rid :turn 1}))

;; --- the done gate on a coding run (no artifacts) ---------------------------

(deftest done-ships-a-coding-answer-with-figures
  ;; The first self-modification run did the work correctly — wrote code, wrote
  ;; a test, ran it — and could not ship, because the number-coverage rung
  ;; refused an answer containing "0 failures" / "3 tests" when the run had no
  ;; artifacts to cover them. A coding run's evidence is tests passing, not
  ;; confirmed claims, so with empty evidence the rung must not fire.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "add a truncate-middle function with tests"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (let [r (tools/run-tool
               {:tool-name "done"
                :args {:answer "Added truncate-middle; its 3 tests pass with 0 failures."}
                :branch (state/new-branch {:id "B1"
                                           :problem "add a truncate-middle function with tests"})
                :conn c :run-id rid :turn 1})]
        (is (:done? r) "an honest coding answer with figures ships")
        (is (= :success (:category r))))))
  (testing "a blank answer still cannot ship"
    (with-db [c]
      (let [rid (runs/start-run! c {:problem "p"})]
        (runs/open-branch! c rid {:branch-id "B1"})
        (let [r (tools/run-tool {:tool-name "done" :args {:answer ""}
                                 :branch (state/new-branch {:id "B1" :problem "p"})
                                 :conn c :run-id rid :turn 1})]
          (is (not (:done? r))))))))

(deftest task-tool-create-and-show
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          r (run-tool c rid "task" {:action "create" :title "split the parser"
                                    :contract "parser handles nested fences"
                                    :tests "test/parser_test.clj"})]
      (is (= :neutral (:category r)) "bookkeeping is not progress")
      (let [id (re-find #"sz-[0-9a-f]+" (:result r))]
        (is (some? id))
        (let [shown (:result (run-tool c rid "task" {:action "show" :id id}))]
          (is (str/includes? shown "split the parser"))
          (is (str/includes? shown "parser handles nested fences"))
          (is (str/includes? shown "test/parser_test.clj")))))))

(deftest task-tool-list-board
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (run-tool c rid "task" {:action "create" :title "first thing"})
      (run-tool c rid "task" {:action "create" :title "second thing" :priority "high"})
      (let [listing (:result (run-tool c rid "task" {:action "list"}))]
        (is (str/includes? listing "first thing"))
        (is (str/includes? listing "second thing"))))))

(deftest task-tool-claim-update-close
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          id (tasks/create! c {:title "claim me"})]
      (run-tool c rid "task" {:action "claim" :id id})
      (is (= rid (:run_id (tasks/get-task c id))))
      (run-tool c rid "task" {:action "update" :id id :status "blocked"
                              :body "waiting on the schema"})
      (let [t (tasks/get-task c id)]
        (is (= "blocked" (:status t)))
        (is (= "waiting on the schema" (:body t))))
      (run-tool c rid "task" {:action "close" :id id})
      (let [t (tasks/get-task c id)]
        (is (= "done" (:status t)))
        (is (some? (:closed_at t)))))))

(deftest task-tool-bad-calls-are-mechanics
  ;; A bad id or a missing action is a call made wrong, not a failed line of
  ;; inquiry — same reasoning as fetch_artifact's miss.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (is (= :mechanics (:category (run-tool c rid "task" {:action "show" :id "sz-nope"}))))
      (is (= :mechanics (:category (run-tool c rid "task" {}))))
      (is (= :mechanics (:category (run-tool c rid "task" {:action "levitate"}))))
      (is (str/includes? (:result (run-tool c rid "task" {:action "create"}))
                         "title")
          "create without a title says what is missing"))))

;; --- through the loop ------------------------------------------------------

(deftest the-model-works-the-board-through-the-fence
  ;; End to end minus the provider: a scripted model creates a task and then
  ;; closes it, through the real fence parse, dispatch, and journal append.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          _ (runs/open-branch! c rid {:branch-id "B1"})
          ctx {:conn c :run-id rid :max-turns 10
               :llm-adapter :a :llm-config {:max-tokens 16384}}
          b (state/new-branch {:id "B1" :problem "p"})
          fence (fn [m] {:content (str "```tool-call\n" (json/write-str m) "\n```")
                         :finish-reason "stop"})
          b1 (with-redefs [llm/chat (fn [& _] (fence {:name "task"
                                                      :args {:action "create"
                                                             :title "prove the loop"
                                                             :contract "task rows appear"}}))]
               (wf/run-turn ctx b 1))
          id (:id (first (tasks/board c {:run-id rid})))]
      (is (some? id) "the scripted turn created a task")
      (with-redefs [llm/chat (fn [& _] (fence {:name "task"
                                               :args {:action "close" :id id}}))]
        (wf/run-turn ctx b1 2))
      (is (= "done" (:status (tasks/get-task c id)))))))

(deftest update-loses-to-a-write-that-lands-in-its-window
  ;; provenance R2-1: update! was a read-then-write pair over two lock
  ;; acquisitions, so a claim landing between them was silently erased by
  ;; the stale write — the a#4 class one def over from the guarded claim!.
  ;; The UPDATE must carry what the read saw and lose to a row that moved.
  (with-db [c]
    (let [id (tasks/create! c {:title "race"})
          stale (tasks/get-task c id)
          real-get tasks/get-task
          served-stale? (atom false)]
      (is (= "run-9" (:run_id (tasks/claim! c id "run-9" "B1"))))
      (with-redefs [tasks/get-task
                    (fn [conn id]
                      (if (compare-and-set! served-stale? false true)
                        stale
                        (real-get conn id)))]
        (tasks/update! c id {:priority "high"}))
      (let [t (tasks/get-task c id)]
        (is (= "run-9" (:run_id t)) "the claim that landed in the window survives")
        (is (= "in_progress" (:status t)))
        (is (= "high" (:priority t)) "and the edit still lands")))))

(deftest claim-does-not-resurrect-a-terminal-task
  ;; provenance R2-14: a closed task with run_id NULL still satisfied
  ;; run_id IS NULL, so a claim flipped done back to in_progress — completed
  ;; work reappearing on the claiming run's board. Claiming is for available
  ;; work; reopening is an explicit status change through update!.
  (with-db [c]
    (let [id (tasks/create! c {:title "finished business"})]
      (tasks/close! c id "done")
      (is (nil? (tasks/claim! c id "run-1" "B1")))
      (let [t (tasks/get-task c id)]
        (is (= "done" (:status t)))
        (is (some? (:closed_at t)))))))

(deftest create-rethrows-non-collision-failures-instead-of-retrying
  ;; provenance R2-15: create!'s retry caught Throwable — retrying failures that
  ;; can never succeed by retrying, then reporting them as an id-allocation
  ;; problem. Only a UNIQUE collision is retryable; the real failure must
  ;; propagate.
  (with-db [c]
    (let [real-execute db/execute!
          inserts (atom 0)]
      (with-redefs [db/execute!
                    (fn [conn q & opts]
                      (when (str/includes? (str (first q)) "INSERT INTO tasks")
                        (swap! inserts inc)
                        (throw (ex-info "disk I/O error" {:errno 5})))
                      (apply real-execute conn q opts))]
        (is (thrown-with-msg? Exception #"disk I/O error"
                              (tasks/create! c {:title "nope"}))))
      (is (= 1 @inserts) "a non-collision failure is not retried"))))
