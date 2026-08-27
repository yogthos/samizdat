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

(ns samizdat.board-bt-test
  "The behavior-tree board variant (karamazov-fut): same cells, same
  collaboration contract as board.edn, but position re-derived from current
  state every tick by :board/sense. These tests pin two things: the variant
  completes the same scenarios the plain board does, and the sense node's
  reverse-order precondition table is exactly the policy its docstring
  states."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.cells :as cells]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.store.tasks :as tasks]
            [samizdat.workflow :as workflow]))

(defn- ships-its-task
  "An owner that ships immediately, engaging its own task's words so the
  done-gate accepts the answer (same stub as board-test)."
  [_ _ messages & _]
  (let [content (str/join " " (map :content messages))
        prob (str/trim (or (second (re-find #"## Problem\s+(.+)" content)) "task"))]
    {:content (str "```tool-call\n{\"name\":\"done\",\"args\":{\"answer\":\"handled "
                   prob "\"}}\n```")
     :finish-reason "stop"}))

(defn- run-board-bt
  [conn opts]
  (workflow/run! (merge {:conn conn
                         :config {:run {:loop "board-bt"}}
                         :llm-adapter :a :llm-config {:max-tokens 16384}
                         :problem "the feature" :max-turns 6}
                        opts)))

;; --- the variant completes what the plain board completes -------------------

(deftest the-bt-board-works-a-problem-to-done
  (with-redefs [llm/chat ships-its-task]
    (let [conn (db/open! ":memory:")]
      (run-board-bt conn {})
      (let [rows (db/fetch conn ["SELECT * FROM tasks ORDER BY created_at, id"])]
        (is (= 1 (count rows)) "one task, made from the problem")
        (is (= "done" (:status (first rows)))
            "sensed claim-due, work-due, review-due in turn, and closed it")))))

(deftest the-bt-board-works-every-seeded-task
  (with-redefs [llm/chat ships-its-task]
    (let [conn (db/open! ":memory:")
          a (tasks/create! conn {:title "storage"})
          b (tasks/create! conn {:title "handlers"})]
      (run-board-bt conn {})
      (is (= "done" (:status (tasks/get-task conn a))))
      (is (= "done" (:status (tasks/get-task conn b))))
      (let [branches (map :branch_id (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"]))]
        (is (= (count branches) (count (distinct branches)))
            "one owner per task, same as the plain board")))))

;; --- the sense table IS the policy ------------------------------------------

(defn- sense
  [conn data]
  (when (nil? (cell/get-cell :board/sense))
    (cells/load-cells!))
  (:board/sense (myc/invoke-cell :board/sense
                                 {:conn conn :run-id "r1"}
                                 data)))

(deftest sense-preconditions-query-in-reverse-order
  (let [conn (db/open! ":memory:")]
    (testing "an empty board with a blank blackboard is done"
      (is (= :done (sense conn {}))))
    (testing "a workable task asks for a claim"
      (tasks/create! conn {:title "t"})
      (is (= :claim-due (sense conn {}))))
    (testing "an unjudged outcome outranks everything — including done-ness:
              the final task's diff is reviewed before the round may finish"
      (is (= :review-due (sense conn {:board/outcome :done})))
      (is (= :review-due (sense conn {:board/outcome :error}))
          "an owner that crashed still faces review, which records give-up"))
    (testing "a revise verdict sends the same owner back to work"
      (is (= :work-due (sense conn {:board/outcome :done
                                    :board/decision :revise}))))
    (testing "a fresh claim not yet worked goes to work"
      (is (= :work-due (sense conn {:board/verdict :task}))))
    (testing "a judged outcome falls through to the next claim"
      (is (= :claim-due (sense conn {:board/outcome :done
                                     :board/decision :pass
                                     :board/verdict :task}))))
    (testing "the board refusing a claim ends the round even while raw
              workable rows remain — :board/next's filters (given-up tasks,
              the runaway cap) are the authority, not the raw query"
      (is (= :done (sense conn {:board/verdict :empty}))))))
