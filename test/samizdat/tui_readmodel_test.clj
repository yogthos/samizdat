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

(ns samizdat.tui-readmodel-test
  "What a front end needs that the run detail did not yet answer: the board,
  and the files the run has touched.

  Both are queries over tables the loop already appends to, which is what
  makes them read model rather than new state — the same property that lets
  every other panel work identically on a live run and a finished one."
  (:require [clojure.test :refer [deftest testing is]]
            [db.jdbc]
            [samizdat.api.runs :as api-runs]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.store.tasks :as tasks]))

(defmacro with-db [[binding] & body]
  `(let [~binding (db/open! ":memory:")]
     (try ~@body (finally (db/close ~binding)))))

(defn- write-turn!
  [c rid branch turn tool path]
  (journal/record-turn! c rid {:branch-id branch :turn turn :tool-name tool
                               :args (cond-> {} path (assoc :path path))
                               :result "ok" :category :success}))

(deftest the-run-detail-carries-the-board
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (tasks/create! c {:run-id rid :title "wire the panel"})
      (tasks/create! c {:run-id rid :title "and close it"})
      (let [board (:tasks (api-runs/get-run c rid))]
        (is (= 2 (count board)))
        (is (= #{"wire the panel" "and close it"} (set (map :title board))))))))

(deftest a-closed-task-leaves-the-board
  ;; The panel shows what is being worked on. A board that accumulated every
  ;; finished task would be a log, and there is already one of those.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          t (tasks/create! c {:run-id rid :title "done soon"})]
      (tasks/close! c t)
      (is (empty? (:tasks (api-runs/get-run c rid)))))))

(deftest modified-files-are-the-paths-the-run-wrote-newest-first
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (write-turn! c rid "B1" 1 "write_file" "src/a.clj")
      (write-turn! c rid "B1" 2 "read" "src/never.clj")
      (write-turn! c rid "B1" 3 "edit_file" "src/b.clj")
      (let [m (:modified (api-runs/get-run c rid))]
        (is (= ["src/b.clj" "src/a.clj"] (mapv :path m))
            "writing tools only, most recently touched first")
        (is (= 3 (:turn (first m))) "and the turn that touched it")))))

(deftest a-file-written-twice-appears-once-at-its-latest-turn
  ;; Per path, not per write: the panel answers "what has this run changed",
  ;; and a file edited eight times is one changed file.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (write-turn! c rid "B1" 1 "write_file" "src/a.clj")
      (write-turn! c rid "B1" 5 "edit_file" "src/a.clj")
      (let [m (:modified (api-runs/get-run c rid))]
        (is (= 1 (count m)))
        (is (= 5 (:turn (first m))))))))

(deftest every-branch-that-touched-a-file-is-named
  ;; A beam has several branches in the same tree; which ones touched a file
  ;; is the question a person watching a beam actually has.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (doseq [b ["B1" "B2"]] (runs/open-branch! c rid {:branch-id b}))
      (write-turn! c rid "B1" 1 "write_file" "src/shared.clj")
      (write-turn! c rid "B2" 2 "edit_file" "src/shared.clj")
      (let [m (first (:modified (api-runs/get-run c rid)))]
        (is (= "src/shared.clj" (:path m)))
        (is (= #{"B1" "B2"} (set (:branches m))))))))

(deftest a-turn-with-no-path-argument-is-not-a-modified-file
  ;; Args are the JSON the model sent, so a malformed or absent path has to
  ;; be skipped rather than filed as a file named "null".
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (write-turn! c rid "B1" 1 "write_file" nil)
      (write-turn! c rid "B1" 2 "bash" nil)
      (is (empty? (:modified (api-runs/get-run c rid)))))))
