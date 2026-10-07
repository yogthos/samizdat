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

(ns samizdat.perturb-test
  "Seeded perturbations of a task's wording (karamazov-0e2c.11, after
  iFixAi's adversarial_mutator): the same task said a little differently, to
  tell a result from a fluke of phrasing."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.perturb :as perturb]))

(def problem
  "Fix the bug in `samizdat.agent.loop/route`. The branch should stop when the cap is reached.\n\nRun `jolt test` after the change.")

(deftest variants-are-seeded
  (is (= (perturb/variants problem 7 3) (perturb/variants problem 7 3)) "the same seed, the same variants")
  (is (not= (perturb/variants problem 7 3) (perturb/variants problem 8 3)))
  (is (= 3 (count (perturb/variants problem 7 3)))))

(deftest a-variant-changes-the-wording-and-nothing-a-task-depends-on
  (doseq [v (perturb/variants problem 11 20)]
    (is (not= problem v))
    (testing "code spans are untouched"
      (is (str/includes? v "`samizdat.agent.loop/route`"))
      (is (str/includes? v "`jolt test`")))
    (testing "the words of the task are all still there, in order"
      (let [words (fn [s] (re-seq #"[A-Za-z]+" (str/replace s #"`[^`]*`" "")))
            core (set (words problem))]
        (is (= (filter core (words problem)) (filter core (words v))))))))
