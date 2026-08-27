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

(ns samizdat.finalization-test
  "Tier 3 guards (karamazov-g86): the completeness ship rung, the same-file
  thrash streak, and retry-carrying-diagnosis. Deterministic mechanism tests,
  per the n=1 measurability rule."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.arbiter :as arbiter]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.loop :as aloop]
            [samizdat.agent.state :as state]
            [samizdat.agent.storm :as storm]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.ship :as ship]))

;; --- the completeness rung --------------------------------------------------

(deftest unfinished-claims-are-detected-by-the-conjunction
  (testing "a first-person plan with a work verb, no second person, trips"
    (is (ship/unfinished-claim?
         "The parser is done. I still need to implement the CLI entry point."))
    (is (ship/unfinished-claim?
         "Tests pass for headings. Next I will add the paragraph handling.")))
  (testing "each leg of the conjunction alone does NOT trip — the conjunction
            IS the control (dirge completeness_gate.rs)"
    (is (not (ship/unfinished-claim?
              "I still have doubts about the design."))
        "forward marker with no work verb: a remark, not a plan")
    (is (not (ship/unfinished-claim?
              "The next maintainer should implement caching here."))
        "work verb with no first-person forward marker")
    (is (not (ship/unfinished-claim?
              "I will explain how you can add more rules to the converter."))
        "second-person address: advice to the reader, a legitimate ending")
    (is (not (ship/unfinished-claim?
              "Implemented the converter and the CLI; the suite is green."))
        "a finished report trips nothing"))
  (testing "the verb match is an exact token, so latest is not test"
    (is (not (ship/unfinished-claim?
              "I will describe the latest results below.")))))

(deftest the-completeness-rung-blocks-a-half-done-ship
  (let [msg (ship/ship-gate-block
             {:answer (str "Built the mdlite converter; headings and "
                           "paragraphs work. I still need to implement "
                           "the code-block handling.")
              :problem "build the mdlite converter"
              :evidence [] :uncovered-numbers []})]
    (is (string? msg))
    (is (str/includes? msg "work remains")))
  (is (nil? (ship/ship-gate-block
             {:answer "Built the mdlite converter, suite green: 12 assertions."
              :problem "build the mdlite converter"
              :evidence [] :uncovered-numbers []}))
      "an honest finished answer ships"))

;; --- the same-file streak ---------------------------------------------------

(deftest the-file-touch-streak-narrows-and-breaks
  (let [t storm/note-file-touch]
    (testing "overlapping touches accumulate and narrow to the intersection"
      (let [s (-> {} (t #{"a.clj"}) (t #{"a.clj" "b.clj"}) (t #{"a.clj"}))]
        (is (= 3 (:streak s)))
        (is (= #{"a.clj"} (:files s)))))
    (testing "a disjoint touch restarts the streak at 1"
      (let [s (-> {} (t #{"a.clj"}) (t #{"b.clj"}))]
        (is (= 1 (:streak s)))
        (is (= #{"b.clj"} (:files s)))))
    (testing "a no-file call resets to zero"
      (is (= {:streak 0 :files #{}}
             (-> {} (t #{"a.clj"}) (t #{})))))))

(deftest touched-paths-reads-both-key-spellings
  (is (= #{"a.clj"} (storm/touched-paths {:path "a.clj"})))
  (is (= #{"a.clj"} (storm/touched-paths {"path" "a.clj"})))
  (is (= #{"a.clj" "b.clj"} (storm/touched-paths {:paths ["a.clj" "b.clj"]})))
  (is (= #{} (storm/touched-paths {:command "ls"})))
  (is (= #{} (storm/touched-paths nil))))

(deftest the-file-thrash-gate-fires-on-the-streak-and-settles-by-rule
  (let [g (first (filter #(= :file-thrash (:gate %)) (gates/gates)))]
    (is (some? g) "the :file-thrash gate entry exists in gates.edn")
    (let [th (gates/threshold :file-thrash-threshold)
          hot (assoc (state/new-branch {:id "B1" :problem "p"})
                     :file-touch {:streak th :files #{"a.clj"}})
          cold (state/new-branch {:id "B1" :problem "p"})]
      (is ((:when g) {:branch hot}))
      (is (not ((:when g) {:branch cold})))
      (testing "settled met when the streak broke, still open while it holds"
        (let [firing {:gate :file-thrash :turn 1 :window 3}]
          (is (= :met (arbiter/settle firing
                                      {:current-turn 2 :tools-called []
                                       :branch-before hot
                                       :branch-after cold})))
          (is (nil? (arbiter/settle firing
                                    {:current-turn 2 :tools-called []
                                     :branch-before hot
                                     :branch-after hot}))))))))

;; --- retry carries the diagnosis --------------------------------------------

(deftest a-different-failure-of-the-same-call-inherits-the-diagnosis
  (let [replies (atom ["no such file: core.clj" "permission denied: core.clj"])]
    (with-redefs [tools/run-tool (fn [{:keys [branch]}]
                                   (let [r (first @replies)]
                                     (swap! replies #(vec (rest %)))
                                     {:branch branch :result r
                                      :category :failure :progress? false}))]
      (let [call {:name "shell" :args {:command "cat core.clj"}}
            b0 (assoc (state/new-branch {:id "B1" :problem "p"}) :task {:id "t"})
            {b1 :branch} (aloop/tool-step {} b0 1 call)
            {r2 :result} (aloop/tool-step {} b1 2 call)]
        (is (str/includes? (str (:result r2)) "failed before, differently"))
        (is (str/includes? (str (:result r2)) "no such file")
            "the previous failure's text rides along")))))

(deftest an-identical-failure-still-gets-the-repeat-message-not-both
  (with-redefs [tools/run-tool (fn [{:keys [branch]}]
                                 {:branch branch :result "boom"
                                  :category :failure :progress? false})]
    (let [call {:name "shell" :args {:command "make x"}}
          b0 (assoc (state/new-branch {:id "B1" :problem "p"}) :task {:id "t"})
          {b1 :branch} (aloop/tool-step {} b0 1 call)
          {r2 :result} (aloop/tool-step {} b1 2 call)]
      (is (str/includes? (str (:result r2)) "failed this exact way"))
      (is (not (str/includes? (str (:result r2)) "failed before, differently"))
          "the identical repeat is repeating-failure?'s business alone"))))
