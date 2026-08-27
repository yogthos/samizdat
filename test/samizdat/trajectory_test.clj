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

(ns samizdat.trajectory-test
  "Continuous trajectory scoring (karamazov-fut, from llm-as-a-verifier's
  ProgressTracker): a letter-scale judge over steps-so-far, K repeats
  averaged. The judge is injected, so every behaviour here is deterministic;
  the flat-low-curve-means-looping claim is what the Qwen campaign validates
  empirically before any decision is wired to the score."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.trajectory :as trajectory]))

;; --- letter parsing ---------------------------------------------------------

(deftest letters-parse-to-the-unit-interval
  (testing "the full scale, A worst to T best"
    (is (= 0.0 (trajectory/parse-letter "A")))
    (is (= 1.0 (trajectory/parse-letter "T")))
    (is (< 0.47 (trajectory/parse-letter "K") 0.54)))
  (testing "the letter is read from the LAST line, so judge prose above it
            cannot collide with the scale"
    (is (= 1.0 (trajectory/parse-letter
                "The Branch Advanced Toward The Goal.\n\nT")))
    (is (= 0.0 (trajectory/parse-letter "Reasoning: no progress\nScore: A"))))
  (testing "unparseable replies are nil, never a fabricated number"
    (is (nil? (trajectory/parse-letter "no letter here 42")))
    (is (nil? (trajectory/parse-letter "")))
    (is (nil? (trajectory/parse-letter nil)))
    (is (nil? (trajectory/parse-letter "U"))
        "U is off the A-T scale")))

;; --- the steps digest -------------------------------------------------------

(deftest the-digest-sees-only-steps-so-far
  (let [rows [{:turn 1 :tool_name "read_file" :args "{\"path\": \"a.clj\"}"
               :category "success" :result (apply str (repeat 500 "x"))}
              {:turn 2 :tool_name "write_file" :args "{\"path\": \"a.clj\"}"
               :category "success" :result "wrote"}
              {:turn 3 :tool_name "shell" :args "{\"command\": \"jolt -M:test\"}"
               :category "failure" :result "1 failure"}]
        d (trajectory/steps-digest (take 2 rows))]
    (is (str/includes? d "read_file"))
    (is (str/includes? d "write_file"))
    (is (not (str/includes? d "shell")) "the judge cannot peek ahead")
    (is (< (count d) 600) "long results are truncated to a snippet")))

;; --- scoring a trajectory ---------------------------------------------------

(defn- scripted-ask
  "An injected judge: pops replies from the script in order."
  [replies]
  (let [q (atom (vec replies))]
    (fn [_prompt]
      (let [r (first @q)]
        (swap! q #(vec (rest %)))
        r))))

(deftest scores-average-k-repeats-and-skip-unparseable
  (let [rows [{:turn 1 :tool_name "shell" :args "{}" :category "success"
               :result "ok"}]
        ;; K=4: T (1.0), A (0.0), garbage (skipped), K (~0.526)
        ask (scripted-ask ["T" "A" "nonsense" "K"])
        scores (trajectory/score-rows ask rows
                                      {:problem "p" :repeats 4 :stride 1})]
    (is (= 1 (count scores)))
    (is (= 1 (:turn (first scores))))
    (let [s (:score (first scores))]
      (is (< 0.50 s 0.52) "mean of 1.0, 0.0, 10/19"))))

(deftest all-unparseable-scores-nil-not-zero
  ;; nil and 0.0 mean opposite things: 0.0 is a judged 'no progress',
  ;; nil is 'the judge said nothing usable'. Conflating them would let a
  ;; broken judge abandon healthy branches.
  (let [rows [{:turn 1 :tool_name "shell" :args "{}" :category "success"
               :result "ok"}]
        scores (trajectory/score-rows (scripted-ask ["x" "y"]) rows
                                      {:problem "p" :repeats 2 :stride 1})]
    (is (nil? (:score (first scores))))))

(deftest the-stride-scores-every-nth-step
  (let [rows (mapv (fn [i] {:turn i :tool_name "shell" :args "{}"
                            :category "success" :result "ok"})
                   (range 1 7))
        asks (atom 0)
        ask (fn [_] (swap! asks inc) "K")
        scores (trajectory/score-rows ask rows
                                      {:problem "p" :repeats 1 :stride 3})]
    (is (= [3 6] (mapv :turn scores))
        "every 3rd step is scored, judged on the steps up to it")
    (is (= 2 @asks))))

(deftest the-policy-is-discoverable-in-gates-edn
  (let [p (gates/trajectory-policy)]
    (is (pos? (:repeats p)))
    (is (pos? (:stride p)))
    (is (number? (:abandon-below p))
        "the eventual decision threshold ships as data even before any
         decision is wired to it")
    (is (seq (:criteria p)) "2-4 narrow criteria, each saying where to look")
    (is (<= 2 (count (:criteria p)) 4))))
