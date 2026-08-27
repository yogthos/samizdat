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

(ns samizdat.tournament-test
  "The pivot tournament (karamazov-fut, from llm-as-a-verifier's
  pivot_tournament.py): best-of-N curation by pairwise comparison at
  linear-in-N cost. These tests drive it with a rigged comparator — real
  quality values behind a Bradley-Terry preference — so 'the best candidate
  wins' is checkable deterministically, per the n=1 measurability rule."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.agent.tournament :as tournament]))

(defn- bt
  "A Bradley-Terry comparator over hidden quality scores: p(a beats b)."
  [quals]
  (fn [a b]
    (let [d (- (double (nth quals a)) (double (nth quals b)))]
      (/ 1.0 (+ 1.0 (Math/exp (- d)))))))

;; --- the seeded shuffle -----------------------------------------------------

(deftest the-shuffle-is-deterministic-and-a-permutation
  (let [s1 (tournament/shuffle-seeded (range 10) 42)
        s2 (tournament/shuffle-seeded (range 10) 42)
        s3 (tournament/shuffle-seeded (range 10) 7)]
    (is (= s1 s2) "same seed, same order — a resumed run re-derives the ring")
    (is (= (set (range 10)) (set s1)) "a permutation, nothing lost or invented")
    (is (not= s1 s3) "a different seed gives a different ring")))

;; --- the ring ---------------------------------------------------------------

(deftest the-ring-visits-every-candidate-once-per-slot
  ;; The random Hamiltonian cycle is the positional-bias control: every
  ;; candidate appears exactly once as the first element and once as the
  ;; second, so slot preference cancels around the ring.
  (let [pairs (tournament/ring-pairs 6 42)]
    (is (= 6 (count pairs)))
    (is (= (set (range 6)) (set (map first pairs))))
    (is (= (set (range 6)) (set (map second pairs))))
    (is (every? (fn [[a b]] (not= a b)) pairs))))

;; --- the tournament ---------------------------------------------------------

(deftest the-best-candidate-wins
  (let [quals [0.1 0.9 0.3 0.2 0.5]
        ranked (tournament/run {:n 5 :compare (bt quals)
                                :pivots 2 :seed 42})]
    (is (= 1 (:index (first ranked)))
        "the highest hidden quality ranks first")
    (is (= 5 (count ranked)))
    (is (apply >= (map :score ranked)) "ranked best-first by mean preference")))

(deftest comparison-count-is-linear-not-quadratic
  ;; N + k(N-k) + C(k,2), the whole point over round-robin's O(N^2).
  (let [calls (atom 0)
        n 8 k 2
        counting (fn [a b] (swap! calls inc) ((bt (repeat n 0.5)) a b))]
    (tournament/run {:n n :compare counting :pivots k :seed 42})
    (is (= (+ n (* k (- n k)) (/ (* k (dec k)) 2)) @calls))))

(deftest small-fields-degenerate-gracefully
  (is (= [] (tournament/run {:n 0 :compare (fn [_ _] 0.5) :pivots 2 :seed 1})))
  (is (= [{:index 0 :score 1.0}]
         (tournament/run {:n 1 :compare (fn [_ _] 0.5) :pivots 2 :seed 1}))
      "a single candidate wins without a single comparison")
  (let [ranked (tournament/run {:n 2 :compare (bt [0.1 0.9])
                                :pivots 2 :seed 1})]
    (is (= 2 (count ranked)))
    (is (= 1 (:index (first ranked))))))

(deftest the-comparator-sees-both-orders
  ;; Both directions of at least one pair get asked across ring + pivot
  ;; rounds, which is what lets a position-biased judge cancel out.
  (let [seen (atom #{})
        n 4
        f (fn [a b] (swap! seen conj [a b]) 0.5)]
    (tournament/run {:n n :compare f :pivots 2 :seed 3})
    (let [pairs @seen
          both (filter (fn [[a b]] (contains? pairs [b a])) pairs)]
      (is (seq both) "at least one pair was judged in both A/B orders"))))

(deftest determinism-under-a-fixed-seed
  (let [quals [0.4 0.6 0.2 0.8 0.5 0.1]
        r1 (tournament/run {:n 6 :compare (bt quals) :pivots 2 :seed 9})
        r2 (tournament/run {:n 6 :compare (bt quals) :pivots 2 :seed 9})]
    (is (= r1 r2))))
