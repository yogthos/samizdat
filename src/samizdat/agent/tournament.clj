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

(ns samizdat.agent.tournament
  "The probabilistic pivot tournament (karamazov-fut), ported from
  llm-as-a-verifier's pivot_tournament.py: rank N candidates by pairwise
  comparison at linear-in-N cost.

  Why pairwise at all: a single absolute critic score asks the judge to hold
  a calibration it does not have; a comparison only asks which of two is
  better, and the measured lift (best-of-5 self-verification 78.7% -> 88.0%
  against a 96.6% oracle) comes with the SAME model judging its own work.

  Why this shape: a full round-robin costs C(N,2) judge calls. The tournament
  spends N + k(N-k) + C(k,2): one RING pass around a random Hamiltonian cycle
  — every candidate judged exactly once in slot A and once in slot B, so
  positional bias cancels around the ring — then the top-k by mean preference
  become PIVOTS, and every remaining candidate is judged against every pivot.

  Mechanism only, and pure: the comparator is injected (an LLM judge in
  production, a rigged Bradley-Terry in tests), the seed comes from policy,
  and nothing here knows what a candidate is. Soft wins, Bradley-Terry style:
  a comparison returns p(a beats b) in [0,1] and BOTH candidates book their
  share, so an uncertain judge moves the ranking less than a confident one.

  Randomness note: the shuffle is a seeded LCG, not Math/random — the caller
  owns the seed, so a re-run or a resume re-derives the identical ring."
  (:refer-clojure :exclude [run]))

;; A classic LCG, kept inside 2^31 by the mod rather than by wraparound —
;; Chez integers are bignums, so shift-based generators would grow without
;; bound instead of wrapping the way they do on 64-bit words.
(def ^:private lcg-a 1103515245)
(def ^:private lcg-c 12345)
(def ^:private lcg-m 2147483648)

(defn- lcg-next [s]
  (mod (+ (* lcg-a s) lcg-c) lcg-m))

(defn shuffle-seeded
  "Fisher-Yates over `coll`, driven by the seed. Same seed, same order."
  [coll seed]
  (let [v (vec coll)
        n (count v)]
    (loop [v v
           i (dec n)
           s (lcg-next (long seed))]
      (if (pos? i)
        (let [j (mod s (inc i))]
          (recur (assoc v i (v j) j (v i)) (dec i) (lcg-next s)))
        v))))

(defn ring-pairs
  "The directed edges of a random Hamiltonian cycle over n candidates:
  n pairs, every candidate exactly once as the first element and once as the
  second. This is the positional-bias control — a judge that favours slot A
  favours every candidate equally around the ring."
  [n seed]
  (let [p (shuffle-seeded (range n) seed)]
    (mapv (fn [i] [(p i) (p (mod (inc i) n))]) (range n))))

(defn- book
  "Book one comparison's soft win into the {index {:w :c}} tally."
  [acc a b p]
  (let [p (-> (double p) (max 0.0) (min 1.0))]
    (-> acc
        (update-in [a :w] (fnil + 0.0) p)
        (update-in [a :c] (fnil inc 0))
        (update-in [b :w] (fnil + 0.0) (- 1.0 p))
        (update-in [b :c] (fnil inc 0)))))

(defn- mean [{:keys [w c]}]
  (if (pos? (or c 0)) (/ (double w) c) 0.0))

(defn run
  "Rank n candidates with the injected comparator.

  opts: :n candidates, :compare (fn [a b] -> p(a beats b) in [0,1]),
  :pivots k, :seed for the ring. Returns [{:index :score}] best-first,
  score = mean preference w/c; ties break toward the lower index so the
  result is total and reproducible."
  [{:keys [n compare pivots seed]}]
  (cond
    (zero? n) []
    (= 1 n) [{:index 0 :score 1.0}]
    :else
    (let [ring (ring-pairs n seed)
          after-ring (reduce (fn [acc [a b]] (book acc a b (compare a b)))
                             {} ring)
          k (min (long pivots) n)
          by-mean (sort-by (fn [i] [(- (mean (after-ring i))) i]) (range n))
          pivot-set (set (take k by-mean))
          non-pivots (remove pivot-set (range n))
          pivot-pairs (concat
                       ;; every remaining candidate against every pivot
                       (for [np non-pivots, p pivot-set] [np p])
                       ;; and the pivots against each other, each pair once
                       (let [ps (sort pivot-set)]
                         (for [i (range (count ps))
                               j (range (inc i) (count ps))]
                           [(nth ps i) (nth ps j)])))
          tally (reduce (fn [acc [a b]] (book acc a b (compare a b)))
                        after-ring pivot-pairs)]
      (->> (range n)
           (map (fn [i] {:index i :score (mean (tally i))}))
           (sort-by (fn [{:keys [index score]}] [(- score) index]))
           vec))))
