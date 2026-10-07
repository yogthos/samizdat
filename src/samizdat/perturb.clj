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

(ns samizdat.perturb
  "Seeded perturbations of a task's wording (karamazov-0e2c.11, after
  iFixAi's harness/adversarial_mutator.py): the same task said a little
  differently, so an arena result can be told from a fluke of phrasing.

  ONLY WHAT CANNOT CHANGE THE TASK. iFixAi flips case and swaps synonyms; a
  coding task names identifiers, paths and commands, and either would change
  them. So: punctuation jitter at sentence ends and filler words between
  sentences, nothing inside a `code span`, and every word of the task left in
  order. MECHANISM ONLY, pure: the arena decides when to use it."
  (:require [clojure.string :as str]))

(def ^:private fillers
  ["Please." "Thanks." "Note:" "Also," "In short:" "To be clear:"])

(defn- rng [seed] (java.util.Random. (long seed)))

(defn- prose-and-code
  "`s` as alternating [:prose text] / [:code text] pieces; a code span is
  backtick-delimited and never touched."
  [s]
  (map (fn [[_ code prose]] (if code [:code code] [:prose prose]))
       (re-seq #"(`[^`]*`)|([^`]+)" s)))

(defn- jitter
  "One prose piece, perturbed: a sentence end gains or loses a space or
  doubles its stop, and a filler may open a sentence."
  [^java.util.Random r text]
  (str/replace text #"([.!?])(\s+)"
               (fn [[_ stop ws]]
                 (case (.nextInt r 4)
                   0 (str stop ws)
                   1 (str stop " " ws)
                   2 (str stop ws (nth fillers (.nextInt r (count fillers))) " ")
                   3 (str stop (if (str/includes? ws "\n") ws "  "))))))

(defn variant
  "One perturbation of `text`, seeded by `seed`."
  [text seed]
  (let [r (rng seed)]
    (loop [attempt 0]
      (let [v (apply str (for [[kind t] (prose-and-code text)]
                           (if (= :code kind) t (jitter r t))))]
        ;; A text with nothing to jitter comes back as it was; one with
        ;; somewhere to jitter is tried until it differs.
        (if (or (not= v text) (> attempt 8)) v (recur (inc attempt)))))))

(defn variants
  "`n` perturbations of `text`, seeded by `seed`: the same seed, the same
  variants."
  [text seed n]
  (let [r (rng seed)]
    (vec (repeatedly n #(variant text (.nextLong r))))))
