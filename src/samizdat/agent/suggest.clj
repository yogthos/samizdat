;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Ported from dirge's src/agent/agent_loop/suggest.rs. The transposition-aware
;; distance, the quarter-of-the-longer-name budget and the ambiguous-tie
;; refusal are all that design, including the measurement behind the budget.

(ns samizdat.agent.suggest
  "DID YOU MEAN — the nearest name, when a name is a typo of one we have.

  TYPOS ONLY, and that boundary is the whole of what makes this safe. A name
  that is a different WORD for the right tool is not a typo and cannot be
  found by measuring characters: `shell` is not a mistyped `bash`. Measuring
  anyway is how a suggestion engine sends a model that wanted a shell off to
  something unrelated, with no hedge in the message, and then the flailing
  that follows gets scored as the model being out of its depth."
  (:require [clojure.string :as str]))

(defn edit-distance
  "Damerau-Levenshtein: insert, delete, substitute, and TRANSPOSE.

  The transposition row is what earns the tight budget below. Plain
  Levenshtein charges 2 for `raed`/`read` — the single most common way a name
  gets mistyped — so a budget loose enough to catch it is loose enough to
  match unrelated names. Paying for the extra row buys both."
  [a b]
  (let [a (vec a) b (vec b)
        n (count a) m (count b)]
    (cond
      (zero? n) m
      (zero? m) n
      :else
      (loop [i 0
             prev2 (vec (repeat (inc m) 0))
             prev (vec (range (inc m)))]
        (if (= i n)
          (nth prev m)
          (let [cur (loop [j 0 cur (assoc (vec (repeat (inc m) 0)) 0 (inc i))]
                      (if (= j m)
                        cur
                        (let [cost (if (= (nth a i) (nth b j)) 0 1)
                              best (min (inc (nth prev (inc j)))
                                        (inc (nth cur j))
                                        (+ (nth prev j) cost))
                              best (if (and (pos? i) (pos? j)
                                            (= (nth a i) (nth b (dec j)))
                                            (= (nth a (dec i)) (nth b j)))
                                     (min best (inc (nth prev2 (dec j))))
                                     best)]
                          (recur (inc j) (assoc cur (inc j) best)))))]
            (recur (inc i) prev cur)))))))

(defn typo-budget
  "Edits allowed between two names before the match stops being a typo: a
  QUARTER of the longer name, rounded down.

  So a three-character name tolerates nothing — one edit on `ls` is a
  different word, not a slip — and an eleven-character one tolerates two.

  Measured, not chosen by eye. The budget was `len/2` capped at 3, which
  against dirge's own tool names produced confident nonsense: `exec` → `spec`,
  `shell` → `skill`, `open` → `spec`, `ls` → `lsp`, `ask` → `task`, `search` →
  `websearch`. Six of eleven resolved guesses pointed at a tool with nothing
  to do with the one asked for. A quarter, with the transposition-aware
  distance, took that sample from eight wrong suggestions to one while keeping
  every real typo."
  [target candidate]
  (quot (max (count (str target)) (count (str candidate))) 4))

(defn closest
  "The single nearest candidate to `target`, or nil.

  Three ways to answer nil, and each is a decision:

  - nothing is within the typo budget — the name is not a slip
  - the runner-up is EQUALLY close — a tie is a toss-up, and guessing between
    two names is worse than saying nothing, because the model acts on it
  - the only match is the target itself

  Case-insensitive when comparing, but a wrong-CASE name is still worth
  suggesting: it lowercases to distance zero and dispatch matches case
  sensitively, so `Read_File` is a real miss with a real answer."
  [target candidates]
  (let [t (str/lower-case (str target))
        scored (map (fn [c] [(edit-distance t (str/lower-case (str c))) (str c)])
                    candidates)]
    (when (seq scored)
      (let [sorted (sort-by first scored)
            [d name] (first sorted)
            runner-up (second sorted)]
        (when (and (<= d (typo-budget t name))
                   (not= name (str target))
                   (not (and runner-up (= d (first runner-up)))))
          name)))))
