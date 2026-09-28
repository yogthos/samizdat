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

(ns samizdat.llm.repetition
  "A REPLY THAT IS REPEATING ITSELF (karamazov-o4wm.5).

  A model that degenerates into emitting the same passage over and over
  produces tokens steadily and never reaches a call, so nothing keyed on
  silence trips and the reply ends at the token cap. The loop read that as
  `truncated before any tool call` and retried at a doubled budget — which,
  for a loop, buys a loop twice as long. Floatboat runs a SimHash window
  detector on the stream for this; samizdat does not stream, so the check
  is over the finished reply, and its one job is to keep the retry honest
  and word the complaint right.

  Exact windows, not similarity hashing: a degenerate repeat is verbatim,
  and an exact match cannot flag two paragraphs that merely resemble each
  other. Regular spacing is required (floatboat's periodicity check), so a
  passage that recurs because the model quoted it twice in an argument does
  not count. Pure; every number is gates.edn :repetition."
  (:require [clojure.string :as str]))

(defn periodic
  "The strongest verbatim repeat in `text`: `{:repeats n :period p}` when
  some `window`-character slice recurs at least `min-repeats` times at gaps
  within `max-gap-variance` of their mean and a period of at least
  `min-period`, else nil. Slices are taken every `step` characters; a step
  of 1 sees every period, a coarser one only periods it divides."
  [text {:keys [window step min-repeats min-period max-gap-variance
               short-min-period short-min-repeats]}]
  (let [text (str text)
        n (count text)]
    (when (and (pos? (or window 0)) (pos? (or step 0))
               (>= n (* window (or min-repeats 1))))
      (let [positions (reduce (fn [m i]
                                (let [w (subs text i (+ i window))]
                                  (if (str/blank? w)
                                    m
                                    (update m w (fnil conj []) i))))
                              {}
                              (range 0 (inc (- n window)) step))
            regular? (fn [ps]
                       (let [gaps (map - (rest ps) ps)
                             mean (/ (double (reduce + gaps)) (count gaps))]
                         (and (pos? mean)
                              (every? #(<= (Math/abs (- % mean))
                                           (* (double max-gap-variance) mean))
                                      gaps))))]
        (->> positions
             (keep (fn [[_ ps]]
                     (when (and (>= (count ps) min-repeats) (regular? ps))
                       (let [period (long (/ (- (last ps) (first ps))
                                             (dec (count ps))))]
                         ;; A period under :min-period is a RUN — a line of
                         ;; dashes, a column of zeros — which repeats
                         ;; perfectly and is not a model repeating itself.
                         ;; Unless it is a short phrase repeated far past
                         ;; the floor: GLM wrote "action":"close", (17
                         ;; characters) some 900 times (karamazov-na2k.19).
                         (when (or (>= period (or min-period 1))
                                   (and short-min-period short-min-repeats
                                        (>= period short-min-period)
                                        (>= (count ps) short-min-repeats)))
                           {:repeats (count ps) :period period})))))
             (sort-by (juxt (comp - :repeats) :period))
             first)))))
