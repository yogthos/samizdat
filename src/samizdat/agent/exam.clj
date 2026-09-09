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

(ns samizdat.agent.exam
  "Whether a write to a test file took assertions OUT of the exam.

  A run that cannot pass its tests can always pass them by changing them, and
  the change looks like ordinary work: run 170f4ec9 replaced four contract
  assertions with a bare call under a docstring explaining that the contract
  would come later, and shipped green. Nothing in the harness noticed.

  MEASURED BEFORE BUILT (karamazov-fgsb), over every run database on the
  machine: 22 runs, 190 writes to test files. Counting assertions only, the
  detector flags 6 events, 5 landed, 3 of them real — the four-assertion gutting
  above, an entire `blank-titles` deftest deleted as collateral in an edit that
  was repairing a different test, and one assertion reduced to its bare call.
  The two false alarms were a deduplicated line and two assertions replaced by
  one correct one; both were drops of exactly ONE, and both severe cases were
  drops of two or more. Hence `:min-drop`, and hence the default of 2: on that
  corpus it caught both severe cases with no false alarm, and a warning that
  cries wolf teaches a supervisor to skip the real one.

  DETECTION, NOT PREVENTION, by the same reasoning that settled M10 — and here
  with a number behind it: 100% precision over three true positives is not a
  mandate to start refusing writes, and the rule already misses a single-
  assertion deletion. `:refuse` exists in the policy so the decision can be
  revisited when more runs have accumulated; it is not the default.

  Pure. The caller supplies the two texts; this namespace never reads a file
  and never writes a journal."
  (:require [clojure.string :as str]
            [samizdat.lexicon :as lexicon]))

(defn policy
  "The exam policy, gates.edn :exam-ratchet."
  []
  (lexicon/policy :exam-ratchet))

(defn test-path?
  "Whether `path` names part of the exam, by the vocabulary in
  wordlists.edn :exam :test-paths — regex fragments, because what marks a test
  file is a project's convention (`test/`, `_test.clj`, `test_x.py`, `.spec.ts`)
  and not something to compile in."
  ([path] (test-path? path (:test-paths (lexicon/wordlist :exam))))
  ([path patterns]
   (boolean (when-let [p (some-> path str not-empty)]
              (some #(re-find (re-pattern %) p) patterns)))))

(defn assertions
  "How many assertions `text` contains, by the vocabulary in
  wordlists.edn :exam :assertion-forms.

  Assertions ONLY, and deliberately not `deftest`: a test moved from one file
  to another loses no assertion, and counting the form that names a test made
  a helper extraction look like a gutting — which is the false positive that
  wasted the first pass of this measurement."
  ([text] (assertions text (:assertion-forms (lexicon/wordlist :exam))))
  ([text patterns]
   (let [s (str text)]
     (reduce + 0 (map #(count (re-seq (re-pattern %) s)) patterns)))))

(defn weakening
  "What this write does to the exam, or nil when it does nothing to it.

  `{:before n :after n :drop n}` when the file held more assertions before the
  write than after, and the drop reaches `:min-drop`. nil for a write that adds
  assertions, leaves them alone, or drops fewer than the policy cares about."
  ([path before after] (weakening path before after (policy)))
  ([path before after p]
   (when (and (test-path? path) (not= :off (:mode p)))
     (let [b (assertions before)
           a (assertions after)
           drop (- b a)]
       (when (>= drop (max 1 (:min-drop p)))
         {:before b :after a :drop drop})))))

(defn refuse?
  "Whether the policy says a weakening write does not land. `:detect` records
  and allows; `:refuse` records and declines."
  ([] (refuse? (policy)))
  ([p] (= :refuse (:mode p))))

(defn describe
  "One line for the record: what the file lost."
  [path {:keys [before after drop]}]
  (str (str/trim (str path)) ": " before " assertion(s) before, " after " after"
       " (" drop " removed)"))
