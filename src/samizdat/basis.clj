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

(ns samizdat.basis
  "WHAT A CLAIM RESTS ON, checked (karamazov-0e2c.12). After sdiff's
  decorate.clj (semantic-namespace/diff, MIT): a derived fact names the tool
  it came from, an inferred one names its author and the facts it rests on,
  and a reference that does not resolve is REPORTED, never dropped
  (validate-ref).

  Here a basis is a list of references into the run's own record:

      t12        this branch's turn 12          B2:t7   branch B2's turn 7
      a#3 / p#3  an artifact of this run        s#2     a shared one
      sz-…       a task                         k-…     a memory

  `unresolved` says which do not resolve and why. A memory or a directive
  that cites a basis is refused when any of it does not resolve — a claim
  about the world held to what the record says happened (samizdat.claims has
  the karamazov-ko5b story of one that was not)."
  (:require [clojure.string]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]
            [samizdat.store.knowledge :as knowledge]
            [samizdat.store.tasks :as tasks]))

(defn- check
  "Why reference `r` does not resolve for the branch `ctx` names, as a
  keyword, or nil when it does."
  [{:keys [conn run-id branch]} r]
  (let [r (str r)]
    (if-let [[_ bid n] (re-matches #"(?:([A-Za-z0-9._~-]+):)?t(\d+)" r)]
      (when-not (journal/branch-turn conn run-id (or bid (:id branch)) (parse-long n))
        :no-such-turn)
      (if-let [[_ space n] (re-matches #"([aps])#(\d+)" r)]
        (when-not (if (= "s" space)
                    (journal/shared-artifact-by-id conn run-id (parse-long n))
                    (journal/artifact-by-id conn run-id (parse-long n)))
          :no-such-artifact)
        (cond
          (re-matches #"sz-[0-9a-f]+" r) (when-not (tasks/get-task conn r) :no-such-task)
          (re-matches #"k-[0-9a-f]+" r) (when-not (knowledge/get-by-id conn r) :no-such-memory)
          :else :not-a-reference)))))

(defn unresolved
  "The references in `refs` that do not resolve, as [{:ref :why}], in order."
  [ctx refs]
  (vec (for [r refs
             :let [why (check ctx r)]
             :when why]
         {:ref (str r) :why why})))

(defn refs
  "A tool argument as a list of references: a vector, or one string of them
  separated by commas or whitespace."
  [v]
  (vec (remove #(= "" %)
               (cond (sequential? v) (map str v)
                     (some? v) (clojure.string/split (str v) #"[\s,]+")
                     :else []))))

(defn refusal
  "The refusal text for `bad` (from `unresolved`)."
  [bad]
  (prompt/render "basis-unresolved"
                 {:refs (mapv (fn [{:keys [ref why]}]
                                {:ref ref :turn (= :no-such-turn why) :artifact (= :no-such-artifact why)
                                 :task (= :no-such-task why) :memory (= :no-such-memory why)
                                 :other (= :not-a-reference why)})
                              bad)}))
