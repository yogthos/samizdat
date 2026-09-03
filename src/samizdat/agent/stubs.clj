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

(ns samizdat.agent.stubs
  "The delegation boundary, as code rather than prose.

  A parent that splits a task writes the STUBS the pieces must fill — the
  function signatures, with docstrings saying what each one owes — and each
  child implements the stubs it was given. That is what makes the two layers
  independent: the parent reasons about the API and how the pieces compose,
  the child reasons about the implementation behind one signature, and the
  only thing they share is the boundary itself.

  It is also what makes the boundary CHECKABLE, which prose never was. The
  harness can ask two questions and get answers rather than judgements:
  did the parent actually write the stub it delegated (`definition`), and did
  the child actually implement it (`filled?`). Both are read off the tree.

  Mechanism only — it knows nothing about when a split happens or what to do
  when a stub is missing. The words for a refusal live in prompts; the
  decision lives in cells/decompose.clj."
  (:require [clojure.string :as str]))

(def ^:private def-forms
  "The top-level forms that introduce a name. Deliberately a set of symbols
  rather than a `str/starts-with? \"def\"` test: `default-parts` is not a
  definition, and a rule that reads names character-wise would say it is."
  '#{def defn defn- defmacro defmulti defmethod defprotocol definterface
     defrecord deftype defonce})

(defn- def-name
  "The name a top-level form defines, as a string, or nil if it defines none.
  `(defmethod f :k [..])` defines no new name — it extends one — so it is not
  a contract a child can be handed."
  [form]
  (when (and (seq? form) (symbol? (first form)))
    (let [head (first form)
          nm (second form)]
      (when (and (contains? def-forms head)
                 (not= 'defmethod head)
                 (symbol? nm))
        (str nm)))))

(defn definitions
  "Every top-level definition in `source`, as {name form}.

  Reads rather than greps, because the reader is the only thing that knows a
  `defn` inside a string or a comment is not a definition.

  A source that does not read yields {} rather than throwing: a parent writing
  stubs will sometimes write unbalanced ones, and that has to come back as a
  refusal naming the file, not as a reader exception out of the middle of a
  split."
  [source]
  (let [forms (try (read-string {:read-cond :allow} (str "[" source "]"))
                   (catch Throwable _ nil))]
    (into {} (keep (fn [f] (when-let [n (def-name f)] [n f]))) forms)))

(defn definition
  "The form defining `name` in `source`, or nil when nothing does."
  [source name]
  (get (definitions source) (str name)))

(defn- body-of
  "A definition's body forms — everything after the name, the optional
  docstring, the optional attribute map and the argument vector. Approximate
  by design: it drops every leading non-body element rather than parsing each
  `def` variant's grammar, which is enough to answer 'does this do anything'
  and cannot be wrong in the direction that matters (a real body is never
  mistaken for an empty one)."
  [form]
  (->> (drop 2 form)
       (drop-while #(or (string? %) (map? %) (vector? %)))))

(defn stub?
  "Whether `form` is still a stub: a definition whose body does nothing but
  announce that it is unimplemented.

  Three shapes count, because they are the three ways a stub actually gets
  written: no body at all, a body that is exactly `nil`, and a body whose only
  form is a `throw`. The split prompt asks for the `throw` shape; the other
  two are recognised so a parent that wrote one of them is not told its stub
  is already finished.

  `nil` is not a stub. A definition that is ABSENT and one that is EMPTY are
  different facts, and collapsing them would let a child that deleted its
  stub look like a child that never started."
  [form]
  (boolean
   (when (seq? form)
     (let [body (body-of form)]
       (or (empty? body)
           (= [nil] (vec body))
           (and (= 1 (count body))
                (seq? (first body))
                (= 'throw (first (first body)))))))))

(defn filled?
  "Whether the stub `name` in `source` has been implemented: it is still
  there, under the same name, and it is no longer a stub.

  This is a child's half of the contract, and both halves of the conjunction
  are load-bearing. A child that deleted the stub rather than implementing it
  has not delivered — the parent's composition calls that name."
  [source name]
  (let [d (definition source name)]
    (boolean (and d (not (stub? d))))))

(defn unfilled
  "Which of `names` `source` does not have a real implementation for — absent
  and still-hollow together, in the order given.

  One answer rather than two because the caller asks one question: what does
  this piece still owe. A stub never written and a stub deleted instead of
  filled are the same fact from the parent's side — its composition calls that
  name and the name has no body."
  [source names]
  (vec (remove #(filled? source %)
               (remove str/blank? (map str names)))))

(defn missing
  "Which of `names` `source` does not define, in the order given.

  Data, not a sentence: the caller renders the refusal through a prompt."
  [source names]
  (vec (remove #(some? (definition source %))
               (remove str/blank? (map str names)))))
