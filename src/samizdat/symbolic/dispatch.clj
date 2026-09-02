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

(ns samizdat.symbolic.dispatch
  "A dispatch table as patterns. MECHANISM: the adapter between mycelium's
  ordered [[label pred] ...] and the pattern engine, and the analysis that
  is the reason to want patterns there at all.

  AN ENTRY is [label spec] or [label pattern guard]. `spec` is either a
  pattern — a map, the hole `_`, or a bare ?var — or what it always was: a
  `(fn [d] ...)` form maestro evals at compile time, or a function. Both are
  accepted so a manifest migrates one table at a time. The data a dispatch
  sees is always a map, so a pattern that is not one — a keyword, a vector,
  `true` — is refused rather than compiled into a branch that never fires.

  THE ANALYSIS runs over the order that RUNS — :default last, the one
  reordering compile-edges applies — over every pair of pattern entries:
    shadowed         an earlier unguarded pattern subsumes a later one, so
                     the later branch can never fire. Refused.
    order-dependent  both can match the same data and neither is more
                     specific, so the list order is the only thing deciding.
                     Reported, not refused: the loop manifest's :parse table
                     has one on purpose.
  A guarded general pattern before a specific one is guard-then-fallthrough
  and is neither. Two entries to the same label are never order-dependent,
  the data going the same way whichever fires. Guards are not analysed, and
  entries that are forms or functions are opaque: they take part in neither
  check, so a table that mixes forms and patterns is only partly checked."
  (:require [samizdat.symbolic :as sym]))

(defn pattern-entry?
  "Is `spec` a pattern, as opposed to a form or a function?"
  [spec]
  (not (or (fn? spec)
           (seq? spec)
           (and (symbol? spec)
                (not (sym/pvar? spec))
                (not (sym/wildcard? spec))))))

(defn- relabel
  "The engine's refusal, carrying the branch it was about."
  [e label]
  (ex-info (str "branch " label ": " (ex-message e))
           (assoc (ex-data e) :label label)
           e))

(defn compile-entry
  "[label spec] or [label pattern guard] -> [label pred]. A form or a
  function passes through untouched; a pattern compiles to a predicate over
  the data map. Throws, naming the branch, on anything the engine refuses."
  [[label spec guard :as entry]]
  (let [guarded? (> (count entry) 2)]
    (if-not (pattern-entry? spec)
      (if guarded?
        (throw (ex-info (str "branch " label ": a guard needs a pattern, not a form")
                        {:error :guard-without-pattern :label label :entry entry}))
        [label spec])
      (do
        (when-not (or (map? spec) (sym/wildcard? spec) (sym/pvar? spec))
          (throw (ex-info (str "branch " label ": pattern " (pr-str spec)
                               " is not a map, and the data always is; _ matches any")
                          {:error :dispatch-pattern-not-a-map
                           :label label :pattern spec :instead '_})))
        (let [r (try (sym/rule (cond-> {:name label :when spec}
                                 guarded? (assoc :if guard)))
                     (catch clojure.lang.ExceptionInfo e
                       (throw (relabel e label))))]
          [label (fn [d] (some? (sym/match r d)))])))))

(defn effective-order
  "The order a table runs in: as written, with :default moved last. The one
  reordering compile-edges applies, shared so the analysis sees what runs."
  [dispatch-vec]
  (let [{defaults true others false} (group-by #(= :default (first %)) dispatch-vec)]
    (vec (concat others defaults))))

(defn analyse
  "{:shadowed [{:label :pattern :by :by-pattern} ...]
    :order-dependent [{:labels [a b] :patterns [pa pb]} ...]}
  over the pattern entries of `dispatch-vec`, in the order that runs."
  [dispatch-vec]
  (let [entries (into []
                      (comp (map-indexed
                             (fn [i [label spec guard :as e]]
                               {:i i :label label :spec spec
                                :guard (when (> (count e) 2) guard)
                                :pattern? (pattern-entry? spec)}))
                            (filter :pattern?))
                      (effective-order dispatch-vec))]
    (reduce
     (fn [acc [a b]]
       (cond
         (sym/subsumes? (:spec a) (:spec b))
         (if (nil? (:guard a))
           (update acc :shadowed conj {:label (:label b) :pattern (:spec b)
                                       :by (:label a) :by-pattern (:spec a)})
           acc)

         (or (= (:label a) (:label b))
             (sym/subsumes? (:spec b) (:spec a))
             (not (sym/overlap? (:spec a) (:spec b))))
         acc

         :else
         (update acc :order-dependent conj {:labels [(:label a) (:label b)]
                                            :patterns [(:spec a) (:spec b)]})))
     {:shadowed [] :order-dependent []}
     (for [a entries b entries :when (< (:i a) (:i b))] [a b]))))

(defn check!
  "Compile every entry, so a malformed one is refused here naming the cell,
  then refuse a shadowed branch. Returns the analysis."
  [cell-name dispatch-vec]
  (doseq [entry dispatch-vec]
    (try (compile-entry entry)
         (catch clojure.lang.ExceptionInfo e
           (throw (ex-info (str "dispatch " cell-name ", " (ex-message e))
                           (assoc (ex-data e) :cell cell-name)
                           e)))))
  (let [analysis (analyse dispatch-vec)]
    (when-let [{:keys [label pattern by by-pattern]} (first (:shadowed analysis))]
      (throw (ex-info (str "dispatch " cell-name ", branch " label " "
                           (pr-str pattern) " can never fire: " by " "
                           (pr-str by-pattern) " before it matches everything it matches")
                      {:error :shadowed-dispatch :cell cell-name
                       :label label :pattern pattern
                       :by by :by-pattern by-pattern})))
    analysis))

(defn report
  "Every order-dependent pair in a whole :dispatches map, each with its cell."
  [dispatches-map]
  (into []
        (for [[cell table] dispatches-map
              pair (:order-dependent (analyse table))]
          (assoc pair :cell cell))))

(def ^:private refusals
  "The :error keys this layer and the engine raise — what a caller rendering
  a refusal keys the pattern rules to."
  #{:shadowed-dispatch :dispatch-pattern-not-a-map :guard-without-pattern
    :var-in-key-position :var-in-literal-collection :unknown-guard
    :guard-arity :unbound-var :malformed-guard :malformed-rule :missing-when})

(defn refusal
  "The ex-data of a pattern refusal, or nil when `e` is anything else."
  [e]
  (let [d (ex-data e)]
    (when (contains? refusals (:error d)) d)))
