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

(ns samizdat.symbolic.dispatch-test
  "Dispatch tables as patterns (karamazov-41a.3): the adapter between
  mycelium's ordered [[label pred] ...] and the pattern engine, and the
  analysis a table of closures could never have — which branches are dead,
  and where the order is doing the deciding."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [samizdat.manifests :as manifests]
            [samizdat.symbolic.dispatch :as d]))

(defn- ex-data-of [thunk]
  (try (thunk) nil (catch Exception e (ex-data e))))

;;; ---------------------------------------------------------- compile-entry

(deftest a-pattern-entry-compiles-to-a-predicate-over-the-data-map
  (let [[label pred] (d/compile-entry '[:provider-error {:call {:ok false}}])]
    (is (= :provider-error label))
    (is (true? (pred {:call {:ok false} :turn 3})))
    (is (false? (pred {:call {:ok true}})))
    (is (false? (pred {})))))

(deftest the-fn-form-passes-through-untouched
  ;; BOTH forms accepted, so migration is per-manifest and nothing breaks.
  (let [form '(fn [d] (:x d))]
    (is (= [:x form] (d/compile-entry [:x form]))
        "a form is left for maestro's compile-time eval"))
  (let [f (fn [d] (:x d))]
    (is (identical? f (second (d/compile-entry [:x f])))
        "a function is already compiled")))

(deftest a-guard-is-the-third-element
  (let [[_ pred] (d/compile-entry '[:big {:turn ?t} [:> ?t 10]])]
    (is (true? (pred {:turn 11})))
    (is (false? (pred {:turn 3})))))

(deftest a-guard-on-a-fn-form-is-refused
  (let [data (ex-data-of #(d/compile-entry '[:x (fn [d] true) [:> 1 0]]))]
    (is (= :guard-without-pattern (:error data)))
    (is (= :x (:label data)))))

(deftest a-top-level-pattern-must-be-a-map-or-a-hole
  ;; The data a dispatch sees is always a map, so a scalar at the top could
  ;; never fire. `:_` is the one a reader reaches for first — it looks like a
  ;; wildcard and is a keyword — so the refusal has to hand back `_`.
  (let [data (ex-data-of #(d/compile-entry [:tool :_]))]
    (is (= :dispatch-pattern-not-a-map (:error data)))
    (is (= '_ (:instead data))))
  (is (= :dispatch-pattern-not-a-map
         (:error (ex-data-of #(d/compile-entry [:done :done?])))))
  (is (= :dispatch-pattern-not-a-map
         (:error (ex-data-of #(d/compile-entry [:done true])))))
  (is (= :dispatch-pattern-not-a-map
         (:error (ex-data-of #(d/compile-entry [:done [:a :b]])))))
  (is (fn? (second (d/compile-entry '[:tool _]))))
  (is (fn? (second (d/compile-entry '[:any ?d])))))

(deftest an-engine-refusal-keeps-its-reason-and-names-the-branch
  (let [data (ex-data-of #(d/compile-entry '[:x {:tools #{?t}}]))]
    (is (= :var-in-literal-collection (:error data)))
    (is (= :x (:label data)))))

(deftest the-pattern-form-agrees-with-the-fn-form-it-replaced
  ;; loop.edn's :parse table, both ways, over every shape the parse cell can
  ;; emit under its declared output schema. This is the migration's proof:
  ;; the same data map takes the same edge.
  (let [fns '[[:provider-error (fn [d] (not (:ok (:call d))))]
              [:no-call (fn [d] (let [p (:parsed d)]
                                  (or (nil? p) (= "__parse_error__" (:name p)))))]
              [:tool (fn [d] true)]]
        pats '[[:provider-error {:call {:ok false}}]
               [:no-call {:parsed nil}]
               [:no-call {:parsed {:name "__parse_error__"}}]
               [:tool _]]
        first-label (fn [table d]
                      (some (fn [[label pred]] (when (pred d) label))
                            (map (fn [e] (let [[l p] (d/compile-entry e)]
                                           [l (if (fn? p) p (eval p))]))
                                 table)))]
    (doseq [data [{:call {:ok false}}
                  {:call {:ok true} :parsed nil}
                  {:call {:ok true} :parsed {:name "__parse_error__"}}
                  {:call {:ok true} :parsed {:name "eval" :args {}}}]]
      (is (= (first-label fns data) (first-label pats data))
          (pr-str data)))))

;;; ---------------------------------------------------------------- analyse

(deftest analyse-finds-a-shadowed-branch
  (let [a (d/analyse '[[:any _] [:x {:a 1}]])]
    (is (= [{:label :x :pattern {:a 1} :by :any :by-pattern '_}]
           (:shadowed a)))
    (is (= [] (:order-dependent a)))))

(deftest specific-before-general-is-the-intended-shape
  (let [a (d/analyse '[[:x {:a 1 :b 2}] [:y {:a 1}] [:z _]])]
    (is (= [] (:shadowed a)))
    (is (= [] (:order-dependent a)))))

(deftest incomparable-overlap-is-order-dependent
  ;; Neither pattern is more specific, one map can match both, so the list
  ;; order is the only thing deciding. Reported, not refused: the loop
  ;; manifest's own :parse table has one of these on purpose.
  (let [a (d/analyse '[[:x {:a 1}] [:y {:b 2}]])]
    (is (= [] (:shadowed a)))
    (is (= [{:labels [:x :y] :patterns [{:a 1} {:b 2}]}]
           (:order-dependent a)))))

(deftest a-guarded-general-branch-does-not-shadow-what-follows
  ;; guard-then-fallthrough: the guard may fail, so the next branch is live.
  (let [a (d/analyse '[[:big {:turn ?t} [:> ?t 10]] [:small {:turn ?t}]])]
    (is (= [] (:shadowed a)))
    (is (= [] (:order-dependent a)))))

(deftest an-unguarded-general-branch-shadows-a-guarded-one-too
  (let [a (d/analyse '[[:small {:turn ?t}] [:big {:turn ?t} [:> ?t 10]]])]
    (is (= [:big] (map :label (:shadowed a))))))

(deftest same-label-overlap-is-not-order-dependent
  ;; Two entries to the same edge: whichever fires, the data goes the same way.
  (let [a (d/analyse '[[:no-call {:parsed nil}] [:no-call {:a 1}]])]
    (is (= [] (:order-dependent a)))))

(deftest default-is-analysed-where-it-runs-which-is-last
  ;; compile-edges moves :default to the end regardless of where it is
  ;; written; the analysis must see the same order or it would refuse a
  ;; table that runs fine.
  (let [a (d/analyse '[[:default _] [:x {:a 1}]])]
    (is (= [] (:shadowed a)))))

(deftest fn-form-entries-are-opaque-to-the-analysis
  (let [a (d/analyse '[[:any (fn [d] true)] [:x {:a 1}]])]
    (is (= [] (:shadowed a)))
    (is (= [] (:order-dependent a)))))

(deftest analyse-tolerates-an-empty-or-missing-table
  (is (= {:shadowed [] :order-dependent []} (d/analyse [])))
  (is (= {:shadowed [] :order-dependent []} (d/analyse nil))))

;;; ----------------------------------------------------------------- check!

(deftest check-refuses-a-shadowed-table-naming-both-branches
  (let [data (ex-data-of #(d/check! :parse '[[:tool _] [:no-call {:parsed nil}]]))]
    (is (= :shadowed-dispatch (:error data)))
    (is (= :parse (:cell data)))
    (is (= :no-call (:label data)))
    (is (= :tool (:by data))))
  (is (thrown-with-msg? Exception #"can never fire"
                        (d/check! :parse '[[:tool _] [:no-call {:parsed nil}]]))))

(deftest check-refuses-a-malformed-entry-naming-the-cell
  (let [data (ex-data-of #(d/check! :parse [[:tool :_]]))]
    (is (= :dispatch-pattern-not-a-map (:error data)))
    (is (= :parse (:cell data)))))

(deftest check-returns-the-analysis-of-a-table-it-accepts
  (is (= [{:labels [:x :y] :patterns [{:a 1} {:b 2}]}]
         (:order-dependent (d/check! :c '[[:x {:a 1}] [:y {:b 2}]]))))
  (is (= {:shadowed [] :order-dependent []}
         (d/check! :c '[[:x (fn [d] true)]]))))

;;; ----------------------------------------------------------------- report

(deftest report-lists-order-dependent-pairs-per-cell
  (is (= [{:cell :c :labels [:x :y] :patterns '[{:a 1} {:b 2}]}]
         (d/report '{:c [[:x {:a 1}] [:y {:b 2}]]
                     :d [[:p {:a 1 :b 2}] [:q {:a 1}]]})))
  (is (= [] (d/report {})))
  (is (= [] (d/report nil))))

;;; ------------------------------------------------------------- the loop

(deftest loop-edn-dispatches-as-patterns-and-only-parse-depends-on-order
  ;; The first migrated manifest. Every table is a pattern table (so it can
  ;; be analysed at all), nothing is shadowed, and the one place the order is
  ;; load-bearing is :parse: a provider failure leaves :parsed unset, so the
  ;; provider-error branch has to be tried first. That is intentional, and
  ;; this pins it as the whole of the manifest's order-dependence.
  (let [def (edn/read-string (slurp (io/resource "manifests/loop.edn")))
        tables (:dispatches def)]
    (is (= #{:measure :cap :parse :route} (set (keys tables))))
    (doseq [[cell table] tables
            [label spec] table]
      (is (d/pattern-entry? spec) (str cell " " label " is still a fn form")))
    (is (= [] (mapcat (comp :shadowed d/analyse) (vals tables))))
    (is (= [{:cell :parse :labels [:provider-error :no-call]
             :patterns '[{:call {:ok false}} {:parsed nil}]}
            {:cell :parse :labels [:provider-error :no-call]
             :patterns '[{:call {:ok false}} {:parsed {:name "__parse_error__"}}]}]
           (d/report tables)))))

(deftest no-shipped-manifest-dispatches-on-a-form
  ;; karamazov-aqsr.4: every shipped dispatch table is patterns now, so every
  ;; one of them is checked for a branch nothing can reach, and `manifest
  ;; show` reports where only the order decides. A form is still accepted —
  ;; a project may need one — but the factory manifests set the example the
  ;; model copies from, and a form there would be a branch the analysis
  ;; cannot see.
  (doseq [n manifests/shipped-manifests
          :let [res (io/resource (manifests/manifest-resource n))]
          :when res
          [cell table] (:dispatches (manifests/read-definition (slurp res)))
          [label spec :as entry] table]
    (is (d/pattern-entry? spec)
        (str n " " cell " " label " dispatches on a form: " (pr-str entry)))))
