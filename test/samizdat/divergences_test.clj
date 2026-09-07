;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.divergences-test
  "The gate on docs/divergences.md.

  docs/divergences.edn is the machine-readable half of the registry of how
  the four in-tree libraries differ from their upstreams; this namespace keeps
  the two from rotting apart: an entry whose prose was deleted, prose that
  describes an entry the registry never heard of, a pinning test that was
  renamed away. It cannot detect an UNLISTED divergence; that needs the
  upstream as an oracle. After ebb.conformance-test."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(def ^:private registry-path "docs/divergences.edn")
(def ^:private prose-path "docs/divergences.md")

(defn- registry [] (read-string (slurp registry-path)))
(defn- prose [] (slurp prose-path))

(defn- marker
  "How an id is spelled in the prose: an HTML comment, invisible when
  rendered and impossible to reword by accident."
  [id]
  (str "<!-- divergence: " (name id) " -->"))

(deftest every-entry-is-well-formed
  (doseq [e (registry)]
    (testing (str (:id e))
      (is (keyword? (:id e)))
      (is (#{:mycelium :maestro :parinferish :ring-chez} (:lib e)) "lib is one of the four")
      (is (#{:port :harness :behaviour} (:kind e)) "kind is one of the three")
      (is (and (string? (:summary e)) (not (str/blank? (:summary e)))))
      (is (or (symbol? (:pinned-by e))
              (and (string? (:unpinned e)) (not (str/blank? (:unpinned e)))))
          "either a test pins it, or :unpinned says why one cannot"))))

(deftest ids-are-unique
  (let [ids (map :id (registry))]
    (is (= (count ids) (count (distinct ids))))))

(deftest every-pinning-test-still-exists
  ;; the rot this catches: a divergence's test renamed or deleted, leaving the
  ;; registry claiming a guarantee nothing checks any more
  (doseq [e (registry)
          :let [sym (:pinned-by e)]
          :when sym]
    (testing (str (:id e) " -> " sym)
      (let [ns-sym (symbol (namespace sym))]
        (is (nil? (require ns-sym)) "the namespace loads")
        (let [v (some-> (find-ns ns-sym) (ns-resolve (symbol (name sym))))]
          (is (some? v) "the test var exists")
          (is (:test (meta v)) "and it is a test, not just any var"))))))

(deftest every-entry-is-described-in-the-prose
  ;; the rot this catches: the prose deleted or rewritten past the entry
  (let [text (prose)]
    (doseq [e (registry)]
      (testing (str (:id e))
        (is (str/includes? text (marker (:id e)))
            (str prose-path " must carry " (marker (:id e))))))))

(deftest the-prose-describes-nothing-the-registry-has-not-heard-of
  ;; the rot this catches: a divergence written up and never registered, so no
  ;; test is ever asked for
  (let [ids (set (map (comp name :id) (registry)))
        seen (map second (re-seq #"<!-- divergence: ([a-z0-9-]+) -->" (prose)))]
    (doseq [m seen]
      (testing m
        (is (contains? ids m) "every marked id must be in docs/divergences.edn")))
    (is (= (count seen) (count (distinct seen))) "no id is marked twice")))
