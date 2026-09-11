;; samizdat - a self-hosting agentic harness
;; Copyright (C) 2026 Dmitri Sotnikov
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.metrics-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [samizdat.metrics :as metrics]
            [samizdat.prompt :as prompt]))

(defn- nested-ifs
  "A source string for one deeply-branched function (cc = n+1) plus a trivial
  one, built by nesting so the parens always balance."
  [n]
  (let [body (reduce (fn [acc i] (str "(if c" i " " i " " acc ")")) "0" (range n))]
    (str "(defn tangled [x] " body ")\n(defn trivial [y] y)\n")))

(deftest cyclomatic-counts-decision-points-plus-one
  (is (= 1 (metrics/cyclomatic '(defn f [x] x)))
      "a straight-line function is 1")
  (is (= 2 (metrics/cyclomatic '(defn f [x] (if x 1 2))))
      "one if adds one")
  (is (= 3 (metrics/cyclomatic '(defn f [x] (when a (if b c d)))))
      "when + if")
  (is (= 3 (metrics/cyclomatic '(defn f [x] (cond a 1 b 2) (or p q))))
      "cond and or each count once, regardless of arity"))

(deftest cyclomatic-includes-lambdas-but-not-nested-defs
  (is (= 2 (metrics/cyclomatic '(defn f [x] (map (fn [y] (if y 1 2)) x))))
      "a branch inside an inline fn belongs to the function")
  (is (= 1 (metrics/cyclomatic '(defn f [x] (defn g [y] (if y 1 2)) x)))
      "a branch inside a NESTED defn does not"))

(deftest top-level-spans-skips-strings-and-comments
  (let [src (str "(defn a [x] x)\n"
                 ";; a comment with ) unmatched parens (\n"
                 "(defn b [y] \"a string with ) a paren\")\n")
        spans (metrics/top-level-spans src)]
    (is (= 2 (count spans)) "two top-level lists; the comment and string open nothing")
    (is (every? #(str/starts-with? (:text %) "(defn") spans))))

(deftest fn-metrics-reports-cc-sloc-and-mass
  (let [src (str "(defn small [x] x)\n\n"
                 "(defn big [x]\n"
                 "  (cond\n"
                 "    (pos? x) :p\n"
                 "    (neg? x) :n\n"
                 "    :else :z))\n")
        m (metrics/fn-metrics src)
        by-name (into {} (map (juxt :name identity)) m)]
    (is (= #{"small" "big"} (set (keys by-name))))
    (is (= 1 (:cc (by-name "small"))))
    (is (= 2 (:cc (by-name "big"))) "one cond")
    (is (= 5 (:sloc (by-name "big"))) "five source lines, blank line excluded")
    (is (< (:mass (by-name "small")) (:mass (by-name "big")))
        "the bigger, branchier function carries more mass")))

(deftest erosion-is-the-mass-fraction-in-complex-functions
  (is (= 0.0 (metrics/erosion [] 10)) "no functions, no erosion")
  (is (= 0.0 (metrics/erosion [{:mass 4.0 :cc 3} {:mass 6.0 :cc 5}] 10))
      "nothing over the limit")
  (is (== 0.5 (metrics/erosion [{:mass 5.0 :cc 20} {:mass 5.0 :cc 3}] 10))
      "half the mass sits in a cc-20 function"))

(deftest clone-lines-flags-duplicated-blocks-only
  (let [dup (str "(defn a [x]\n  (let [y 1]\n    (+ y y)\n    (* y y)\n    (- y y)))\n"
                 "(defn b [x]\n  (let [y 1]\n    (+ y y)\n    (* y y)\n    (- y y)))\n")
        uniq (str "(defn a [x] (+ x 1))\n(defn b [y] (* y 2))\n(defn c [z] (- z 3))\n")]
    (is (pos? (metrics/clone-lines {"f.clj" dup} 3))
        "a repeated block is clone lines")
    (is (zero? (metrics/clone-lines {"f.clj" uniq} 3))
        "distinct one-liners are not clones")))

(deftest report-pulls-the-three-heuristics-together
  (let [clean {"a.clj" "(defn f [x] (+ x 1))\n(defn g [y] (* y 2))\n"}
        r (metrics/report clean {:cc-fn-limit 10 :clone-min-forms 3})]
    (is (= 0.0 (:erosion r)))
    (is (= 0.0 (:verbosity r)))
    (is (pos? (:loc r)))
    (is (= [] (:hotspots r)))))

(deftest findings-are-structured-advisory-under-the-block-line-blocking-over-it
  ;; findings is DATA (severity + a kind flag + numbers); the prose lives in
  ;; prompts/metrics-findings.md. Advisory vs blocking is the severity: medium
  ;; under the block ceiling, high over it.
  (let [cfg {:cc-fn-limit 10 :verbosity-max 0.21 :verbosity-block 0.40
             :erosion-max 0.48 :erosion-block 0.68}]
    (is (= [] (metrics/findings {:verbosity 0.10 :erosion 0.20 :max-cc 4 :hotspots []} cfg))
        "clean code produces no findings")
    (let [advisory (first (metrics/findings {:verbosity 0.10 :erosion 0.55 :max-cc 4 :hotspots []} cfg))]
      (is (= "medium" (:severity advisory)))
      (is (:erosion advisory))
      (is (= "0.55" (:value advisory))))
    (let [blocking (first (metrics/findings {:verbosity 0.10 :erosion 0.71 :max-cc 4 :hotspots []} cfg))]
      (is (= "high" (:severity blocking)) "erosion past the block ceiling blocks")
      (is (:erosion blocking)))
    (let [hot (first (metrics/findings {:verbosity 0.10 :erosion 0.20 :max-cc 14
                                        :hotspots [{:name "tangled" :cc 14 :mass 40.0}]} cfg))]
      (is (= "low" (:severity hot)))
      (is (:complexity hot))
      (is (str/includes? (:fns hot) "tangled")))))

(deftest review-composes-report-and-findings
  (let [cfg {:cc-fn-limit 10 :verbosity-max 0.21 :verbosity-block 0.40
             :erosion-max 0.48 :erosion-block 0.68 :clone-min-forms 3}]
    (is (= [] (metrics/review {"c.clj" "(defn f [x] (+ x 1))\n"} cfg))
        "clean sources produce no findings")
    (is (= [] (metrics/review {} cfg)) "an empty corpus is empty")
    (let [fs (metrics/review {"e.clj" (nested-ifs 11)} cfg)]
      (is (seq fs) "an eroded corpus produces findings")
      (is (some #(and (= "high" (:severity %)) (:erosion %)) fs)
          "a cc-12 function dominating the mass erodes past the block ceiling"))))

(deftest the-template-renders-structured-findings-with-a-severity-tag
  ;; The metrics -> prose pipeline: the cell renders findings through
  ;; prompts/metrics-findings.md, and the [severity] tag it emits is what the
  ;; diff critic's judge/blocking-findings parses. A [high] must survive the
  ;; render, an advisory [medium] must read as such.
  (let [blocking (prompt/render "metrics-findings"
                                {:findings [{:severity "high" :erosion true
                                             :value "0.71" :limit "0.68"}]})
        advisory (prompt/render "metrics-findings"
                                {:findings [{:severity "medium" :verbosity true
                                             :value "0.30" :limit "0.21"}]})]
    (is (re-find #"(?i)\[high\]" blocking))
    (is (str/includes? blocking "Erosion 0.71"))
    (is (re-find #"(?i)\[medium\]" advisory))
    (is (not (re-find #"(?i)\[high\]" advisory)))))
