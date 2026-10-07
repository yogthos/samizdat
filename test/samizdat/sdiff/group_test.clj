;; Ported from sdiff, https://github.com/semantic-namespace/diff (commit
;; 70e436e), MIT License, Copyright (c) 2026 @tangrammer.
;;
;; SPDX-License-Identifier: MIT

(ns samizdat.sdiff.group-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [samizdat.sdiff.core :as core]
            [samizdat.sdiff.group :as group]))

(def report
  {:clj [(core/file-report "src/a.clj"
                           "(ns a (:require [b :as b]))\n(defn f [x] (b/g x))\n(defn lone [] 1)\n"
                           "(ns a (:require [b :as b]))\n(defn f [x] (b/g (inc x)))\n(defn lone [] 2)\n")
         (core/file-report "src/b.clj"
                           "(ns b)\n(defn g [x] x)\n"
                           "(ns b)\n(defn g [x] [x])\n")]})

(deftest forms-that-call-one-another-share-a-group
  (let [{:keys [groups singles]} (group/by-calls report)]
    (is (= #{["src/a.clj" "defn f"] ["src/b.clj" "defn g"]} (set (first groups))))
    (is (= [["src/a.clj" "defn lone"]] singles))))

(deftest components-are-largest-first
  (is (= [[:a :b :c] [:d]] (group/components [:a :b :c :d] #{#{:a :b} #{:b :c}}))))

(deftest a-hub-does-not-join-its-callers
  (let [hub ["src/util.clj" "defn shared"]
        callers (for [i (range 4)] [(str "src/c" i ".clj") "defn use"])
        edges (set (for [c callers] #{c hub}))
        [groups shared] (group/split-hubs (cons hub callers) edges)]
    (is (= [hub] shared))
    (is (every? #(= 1 (count %)) groups) "each caller is its own group once the hub is set aside")))

(deftest the-groups-read-as-text
  (let [t (group/outline-text (group/by-calls report))]
    (is (str/includes? t "defn f"))
    (is (str/includes? t "defn lone"))))
