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

(ns samizdat.sdiff.address-test
  "Naming a form by the shortest thing that is unambiguous
  (karamazov-0e2c.14, after sdiff's review find-form / find-change)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.tools]
            [jolt.fs :as fs]
            [samizdat.sdiff.address :as address]))

(def src
  (str "(ns demo.core)\n"
       "\n"
       "(defn route [x]\n"
       "  (let [cap 10\n"
       "        n (inc x)]\n"
       "    (if (> n cap) :stop :go)))\n"
       "\n"
       "(defn helper\n"
       "  ([a] (helper a 1))\n"
       "  ([a b] (let [cap b] (+ a cap))))\n"
       "\n"
       "(defmethod run :fast [m] m)\n"))

(deftest a-form-by-its-id-or-its-unique-name
  (is (= [3 6] (:rows (address/locate src "defn route"))))
  (is (= [3 6] (:rows (address/locate src "route"))) "a bare name when it is unique")
  (is (= [12 12] (:rows (address/locate src "defmethod run :fast"))))
  (let [miss (address/locate src "routes")]
    (is (= :none (:error miss)))
    (is (some #{"defn route"} (:forms miss)) "a miss lists what there is")))

(deftest a-place-inside-a-form-by-its-path-or-a-unique-end-of-it
  (is (= [4 4] (:rows (address/locate src "route" "body › binding cap"))))
  (is (= [4 4] (:rows (address/locate src "route" "binding cap"))) "an end no sibling shares")
  (is (= [10 10] (:rows (address/locate src "helper" "arity 2 › let 2 › binding cap"))))
  (testing "an end two places share is ambiguous, and both are named"
    (let [r (address/locate src "helper" "vector 1")]
      (is (= :ambiguous (:error r)))
      (is (= 2 (count (:candidates r)))))))

(deftest read-file-takes-a-form
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (str root "/src/demo"))
    (spit (str root "/src/demo/core.clj") src)
    (let [r (:result (samizdat.agent.tools/run-tool
                      {:tool-name "read_file" :branch {:id "B1"} :root root
                       :args {:path "src/demo/core.clj" :form "route"}}))]
      (is (str/includes? r "(defn route [x]") r)
      (is (not (str/includes? r "defn helper")) "only that form"))
    (let [r (:result (samizdat.agent.tools/run-tool
                      {:tool-name "read_file" :branch {:id "B1"} :root root
                       :args {:path "src/demo/core.clj" :form "nope"}}))]
      (is (str/includes? r "defn route") "a miss names the forms there are"))))

;; --- what changed since a branch last read the file (karamazov-0e2c.15) ----------

(deftest a-re-read-says-which-forms-changed-since-the-last-one
  (let [root (str (fs/create-temp-dir))
        read (fn [bid] (:result (samizdat.agent.tools/run-tool
                                 {:tool-name "read_file" :branch {:id bid} :root root :run-id "R"
                                  :args {:path "src/demo/core.clj"}})))]
    (fs/create-dirs (str root "/src/demo"))
    (spit (str root "/src/demo/core.clj") src)
    (try
      (is (not (str/includes? (read "B1") "since you last read")) "a first read has nothing to compare")
      (spit (str root "/src/demo/core.clj")
            (-> src
                (str/replace "(if (> n cap) :stop :go)" "(if (>= n cap) :stop :go)")
                (str/replace "\n\n(defn helper" "\n\n\n;; moved down\n(defn helper")
                (str "\n(defn added [] 1)\n")))
      (let [r (read "B1")]
        (is (str/includes? r "since you last read") r)
        (is (re-find #"changed:.*defn route" r) r)
        (is (re-find #"new:.*defn added" r) r)
        (is (not (re-find #"changed:.*defn helper" r)) "moving a form or adding a comment is not a change"))
      (testing "per branch: a sibling's first read has nothing to compare"
        (is (not (str/includes? (read "B2") "since you last read"))))
      (finally (samizdat.sdiff.address/forget-reads! "R")))))
