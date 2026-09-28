;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.repetition-test
  "A reply repeating itself is a loop, not a reply that needs more room
  (karamazov-o4wm.5). Pure over the text."
  (:require [clojure.test :refer [deftest is testing]]
            [samizdat.llm.repetition :as rep]))

(def ^:private policy
  {:window 40 :step 1 :min-repeats 3 :min-period 20 :max-gap-variance 0.5})

(def ^:private passage
  "I will now inspect the file to understand the failing test and then fix it. ")

(deftest a-passage-repeated-verbatim-at-regular-gaps-is-periodic
  (let [r (rep/periodic (apply str (repeat 6 passage)) policy)]
    (is (some? r))
    (is (>= (:repeats r) 3))
    (is (pos? (:period r)))))

(deftest prose-that-does-not-repeat-is-not
  (is (nil? (rep/periodic (str "The first paragraph says one thing about the parser. "
                               "The second says something else about the fence. "
                               "The third moves on to the tests, and the fourth to "
                               "what to do next, none of them alike.")
                          policy))))

(deftest a-quote-repeated-twice-in-an-argument-is-not-a-loop
  (testing "below the repeat floor"
    (is (nil? (rep/periodic (str passage "So, as I said, " passage) policy))))
  (testing "or repeated three times at wildly irregular gaps"
    (is (nil? (rep/periodic (str passage
                                 (apply str (repeat 30 "x "))
                                 passage
                                 (apply str (repeat 300 "y "))
                                 passage)
                            policy)))))

(deftest too-short-to-tell-and-blank-runs-do-not-count
  (is (nil? (rep/periodic "short" policy)))
  (is (nil? (rep/periodic (apply str (repeat 400 " ")) policy))
      "a run of whitespace repeats perfectly and means nothing")
  (is (nil? (rep/periodic (apply str (repeat 200 "-=")) policy))
      "so does a rule of dashes: a period under the floor is a run, not a loop"))

(deftest a-short-period-repeated-hundreds-of-times-is-a-loop
  ;; karamazov-na2k.19, run 390dcd17 T0 turn 73: GLM-5.3 emitted
  ;; "action":"close", about 900 times — a 17-character period, under the
  ;; :min-period floor meant for rules of dashes — and nothing called it a
  ;; loop. A short period counts when it repeats far past the long floor.
  (let [policy (assoc policy :short-min-period 8 :short-min-repeats 20)
        loop-text (str "{\"name\": \"task\", \"args\": {"
                       (apply str (repeat 300 "\"action\":\"close\",")))]
    (is (some? (rep/periodic loop-text policy)))
    (testing "a few short repeats are still not a loop"
      (is (nil? (rep/periodic (str "{" (apply str (repeat 5 "\"action\":\"close\",")) "}")
                              (assoc policy :window 20)))))
    (testing "and a rule of dashes is still a run"
      (is (nil? (rep/periodic (apply str (repeat 200 "-=")) policy))))))
