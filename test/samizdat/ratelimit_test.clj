;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.ratelimit-test
  "The process-global rate-limit latch, ported from dirge's rate_limit_gate.rs.

  The beam runs several branches against one endpoint on one key and they do
  not know about each other, so an exhausted provider costs one 429 per
  branch, repeatedly — dirge measured 118 of 154 requests refused in three
  minutes. A latch turns that into one discovery."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.llm.ratelimit :as rl]))

(deftest only-a-definitive-signal-latches
  ;; A BARE 429 IS NOT ONE. It may be a burst limit that clears in a second,
  ;; and latching the host on it would stop every branch over one unlucky
  ;; request. What counts is the provider stating WHEN.
  (is (nil? (rl/definitive-signal 429 {})) "a bare 429 says nothing about when")
  (is (= 30000 (rl/definitive-signal 429 {"retry-after" "30"}))
      "Retry-After is the provider stating a time outright")
  (is (= 45000 (rl/definitive-signal 429 {"x-ratelimit-remaining-requests" "0"
                                          "x-ratelimit-reset-requests" "45s"}))
      "and a zero remaining-count with a reset is the same statement")
  (testing "the ZERO matters — a remaining count above zero with a reset is the
            ordinary shape of a healthy response"
    (is (nil? (rl/definitive-signal 429 {"x-ratelimit-remaining-requests" "5"
                                         "x-ratelimit-reset-requests" "45s"}))))
  (testing "and only a 429 latches; a 500 with a Retry-After is a server
            problem, not an exhausted window"
    (is (nil? (rl/definitive-signal 500 {"retry-after" "30"})))))

(deftest the-latch-is-per-host-and-shared-by-every-branch
  (rl/reset-all!)
  (rl/latch! "https://api.example.com/v1/chat/completions" 30000 1000)
  (testing "one branch's discovery refuses every other branch's request to the
            same endpoint — that is the whole point"
    (is (= 29000 (rl/latched-for "https://api.example.com/v1/models" 2000))))
  (testing "a different host is untouched: one account is one window"
    (is (nil? (rl/latched-for "https://other.example.com/v1" 2000))))
  (testing "an expired latch is dropped as it is read, so nothing has to sweep"
    (is (nil? (rl/latched-for "https://api.example.com/v1/chat/completions" 999999)))
    (is (nil? (rl/latched-for "https://api.example.com/v1/chat/completions" 2000))
        "and it stays dropped")))

(deftest a-success-releases-the-host
  ;; The provider saying yes is better evidence than our arithmetic about when
  ;; it would.
  (rl/reset-all!)
  (rl/latch! "https://api.example.com/a" 60000 1000)
  (rl/clear! "https://api.example.com/b")
  (is (nil? (rl/latched-for "https://api.example.com/a" 2000))
      "cleared by host, not by path"))

(deftest two-branches-cannot-shorten-each-others-window
  (rl/reset-all!)
  (rl/latch! "https://api.example.com/x" 60000 1000)
  (rl/latch! "https://api.example.com/x" 5000 1000)
  (is (= 59000 (rl/latched-for "https://api.example.com/x" 2000))
      "the LONGER of the two wins — a second branch learning about the same
       window must not cut it short"))

(deftest a-url-that-does-not-parse-still-keys-stably
  (rl/reset-all!)
  (rl/latch! "not-a-url" 10000 0)
  (is (some? (rl/latched-for "not-a-url" 1000))
      "the key only has to be stable and per-endpoint, not correct as a URL"))
