;; samizdat - a claim-first verification harness
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

(ns samizdat.mechanics-test
  "The no-call guards (karamazov-068): a branch that produces no usable tool
  call, and the particular way a long-running one learns to produce none.

  The live failure these pin: a supervisor with 119 turns of history, whose
  context was by then almost entirely `[unloaded]` digests standing in for
  its own past turns, started emitting digests instead of calls — eight in a
  row, answered each time with a generic 'emit a tool call' that produced
  another digest, with nothing able to stop it because no call ever reached
  dispatch."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.loop :as aloop]
            [samizdat.agent.state :as state]
            [samizdat.llm.message :as message]
            [samizdat.store.journal :as journal]))

;; --- the marker announces itself before it says anything else ---------------

(deftest the-unloaded-marker-leads-the-line
  (let [msgs [{:role "system" :content "sys"}
              {:role "user" :content "problem"}
              {:role "assistant" :content (apply str (repeat 400 "a"))}
              {:role "user" :content (apply str (repeat 400 "b")) :turn 1}
              {:role "assistant" :content "recent" :turn 9}
              {:role "user" :content "newest" :turn 9}]
        out (message/compact msgs [{:turn 1 :tool "grep" :category :neutral}]
                             {:keep-pairs 1 :threshold-chars 10})
        compacted (filter message/unloaded? (map :content out))]
    (is (seq compacted) "something was unloaded at this threshold")
    (doseq [c compacted]
      (is (str/starts-with? c "[unloaded]")
          (str "the marker must lead, so a model reading left to right sees "
               "harness bookkeeping before it sees imitable content: " c)))))

(deftest unloaded?-recognizes-the-harness-marker
  (is (message/unloaded? "[unloaded] t69 cell → neutral"))
  (is (message/unloaded? "```tool-call\n<tool_call> [unloaded]")
      "the trailing form too — a branch compacted before this change carries it")
  (is (not (message/unloaded? "a perfectly ordinary reply")))
  (is (not (message/unloaded? nil))))

;; --- the imitation gets its own answer, and its own prefill treatment -------

(defn- no-call
  "Drive no-call-step over a stubbed journal, returning the branch."
  [said]
  (with-redefs [journal/record-turn! (fn [& _] nil)]
    (aloop/no-call-step {} (state/new-branch {:id "S0" :problem "p"}) 5
                        {:parsed nil :signals {} :said said :response {}})))

(deftest an-imitated-digest-gets-a-specific-complaint
  (let [b (no-call "```tool-call\n<tool_call> [unloaded]")
        msg (str (:content (last (:messages b))))]
    (is (str/includes? msg "bookkeeping")
        "named for what it is — told merely to emit a tool call, the model
         emits another digest")
    (is (not (str/includes? msg "No ```tool-call block"))
        "not the generic complaint")))

(deftest an-imitation-turn-is-not-prefilled
  ;; The prefill is half the trap: opening inside a fence, with a context of
  ;; digest lines, the likeliest continuation is another digest.
  (is (nil? (:prefill (no-call "[unloaded] t70 cell → neutral")))
      "a clean slate to reason in")
  (is (= "```tool-call\n" (:prefill (no-call "I think I should probably...")))
      "an ordinary no-call still has prose withheld — that is what works"))

(deftest a-truncated-reply-is-still-a-truncation
  ;; Truncation outranks imitation: a reply cut off mid-digest wants more
  ;; tokens, not a lecture about bookkeeping.
  (with-redefs [journal/record-turn! (fn [& _] nil)]
    (let [b (aloop/no-call-step {} (state/new-branch {:id "S0" :problem "p"}) 5
                                {:parsed nil :signals {:truncated true}
                                 :said "[unloaded] t1 grep" :response {}})
          msg (str (:content (last (:messages b))))]
      (is (str/includes? msg "token limit")))))

;; --- the streak rung: something bites wherever the arbiter runs -------------

(deftest the-mechanics-streak-gate-forces-an-honest-give-up
  (let [g (first (filter #(= :mechanics-streak (:gate %)) (gates/gates)))
        th (gates/threshold :mechanics-streak-threshold)
        fires? (fn [n] (boolean ((:when g) {:branch (assoc (state/new-branch
                                                            {:id "S0" :problem "p"})
                                                           :consecutive-mechanics-failures n)})))]
    (is (some? g) "the gate exists in gates.edn")
    (is (= "give_up" (:tool g)))
    (is (some? (gates/threshold (:budget g))))
    (is (fires? th))
    (is (not (fires? (dec th))))
    (testing "it outranks storm — a branch making no calls at all is further
              gone than one repeating a call, and the storm window cannot see
              it because nothing reaches dispatch"
      (let [storm (first (filter #(= :storm (:gate %)) (gates/gates)))]
        (is (< (:priority g) (:priority storm)))))))

(deftest the-streak-counter-clears-on-any-well-formed-call
  ;; The rung must not fire on a branch that recovered: record-outcome clears
  ;; the mechanics tally on any category that reached a tool.
  (let [b (-> (state/new-branch {:id "S0" :problem "p"})
              (state/record-outcome {:category :mechanics :progress? false})
              (state/record-outcome {:category :mechanics :progress? false})
              (state/record-outcome {:category :neutral :progress? false}))]
    (is (zero? (:consecutive-mechanics-failures b)))))
