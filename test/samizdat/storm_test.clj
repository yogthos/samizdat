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

(ns samizdat.storm-test
  "Tier 1 loop guards (karamazov-ekk): the storm window.

  Every guard the harness had before this keyed on FAILURE — repeating-failure?
  needs two identical errors, the stuck gate needs a failure streak, the
  studying gate needs an inspection-only stretch. A model repeating the same
  SUCCESSFUL call, or alternating A-B-A-B between two calls, tripped nothing
  and burned the run (dirge storm.rs caught exactly this; karamazov-j5t re-read
  one file twenty turns running). These tests pin the mechanism deterministically:
  fired when it should, silent otherwise — per the n=1 measurability rule in
  agent-test's docstring."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.loop :as aloop]
            [samizdat.agent.state :as state]
            [samizdat.agent.storm :as storm]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.base :as tools-base]))

;; A local policy so the pure-fn tests do not depend on gates.edn values;
;; the integration tests below use the real (gates/storm-policy).
(def policy
  {:enabled? true
   :window-size 6
   :threshold 3
   :timeout-floor 2
   :min-cycles 2
   :strikes-to-force 3
   :exempt-tools #{"read_file" "grep"}
   :mutating-tools #{"write_file" "edit_file" "shell" "eval"}})

(defn- note-n
  "Push the same signature n times as non-mutating entries."
  [window sig n]
  (reduce (fn [w _] (storm/note-call w {:sig sig} policy)) window (range n)))

;; --- canonical signatures ---------------------------------------------------

(deftest signatures-are-canonical
  (testing "key order does not matter"
    (is (= (storm/signature "shell" {:command "ls" :cwd "."})
           (storm/signature "shell" {:cwd "." :command "ls"}))))
  (testing "keyword and string keys agree — live args are keywordized JSON,
            resume re-parses the journal's verbatim JSON"
    (is (= (storm/signature "shell" {"command" "ls"})
           (storm/signature "shell" {:command "ls"}))))
  (testing "integral doubles read as their integer, like dirge's canonical_json,
            so 1 and 1.0 cannot dodge the window"
    (is (= (storm/signature "t" {:n 1}) (storm/signature "t" {:n 1.0})))
    (is (not= (storm/signature "t" {:n 1.5}) (storm/signature "t" {:n 1}))))
  (testing "nested structures normalize the same way"
    (is (= (storm/signature "t" {:a {:x 1 :y 2} :v [1 2]})
           (storm/signature "t" {"a" {"y" 2.0 "x" 1} "v" [1.0 2]}))))
  (testing "different args or a different tool are different calls"
    (is (not= (storm/signature "t" {:a 1}) (storm/signature "t" {:a 2})))
    (is (not= (storm/signature "t" {:a 1}) (storm/signature "u" {:a 1})))
    (is (not= (storm/signature "t" nil) (storm/signature "t" {:a 1})))))

;; --- the window -------------------------------------------------------------

(deftest the-window-is-bounded
  (let [w (note-n [] "s" 8)]
    (is (= 6 (count w)) "trimmed to :window-size, oldest first")))

(deftest a-mutating-call-clears-bystanders
  ;; A mutation changes the world, so a repeated non-mutating call after it may
  ;; legitimately read differently — clearing them is what keeps a legitimate
  ;; check-fix-check cycle out of the window. Mutators still count amongst
  ;; themselves: three identical edits in a row IS a storm.
  (let [w (-> []
              (storm/note-call {:sig "read-ish"} policy)
              (storm/note-call {:sig "edit-a" :mutating? true} policy)
              (storm/note-call {:sig "read-ish"} policy)
              (storm/note-call {:sig "edit-a" :mutating? true} policy))]
    (is (= ["edit-a" "edit-a"] (map :sig w))
        "non-mutating entries dropped at each mutation, mutators kept")))

(deftest tracked?-honours-the-exempt-set
  (is (not (storm/tracked? policy "read_file")))
  (is (not (storm/tracked? policy "grep")))
  (is (storm/tracked? policy "shell"))
  (is (storm/tracked? policy "task")))

;; --- repeat blocking --------------------------------------------------------

(deftest the-third-identical-call-is-blocked
  (let [sig "shell {:command \"make test\"}"]
    (is (not (storm/blocked? (note-n [] sig 1) sig policy))
        "one prior copy: an honest retry is allowed")
    (is (storm/blocked? (note-n [] sig 2) sig policy)
        "two prior copies: the third identical call is withheld")
    (is (not (storm/blocked? (note-n [] "other" 4) sig policy))
        "a full window of a DIFFERENT signature blocks nothing")))

(deftest interleaved-signatures-count-only-their-own
  ;; Occurrences in the window, not consecutive runs: interleaving a second
  ;; call between repeats must not launder them.
  (let [w (-> [] (note-n "a" 1) (note-n "b" 2))]
    (is (not (storm/blocked? w "a" policy)) "one prior copy of a: allowed")
    (is (storm/blocked? w "b" policy) "two prior copies of b: withheld")
    (is (storm/blocked? (note-n w "a" 1) "a" policy)
        "the second interleaved a makes the third a a storm too")))

(deftest a-timed-out-call-gets-one-fewer-retry
  ;; A timeout burned its whole budget; letting the model run three identical
  ;; copies of a hanging command costs three budgets. Floor at :timeout-floor.
  (let [sig "shell {:command \"sleep 999\"}"
        w (-> (note-n [] sig 1) (storm/mark-timeout sig))]
    (is (storm/blocked? w sig policy)
        "one prior TIMED-OUT copy already blocks the second")
    (is (not (storm/blocked? (note-n [] sig 1) sig policy))
        "without the timeout mark the second try is allowed")))

;; --- oscillation ------------------------------------------------------------

(deftest oscillation-is-blocked-when-the-cycle-would-extend
  (let [w (-> [] (storm/note-call {:sig "a"} policy)
              (storm/note-call {:sig "b"} policy)
              (storm/note-call {:sig "a"} policy))]
    (is (storm/oscillating? w "b" policy)
        "a-b-a + b completes two full cycles")
    (is (not (storm/oscillating? w "c" policy))
        "a third distinct call is divergence, not oscillation")
    (is (not (storm/oscillating? w "a" policy))
        "a-b-a + a is not alternation (and the repeat rule owns plain repeats)"))
  (let [aa (note-n [] "a" 3)]
    (is (not (storm/oscillating? aa "a" policy))
        "a pure repeat is the repeat rule's business, never oscillation"))
  (let [w3 (-> [] (storm/note-call {:sig "a"} policy)
               (storm/note-call {:sig "b"} policy)
               (storm/note-call {:sig "a"} policy))
        p3 (assoc policy :min-cycles 3)]
    (is (not (storm/oscillating? w3 "b" p3))
        "min-cycles 3 needs a-b-a-b-a + b, not a-b-a + b")))

;; --- weighted failure cost --------------------------------------------------

(deftest timeouts-cost-double-on-the-failure-counter
  (let [b (state/new-branch {:id "B1" :problem "p"})]
    (is (= 2 (:consecutive-failures
              (state/record-outcome b {:category :failure :weight 2})))
        "a timeout carries weight 2, so streak gates trip sooner")
    (is (= 1 (:consecutive-failures
              (state/record-outcome b {:category :failure})))
        "an ordinary failure still costs 1")
    (is (= 0 (:consecutive-failures
              (-> b
                  (state/record-outcome {:category :failure :weight 2})
                  (state/record-outcome {:category :success}))))
        "success still clears the streak outright")))

;; --- the refusal rules are data (phases.edn), the thresholds are policy
;;     (gates.edn), and the detector is mechanism (storm.clj) ----------------

(deftest the-storm-policy-is-discoverable-in-gates-edn
  (let [p (gates/storm-policy)]
    (is (:enabled? p))
    (is (every? pos? [(:window-size p) (:threshold p)
                      (:timeout-floor p) (:min-cycles p)
                      (:strikes-to-force p)]))
    (is (<= (:timeout-floor p) (:threshold p)))
    (is (contains? (:exempt-tools p) "read_file"))
    (is (contains? (:exempt-tools p) "done")
        "done/give_up are the done-blocked and last-call rungs' business —
         storm blocking a forced done would be one guard punishing what
         another demands (dirge-e1nv)")
    (is (contains? (:mutating-tools p) "write_file"))))

(deftest the-storm-rule-refuses-the-third-identical-call
  (let [args {:command "make test"}
        sig (storm/signature "shell" args)
        real (gates/storm-policy)
        b (-> (state/new-branch {:id "B1" :problem "p"})
              (assoc :task {:id "t1"})
              (assoc :storm-window (-> []
                                       (storm/note-call {:sig sig :mutating? true} real)
                                       (storm/note-call {:sig sig :mutating? true} real))))
        r (tools/phase-refusal {:branch b :tool-name "shell" :args args})]
    (is (some? r) "the third identical shell call is withheld")
    (is (= :storm (:refusal-rule r)))
    (is (= :mechanics (:category r)))
    (is (:policy-refusal? r)
        "a withheld call is not evidence about the branch's line of inquiry")
    (testing "the message steers toward a pivot, not a bare don't-repeat"
      (is (str/includes? (str (:result r)) "different")))
    (testing "a different command from the same tool proceeds"
      (is (nil? (tools/phase-refusal
                 {:branch b :tool-name "shell" :args {:command "ls"}}))))
    (testing "an exempt tool is never refused for repetition"
      (let [rsig (storm/signature "read_file" {:path "a.clj"})
            br (assoc b :storm-window (note-n [] rsig 5))]
        (is (nil? (tools/phase-refusal
                   {:branch br :tool-name "read_file" :args {:path "a.clj"}})))))))

(deftest the-oscillation-rule-refuses-the-extending-call
  (let [a {:command "make test"}
        bargs {:file "core.clj" :content "x"}
        siga (storm/signature "shell" a)
        sigb (storm/signature "write_file" bargs)
        real (gates/storm-policy)
        b (-> (state/new-branch {:id "B1" :problem "p"})
              (assoc :task {:id "t1"})
              (assoc :storm-window [{:sig siga :mutating? true}
                                    {:sig sigb :mutating? true}
                                    {:sig siga :mutating? true}]))
        r (tools/phase-refusal {:branch b :tool-name "write_file" :args bargs})]
    (is (some? r) "a-b-a + b is withheld")
    (is (= :storm-oscillation (:refusal-rule r)))))

(defn- branch-with-task []
  (-> (state/new-branch {:id "B1" :problem "p"})
      (assoc :task {:id "t1"})))

;; --- the verify exemption ---------------------------------------------------

(deftest verify-calls-are-invisible-to-the-guard
  ;; The verify loop IS repetition: red, edit, rerun the identical suite
  ;; command. The best GLM-campaign run's only identical repeat was its
  ;; verify-cmd (run 4e785664) — blocking the third suite run punishes TDD.
  (testing "verify-call? matches the configured command, with suffixes"
    (is (storm/verify-call? "shell" {:command "jolt -M:test"} "jolt -M:test"))
    (is (storm/verify-call? "shell" {:command "jolt -M:test; echo EXIT:$?"}
                            "jolt -M:test"))
    (is (not (storm/verify-call? "shell" {:command "ls"} "jolt -M:test")))
    (is (not (storm/verify-call? "eval" {:code "(+ 1 1)"} "jolt -M:test")))
    (is (not (storm/verify-call? "shell" {:command "jolt -M:test"} nil))
        "no verify-cmd configured: nothing is a verify call"))
  (testing "the repeat rule skips a verify call even with copies in the window"
    (let [real (assoc (gates/storm-policy) :verify-exempt? true)
          args {:command "jolt -M:test"}
          sig (storm/signature "shell" args)
          b (-> (state/new-branch {:id "B1" :problem "p"})
                (assoc :storm-window (note-n [] sig 4)))
          ctx {:branch b :tool-name "shell" :args args
               :config {:run {:verify-cmd "jolt -M:test"}}}]
      (is (not (storm/repeat-blocked? ctx real)))
      (is (storm/repeat-blocked? (dissoc ctx :config) real)
          "the same call with no verify-cmd in ctx is an ordinary storm")))
  (testing "tool-step never notes a verify call in the window"
    (with-redefs [tools/run-tool (fn [{:keys [branch]}]
                                   {:branch branch :result "ok"
                                    :category :success :progress? false})]
      (let [ctx {:config {:run {:verify-cmd "jolt -M:test"}}}
            call {:name "shell" :args {:command "jolt -M:test"}}
            {b1 :branch} (aloop/tool-step ctx (branch-with-task) 1 call)]
        (is (empty? (:storm-window b1))))))
  (testing "window-from-turns skips verify rows on resume"
    (let [pol (assoc (gates/storm-policy) :verify-cmd "jolt -M:test")
          rows [{:tool_name "shell" :args "{\"command\": \"jolt -M:test\"}"
                 :category "success" :result "ok"}
                {:tool_name "shell" :args "{\"command\": \"ls\"}"
                 :category "success" :result "ok"}]]
      (is (= 1 (count (storm/window-from-turns rows pol)))))))

;; --- tool-step wiring: window notes, strikes, reflexion ---------------------

(deftest tool-step-notes-dispatched-calls
  (with-redefs [tools/run-tool (fn [{:keys [branch]}]
                                 {:branch branch :result "ok"
                                  :category :success :progress? false})]
    (let [{b1 :branch} (aloop/tool-step {} (branch-with-task) 1
                                        {:name "shell" :args {:command "ls"}})]
      (is (= 1 (count (:storm-window b1))))
      (is (= (storm/signature "shell" {:command "ls"})
             (:sig (peek (:storm-window b1)))))
      (testing "an exempt tool leaves no window entry"
        (let [{b2 :branch} (aloop/tool-step {} (branch-with-task) 1
                                            {:name "read_file" :args {:path "x"}})]
          (is (empty? (:storm-window b2))))))))

(deftest tool-step-withholds-the-storm-and-counts-strikes
  (let [dispatched (atom 0)]
    (with-redefs [tools/run-tool (fn [{:keys [branch]}]
                                   (swap! dispatched inc)
                                   {:branch branch :result "ok"
                                    :category :success :progress? false})]
      (let [call {:name "shell" :args {:command "make test"}}
            b0 (branch-with-task)
            {b1 :branch} (aloop/tool-step {} b0 1 call)
            {b2 :branch} (aloop/tool-step {} b1 2 call)
            {b3 :branch r3 :result} (aloop/tool-step {} b2 3 call)
            {b4 :branch r4 :result} (aloop/tool-step {} b3 4 call)]
        (is (= 2 @dispatched) "the third and fourth identical calls never ran")
        (is (= :mechanics (:category r3)))
        (is (= :storm (:refusal-rule r3)))
        (is (= 1 (:storm-strikes b3)))
        (is (= 2 (:storm-strikes b4)) "consecutive withholds accumulate strikes")
        (is (= :storm (:refusal-rule r4)))
        (testing "the withheld call enters the reflexion log once"
          (is (some #(str/includes? % "make test") (:abandoned b3))))
        (testing "a genuinely different dispatched call resets the strikes"
          (let [{b5 :branch} (aloop/tool-step {} b4 5
                                              {:name "shell" :args {:command "ls"}})]
            (is (= 3 @dispatched))
            (is (zero? (:storm-strikes b5)))))))))

(deftest tool-step-marks-timeouts-in-the-window
  (with-redefs [tools/run-tool (fn [{:keys [branch]}]
                                 {:branch branch :result "[timed out after 120000ms]"
                                  :category :failure :progress? false
                                  :timeout? true})]
    (let [call {:name "shell" :args {:command "sleep 999"}}
          {b1 :branch} (aloop/tool-step {} (branch-with-task) 1 call)]
      (is (:timeout? (peek (:storm-window b1))))
      (testing "the second identical call is already withheld at the floor"
        (let [{r2 :result} (aloop/tool-step {} b1 2 call)]
          (is (= :storm (:refusal-rule r2)))))
      (testing "and it costs double on the failure counter"
        (is (= 2 (:consecutive-failures b1)))))))

;; --- the give-up escalation gate --------------------------------------------

(deftest the-storm-gate-forces-give-up-after-repeated-strikes
  (let [g (first (filter #(= :storm (:gate %)) (gates/gates)))]
    (is (some? g) "the :storm gate entry exists in gates.edn")
    (is (= "give_up" (:tool g)))
    (is (some? (:budget g)))
    (is (some? (gates/threshold (:budget g))))
    (let [fires? (fn [b] (boolean ((:when g) {:branch b})))]
      (is (fires? (assoc (branch-with-task) :storm-strikes
                         (gates/threshold :storm-strikes-to-force))))
      (is (not (fires? (branch-with-task)))
          "no strikes, no firing")
      (is (not (fires? (assoc (branch-with-task) :storm-strikes
                              (dec (gates/threshold :storm-strikes-to-force)))))))))

;; --- resume -----------------------------------------------------------------

(deftest resume-rebuilds-the-window-from-journal-args
  ;; The journal's turns table stores the model's args verbatim; rebuilding the
  ;; window from them means a resumed branch is still protected — unlike
  ;; repeating-failure?, which documents that it comes back blind (resume.clj).
  (let [real (gates/storm-policy)
        rows [{:tool_name "shell" :args "{\"command\": \"make test\"}"
               :category "failure" :result "boom"}
              {:tool_name "read_file" :args "{\"path\": \"a.clj\"}"
               :category "success" :result "..."}
              {:tool_name "shell" :args "{\"command\": \"make test\"}"
               :category "failure" :result "[timed out after 120000ms]"}]
        w (storm/window-from-turns rows real)]
    (is (= 2 (count w)) "the exempt read_file row leaves no entry")
    (is (every? #(= (storm/signature "shell" {:command "make test"}) (:sig %)) w))
    (is (some :timeout? w) "the timed-out row is marked from its result text")
    (is (storm/blocked? w (storm/signature "shell" {:command "make test"}) real)
        "two prior copies, one timed out: the next identical call is withheld")))

(deftest storm-vocab-names-are-real-tools
  ;; Same walk agent-test does for the settle vocab: a vocabulary naming a tool
  ;; no run-tool dispatches is a config typo that silently disables the guard.
  (doseq [k [:storm-exempt :storm-mutating]
          n (gates/tool-vocab k)]
    (is (contains? (methods tools-base/run-tool) n)
        (str "storm vocab " k " names `" n "` — no run-tool method dispatches it"))))
