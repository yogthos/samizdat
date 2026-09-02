;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.reflex-test
  "The supervisor as a watcher: a thread that observes a run WHILE it runs.

  The supervisor role is a manifest node, so it runs between rounds, in
  sequence, and only in the workflows that wire it. A run losing every turn to
  empty provider replies reaches no round boundary quickly and the node never
  gets a look — which is the case that motivated this."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [samizdat.session :as session]
            [samizdat.store.db :as db]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.runs :as runs]
            [samizdat.agent.oversight :as oversight]))

(use-fixtures :each (fn [f] (session/reset!) (f) (session/reset!)))

(defn- with-run [f]
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})]
    (try (f c rid) (finally (db/close c)))))

(defn- struggling! []
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (dotimes [_ 4] (session/observe! [:provider :empty-reply])
                 (session/observe-turn! {:tool "__provider_error__" :category :neutral
                                         :signals {}})))

(deftest it-speaks-through-the-same-queue-a-human-uses
  ;; The design decision that matters. RFC-006's rule — a directive lands on a
  ;; turn boundary, because a branch mid-turn holds a ledger it read before the
  ;; change — is not a rule the harness's own observer gets to skip. So it
  ;; submits a directive and the driver drains it exactly as it drains a
  ;; person's, with the same guards and the same record.
  (with-run
    (fn [c rid]
      (struggling!)
      (let [raised (oversight/reflex! {:conn c :run-id rid} (atom #{}))
            pending (interventions/pending c rid)]
        (is (seq raised))
        (is (= 1 (count pending)))
        (is (= "message" (:kind (first pending)))
            "it observes and advises; deciding a run should STOP is a
             judgement with a cost, and belongs to a person or the supervisor
             role, not to a threshold that fired")
        (is (= "watch" (:issued_by (first pending)))
            "distinguishable from a human's directive in the record, and
             otherwise identical")))))

(deftest a-finding-is-raised-once-per-run
  ;; An observer that says the same thing every four seconds is noise a branch
  ;; learns to ignore, which is worse than silence.
  (with-run
    (fn [c rid]
      (struggling!)
      (let [seen (atom #{})]
        (is (seq (oversight/reflex! {:conn c :run-id rid} seen)))
        (is (empty? (oversight/reflex! {:conn c :run-id rid} seen)))
        (is (empty? (oversight/reflex! {:conn c :run-id rid} seen)))
        (is (= 1 (count (interventions/pending c rid))))))))

(deftest it-is-bounded-because-every-word-costs-the-branch-a-turn
  (with-run
    (fn [c rid]
      ;; Enough distinct high-severity findings to exceed the cap.
      (dotimes [_ 10] (session/observe-turn! {:tool "eval" :category :mechanics
                                              :signals {:parse-error true
                                                        :truncated true}}))
      (dotimes [_ 4] (session/observe! [:provider :empty-reply]))
      (dotimes [_ 3] (session/observe! [:verify :skipped]))
      (let [seen (atom #{})]
        (oversight/reflex! {:conn c :run-id rid} seen)
        (is (<= (count (interventions/pending c rid)) 3)
            "an observer with no budget can spend the whole run explaining why
             the run is going badly")))))

(deftest a-healthy-run-is-left-alone
  (with-run
    (fn [c rid]
      (dotimes [_ 12] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
      (is (empty? (oversight/reflex! {:conn c :run-id rid} (atom #{}))))
      (is (empty? (interventions/pending c rid))))))

(deftest only-severe-findings-interrupt
  ;; Most findings are worth SEEING and only some are worth a turn: a
  ;; supervisor reading a block can weigh a medium finding, while a branch
  ;; mid-task handed one is just distracted.
  (with-run
    (fn [c rid]
      (dotimes [_ 10] (session/observe-turn! {:tool "eval" :category :success
                                              :signals {:auto-repaired true}}))
      (let [fs (session/findings)]
        (is (some #(= :calls-need-repair (:kind %)) fs) "the finding exists")
        (is (every? #(not= :high (:severity %))
                    (filter #(= :calls-need-repair (:kind %)) fs)))
        (is (empty? (oversight/reflex! {:conn c :run-id rid} (atom #{})))
            "and it is not worth interrupting for")))))

(deftest the-message-says-what-it-rules-out
  ;; A branch told only that turns are being wasted will reword something,
  ;; which is the expensive wrong move.
  (with-run
    (fn [c rid]
      (struggling!)
      (oversight/reflex! {:conn c :run-id rid} (atom #{}))
      (let [text (str (:payload (first (interventions/pending c rid))))]
        (is (str/includes? text "not more steering"))
        (is (str/includes? text "budget")))))) 

(deftest a-reflex-that-throws-does-not-take-the-run-with-it
  ;; It is an observer; its failure must cost the run nothing — and now that it
  ;; shares the supervisor's stream, it must not cost the stream its reasoning
  ;; pass either. start! wraps it for exactly that.
  (with-run
    (fn [c rid]
      (struggling!)
      (with-redefs [interventions/submit! (fn [& _] (throw (ex-info "boom" {})))]
        (is (thrown? Exception (oversight/reflex! {:conn c :run-id rid} (atom #{})))
            "the reflex itself propagates — the stream is what contains it")
        (let [stop (oversight/start!
                    {:enabled? true :poll-ms 5 :every-ms 10000 :budget 0
                     :conn c :run-id rid
                     :reflex-fn (fn [_] (throw (ex-info "boom" {})))}
                    (fn [_] {}))]
          (Thread/sleep 40)
          (is (nil? (stop))
              "a throwing reflex left the stream running and stopped cleanly"))))))

(deftest the-reflex-runs-every-poll-and-spends-no-budget
  ;; PHASE 1 is unbudgeted (RFC-012). :every-ms and :budget ration MODEL calls,
  ;; which is phase 2; the reflex is rule-based and cheap, and rationing it
  ;; would mean the supervisor stops watching between reasoning passes —
  ;; exactly the gap a second thread was invented to cover.
  (let [reflexes (atom 0)
        passes (atom 0)
        stop (oversight/start!
              {:enabled? true :poll-ms 5 :every-ms 100000 :budget 0
               :reflex-fn (fn [_] (swap! reflexes inc))}
              (fn [_] (swap! passes inc) {}))]
    (Thread/sleep 60)
    (stop)
    (is (pos? @reflexes) "the reflex never ran")
    (is (zero? @passes)
        "a budget of zero stopped the reasoning pass, as it should — and did
         not stop the reflex")))

(deftest a-stream-that-is-off-gets-a-no-op-stop
  ;; A unit test or a REPL call has nowhere to submit a directive, and must not
  ;; pay for a thread to discover that.
  (let [stop (oversight/start! {} (fn [_] {}))]
    (is (fn? stop))
    (is (nil? (stop)))))
