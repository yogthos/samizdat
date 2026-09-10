;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.replay-test
  "Deterministic replay: the substrate the held-out validation gate stands on
  (karamazov-ylte.4 / karamazov-7mo.4).

  A recorded run's assistant replies are fed back in place of the provider, so
  the SAME loop runs — every gate, every cell, every prompt render, every
  routing decision — against pre-edit and post-edit userspace, on identical
  input. That is what makes the comparison a measurement rather than two
  stretches of different work, which is the limitation RFC-010 concedes.

  What replay CANNOT do, stated here so it is never rediscovered as a
  surprise: the recorded reply is fixed, so a changed PROMPT cannot change the
  model's behaviour under replay. Replay measures what the HARNESS does with a
  given conversation. Whether different words would have produced a better
  conversation is the live half's question."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.infer :as infer]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.loop :as aloop]
            [samizdat.llm.client :as llm]
            [samizdat.replay :as replay]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn- recorded-run
  "A run with three recorded turns on one branch."
  [conn]
  (let [rid (runs/start-run! conn {:problem "make the tests pass"})]
    (runs/open-branch! conn rid {:branch-id "T0"})
    (doseq [[n text tool] [[1 "Thought: look first\n```json\n{\"tool\":\"read_file\"}\n```" "read_file"]
                           [2 "Thought: now edit\n```json\n{\"tool\":\"edit_file\"}\n```" "edit_file"]
                           [3 "Thought: done\n```json\n{\"tool\":\"done\"}\n```" "done"]]]
      (journal/record-turn! conn rid {:branch-id "T0" :turn n :tool-name tool
                                      :args "{}" :result "ok"
                                      :assistant-text text}))
    rid))

(deftest a-case-is-the-runs-replies-in-order-per-branch
  ;; The fixture is the model's side of the conversation and nothing else. The
  ;; harness's side is what is under test, so recording it would pin the very
  ;; thing an edit is allowed to change.
  (let [c (db/open! ":memory:")
        rid (recorded-run c)
        case- (replay/record c rid)]
    (is (= "make the tests pass" (:problem case-)))
    (is (= ["T0"] (keys (:replies case-))))
    (is (= 3 (count (get-in case- [:replies "T0"]))))
    (is (= "Thought: look first\n```json\n{\"tool\":\"read_file\"}\n```"
           (first (get-in case- [:replies "T0"]))))
    (testing "and it carries what it was recorded against, because a score
              without its fixture and revision beside it is only a number"
      (is (= rid (:run-id case-)))
      (is (some? (:recorded-at case-))))))

(deftest replay-serves-the-recorded-replies-in-order
  (let [c (db/open! ":memory:")
        rid (recorded-run c)
        complete (replay/complete-fn (replay/record c rid) "T0")]
    (is (= "Thought: look first\n```json\n{\"tool\":\"read_file\"}\n```"
           (get-in (complete {:id "T0"}) [:response :content])))
    (is (= "Thought: now edit\n```json\n{\"tool\":\"edit_file\"}\n```"
           (get-in (complete {:id "T0"}) [:response :content])))
    (is (= "Thought: done\n```json\n{\"tool\":\"done\"}\n```"
           (get-in (complete {:id "T0"}) [:response :content])))))

(deftest a-replay-that-runs-past-its-recording-says-so-rather-than-inventing
  ;; The case that matters most and is easiest to get wrong. An edit that makes
  ;; the loop take MORE turns than the recording has runs off the end of the
  ;; fixture. Returning nil, or looping the last reply, would let the candidate
  ;; be scored on a conversation that never happened. Exhaustion is a fact
  ;; about the comparison and has to surface as one.
  (let [c (db/open! ":memory:")
        rid (recorded-run c)
        complete (replay/complete-fn (replay/record c rid) "T0")]
    (dotimes [_ 3] (complete {:id "T0"}))
    (let [r (complete {:id "T0"})]
      (is (false? (:ok r)))
      (is (= :replay/exhausted (:reason r))
          "named, so the gate can tell 'ran longer than the recording' from
           'the provider broke'")
      (is (= "T0" (:branch r)))
      (is (= 3 (:recorded r))
          "structured, because the reader is the gate — and because a sentence
           here would be model-facing prose in src/"))))

(deftest replay-costs-nothing-and-is-deterministic
  ;; The whole reason this is the pre-commit gate rather than the live sweep:
  ;; two replays of one case are identical and neither calls a provider.
  (let [c (db/open! ":memory:")
        rid (recorded-run c)
        case- (replay/record c rid)
        once (mapv #(get-in ((replay/complete-fn case- "T0") %) [:response :content])
                   (repeat 3 {:id "T0"}))
        twice (mapv #(get-in ((replay/complete-fn case- "T0") %) [:response :content])
                    (repeat 3 {:id "T0"}))]
    (is (= once twice))
    (is (every? some? once))))

;; --- the seam: replay drives the REAL loop ----------------------------------

(deftest an-injected-complete-reaches-call-model
  ;; The seam itself. NOT proof that a run can use it — see the next test,
  ;; which is the one that matters and which this test's earlier name
  ;; ("...in-the-real-loop") wrongly claimed to be.
  (let [asked (atom 0)
        complete (fn [_tape]
                   (swap! asked inc)
                   {:ok true :response {:content "Thought: hi\n```json\n{\"tool\":\"done\"}\n```"
                                        :finish-reason "stop"
                                        :usage {:total-tokens 0}}})
        out (aloop/call-model {:complete complete :llm-config {}}
                              {:id "T0" :messages [] :problem "p"})]
    (is (= 1 @asked))
    (is (true? (:ok out)))))

(deftest without-an-injected-complete-nothing-changes
  ;; A seam that quietly altered the live path would be a worse bug than the
  ;; one it fixes.
  (let [built (atom false)]
    (with-redefs [infer/complete-fn
                  (fn [_ctx] (reset! built true) (fn [_] {:ok true :response {:content ""}}))]
      (aloop/call-model {:llm-config {}} {:id "T0" :messages []})
      (is (true? @built)))))

(deftest a-drivers-injected-complete-actually-reaches-the-turn
  ;; THE TEST THAT WAS MISSING, and the defect it pins is the reason to have
  ;; it. beam/run! and workflow/run! BUILD their ctx from a named key list
  ;; rather than threading the caller's opts, so :complete was silently
  ;; dropped between the driver and call-model. Every unit test passed —
  ;; call-model honoured an injected complete, replay served replies, the
  ;; case recorded — and a real replayed run still called the provider on
  ;; every branch and spent 684k tokens. A seam nothing can reach is not a
  ;; seam, and only a driver-level test says so.
  (let [asked (atom 0)
        complete (fn [_tape]
                   (swap! asked inc)
                   {:ok true :response {:content "x" :finish-reason "stop"
                                        :usage {:total-tokens 0}}})
        seen (atom nil)]
    (with-redefs [beam/run-rounds (fn [ctx _branches _turn]
                                    (reset! seen ctx)
                                    {:status :completed :branches []})
                  llm/chat (fn [& _]
                             (throw (ex-info "the provider must not be called" {})))]
      (let [c (db/open! ":memory:")]
        (try
          (beam/run! {:conn c :config {:run {:width 1}} :llm-adapter :a
                      :llm-config {} :problem "p" :complete complete})
          (is (identical? complete (:complete @seen))
              "the driver's ctx carries the caller's complete through to the turn")
          (finally (db/close c)))))))
