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
            [clojure.test :refer [deftest testing is use-fixtures]]
            [samizdat.agent.select :as select]
            [samizdat.agent.infer :as infer]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.loop :as aloop]
            [samizdat.llm.client :as llm]
            [samizdat.llm.message :as message]
            [samizdat.replay :as replay]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

;; Triage off: these drive the beam with a scripted llm/chat, and the triage
;; call would take the first scripted reply (karamazov-1wv9). What triage
;; does is select-test's and beam-test's to say.
(use-fixtures :each (fn [f] (with-redefs [select/triage! (constantly nil)] (f))))

(defn- recorded-run
  "A run with three recorded turns on one branch. With `digests`, each turn
  also records the digest of the request it answered, as a live turn does
  (karamazov-luqc.2); without, the rows are as a pre-v34 journal left them."
  ([conn] (recorded-run conn nil))
  ([conn digests]
   (let [rid (runs/start-run! conn {:problem "make the tests pass"})]
     (runs/open-branch! conn rid {:branch-id "T0"})
     (doseq [[n text tool] [[1 "Thought: look first\n```json\n{\"tool\":\"read_file\"}\n```" "read_file"]
                            [2 "Thought: now edit\n```json\n{\"tool\":\"edit_file\"}\n```" "edit_file"]
                            [3 "Thought: done\n```json\n{\"tool\":\"done\"}\n```" "done"]]]
       (journal/record-turn! conn rid (cond-> {:branch-id "T0" :turn n :tool-name tool
                                               :args "{}" :result "ok"
                                               :assistant-text text}
                                        digests (assoc :prefix {:request-hash (nth digests (dec n))}))))
     rid)))

(defn- tape
  "A tape whose wire messages are `contents`, one user message each."
  [id & contents]
  {:id id :turns [] :problem "p"
   :messages (mapv (fn [c] {:role "user" :content c}) contents)})

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

(deftest a-case-carries-the-digest-each-reply-answered
  ;; karamazov-luqc.2, from ZCode's workflow engine, which keys every replayed
  ;; step on a hash of its input. The reply alone cannot say what question it
  ;; answered; beside its digest it can, and a replay can then tell whether
  ;; the tape a candidate renders is that question.
  (let [c (db/open! ":memory:")
        rid (recorded-run c [11 22 33])
        case- (replay/record c rid)]
    (is (= {"T0" [11 22 33]} (:sent case-)))
    (testing "a recording from before the column carries nils, not zeros"
      (is (= {"T0" [nil nil nil]} (:sent (replay/record c (recorded-run c))))))))

(deftest a-reply-served-against-a-different-tape-is-reported-diverged-once
  ;; THE BLIND SPOT, MEASURED. replay.clj states that a recorded reply is
  ;; fixed and a changed tape cannot change it; until now that was true of
  ;; every replay and visible in none. The reply is still served — replay
  ;; measures what the harness does with a given conversation — but the turn
  ;; at which the conversation stopped being the recorded one is named, so a
  ;; battery verdict can say it scored the harness side only from there.
  (let [c (db/open! ":memory:")
        t1 (tape "T0" "sys" "problem")
        t2 (tape "T0" "sys" "problem" "reply1" "result1")
        t3 (tape "T0" "sys" "problem" "reply1" "result1" "reply2" "result2")
        rid (recorded-run c (mapv replay/tape-digest [t1 t2 t3]))
        case- (replay/record c rid)]
    (testing "the tapes the recording answered replay clean"
      (let [f (replay/complete-fn case- "T0")]
        (is (nil? (get-in (f t1) [:response :replay])))
        (is (nil? (get-in (f t2) [:response :replay])))
        (is (nil? (get-in (f t3) [:response :replay])))))
    (testing "a tape the recorded reply did not answer is flagged at its turn, once"
      (let [f (replay/complete-fn case- "T0")
            other (tape "T0" "sys" "problem" "reply1" "DIFFERENT")]
        (is (nil? (get-in (f t1) [:response :replay])))
        (let [r (f other)]
          (is (true? (:ok r)))
          (is (= "Thought: now edit\n```json\n{\"tool\":\"edit_file\"}\n```"
                 (get-in r [:response :content]))
              "the reply is still served: replay measures the harness side")
          (is (= {:turn 2
                  :recorded (replay/tape-digest t2)
                  :rendered (replay/tape-digest other)}
                 (get-in r [:response :replay :diverged]))))
        (is (nil? (get-in (f t3) [:response :replay]))
            "past the first divergence nothing is repeated — one note per branch")))
    (testing "a turn recorded without a digest is not judged"
      (let [f (replay/complete-fn (replay/record c (recorded-run c)) "T0")]
        (is (nil? (get-in (f (tape "T0" "anything")) [:response :replay])))))))

(deftest a-served-reply-carries-the-wire-it-answered
  ;; So a replayed run journals its prefix columns and its own digest like a
  ;; live one: a compaction retune replayed against a case then shows its
  ;; cache shape in journal/cache-misses, and recording the replayed run
  ;; yields a case whose digests are the candidate's.
  (let [c (db/open! ":memory:")
        t1 (tape "T0" "sys" "problem")
        f (replay/complete-fn (replay/record c (recorded-run c)) "T0")]
    (is (= (infer/wire-fingerprint (message/prepare (infer/render t1)))
           (get-in (f t1) [:response :wire])))))

(deftest call-model-journals-a-replay-divergence
  ;; The seam's owner is the one place that sees every complete's answer, so
  ;; a divergence a replay reports lands in the run's journal there, as a
  ;; note the battery reads — the loop stays replay-blind otherwise.
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})
        complete (fn [_tape]
                   {:ok true :response {:content "x" :finish-reason "stop"
                                        :usage {:total-tokens 0}
                                        :replay {:diverged {:turn 4 :recorded 1 :rendered 2}}}})]
    (aloop/call-model {:complete complete :llm-config {} :conn c :run-id rid}
                      {:id "T0" :messages [] :problem "p"})
    (is (= [{:branch "T0" :turn 4 :recorded 1 :rendered 2}]
           (journal/notes c rid :replay-diverged)))
    (testing "and nothing is noted when nothing diverged"
      (let [rid2 (runs/start-run! c {:problem "p"})]
        (aloop/call-model {:complete (fn [_] {:ok true :response {:content "x" :finish-reason "stop"
                                                                    :usage {:total-tokens 0}}})
                           :llm-config {} :conn c :run-id rid2}
                          {:id "T0" :messages [] :problem "p"})
        (is (empty? (journal/notes c rid2 :replay-diverged)))))))

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

(deftest a-branch-the-recording-does-not-have-is-refused-not-substituted
  ;; Found driving a real replay. The feature loop escalated T0 -> T0v1..v5 and
  ;; then fanned out to workers W0v6..W3v6 — ten branches the recording had
  ;; never seen. A convenience fallback that served T0's replies to each of
  ;; them produced a run that looked like a clean replay and was not one:
  ;; feeding one conversation's replies into a different conversation is not
  ;; replaying, and turn 11 of every worker came back __no_call__ because the
  ;; reply belonged to another context.
  ;;
  ;; A battery scored on that is scored on fiction, so an unknown branch is
  ;; refused by name and the caller decides what that means.
  (let [c (db/open! ":memory:")
        rid (recorded-run c)
        complete (replay/case-complete-fn (replay/record c rid))]
    (is (true? (:ok (complete {:id "T0"}))) "a recorded branch is served")
    (let [r (complete {:id "W0v6"})]
      (is (false? (:ok r)))
      (is (= :replay/unknown-branch (:reason r)))
      (is (= "W0v6" (:branch r))
          "named, so a gate can say WHICH branch the recording lacked"))))

(deftest one-complete-keeps-a-cursor-per-branch
  ;; A beam is several conversations. One cursor across them interleaves the
  ;; recording and every branch reads someone else's next line.
  (let [c (db/open! ":memory:")
        rid (recorded-run c)
        complete (replay/case-complete-fn (replay/record c rid))]
    (is (= (get-in (complete {:id "T0"}) [:response :content])
           (first (get-in (replay/record c rid) [:replies "T0"]))))
    (testing "a second branch starts at ITS own beginning, not where T0 left off"
      (let [c2 (db/open! ":memory:")
            rid2 (recorded-run c2)
            case2 (replay/record c2 rid2)
            f (replay/case-complete-fn case2)]
        (f {:id "T0"}) (f {:id "T0"})
        (is (= (get-in (f {:id "T0"}) [:response :content])
               (nth (get-in case2 [:replies "T0"]) 2))
            "T0 advanced by three, independently")))))
