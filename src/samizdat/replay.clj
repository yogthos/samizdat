;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.replay
  "Deterministic replay of a recorded run. MECHANISM: pure given a case, one
  read to build one, and no opinion about what replay is used for.

  WHY IT EXISTS. RFC-010 concedes the gap this closes: \"Nothing here is held
  out. Fitness is still computed over the same turns the change was made on,
  so the comparison is between two stretches of different work rather than
  between two configurations on the same work.\" A recorded case fixes the
  work. Run it against pre-edit and post-edit userspace and the difference is
  the edit, which is what makes the result a measurement rather than evidence
  (karamazov-7mo.4, karamazov-ylte.4).

  HOW. The model's side of a conversation is already in the journal —
  `turns.assistant_text`, per branch, in order. A case is that and the
  problem; `complete-fn` hands the replies back one per call, in the shape
  `infer/complete-fn` produces, so the SAME loop runs on them: every gate,
  every cell, every prompt render, every routing decision. Nothing is stubbed
  and no code path is special-cased for replay.

  WHAT IS DELIBERATELY NOT RECORDED: the harness's own side. The tape it
  assembled, the gates that fired, where it routed — those are what an edit is
  allowed to change, and recording them would pin the thing under test.

  THE BLIND SPOT, stated here so it is never rediscovered as a surprise. The
  recorded reply is FIXED, so under replay a changed prompt cannot change the
  model's behaviour. Replay measures what the harness DOES with a given
  conversation. Whether different words would have produced a better
  conversation is the live sweep's question, and it is why the validation
  design is a hybrid rather than a cost compromise.

  RUNNING OFF THE END IS A RESULT, NOT A GAP. An edit that makes the loop take
  more turns than the recording has exhausts the fixture. Returning nil, or
  repeating the last reply, would score the candidate on a conversation that
  never happened — so exhaustion comes back as a named failure the gate can
  tell apart from a provider error."
  (:require [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn record
  "A replay case from run `run-id`: its problem and, per branch, the model's
  replies in turn order.

  A turn with no assistant text is skipped rather than replayed as an empty
  reply: a row written by something other than a model call (a provider
  failure, a synthesised turn) is not part of the conversation, and feeding
  \"\" back would make the loop parse-error on a turn that never did."
  [conn run-id]
  (let [rows (db/fetch conn ["SELECT branch_id, turn, assistant_text
                               FROM turns
                              WHERE run_id = ?
                                AND assistant_text IS NOT NULL
                                AND assistant_text <> ''
                              ORDER BY branch_id, turn"
                             run-id])]
    {:run-id run-id
     :problem (:problem (runs/get-run conn run-id))
     :recorded-at (db/now)
     :replies (reduce (fn [m {:keys [branch_id assistant_text]}]
                        (update m branch_id (fnil conj []) assistant_text))
                      {}
                      rows)}))

(defn complete-fn
  "A `complete` for branch `branch-id` of `case`: (fn [tape] -> {:ok ...}).

  The same shape `infer/complete-fn` returns, so it drops into the loop
  wherever that one does. Stateful by necessity — a conversation is ordered —
  and one per branch, because two branches of a beam are two conversations and
  sharing a cursor would interleave them.

  `:usage` is zeroed rather than replayed. The recorded run's token counts
  belong to the recorded run; a replay spends nothing, and reporting the old
  numbers would let a candidate look cheaper or dearer than it is."
  [case branch-id]
  (let [remaining (atom (get-in case [:replies branch-id] []))]
    (fn [_tape]
      (if-let [reply (first @remaining)]
        (do (swap! remaining rest)
            {:ok true
             :response {:content reply
                        :finish-reason "stop"
                        :usage {:prompt-tokens 0 :completion-tokens 0 :total-tokens 0}
                        :elapsed-ms 0}})
        ;; NAMED, so the gate can tell "this candidate ran longer than the
        ;; recording" from "the provider broke". They mean opposite things: the
        ;; first is a real difference in the loop's behaviour and possibly the
        ;; point of the edit, the second is noise.
        ;; STRUCTURED, not a sentence. The reader is the validation gate, not
        ;; a model, and it wants the branch and the count to report a
        ;; regression — while a sentence here would be model-facing prose in
        ;; src/, which belongs in resources/prompts (base-test's ratchet).
        {:ok false
         :reason :replay/exhausted
         :branch branch-id
         :recorded (count (get-in case [:replies branch-id] []))
         :error "replay exhausted"}))))

(defn branches
  "The branch ids a case can replay, longest conversation first."
  [case]
  (->> (:replies case) (sort-by (comp - count val)) (mapv key)))

(defn turns-of
  "How many replies the case holds for `branch-id`."
  [case branch-id]
  (count (get-in case [:replies branch-id] [])))
