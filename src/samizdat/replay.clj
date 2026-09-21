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
  allowed to change, and recording them would pin the thing under test. One
  number about it IS kept: the DIGEST of each request (turns.request_hash,
  infer/request-digest), which pins nothing — it only lets the replay say
  whether the tape it is handed is the one the reply answered.

  THE BLIND SPOT, stated here so it is never rediscovered as a surprise, and
  now MEASURED rather than only stated (karamazov-luqc.2, after ZCode's
  workflow engine, which keys every replayed step on a hash of its input).
  The recorded reply is FIXED, so under replay a changed prompt cannot change
  the model's behaviour. Replay measures what the harness DOES with a given
  conversation. Whether different words would have produced a better
  conversation is the live sweep's question, and it is why the validation
  design is a hybrid rather than a cost compromise. What `complete-fn` adds
  is the turn at which that stopped being a given conversation: the first
  reply served against a tape whose digest is not the recorded one comes
  back with `:replay {:diverged {:turn k …}}`, loop/call-model journals it,
  and battery/reading carries it as `:diverged-at` — so a verdict says
  \"harness side only from turn k\" instead of scoring turns k+1… on a
  conversation that never happened. The reply is still served, because the
  harness side is still what replay measures.

  RUNNING OFF THE END IS A RESULT, NOT A GAP. An edit that makes the loop take
  more turns than the recording has exhausts the fixture. Returning nil, or
  repeating the last reply, would score the candidate on a conversation that
  never happened — so exhaustion comes back as a named failure the gate can
  tell apart from a provider error."
  (:require [samizdat.agent.infer :as infer]
            [samizdat.llm.message :as message]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn tape-digest
  "The digest of what `tape` would send: the same fingerprint over the same
  prepared render the live call takes (infer/complete-fn), hashed the same
  way, so a recorded `request_hash` and this are comparable."
  [tape]
  (infer/request-digest (infer/wire-fingerprint (message/prepare (infer/render tape)))))

(defn record
  "A replay case from run `run-id`: its problem and, per branch, the model's
  replies in turn order — and beside each reply, under `:sent`, the digest
  of the request it answered (nil on a row from before turns.request_hash).

  A turn with no assistant text is skipped rather than replayed as an empty
  reply: a row written by something other than a model call (a provider
  failure, a synthesised turn) is not part of the conversation, and feeding
  \"\" back would make the loop parse-error on a turn that never did."
  [conn run-id]
  (let [rows (db/fetch conn ["SELECT branch_id, turn, assistant_text, request_hash
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
                      rows)
     :sent (reduce (fn [m {:keys [branch_id request_hash]}]
                     (update m branch_id (fnil conj []) request_hash))
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
  numbers would let a candidate look cheaper or dearer than it is.

  `:wire` is the fingerprint of what the tape WOULD have sent, as a live call
  carries, so the replayed run journals its prefix columns and its own
  request digest: a compaction retune replayed against a case shows its
  cache shape in journal/cache-misses, and recording the replayed run yields
  a case whose digests are the candidate's.

  `:replay {:diverged {:turn :recorded :rendered}}` rides the FIRST reply
  served against a tape whose digest is not the recorded one, and only that
  one — later turns are past the divergence by construction, and one note
  per branch is what the battery reads. A recorded turn with no digest is
  not judged."
  [case branch-id]
  (let [remaining (atom (get-in case [:replies branch-id] []))
        sent (atom (get-in case [:sent branch-id] []))
        turn (atom 0)
        diverged (atom false)]
    (fn [tape]
      (if-let [reply (first @remaining)]
        (let [wire (infer/wire-fingerprint (message/prepare (infer/render tape)))
              rendered (infer/request-digest wire)
              recorded (first @sent)
              k (swap! turn inc)
              divergence (when (and (not @diverged) (some? recorded) (not= recorded rendered))
                           (reset! diverged true)
                           {:turn k :recorded recorded :rendered rendered})]
          (swap! remaining rest)
          (swap! sent rest)
          {:ok true
           :response (cond-> {:content reply
                              :finish-reason "stop"
                              :usage {:prompt-tokens 0 :completion-tokens 0 :total-tokens 0}
                              :elapsed-ms 0
                              :wire wire}
                       divergence (assoc :replay {:diverged divergence}))})
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

(defn case-complete-fn
  "One `complete` for a whole case, keeping a cursor PER BRANCH and dispatching
  on the tape's `:id`.

  Prefer this to `complete-fn` for anything driving a real run. A beam is
  several conversations, and one cursor across them interleaves the recording
  so every branch reads another's next line.

  AN UNKNOWN BRANCH IS REFUSED, and that is the point. Driving a real replay,
  the feature loop escalated T0 through five revisions and then fanned out to
  four workers — ten branches the recording had never seen. Serving them the
  recorded branch's replies produced a run that looked like a clean replay and
  was not one: a reply written in one context, replayed into another, is not
  that branch's conversation, and every worker's turn 11 came back
  __no_call__ because of it. A battery scored on that is scored on fiction.
  The refusal is named so a gate can report which branch the recording lacked
  rather than silently measuring a fabrication."
  [case]
  (let [cursors (atom {})]
    (fn [tape]
      (let [b (:id tape)]
        (if-not (contains? (:replies case) b)
          {:ok false
           :reason :replay/unknown-branch
           :branch b
           :known (vec (sort (keys (:replies case))))
           ;; Terse, like :replay/exhausted above: the reader is the gate, and
           ;; :reason / :branch / :known already carry everything it needs. A
           ;; sentence here is model-facing prose in src/ and belongs in
           ;; resources/prompts (base-test's ratchet).
           :error "unknown replay branch"}
          (let [f (or (get @cursors b)
                      (let [f (complete-fn case b)]
                        (swap! cursors assoc b f)
                        f))]
            (f tape)))))))

(defn branches
  "The branch ids a case can replay, longest conversation first."
  [case]
  (->> (:replies case) (sort-by (comp - count val)) (mapv key)))

(defn turns-of
  "How many replies the case holds for `branch-id`."
  [case branch-id]
  (count (get-in case [:replies branch-id] [])))
