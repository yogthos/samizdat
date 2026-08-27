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
;;
;; ---------------------------------------------------------------------------
;; The driver shapes (step / bounce / trampoline / ab) are ported from
;; llm-repl (us.whitford.llm-repl.core), MIT licensed, (c) 2026 Michael
;; Whitford. Full notice in src/samizdat/tape.clj.
;; ---------------------------------------------------------------------------

(ns samizdat.agent.infer
  "The inference step, and the drivers that apply it.

  ONE STEP, FOUR DRIVERS. The step is `tape -> tape'`: send the array, absorb
  the reply. The single effect — the model call — is INJECTED as `complete`,
  so everything else in this namespace is a pure function of a tape value.
  Each driver is a different way of applying that same step:

    step        advance the tape          the committed turn
    bounce      apply and DISCARD        one probe, tape unchanged
    trampoline  map over a fixed tape    N independent probes, tape unchanged
    ab          vary `complete`, not the tape   the same probe under N configs

  This is llm-repl's reduction contract, and the reason to adopt it here is
  observability. Before this, a turn's inference reached into a branch map for
  its messages and its request knobs, journalled from inside the call, and had
  no seam a caller could drive — so there was no way to ask what a turn WOULD
  do without running one against the live provider and the live db. Now a tape
  is a value, `complete` is an argument, and every driver is drivable from a
  literal.

  WHAT A PROBE DELIBERATELY CANNOT DO. samizdat's full turn is not a pure
  function of the tape all the way through: `:tool/dispatch` runs shell
  commands and writes files. So the probe drivers stop at inference and parse,
  and the enforcement is structural rather than a rule anyone has to remember
  — there is no tool seam in this namespace at all. A probe can tell you what
  call the model WOULD issue; running it is the committed path's business.

  Policy lives elsewhere. Nothing here decides how deep to fork, what to
  probe, how to score the results or whether to commit a winner — that is a
  cell's job, in resources, where the supervisor can rewrite it."
  (:require [clojure.tools.logging :as log]
            [samizdat.agent.gates :as gates]
            [samizdat.llm.client :as llm]
            [samizdat.llm.fence :as fence]
            [samizdat.llm.message :as message]
            [samizdat.store.journal :as journal]
            [samizdat.tape :as tape]))

;; --- the tape projection -----------------------------------------------------

(defn of-branch
  "A branch's TAPE: the message array, the turn log the compactor reads, and
  the two per-turn request knobs.

  `:id` rides along for two reasons, both real: the retry note needs a branch
  id to journal against, and it is the stable conversation key an endpoint
  pins its prefix cache to (llm-repl's slug ≡ `conversation/id` ≡ KV slot).
  Everything a model call depends on is in here and nothing else is, which is
  what makes the step drivable from a literal."
  [branch]
  {:id (:id branch)
   :messages (vec (:messages branch))
   :turns (vec (:turns branch))
   ;; The overflow squeeze level (state/squeeze-context) rides the tape so
   ;; render can scale the compaction budget without reaching back into the
   ;; branch (karamazov-d41).
   :squeeze (:context-squeeze branch)
   :prefill (:prefill branch)
   :force-tool (:force-tool branch)})

(defn into-branch
  "Write a tape's messages back onto `branch` and clear the per-turn knobs.

  Cleared HERE rather than where they were set, because one steer forecloses
  prose on ONE turn: leaving `:prefill` would make every later turn start
  inside a fence, and leaving `:force-tool` would force the same terminal call
  forever."
  [branch tape]
  (-> branch
      (assoc :messages (:messages tape))
      (dissoc :prefill :force-tool)))

(defn squeezed-budget
  "The compaction budget after this tape's overflow squeeze (karamazov-d41).

  A branch that overflowed the provider's window carries a squeeze level
  (state/squeeze-context); each level scales :keep-pairs and
  :compaction-chars down by the policy's :factor, floored at the minimums so
  a squeezed branch still sees its current exchange. Level 0 (or nil) is the
  budget untouched."
  [{:keys [keep-pairs compaction-chars]} squeeze
   {:keys [factor min-keep-pairs min-compaction-chars]}]
  (let [scale (if (pos? (or squeeze 0))
                (Math/pow (double factor) (double squeeze))
                1.0)]
    {:keep-pairs (max (long min-keep-pairs)
                      (long (* keep-pairs scale)))
     :threshold-chars (max (long min-compaction-chars)
                           (long (* compaction-chars scale)))}))

(defn render
  "The tape as it goes to the wire: older turns compacted, recent ones
  verbatim. The tape's own array is untouched, so the journal and a resume
  still hold everything that was really said.

  The compaction budget is read HERE rather than inside the compactor:
  llm.message is pure message shaping and stays that way, so the numbers that
  decide how much history survives are supplied by the caller that knows about
  policy. gates.edn :context-budget owns them."
  [{:keys [messages turns squeeze]}]
  (message/compact messages turns
                   (squeezed-budget (gates/threshold :context-budget)
                                    squeeze
                                    (gates/threshold :context-squeeze))))

;; --- the effect seam --------------------------------------------------------

(def ^:private max-call-attempts
  "One retry, then the turn is spent. Unbounded escalation here would let a
  single turn eat a branch's whole budget, and a model that has not reached a
  tool call in twice its cap is not one token short."
  2)

(defn- truncated-without-call?
  "The response ran out of tokens before it emitted a usable tool call.

  fence/signals already separates this from `:no-fence` and its docstring says
  what to do about it — 'the fix is more tokens, not more steering' — but the
  loop steered anyway and forfeited the turn. gen-12 opened with three of these
  in a single round; gen-11 spent 12% of its turns this way against gen-10's
  4%. Truncation that still carried a call is a complete turn and is left
  alone.

  Takes the prefill for the same reason the parser does: a prefilled response
  begins mid-fence, so parsing it without the opener finds no call and would
  bill the branch a retry for a turn that had in fact issued one."
  [response prefill]
  (let [parsed (fence/parse-tool-call (:content response) {:prefill prefill})]
    (and (:truncated (fence/signals response parsed))
         (or (nil? parsed) (= "__parse_error__" (:name parsed))))))

(defn complete-fn
  "ctx -> (fn [tape] -> {:ok true :response r} | {:ok false :error s}).

  THE ONE EFFECT, as an injectable value — everything else here is pure. A
  test passes a stub; a probe cell can pass a variant's adapter and config
  without touching the run's own; `ab` builds one of these per arm.

  Retried once at a DOUBLED budget when the first response hit the token cap
  before emitting a tool call. Doubled rather than repeated, since a response
  that ran out of room needs room, and repeating at the same cap reproduces
  the same truncation.

  `opts`:
    :journal? — record the retry note (default true when ctx has a run).
                A probe passes false: a bounce that retried is not a turn the
                run took, and journalling it would put spend that never
                reached the tape into the permanent record."
  ([ctx] (complete-fn ctx nil))
  ([ctx {:keys [journal?] :or {journal? true}}]
   (fn [{:keys [id prefill force-tool] :as tape}]
     (loop [attempt 1]
       (let [base (or (:max-tokens (:llm-config ctx))
                      ;; No configured cap: the FIRST attempt keeps the
                      ;; provider's default, but a retry exists to buy room —
                      ;; doubling nothing was a same-budget repeat (blt.38).
                      (when (> attempt 1)
                        (:retry-base-tokens (gates/threshold :context-budget))))
             budget (when base (* base (bit-shift-left 1 (dec attempt))))
             r (try
                 {:ok true
                  :response (llm/chat (:llm-adapter ctx) (:llm-config ctx)
                                      (render tape)
                                      (cond-> {}
                                        budget (assoc :max-tokens budget)
                                        ;; Set by the previous turn's steer. The
                                        ;; adapter drops it if the provider cannot
                                        ;; continue a trailing assistant message,
                                        ;; so this is a hint, never a requirement.
                                        prefill (assoc :prefill prefill)
                                        ;; A gate forcing a specific tool: sent as a
                                        ;; native tool_choice, honoured on every
                                        ;; OpenAI-compatible provider (GLM included).
                                        force-tool (assoc :force-tool force-tool)
                                        ;; The stable conversation key an endpoint
                                        ;; pins its prefix cache to. Only the local
                                        ;; adapter emits it; see LR-5.
                                        id (assoc :cache-key (str id))))}
                 (catch Throwable e
                   ;; The reason travels with the failure. `provider-error-step`
                   ;; counts it, and an empty reply wants a different response
                   ;; from a refused connection: more tokens or reasoning off,
                   ;; versus wait and retry. Without this the loop knows only
                   ;; `the call failed` and every provider problem looks alike.
                   {:ok false :error (ex-message e)
                    :reason (or (:reason (ex-data e)) :call-failed)}))]
         (if (and (:ok r)
                  (< attempt max-call-attempts)
                  (truncated-without-call? (:response r) prefill))
           (do (when (and journal? (:conn ctx) (:run-id ctx))
                 (journal/note! (:conn ctx) (:run-id ctx) :turn-retry
                                {:branch-id id
                                 :data {:reason "truncated before any tool call"
                                        :budget budget}}))
               (recur (inc attempt)))
           r))))))

;; --- the pure absorb --------------------------------------------------------

(defn absorb
  "PURE: tape ⊕ response -> {:tape :parsed :signals :said}.

  Parse the fence, read the mechanics signals, and append what the assistant
  actually said — opener included, because storing the bare completion would
  leave a turn beginning mid-fence in the transcript, misrepresenting the
  format back to the model on every later turn.

  `turn`, when given, is stamped on the appended message as provenance
  compaction later reads. Optional because a probe has no turn number: it is
  not taking one.

  The returned tape has its knobs cleared, so the caller that writes it back
  onto a branch does not have to remember to."
  ([tape response] (absorb tape response nil))
  ([{:keys [messages prefill] :as tape} response turn]
   (let [content (:content response)
         ;; The prefill the request ended with, if any. Without it the response
         ;; starts mid-fence and parses as a no-call — the very failure the
         ;; prefill exists to prevent.
         parsed (fence/parse-tool-call content {:prefill prefill})
         signals (fence/signals response parsed)
         said (fence/reattach content prefill)]
     {:parsed parsed
      :signals signals
      :said said
      :tape (-> tape
                (assoc :messages (tape/append-assistant
                                  messages said (when turn {:turn turn})))
                (dissoc :prefill :force-tool))})))

;; --- the drivers ------------------------------------------------------------

(defn step
  "THE STEP: `complete` ⊕ tape -> {:tape :call :parsed :signals :said}.

  Pure given `complete`. A provider failure comes back as `:call {:ok false}`
  with the tape UNCHANGED — the tape only ever advances on a reply, so a
  failed call costs the turn and not the history."
  [complete tape]
  (let [call (complete tape)]
    (if-not (:ok call)
      {:tape tape :call call}
      (assoc (absorb tape (:response call)) :call call))))

(defn bounce
  "Apply the step to a FIXED tape and read the outcome, leaving the tape
  exactly as it was — the non-committing probe.

  Returns {:depth :parsed :said :call} at the tape's ORIGINAL depth, or
  {:depth :error} as data. Nothing is appended, nothing is journalled, no tool
  runs: this answers what the model WOULD do next, at the cost of one
  inference."
  [complete tape]
  (try
    (let [{:keys [call parsed said]} (step complete tape)]
      (if (:ok call)
        {:depth (tape/depth (:messages tape)) :parsed parsed :said said :call call}
        {:depth (tape/depth (:messages tape)) :error (:error call)}))
    (catch Throwable e
      {:depth (tape/depth (:messages tape)) :error (str "probe failed: " (ex-message e))})))

(defn trampoline
  "Bounce N varied inputs off the same FIXED tape — fan-out from a fixed point.

  `inputs` are strings appended as one user turn each on a COPY of the tape;
  they never accumulate into one another, because each bounce forks the
  immutable prefix. The tape is unchanged at the end.

  Returns {:depth :bounces [{:input …outcome} …]}. PER-BOUNCE errors as data:
  one failed probe does not sink the scan, unlike a fold where a mid-sequence
  failure invalidates everything after it. This is the cheap driver — a local
  endpoint reuses the prefix cache across every bounce, which is what makes
  scanning five candidate next-moves cost five completions and not five
  prefills."
  [complete tape inputs]
  {:depth (tape/depth (:messages tape))
   :bounces (mapv (fn [input]
                    (assoc (bounce complete
                                   (update tape :messages tape/append-user input))
                           :input input))
                  (vec inputs))})

(defn ab
  "Fan ONE probe across VARIED configs from a common tape — the dual of
  `trampoline`, which varies the input and holds the config.

  `complete-for` is (fn [variant-key] -> complete), so each arm gets its own
  effect seam: a different model, a different temperature, a different
  adapter. `input`, when given, is appended as a user turn on every arm, so
  the arms differ ONLY by their config — which is what makes the comparison
  mean something.

  Sequential on purpose. Local endpoints contend on KV slots, and a
  deterministic order is worth more here than the wall clock: these results
  get compared.

  Returns {:depth :variants {vk outcome}} with per-arm errors as data."
  ([complete-for tape variants] (ab complete-for tape variants nil))
  ([complete-for tape variants input]
   (let [probe-tape (cond-> tape
                      (seq (str input)) (update :messages tape/append-user input))]
     {:depth (tape/depth (:messages tape))
      :variants (into {}
                      (map (fn [vk]
                             [vk (try
                                   (bounce (complete-for vk) probe-tape)
                                   (catch Throwable e
                                     {:error (str "variant " vk " failed: "
                                                  (ex-message e))}))]))
                      variants)})))

(defn log-probe!
  "Note a probe on the run's journal WITHOUT it entering any tape.

  A probe is spend, and spend that leaves no trace is spend nobody can
  account for later — but it is not a turn, so it must not reach the turns
  table where the gates read. This is the receipt: what was probed, how many
  arms, how many failed. llm-repl's lesson, stated as a design rule —
  receipts index what happened, payloads live at the nodes."
  [{:keys [conn run-id]} branch-id kind {:keys [arms errors]}]
  (when (and conn run-id)
    (journal/note! conn run-id :probe
                   {:branch-id branch-id
                    :data {:kind (name kind) :arms arms :errors errors}}))
  (log/debug "probe" (name kind) "arms" arms "errors" errors)
  nil)
