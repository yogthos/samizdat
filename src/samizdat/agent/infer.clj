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
            [samizdat.events :as events]
            [samizdat.cancel :as cancel]
            [samizdat.agent.tools.base :as tools]
            [samizdat.config :as config]
            [samizdat.llm.client :as llm]
            [samizdat.llm.fence :as fence]
            [samizdat.llm.grammar :as grammar]
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
   :force-tool (:force-tool branch)
   ;; The tool the harness just refused, if any — what a llama.cpp endpoint
   ;; may be asked to leave off the next decision (state/record-outcome).
   :refused-tool (:refused-tool branch)})

(defn into-branch
  "Write a tape's messages back onto `branch` and clear the per-turn knobs.

  Cleared HERE rather than where they were set, because one steer forecloses
  prose on ONE turn: leaving `:prefill` would make every later turn start
  inside a fence, and leaving `:force-tool` would force the same terminal call
  forever."
  [branch tape]
  (-> branch
      (assoc :messages (:messages tape))
      (dissoc :prefill :force-tool :refused-tool)))

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

;; --- prefix identity ---------------------------------------------------------

(defn wire-fingerprint
  "Per-message fingerprints of `messages` as they go over the wire: a vector
  of `[hash chars]`, one per message, over role and content.

  Taken over the PREPARED messages — think blocks stripped, stale ledgers
  dropped — because those are the bytes the provider hashed, and a
  fingerprint of anything else would measure a request nobody sent. Per
  message rather than one hash of the whole, so the comparison below can say
  WHERE the two renders diverged, which is the whole question
  (karamazov-o4wm.1)."
  [messages]
  (mapv (fn [{:keys [role content]}]
          [(hash [(str role) (str content)]) (count (str content)) (str role)])
        messages))

(defn request-digest
  "One number for a whole request: the wire fingerprint hashed again.

  The per-message fingerprint answers HOW MUCH of a request changed since
  the last one; this answers WHICH request it was, which is the question a
  replay asks (karamazov-luqc.2): is the tape the candidate renders at turn
  k the tape the recorded reply answered? Written to the journal beside the
  prefix columns and read back by a later process, so it has to be stable
  across processes — jolt's `hash` is, and there is no digest library on
  this runtime. A runtime whose `hash` changed would make every old case
  read as diverged at turn 1, which is loud rather than wrong: nothing is
  scored on a comparison that was never made."
  [wire]
  (hash (vec wire)))

(defn prefix-stats
  "What this call's render shares with the previous one, from the front.

  `prev` and `cur` are wire fingerprints; `prev` nil is a branch's first
  call. Returns `{:change :stable-msgs :stable-chars :chars}` where
  `:change` is one of

    :first      no previous render
    :tail       only the previous render's last message changed (it lost
                its ledger on the way to the wire) — the normal turn, and
                the prefix the provider can serve is everything before it
    :rewritten  a message behind the tail changed: a fold, a cap, a prune,
                a withheld digest. Compaction's doing, and the one shape a
                policy retune can move.

  When something changed, `:changed-role`, `:was-chars` and `:now-chars`
  say which message did and what it turned into (karamazov-pdes): a digest
  is a small message where a big one was, a re-rendered pinned block is the
  same size; without them a rewrite with no compaction note beside it —
  twelve on the first live run of these columns — was a guess.

  A forced native tool_choice is NOT visible here — the prefix is
  byte-stable and the provider misses anyway — which is why the loop records
  the forced tool beside this rather than folding it in. Pure."
  [prev cur]
  (let [cur (vec cur)
        total (reduce + 0 (map second cur))]
    (if (nil? prev)
      {:change :first :stable-msgs 0 :stable-chars 0 :chars total}
      (let [prev (vec prev)
            d (count (take-while true? (map (fn [[a _] [b _]] (= a b)) prev cur)))
            [_ was] (get prev d)
            [_ now role] (get cur d)]
        (cond-> {:change (if (>= d (dec (count prev))) :tail :rewritten)
                 :stable-msgs d
                 :stable-chars (reduce + 0 (map second (subvec cur 0 d)))
                 :chars total}
          (< d (count cur)) (assoc :changed-role role :was-chars was :now-chars now))))))

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
  (let [parsed (fence/parse-tool-call (:content response) {:prefill prefill})
        signals (fence/signals response parsed)]
    (and (:truncated signals)
         ;; A reply repeating itself did not run out of room, it ran out of
         ;; anything else to say: a doubled budget buys a loop twice as long
         ;; (karamazov-o4wm.5). Left for the no-call step to answer.
         (not (:periodic signals))
         (or (nil? parsed) (= "__parse_error__" (:name parsed))))))

(defn- restriction-grammar
  "The grammar that leaves the tool the harness just refused off the next
  decision, or nil: gates.edn :local-grammar :restrict-after-refusal?
  (karamazov-fp21.1), only on an endpoint whose features include :grammar."
  [{:keys [llm-config]} {:keys [refused-tool]} think-close]
  (let [{:keys [restrict-after-refusal?]} (gates/threshold :local-grammar)]
    (when (and refused-tool restrict-after-refusal?
               (config/supports? llm-config :grammar))
      (grammar/fence-grammar {:tools (remove #{(str refused-tool)} (tools/tool-names))
                              :require? false :think-close think-close}))))

(defn force-mechanism
  "HOW this call is forced, if a gate asked for a force: the knobs to hand
  the client, chosen ONCE here from gates.edn :force-mechanism's order
  against what the endpoint's features say it can do (samizdat.config).

  Two occasions. A gate that names a forceable tool sets both a prefill with
  the name and a :force-tool spec — the NAMED force (:named order). A gate
  that merely forecloses prose, and the no-call clamp, set a bare fence
  prefill — the FENCE force (:fence order). The mechanisms:

    :prefill  the trailing assistant message the endpoint continues; the
              model starts inside the fence and cannot answer in prose
    :grammar  a sampling grammar under which the reply cannot end without a
              well-formed fence naming the tool (or, for a fence force, any
              registered tool); the reasoning stays free (:think-close)
    :native   tools + tool_choice naming the tool — a named force only

  Exactly one mechanism's knobs go out: a prefill beside a grammar would
  skip the reasoning the grammar leaves free, and a tools array beside
  either rewrites the prefix for nothing. A provider with none of the
  ordered mechanisms is steered by words alone, which is what a bare steer
  on GLM always was — and what the no-call clamp on llama.cpp was by
  ACCIDENT until features said it could prefill (karamazov-srw9). The
  :force-tool spec still travels with a prefill so the journal can name the
  forced tool (client/chat records :forced and :forced-via)."
  [{:keys [llm-config] :as ctx} {:keys [prefill force-tool] :as tape}]
  (let [{:keys [named fence]} (gates/threshold :force-mechanism)
        {:keys [think-close]} (gates/threshold :local-grammar)
        ;; Whether THIS call thinks: configured to, and the runaway breaker
        ;; has not turned it off for this branch (the per-call
        ;; :reasoning-effort is the breaker's off-value then; call-model
        ;; applied it before this ran). A grammar must know, or it
        ;; constrains the reasoning (samizdat.llm.grammar).
        thinks? (and (:thinking? llm-config)
                     (not= (:reasoning-effort llm-config)
                           (:off-value (gates/threshold :thinking-budget))))
        close (when thinks? think-close)
        can? #(config/supports? llm-config %)
        occasion (cond force-tool :named
                       (seq prefill) :fence)
        chosen (when occasion
                 (some (fn [m]
                         (case m
                           :prefill (when (can? :prefill) m)
                           :grammar (when (can? :grammar) m)
                           :native  (when (and (= :named occasion) (can? :native-tool-choice)) m)
                           nil))
                       (if (= :named occasion) named fence)))
        restriction (when-not chosen (restriction-grammar ctx tape close))]
    (case chosen
      :prefill {:prefill prefill :force-tool force-tool}
      :grammar {:grammar (grammar/fence-grammar
                          {:tools (if force-tool [(:name force-tool)] (tools/tool-names))
                           :require? true :think-close close})
                :force-tool force-tool}
      :native  {:force-tool force-tool}
      (cond-> {} restriction (assoc :grammar restriction)))))

(defn- reasoning-budget-for
  "The per-call thinking cap for this call on a llama.cpp endpoint, or nil
  for the server's own default (karamazov-w7n4). gates.edn
  :local-reasoning-budget names one per occasion: `:forced` when a gate
  forces a tool, `:turn` otherwise. Only where the call thinks at all — a
  call with thinking off has nothing to cap, and sending a cap for it would
  be a knob on the wire that changes nothing."
  [{:keys [llm-config]} {:keys [force-tool]}]
  (when (and (config/supports? llm-config :reasoning-budget)
             (:thinking? llm-config))
    (let [{:keys [turn forced]} (gates/threshold :local-reasoning-budget)]
      (if force-tool forced turn))))

(defn- delta-publisher
  "An `on-delta` for one call on `branch-id` of `run-id`: the reply's text and
  reasoning gathered and published to the bus at most every gates.edn
  :delta-publish-ms, each piece with the offset it starts at, and a `flush!`
  for what is still gathered when the call returns. {:on-delta :flush!}."
  [run-id branch-id]
  (let [every (gates/threshold :delta-publish-ms)
        st (atom {:text "" :reasoning "" :sent-text 0 :sent-reasoning 0 :at 0})
        publish! (fn [{:keys [text reasoning sent-text sent-reasoning]}]
                   (let [t (subs text sent-text) r (subs reasoning sent-reasoning)]
                     (when (or (seq t) (seq r))
                       (events/publish! (cond-> {:kind :delta :run-id run-id :branch-id branch-id}
                                          (seq t) (assoc :text t :text-at sent-text)
                                          (seq r) (assoc :reasoning r :reasoning-at sent-reasoning))))))
        mark (fn [s now] (-> s
                             (assoc :sent-text (count (:text s))
                                    :sent-reasoning (count (:reasoning s)) :at now)
                             (update :marks (fnil inc 0))))]
    {:on-delta (fn [{:keys [text reasoning]}]
                 (let [now (System/currentTimeMillis)
                       [before after] (swap-vals! st #(cond-> (-> % (update :text str text)
                                                                   (update :reasoning str reasoning))
                                                        (>= (- now (:at %)) every) (mark now)))]
                   (when (not= (:marks before) (:marks after))
                     (publish! (assoc after :sent-text (:sent-text before)
                                            :sent-reasoning (:sent-reasoning before))))))
     :flush! (fn [] (let [[before _] (swap-vals! st mark 0)] (publish! before)))}))

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
     ;; The fingerprint of what this call sends, taken once: a retry below
     ;; re-renders the same tape. It rides the response beside :prefilled,
     ;; because that is the value the loop already threads from the call to
     ;; the journal (karamazov-o4wm.1). The grammar is decided once for the
     ;; same reason: a retry of a forced turn is still forced.
     (let [wire (wire-fingerprint (message/prepare (render tape)))
           ;; Which force goes out, if any — decided once, here, from the
           ;; endpoint's features and policy (force-mechanism).
           {:keys [grammar] forced-prefill :prefill forced-tool :force-tool}
           (force-mechanism ctx tape)
           reasoning-budget (reasoning-budget-for ctx tape)]
     (loop [attempt 1]
       (let [;; Somebody may be watching the run: its calls stream, and what
             ;; the model writes goes onto the bus as it is written. Not a
             ;; probe's (journal? false): a bounce is not a turn anyone sees.
             watch (when (and journal? (:run-id ctx) id)
                     (delta-publisher (:run-id ctx) (str id)))
             base (or (:max-tokens (:llm-config ctx))
                      ;; No configured cap: the FIRST attempt keeps the
                      ;; provider's default, but a retry exists to buy room —
                      ;; doubling nothing was a same-budget repeat (blt.38).
                      (when (> attempt 1)
                        (:retry-base-tokens (gates/threshold :context-budget))))
             budget (when base (* base (bit-shift-left 1 (dec attempt))))
             r (try
                 {:ok true
                  :response (assoc (llm/chat (:llm-adapter ctx) (:llm-config ctx)
                                      (render tape)
                                      (cond-> {}
                                        budget (assoc :max-tokens budget)
                                        ;; The one force mechanism force-mechanism
                                        ;; chose for this call: a prefill the
                                        ;; endpoint continues, a sampling grammar,
                                        ;; or a native tool_choice — never two.
                                        forced-prefill (assoc :prefill forced-prefill)
                                        forced-tool (assoc :force-tool forced-tool)
                                        grammar (assoc :grammar grammar)
                                        ;; A per-call thinking cap on llama.cpp
                                        ;; (reasoning-budget-for).
                                        reasoning-budget (assoc :reasoning-budget reasoning-budget)
                                        ;; The stable conversation key an endpoint
                                        ;; pins its prefix cache to. Only the local
                                        ;; adapter emits it; see LR-5.
                                        id (assoc :cache-key (str id))
                                        watch (assoc :on-delta (:on-delta watch))))
                                   :wire wire)}
                 (catch Throwable e
                   ;; The reason travels with the failure. `provider-error-step`
                   ;; counts it, and an empty reply wants a different response
                   ;; from a refused connection: more tokens or reasoning off,
                   ;; versus wait and retry. Without this the loop knows only
                   ;; `the call failed` and every provider problem looks alike.
                   {:ok false :error (ex-message e)
                    :reason (or (:reason (ex-data e)) :call-failed)}))
             ;; After the call rather than in a `finally` round it: the catch
             ;; above already turns every throw into a value, and a park under
             ;; a finally is one ebb refuses where a park is a continuation
             ;; capture — which is how every call of run 74ddebb8's supervisor
             ;; failed with "Cannot fork inside a try/finally" (karamazov-ah7d).
             _ (some-> watch :flush! (apply []))]
         (if (and (:ok r)
                  (< attempt max-call-attempts)
                  ;; The prefill the adapter ACTUALLY sent (nil where it was
                  ;; dropped), so a GLM reply is not parsed as if it began
                  ;; mid-fence — see absorb (karamazov-0r8s).
                  (truncated-without-call? (:response r)
                                           (get (:response r) :prefilled forced-prefill)))
           (do ;; A cancel that landed during the call is honoured before the
               ;; note and the re-ask, not after: no journal write past a
               ;; forfeit (RFC-013).
               (cancel/check!)
               (when (and journal? (:conn ctx) (:run-id ctx))
                 (journal/note! (:conn ctx) (:run-id ctx) :turn-retry
                                {:branch-id id
                                 :data {:reason "truncated before any tool call"
                                        :budget budget}}))
               (recur (inc attempt)))
           r)))))))

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
         ;; The prefill the request ended with, if any. The CLIENT reports
         ;; which prefill it actually sent as :prefilled — nil where the
         ;; adapter dropped it (GLM, DeepSeek /v1) — so an unsupported provider
         ;; is not credited a fence opener it never continued, which otherwise
         ;; stored a doubled ```tool-call on every steered turn (karamazov-0r8s).
         ;; A stub response with no :prefilled key falls back to the tape's
         ;; knob, which is how the pure tests drive it.
         prefill (if (contains? response :prefilled) (:prefilled response) prefill)
         ;; Without the opener the response starts mid-fence and parses as a
         ;; no-call — the very failure the prefill exists to prevent.
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
