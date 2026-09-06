;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; The agentic loop's cells — dynamically loaded from resources, NOT compiled
;; into src. Each is a thin cell over a named step in samizdat.agent.loop, so
;; the cell layer is policy (wiring, docs, effects) and the step logic it calls
;; is core infrastructure. This file is loaded at runtime by samizdat.cells;
;; edit it and reload to change the loop's behavior without recompiling.
;;
;; The workflow data map carries {:branch :turn} plus per-turn products
;; (:call :parsed :signals :said :result :tool :verdict); resources carry the
;; run ctx ({:conn :run-id :config :llm-adapter :llm-config :max-turns}).
;; Naming is load-bearing: :llm/*, :tool/*, :journal/*, :gate/* are what
;; glob-scoped interceptors match on.
(ns cells.loop
  (:require [mycelium.cell :as cell]
            [samizdat.agent.loop :as turn]
            [samizdat.agent.reflect :as reflect]
            [samizdat.agent.state :as state]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

;; THE SHAPES. :input is what a cell REQUIRES upstream to have produced and
;; :output is what it lets downstream rely on; mycelium walks every path from
;; :start accumulating outputs and refuses a manifest where a required input is
;; not among them. Declare only what the handler actually reads and writes —
;; malli maps are open, so a cell that names three keys still receives the
;; twenty the turn carries.
;;
;; Optional means "this cell copes with it missing", not "usually there". Each
;; one below is optional for a reason worth reading before tightening it: the
;; key arrives on some incoming edges and not others, or a manifest that is not
;; loop.edn reaches this cell by another route.
;;
;; Two things mycelium does not model, so do not expect it to catch them:
;; a cell that REMOVES a key (:loop/route dissocs the per-turn products on
;; :continue), and a key's meaning as opposed to its presence.
(cell/defcell :loop/assemble
  {:doc "Open the turn: capture the before-snapshot the settle step compares
        against, and run the explore-prologue release valve so its message
        lands before the model call."
   :pure true
   :requires []
   :input  [:map [:branch :map] [:turn :int]]
   :output [:map [:branch :map] [:before :map]]}
  (fn [_ctx {:keys [branch turn] :as data}]
    (assoc data
           :before branch
           :branch (turn/phase-valve branch turn))))

(cell/defcell :llm/infer
  {:doc "One model call, retried once at a doubled budget when the response hit
        the token cap before emitting a tool call. Produces :call {:ok
        :response} or {:ok false :error} — a provider failure is data, never an
        exception."
   :effects [:net :db]
   :requires []
   :input  [:map [:branch :map]]
   :output [:map [:call [:map [:ok :boolean]]]]}
  (fn [ctx {:keys [branch] :as data}]
    (assoc data :call (turn/call-model ctx branch))))

(cell/defcell :llm/parse
  {:doc "Fold the response into the branch: parse the fence, record mechanics
        signals, append what the assistant actually said. On a provider failure
        this passes the data through untouched for the error route."
   :pure true
   :requires []
   :input  [:map [:branch :map] [:call :map] [:turn :int]]
   ;; PER-TRANSITION, because the two halves of the docstring are two shapes.
   ;; On :provider-error the handler returns `data` untouched and adds nothing
   ;; — declaring the parse products here would tell :loop/no-call it may rely
   ;; on keys that route never produced.
   :output [:per-transition
            {:provider-error [:map]
             :no-call [:map [:branch :map] [:parsed :any]
                       [:signals :any] [:said :any]]
             :tool    [:map [:branch :map] [:parsed :any]
                       [:signals :any] [:said :any]]}]}
  (fn [_ctx {:keys [branch call turn] :as data}]
    (if-not (:ok call)
      data
      (let [{:keys [branch parsed signals said]}
            (turn/absorb-response branch (:response call) turn)]
        (assoc data :branch branch :parsed parsed :signals signals :said said)))))

(cell/defcell :loop/provider-error
  {:doc "A provider failure is not the branch's fault: journal it as neutral
        and tell the branch to try again."
   :effects [:db]
   :requires []
   :input  [:map [:branch :map] [:turn :int] [:call :map]]
   :output [:map [:branch :map]]}
  (fn [ctx {:keys [branch turn call] :as data}]
    (assoc data :branch (turn/provider-error-step ctx branch turn
                                                  (:error call) (:reason call)))))

(cell/defcell :loop/no-call
  {:doc "The response carried no usable tool call: say exactly what was wrong,
        journal the turn as mechanics, and make the next request start
        mid-fence so prose is not an available reply."
   :effects [:db]
   :requires []
   ;; :parsed/:signals/:said arrive on :llm/parse's :no-call transition, which
   ;; is the only edge loop.edn routes here — but no-call-step reads them out
   ;; of a map it builds and tolerates nils, and a manifest may reach this cell
   ;; from elsewhere, so they are declared rather than required.
   :input  [:map [:branch :map] [:turn :int] [:call :map]
            [:parsed {:optional true} :any]
            [:signals {:optional true} :any]
            [:said {:optional true} :any]]
   :output [:map [:branch :map]]}
  (fn [ctx {:keys [branch turn parsed signals said call] :as data}]
    (assoc data :branch (turn/no-call-step ctx branch turn
                                           {:parsed parsed :signals signals
                                            :said said :response (:response call)}))))

(cell/defcell :tool/dispatch
  {:doc "Phase policy first, then the tool, then the branch bookkeeping the
        outcome demands (outcome counters, artifact banking, repeat-failure
        escalation)."
   :effects [:db :fs :proc]
   :requires []
   :input  [:map [:branch :map] [:turn :int] [:parsed :any]]
   :output [:map [:branch :map] [:result :any] [:tool :any]]}
  (fn [ctx {:keys [branch turn parsed] :as data}]
    (merge data (turn/tool-step ctx branch turn parsed))))

(cell/defcell :journal/record
  {:doc "The durable record of the turn: the turn row, any artifact and its
        entry into the shared pool, any failure, any thesis. Everything a gate
        reads and everything resume replays goes through here."
   :effects [:db]
   :requires []
   ;; Everything past :branch/:turn is optional because this cell is the one
   ;; every route passes through — the provider-error and no-call paths reach
   ;; it with no :parsed and no :result, and journal-step! is written for that.
   :input  [:map [:branch :map] [:turn :int]
            [:parsed {:optional true} :any]
            [:result {:optional true} :any]
            [:tool {:optional true} :any]
            [:said {:optional true} :any]
            [:call {:optional true} :map]]
   ;; Returns `data`: the record is a side effect, not a product.
   :output [:map]}
  (fn [ctx {:keys [branch turn parsed result tool said call] :as data}]
    (turn/journal-step! ctx branch turn {:parsed parsed :result result
                                         :tool tool :said said
                                         :response (:response call)})
    data))

(cell/defcell :gate/settle
  {:doc "Close out the predictions this turn resolved — before the arbiter
        chooses, so a resolution closes against the gate that asked for it
        and not the one about to. Deterministic; no model in the path."
   :effects [:db]
   :requires []
   ;; :before is REQUIRED and is the point: settling a prediction compares the
   ;; branch as it entered the turn against the branch now, and only
   ;; :loop/assemble writes it. A manifest that routes around assemble settles
   ;; against nil, and this is what refuses it.
   :input  [:map [:before :map] [:branch :map] [:turn :int]
            [:parsed {:optional true} :any]]
   :output [:map [:branch :map] [:settled :map]]}
  (fn [ctx {:keys [before branch turn parsed] :as data}]
    (let [{:keys [branch closed]} (turn/settle-step ctx before branch turn
                                                    {:parsed parsed})]
      (assoc data :branch branch :settled {:turn turn :closed closed}))))

(cell/defcell :gate/arbiter
  {:doc "The single boundary: at most one steer, chosen in priority, plus the
        context block of shared artifacts and similar failures."
   :effects [:db]
   :requires []
   ;; :settled is REQUIRED and is the invariant: only :gate/settle writes it,
   ;; so a manifest that reaches the arbiter without closing this turn's
   ;; predictions first — crediting a gate with an outcome that preceded it —
   ;; is refused by the schema chain, naming the path. It used to be one cell
   ;; doing both in order, which no path-based check could see.
   :input  [:map [:settled :map] [:branch :map] [:turn :int]
            [:parsed {:optional true} :any]
            [:result {:optional true} :any]]
   :output [:map [:branch :map]]}
  (fn [ctx {:keys [branch turn parsed result] :as data}]
    (assoc data :branch (turn/steer-step ctx branch turn
                                         {:parsed parsed :result result}))))

(cell/defcell :loop/route
  {:doc "Decide the turn's verdict: :continue (next turn), :done, :abandoned,
        or :exhausted at the turn cap. On :continue the per-turn products are
        dropped so the data map does not grow without bound. Reads only the
        branch and the configured cap — no side effects."
   :pure true
   :requires [:max-turns]
   :input  [:map [:branch :map] [:turn :int]]
   ;; :turn as well as :verdict, because the :continue branch increments it.
   ;; The dissoc of the per-turn products is invisible to mycelium — it models
   ;; what a cell ADDS, never what it drops — and that is safe here only
   ;; because :continue routes back to :start, which the walk has visited.
   :output [:map [:verdict :keyword] [:turn :int]]}
  (fn [ctx {:keys [branch turn] :as data}]
    ;; Plus whatever `extend` directives granted this branch: ctx is fixed for
    ;; the life of the run, so a raised cap has to travel on the branch
    ;; (karamazov-blt.12).
    (let [max-turns (+ (:max-turns ctx) (or (:extended-turns branch) 0))
          verdict (cond
                    (not (state/active? branch))
                    (if (:final-answer branch) :done :abandoned)

                    (>= turn max-turns) :exhausted
                    :else :continue)]
      (cond-> (assoc data :verdict verdict)
        (= verdict :continue)
        (-> (update :turn inc)
            (dissoc :before :call :parsed :signals :said :result :tool :settled)
            ;; Each mycelium trace entry snapshots the whole data map — branch
            ;; message history included — so an uncapped trace grows
            ;; quadratically over a run. The journal is the durable record; the
            ;; in-data trace is a debugging window, and a window has edges.
            (update :mycelium/trace #(vec (take-last 20 %))))))))

(cell/defcell :memory/distil
  {:doc "What this task leaves behind about the PROJECT.

        Runs when a task ENDS, however it ended. A run that shipped knows how
        the project is built; a run that gave up knows what wasted its turns,
        and that is often the more valuable of the two — a gotcha recorded once
        saves every later session the turn it costs to rediscover.

        A STEP rather than an instruction. The prompt has asked the model to
        `remember` project facts for a while and produced none: 46 turns of
        live runs, zero calls. A node in the manifest runs whether or not the
        model felt like it.

        Fails safe to the data it was given: recording what was learned must
        never be able to stop a finished task from finishing."
   :effects [:net :db]
   :requires [:conn :run-id :llm-adapter :llm-config]
   :input  [:map [:branch :map]]
   ;; Returns the data it was given — see "fails safe" above.
   :output [:map]}
  (fn [ctx {:keys [branch] :as data}]
    (reflect/distil-task! ctx branch)
    data))

(cell/defcell :loop/finish
  {:doc "Close the run the way the verdict says: branch row, run row, and for
        an exhausted run the residual — what the branch believed it was close
        to when the budget ran out."
   :effects [:db]
   :requires [:conn :run-id]
   ;; :verdict is REQUIRED, which is the whole point of this cell: the `case`
   ;; below dispatches on it and has no default clause, so a verdict outside
   ;; #{:done :abandoned :exhausted} — a nil, or a :continue leaking through —
   ;; THROWS "No matching clause". Worth being exact about, because a throw
   ;; and a nil are handled very differently downstream: feature's `safely`
   ;; catches the first and the beam's unwrap-round-error digs into it, while
   ;; a nil would travel on as a run that quietly closed nothing.
   ;;
   ;; It took the rest of the rollout to get here. Five different cells
   ;; produce it — :loop/route, :decompose/run, :team/supervise,
   ;; :feature/route on its :ship transition, and the composed :loop/worker,
   ;; whose output samizdat derives rather than letting mycelium infer it
   ;; from end-reaching cells alone (karamazov-6y7.6).
   :input  [:map [:branch :map] [:turn :int] [:verdict :keyword]]
   :output [:map [:status :keyword]
            [:answer {:optional true} :any]
            [:residual {:optional true} :any]]}
  (fn [{:keys [conn run-id]} {:keys [branch turn verdict] :as data}]
    (case verdict
      (:done :abandoned)
      (let [status (if (:final-answer branch) :completed :abandoned)]
        (runs/close-branch! conn run-id (:id branch)
                            (:status branch) (:inactive-reason branch))
        (runs/finish-run! conn run-id status (:final-answer branch))
        (assoc data :status status :answer (:final-answer branch)))

      :exhausted
      (let [residual (state/residual branch)]
        (runs/close-branch! conn run-id (:id branch) :exhausted
                            (str "turn cap of " turn " reached"))
        (journal/note! conn run-id :residual
                       {:branch-id (:id branch) :data residual})
        (runs/finish-run! conn run-id :failed nil)
        (assoc data :status :exhausted :residual residual)))))
