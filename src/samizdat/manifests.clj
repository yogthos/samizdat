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

(ns samizdat.manifests
  "Reading, validating and compiling a workflow manifest. MECHANISM ONLY.

  Extracted from samizdat.workflow (karamazov-blt.2/.3/.4/.6) so the
  self-modification tools can validate an edit EXACTLY the way the loader
  will. They could not require samizdat.workflow — it requires the branch
  loop, which requires the tool dispatcher, which requires the tools — so
  each tool re-implemented a slice of the pipeline against `io/resource`,
  and the slices drifted: cell edits soaked against the factory loop.edn, a
  saved manifest skipped check-requires! and the constraint derivation, and
  role sub-loops never saw a stored version at all.

  This namespace requires only mycelium, the cell loader and the userspace
  seam — no driver, no tools — so anything may require it. samizdat.workflow
  now delegates here, and every manifest body resolves through
  samizdat.userspace: the project's newest version, seeding the factory
  template on the way past (RFC-001)."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [maestro.core :as fsm]
            [mycelium.cell :as cell]
            [mycelium.compose :as compose]
            [mycelium.core :as myc]
            [mycelium.schema :as schema]
            [mycelium.workflow :as wf]
            [samizdat.cells :as cells]
            [samizdat.lexicon :as lexicon]
            [samizdat.store.userspace :as us]
            [samizdat.userspace :as userspace]))

(def shipped-manifests
  "The manifests that ship with the harness, ENUMERATED not globbed — a
  classpath has no directory listing, so a glob finds nothing inside a built
  binary (same reasoning as cells/shipped-cells and prompt/shipped-prompts).
  Pinned against the directory by workflow-test."
  ["loop" "beam" "critic" "orchestrator" "probe" "review" "reviewer"
   "supervisor" "oversight" "worker" "team" "board" "board-bt" "feature" "decompose"
   "repair" "repl"])

(defn manifest-resource
  "The factory resource path a manifest name seeds from, e.g. \"loop\" ->
  \"manifests/loop.edn\". A manifest with no such resource lives only in the
  userspace store — one the agent authored at runtime."
  [name]
  (str "manifests/" name ".edn"))

(defn manifest-body
  "The named manifest's EDN text through the userspace seam: the project's
  newest version (seeding the factory template as v1 on the way past), the
  template alone when unbound, nil when neither exists."
  [name]
  (userspace/body :manifest name))

(defn manifest-body!
  "`manifest-body`, throwing when the name resolves to nothing."
  [name]
  (or (manifest-body name)
      (throw (ex-info (str "no manifest named '" name "' — no factory resource"
                           " at " (manifest-resource name) " and nothing stored")
                      {:manifest name}))))

(defn read-definition
  "Parse a workflow definition from EDN text. Dispatch predicates stay as
  forms here; maestro evaluates them at compile time."
  [edn-text]
  (edn/read-string edn-text))

(defn- cell-ref-id
  "The cell id out of a manifest's `:cells` value.

  mycelium accepts two forms — a bare `:ns/id` and a map `{:id :ns/id ...}`
  carrying params — and every shipped manifest here uses the bare one. The map
  form still has to be unwrapped: `cell/get-cell` returns nil for a map, so a
  composed child written that way would silently declare nothing, which is the
  bug `child-input-schema` exists to fix, reintroduced one level down. Both
  mutation.clj and tools/mutate.clj already unwrap it the same way."
  [cell-ref]
  (if (map? cell-ref) (:id cell-ref) cell-ref))

(defn- child-input-schema
  "The `:input` a composed sub-workflow cell declares: whatever its child's
  START cell requires, since that is the cell the composed handler feeds first.

  mycelium already infers the composed cell's `:output` from the child's
  end-reaching cells (compose/infer-workflow-output-schema); `:input` it takes
  verbatim from what it is handed, and this used to hand it `{}`. The effect
  was not cosmetic: mycelium's schema chain seeds `available` from the START
  cell's input keys, so a parent whose entry node is a composed cell began its
  walk with nothing available and refused the first downstream cell that
  required anything — orchestrator, whose :start is the whole worker loop.

  nil when the child's start cell is unregistered or declares no input, which
  is the pre-schema state and the same `{}` as before."
  [child]
  (when-let [in (some-> (cell-ref-id (:start (:cells child)))
                        cell/get-cell
                        (get-in [:schema :input]))]
    {:input in}))

(defn- edge-targets [edge-def]
  (cond (keyword? edge-def) #{edge-def}
        ;; nil targets removed: a malformed edge must not make :end look
        ;; unreachable, which would make reaches-end? call every node a cut
        ;; vertex — see its docstring.
        (map? edge-def) (into #{} (remove nil?) (vals edge-def))
        :else #{}))

(defn- reaches-end?
  "Whether :end is reachable from :start through `edges`, with `skip` removed.

  `:start` written out rather than `start-node`, which is defined further down
  with the per-turn slice — same as `child-input-schema` above.

  `(seq queue)` rather than `if-let` on its head: a nil sitting IN the queue
  would otherwise read as an empty queue and end the walk early, reporting
  unreachable with work still to do."
  [edges skip]
  (loop [queue [:start] seen #{}]
    (if (seq queue)
      (let [node (first queue)]
        (cond
          (= :end node) true
          (or (nil? node) (seen node) (= skip node)) (recur (vec (rest queue)) seen)
          :else (recur (into (vec (rest queue)) (edge-targets (get edges node)))
                       (conj seen node))))
      false)))

(defn- guaranteed-output-keys
  "The keys a cell writes on EVERY transition, optional ones excluded.

  A per-transition output is intersected across its transitions, which is
  what makes this safe: :llm/parse promises the parse products on two of its
  three edges and nothing on the provider-error edge, so it guarantees
  nothing at all.

  Per-transition detection goes through mycelium's own `per-transition?` and
  `transitions-map` rather than matching the vector by hand, so a malformed
  declaration is rejected by the same predicate mycelium uses instead of
  reaching `vals` and throwing a ClassCastException that names no cell.

  Lite schemas count: mycelium accepts `{:x :int}` for `[:map [:x :int]]`, and
  a cell written that way promised nothing here while delivering everything."
  [cell-id]
  (let [output (:output (:schema (cell/get-cell cell-id)))
        keys-of (fn [m]
                  (cond
                    (map? m) (set (keys m))
                    (and (vector? m) (= :map (first m)))
                    (set (keep (fn [e]
                                 (when (and (vector? e)
                                            (not (:optional (second e))))
                                   (first e)))
                               (rest m)))))]
    (cond
      (nil? output) #{}

      (schema/per-transition? output)
      (let [per (vals (schema/transitions-map output))]
        (if (seq per) (reduce set/intersection (map #(or (keys-of %) #{}) per)) #{}))

      :else (or (keys-of output) #{}))))

(defn- child-output-schema
  "The `:output` a composed sub-workflow cell may promise: what the child
  writes on every path it can take.

  mycelium infers this from the child's END-REACHING cells only — the ones
  whose edges go to :end — while the composed handler returns the child's
  whole final data map. So a key written mid-graph is delivered and not
  declared, and orchestrator is where that bit: it dispatches on :verdict,
  which the worker's :loop/route writes, and :loop/route is not an
  end-reaching cell. :loop/finish therefore could not require :verdict.

  Computed as the union over cells that lie on EVERY path from :start to
  :end — a cell whose removal disconnects them — of the keys each guarantees
  on every transition. Both halves matter for safety. Unioning all the
  child's cells would declare keys written only on paths a run may not take,
  which is the failure that produces false confidence rather than false
  refusals: a parent compiles, then reads nil."
  [child]
  (let [edges (:edges child)
        ;; A child whose :end is unreachable at all would make EVERY node look
        ;; like a cut vertex, and the union of everything is exactly the
        ;; over-approximation the paragraph above rules out. mycelium refuses
        ;; such a graph at compile, so this only guards the order in which the
        ;; two run — but promising more than a child delivers is the failure
        ;; that compiles and then reads nil, so it is worth not being able to
        ;; reach.
        reachable? (reaches-end? edges nil)
        on-every-path (when reachable?
                        (filter #(not (reaches-end? edges %)) (keys (:cells child))))
        ks (reduce into #{}
                   (map #(guaranteed-output-keys
                          (cell-ref-id (get (:cells child) %)))
                        on-every-path))]
    (when (seq ks)
      {:output (into [:map] (map (fn [k] [k :any])) (sort ks))})))

(defn register-subworkflows!
  "A manifest can compose sub-loops: `:subworkflows {cell-id manifest-name}`
  registers each named manifest as a workflow-cell (mycelium.compose) under
  cell-id, so the parent can run it as one node. Runs before the parent
  compiles, since the parent references these cell ids. A no-op for a flat
  manifest.

  Sub-manifests resolve through the userspace seam — the project's own
  version wins, and a parent may compose a manifest the agent authored that
  has no factory resource at all. Reading them from `io/resource` was how
  `manifest save` of a tuned child changed nothing and a stored-only child
  could not be composed (karamazov-blt.3/.6)."
  [definition]
  (doseq [[cell-id mname] (:subworkflows definition)]
    (let [child (read-definition (manifest-body! mname))]
      (compose/register-workflow-cell!
       cell-id child (merge (child-input-schema child)
                            (child-output-schema child))))))

(def ctx-keys
  "The run-scoped resources every driver hands a cell, as a set.

  A cell receives `ctx` and `data`. `data` is the workflow's value and
  mycelium checks its shape; `ctx` is mycelium's `resources` slot, and its
  keys were conventional — RFC-002 recorded that a cell reading a key the
  driver does not set gets `nil` at run time, with nothing to say so until
  something downstream fell over.

  This is the contract, and it is checked from both ends: `compile-loop`
  refuses a cell whose `:requires` names a key that is not here, and
  `beam-test` asserts the production ctx actually carries every key that is.
  One without the other is worth little — a contract nobody satisfies, or a
  driver nobody holds to it.

  Mechanism, not policy: it describes what the base provides, not what any
  project should do with it."
  #{;; RFC-002's documented set
    :conn :run-id :config :llm-adapter :llm-config :root :max-turns :abort
    ;; The run's token budget, nil when unbounded (karamazov-aqsr.3)
    :token-budget
    ;; What the beam driver adds
    :problem :beam? :beam-width :turn-workflow :iterating-loop? :git-baseline
    :repl-session :live-branches :in-flight})

(defn cell-requires
  "The ctx keys `cell-id` declares it reads. `:requires` is mycelium's own
  vocabulary — the guide documents it beside `:doc` and `:effects` — and it
  was simply unused here, the same shape of miss as the manifests using only
  `:must-follow` when `:must-precede` was sitting there."
  [cell-id]
  (set (:requires (cell/get-cell cell-id))))

(defn- check-requires!
  "Refuse a manifest whose cells want ctx keys no driver provides.

  At COMPILE time, so a bad edit fails in the mutation protocol's validate
  step — before the soak, and long before a nil surfaces as a
  NullPointerException six cells downstream with nothing pointing back here."
  [definition]
  (let [wanted (for [[node cell-id] (:cells definition)
                     k (cell-requires cell-id)
                     :when (not (ctx-keys k))]
                 {:node node :cell cell-id :key k})]
    (when (seq wanted)
      (throw (ex-info
              (str "cells require ctx keys no driver provides: "
                   (str/join ", " (for [{:keys [node cell key]} wanted]
                                    (str node " (" cell ") wants " key)))
                   ". Either the key belongs in manifests/ctx-keys and the"
                   " drivers must set it, or the cell should not be asking.")
              {:wanted wanted :provided (sort ctx-keys)})))))

(defn preconditions
  "Every node's preconditions as ONE report, and whether each holds
  (karamazov-41a.6): {:nodes {node {:cell :ctx-requires :ctx-missing
  :data-requires :established}} :unsatisfied [...]}.

  Two checks already refuse a manifest at compile, each by throwing:
  `check-requires!` for the ctx keys a cell reads that no driver provides,
  and mycelium's schema chain for a data key a cell requires that nothing
  upstream produces on some path. This is the same two facts as data, per
  node, BEFORE an edit rather than as the refusal after it: what a node
  requires from the driver and from upstream, and `:established` — what
  holds on EVERY path that reaches it, the intersection over paths, which is
  the only sense in which a key can be relied on.

  An `:unsatisfied` entry is {:kind :ctx|:data :node :cell :missing}, and a
  :data one carries the `:path` that reaches the node lacking the key — the
  edge to fix is on it. Composed sub-loops are registered first, as
  `compile-definition` does, so a parent reading a child's outputs resolves.

  Pure Clojure, no solver: matching a required set against an established
  set needs no search. That is Tier 1; the numeric questions are Tier 2
  (`samizdat.symbolic/widest-beam`)."
  [definition]
  (register-subworkflows! definition)
  (let [cells (:cells definition)
        ctx-of (fn [cell-id] (cell-requires cell-id))
        ctx-missing (fn [cell-id] (set (remove ctx-keys (ctx-of cell-id))))
        {:keys [errors requires established]} (wf/schema-chain-report definition)
        nodes (into {}
                    (for [[node cell-ref] cells
                          :let [cell-id (if (map? cell-ref) (:id cell-ref) cell-ref)]]
                      [node {:cell cell-id
                             :ctx-requires (ctx-of cell-id)
                             :ctx-missing (ctx-missing cell-id)
                             :data-requires (get requires node #{})
                             :established (get established node #{})}]))
        unsatisfied (concat
                     (for [[node {:keys [cell ctx-missing]}] nodes
                           :when (seq ctx-missing)]
                       {:kind :ctx :node node :cell cell :missing ctx-missing})
                     (for [{:keys [cell-name cell-id missing-keys path]} errors]
                       {:kind :data :node cell-name :cell cell-id
                        :missing missing-keys :path path}))]
    {:nodes nodes :unsatisfied (vec unsatisfied)}))

(defn invariants
  "Every ordering rule a manifest CLAIMS, enforced or not.

  The list exists because `:constraints` alone could not answer the question
  an editor actually has. A manifest carrying two constraints looks like a
  manifest with two invariants; the beam had four and the turn had five, and
  the rest lived in cell docstrings — so there was no way to tell, from the
  file being edited, which of its rules the compiler would catch. RFC-002
  recorded that as a gap: an editor cannot know what is defended."
  [definition]
  (vec (:invariants definition)))

(def ^:private constraint-keys
  "The keys mycelium's checker reads, by constraint type. Anything else in an
  invariant entry — `:protects`, `:enforced`, `:unenforced-because` — is for
  the reader and must not reach the compiler."
  [:type :if :then :cell :before :cells])

(defn enforced-constraints
  "mycelium `:constraints`, DERIVED from the enforced invariants.

  One list, not two. A manifest that declared its invariants separately from
  its constraints would let the two disagree, and the disagreement would say
  the opposite of the truth in the more dangerous direction — a rule
  documented as enforced that nothing checks.

  An explicit `:constraints` is still honoured and appended, so a project
  manifest stored before this key existed keeps compiling unchanged."
  [definition]
  (into (vec (:constraints definition))
        (comp (filter :enforced)
              (map #(into {} (filter (fn [[k _]] (some #{k} constraint-keys))) %)))
        (:invariants definition)))

(defn unenforced-invariants
  "The rules a manifest claims that nothing checks. Each must say why: `no
  constraint` and `no constraint yet` are different facts, and only one of
  them is a decision."
  [definition]
  (vec (remove :enforced (:invariants definition))))

(def ^:private default-validate-mode
  "What a schema mismatch costs when gates.edn has nothing to say — during a
  test that stubbed the policy out, or before a project has a store bound.

  :warn rather than :strict, and rather than :off. :off would let a missing
  policy silently switch off checking, which is the failure mode where nobody
  finds out; :strict would let it halt a run over a declaration the rollout
  has not finished making precise."
  :warn)

(defn validate-mode
  "What a cell whose data does not match its declared shape costs at RUN time:
  :warn, :strict or :off, from gates.edn :schema-validation.

  Read through `lexicon`, not `gates`: `gates` requires state and supervisor,
  and this namespace is required by everything, including them.

  It does NOT reach the schema CHAIN check. mycelium walks the graph inside
  validate-workflow whatever this returns, so an edit that breaks the wiring
  is refused at `manifest save` regardless — the half the validated thing must
  not be able to switch off."
  []
  (or (:mode (lexicon/policy :schema-validation)) default-validate-mode))

(defn- on-error
  "A SCHEMA violation comes back as data; everything else still throws.

  mycelium installs an ::fsm/error state only when a compile is handed an
  :on-error, so without one maestro's default runs and throws ex-info
  \"execution error\" carrying the whole FSM map. Every driver here is written
  for the other contract — run-turn, run! and beam/advance all read
  (myc/error? data) — so under :schema-validation :strict that branch was
  unreachable, the beam could not abandon one branch with a reason, and what
  surfaced was a page of FSM internals instead of mycelium's own message
  naming the cell and the missing keys.

  SURGICAL, and deliberately so. Handler exceptions keep maestro's behaviour,
  because things downstream are built on it: feature's `safely` catches a
  throwing stage to record it and fall through to a safe default, and the
  beam's unwrap-round-error digs the real cause out of the nested ex-data. A
  blanket on-error would turn every nested crash into a nil verdict that
  nothing notices — trading a loud failure for a silent one."
  [resources fsm-state]
  (if (get-in fsm-state [:data :mycelium/schema-error])
    ;; PARKED, not ended. :mycelium/resume names the state to re-enter and
    ;; mycelium's resume-compiled takes it from there, so the failure is a
    ;; place the run stopped rather than the end of it. `:last-state-id` is
    ;; the cell that failed, which is deliberately where a resume re-enters:
    ;; the supervisor fixes that cell and the fix is RETRIED rather than
    ;; skipped past.
    (-> (:data fsm-state)
        (assoc :mycelium/halt true
               :mycelium/resume (:last-state-id fsm-state)))
    (fsm/default-on-error resources fsm-state)))

(defn compile-definition
  "The full static check WITHOUT reloading the cell registry: structure,
  dispatch coverage, reachability, sub-workflow registration, ctx-key
  requires, and the :constraints derived from the enforced invariants.
  Throws on any violation.

  The seam the mutation protocol validates through: propose-cell! has just
  load-stringed a CANDIDATE into the live image, and `compile-loop`'s
  registry reload would replace the candidate with the stored cells — so the
  validate would check the loop against the code it is about to stop
  running. A caller that has not touched the registry wants `compile-loop`.

  `opts` may carry `:on-trace`, mycelium's per-cell callback — how a driver
  puts the implementer's stream in front of the supervisor (RFC-012). It is a
  COMPILE-time opt because mycelium bakes it into the interceptors, which is
  also why it takes the run's tracer rather than reading anything at run time."
  ([definition] (compile-definition definition nil))
  ([definition opts]
   (when-not (seq (:cells definition))
     (throw (ex-info (str "not a workflow definition: no :cells"
                          (when (nil? definition) " (the definition is nil)"))
                     {:definition definition})))
   ;; Register any composed sub-loops as cells before the parent references them.
   (register-subworkflows! definition)
   (check-requires! definition)
   (let [compiled (myc/pre-compile
                   (assoc definition :constraints (enforced-constraints definition))
                   ;; Baked in HERE because mycelium reads :validate at compile
                   ;; time, into the interceptors it builds — there is no
                   ;; per-run override later. A manifest compiled fresh per run
                   ;; (which every driver does) therefore picks up a policy edit
                   ;; on the next run, like every other gates.edn value.
                   (cond-> {:validate (validate-mode)
                            :on-error on-error}
                     (:on-trace opts) (assoc :on-trace (:on-trace opts))))]
     (when-let [warnings (:mycelium/compile-warnings (:compiled-fsm compiled))]
       (log/warn "loop definition compiled with warnings:" (pr-str warnings)))
     compiled)))

(defn compile-loop
  "Compile a loop definition through mycelium's full static checking:
  structure, dispatch coverage, reachability, and the :constraints that make
  the loop's invariants compile-time errors. Throws on any violation —
  which is the mutation protocol's first line of defense. Logs, and returns
  compiled with, any :mycelium/compile-warnings (undeclared cell effects).

  `opts` passes through to compile-definition — notably `:on-trace`."
  ([definition] (compile-loop definition nil))
  ([definition opts]
  ;; Load the cells before every compile. The cell registry is global mutable
  ;; state, and a non-empty registry is not proof the LOOP's cells are present
  ;; (a test or another workflow may have registered different ones) — so this
  ;; always loads rather than guarding on emptiness. Idempotent, cheap, and it
  ;; picks up any edited cell, which is the hot-reload the mutation protocol
  ;; builds on.
   (cells/load-cells!)
   (compile-definition definition opts)))

;; --- the per-turn slice ------------------------------------------------------

(def start-node
  "The manifest's entry node. A convention every shipped manifest follows and
  mycelium's own compile assumes."
  :start)

(defn finish-nodes
  "Nodes whose cell is :loop/finish — the whole-run teardown the beam owns."
  [definition]
  (set (keep (fn [[node cell]] (when (= :loop/finish cell) node))
             (:cells definition))))

(defn iterating?
  "Whether one pass through this manifest's slice is one TURN — a single model
  call the beam can schedule against four siblings — or a whole-run workflow
  that does its own looping inside one call.

  Two conditions, and both are needed. The slice must contain :llm/infer, so
  that a pass is one model call: `orchestrator` loops back to its start node,
  but that node is an entire nested worker RUN, and treating it as a turn
  would put a multi-minute job under the 900s turn deadline and run five of
  them at once. And the chain must loop back to the start node, so that a pass
  is a turn rather than the whole job: `team`, `feature` and `decompose` run
  straight through.

  loop / critic / review / worker / reviewer / supervisor iterate; team,
  feature, decompose and orchestrator do not. The answer decides the beam's
  width and whether the per-turn deadline applies."
  [definition]
  (let [cells (set (vals (:cells definition)))
        loops-back? (some (fn [[_ to]]
                            (if (map? to)
                              (some #(= start-node %) (vals to))
                              (= start-node to)))
                          (:edges definition))]
    (boolean (and (contains? cells :llm/infer) loops-back?))))

(defn start-back-edge
  "The `[node label]` of an edge routing back to the start node, or nil."
  [definition]
  (first
   (for [[node to] (:edges definition)
         [label target] (if (map? to) to {nil to})
         :when (= start-node target)]
     [node label])))

(defn check-entry-back-edge!
  "Refuse to SLICE a non-iterating manifest that routes an edge back to its
  start node.

  For an ITERATING loop, looping back to start IS the definition of a turn and
  the slice cuts it deliberately. For a whole-run workflow the same shape is
  silent data loss: `turn-manifest` redirects that edge into :end, so under the
  beam driver each cycle runs as a separate turn on a FRESH data map. Live in
  run 3b8d2af5 — the feature loop's revise edge went to :start, so every
  revision round reset :feature/revisions, collided branch ids, lost the
  guidance, and crash-looped the judge and supervisor calls.

  The shipped manifests were fixed by adding a re-entry node of their own
  (feature's :redispatch, orchestrator's :retry) and manifest_test pins their
  shape. This is the other half: an AGENT-AUTHORED manifest saved at runtime
  cannot reintroduce it, because the mutation protocol compiles before it
  stores (karamazov-emw).

  CHECKED AT THE SLICE, not at compile. The shape is only dangerous for a
  manifest the beam turn-slices, and that is knowable here and not from the
  definition alone: `beam.edn`, the scheduler's OWN manifest, routes :tick
  back to :start and is non-iterating by the same test — it schedules the
  branches that make model calls rather than making one — and it is never
  sliced, so it was never in danger. Compiling the check caught it and was
  wrong to."
  [definition]
  (when-not (iterating? definition)
    (when-let [[node label] (start-back-edge definition)]
      (throw (ex-info
              (str "node " node " routes " (if label (str "its " label " edge") "an edge")
                   " back to " start-node ", and this manifest does not iterate."
                   " Under the beam driver that edge is cut into :end, so each"
                   " cycle runs as a separate turn on a fresh data map — the"
                   " counters, the ids and the guidance all reset. Add a"
                   " re-entry node that repeats " start-node "'s dispatch and"
                   " route to that instead, the way feature uses :redispatch.")
              {:node node :label label :start start-node})))))

(defn turn-manifest
  "`definition` reduced to ONE turn: edges back to the start node and edges
  into :loop/finish are redirected to :end, and the finish node is dropped
  (mycelium's reachability check refuses an orphan).

  Returns a definition that compiles and runs exactly like the original up to
  the turn boundary, and then stops.

  REFUSES a non-iterating manifest with a back edge to the start node — see
  check-entry-back-edge!. This is the operation that turns that edge into
  silent data loss, so it is the operation that must not perform it."
  [definition]
  (check-entry-back-edge! definition)
  (let [finish (finish-nodes definition)
        terminal (conj finish start-node)
        retarget (fn [to] (if (contains? terminal to) :end to))]
    (-> definition
        (assoc :cells (into {} (remove (comp finish key)) (:cells definition)))
        (assoc :edges
               (into {}
                     (for [[from to] (:edges definition)
                           ;; The finish node's own outgoing edge goes with it.
                           :when (not (contains? finish from))]
                       [from (if (map? to)
                               (into {} (map (juxt key (comp retarget val))) to)
                               (retarget to))]))))))

(defn compiled-manifest
  "Compile the named manifest to a runnable sub-loop. The seam a role cell
  uses to run a role's own loop (worker for an implementor, reviewer for a
  reviewer). Compiled fresh each call, so a cell or manifest edit is picked
  up — and resolved through the userspace seam, so the project's stored
  version of a role loop is the one that runs. Reading the factory resource
  here was how `manifest save \"supervisor\"` never took effect
  (karamazov-blt.3). Throws when the name resolves to nothing."
  [name]
  (compile-loop (read-definition (manifest-body! name))))

(defn catalog
  "The workflows available to select or adapt: every factory manifest and
  every stored one, each with its :description. This is the set the
  supervisor reads to decide whether to switch a run to a different workflow,
  tweak an existing one, or author a new one — the compiled menu the
  self-healing loop chooses from. A manifest with no :description still
  lists, with an empty one.

  The STORED body wins where one exists: the menu must describe the version
  that will actually run, and serving the factory description for an evolved
  manifest was karamazov-blt.4."
  [conn]
  (let [factory (->> shipped-manifests
                     (filter #(io/resource (manifest-resource %)))
                     set)
        stored (->> (try (us/names conn :manifest) (catch Throwable _ nil))
                    ;; us/names yields rows ({:name :version :versions}),
                    ;; factory yields name strings — normalise to names.
                    (map (fn [x] (if (map? x) (:name x) x)))
                    (remove nil?)
                    set)]
    (->> (sort (into factory stored))
         (keep (fn [nm]
                 (let [edn (or (when conn
                                 (try (some-> (us/load-latest conn :manifest nm) :body)
                                      (catch Throwable _ nil)))
                               (some-> (io/resource (manifest-resource nm)) slurp))]
                   (when edn
                     (let [d (try (read-definition edn) (catch Throwable _ nil))]
                       {:name nm :description (str (:description d))})))))
         vec)))
