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
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [mycelium.cell :as cell]
            [mycelium.compose :as compose]
            [mycelium.core :as myc]
            [samizdat.cells :as cells]
            [samizdat.store.userspace :as us]
            [samizdat.userspace :as userspace]))

(def shipped-manifests
  "The manifests that ship with the harness, ENUMERATED not globbed — a
  classpath has no directory listing, so a glob finds nothing inside a built
  binary (same reasoning as cells/shipped-cells and prompt/shipped-prompts).
  Pinned against the directory by workflow-test."
  ["loop" "beam" "critic" "orchestrator" "probe" "review" "reviewer"
   "supervisor" "worker" "team" "board" "board-bt" "feature" "decompose"
   "repair"])

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
    (compose/register-workflow-cell!
     cell-id (read-definition (manifest-body! mname)) {})))

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

(defn compile-definition
  "The full static check WITHOUT reloading the cell registry: structure,
  dispatch coverage, reachability, sub-workflow registration, ctx-key
  requires, and the :constraints derived from the enforced invariants.
  Throws on any violation.

  The seam the mutation protocol validates through: propose-cell! has just
  load-stringed a CANDIDATE into the live image, and `compile-loop`'s
  registry reload would replace the candidate with the stored cells — so the
  validate would check the loop against the code it is about to stop
  running. A caller that has not touched the registry wants `compile-loop`."
  [definition]
  (when-not (seq (:cells definition))
    (throw (ex-info (str "not a workflow definition: no :cells"
                         (when (nil? definition) " (the definition is nil)"))
                    {:definition definition})))
  ;; Register any composed sub-loops as cells before the parent references them.
  (register-subworkflows! definition)
  (check-requires! definition)
  (let [compiled (myc/pre-compile
                  (assoc definition :constraints (enforced-constraints definition)))]
    (when-let [warnings (:mycelium/compile-warnings (:compiled-fsm compiled))]
      (log/warn "loop definition compiled with warnings:" (pr-str warnings)))
    compiled))

(defn compile-loop
  "Compile a loop definition through mycelium's full static checking:
  structure, dispatch coverage, reachability, and the :constraints that make
  the loop's invariants compile-time errors. Throws on any violation —
  which is the mutation protocol's first line of defense. Logs, and returns
  compiled with, any :mycelium/compile-warnings (undeclared cell effects)."
  [definition]
  ;; Load the cells before every compile. The cell registry is global mutable
  ;; state, and a non-empty registry is not proof the LOOP's cells are present
  ;; (a test or another workflow may have registered different ones) — so this
  ;; always loads rather than guarding on emptiness. Idempotent, cheap, and it
  ;; picks up any edited cell, which is the hot-reload the mutation protocol
  ;; builds on.
  (cells/load-cells!)
  (compile-definition definition))

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

(defn turn-manifest
  "`definition` reduced to ONE turn: edges back to the start node and edges
  into :loop/finish are redirected to :end, and the finish node is dropped
  (mycelium's reachability check refuses an orphan).

  Returns a definition that compiles and runs exactly like the original up to
  the turn boundary, and then stops."
  [definition]
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
