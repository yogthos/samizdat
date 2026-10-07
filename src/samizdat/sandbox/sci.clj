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

(ns samizdat.sandbox.sci
  "Where agent-editable CODE runs: in SCI, against an allowlist, not in the
  harness compiler (karamazov-3vu1.9).

  Three kinds of text the agent may rewrite while it runs are code rather
  than data: the cells (resources/cells/*.clj and a project's copies), the
  policy tables' forms (gates.edn :gates / :ship-gates / :give-up-gates
  :when, :message-form and :measure; phases.edn :refusals :when and
  :finished-key), and a manifest's `(fn [d] ...)` dispatch predicates. They
  were load-stringed and `eval`ed into the harness image, so an edited cell
  or rule had the whole process: jolt.process, jolt.ffi, slurp/spit, the
  database raw, `require` of anything. Here each is evaluated in an SCI
  context that holds clojure.core (SCI's, which has no file, process or host
  access) plus exactly the vars below. A symbol outside them does not
  resolve, and SCI resolves every symbol of a form before running any of
  it, so an edit that reaches past the list is refused at LOAD, with the
  symbol named, and never half-runs.

  THE ALLOWLIST IS CONFINEMENT, SO IT LIVES IN src/. Same reasoning as
  policy/base-rules and repl.route/sandbox-spec: resources are agent-
  editable, and a list there could be widened by the party it constrains. A
  cell that needs a function the list lacks is a change a human makes here.

  HOW A VAR IS EXPOSED. Each entry becomes a function that derefs the host
  var at CALL time, not a copy of its value, so `with-redefs` and a reloaded
  namespace reach code running in SCI exactly as they reach native callers.
  A non-function var is exposed as its value.

  WHY EACH EFFECTFUL ENTRY IS ACCEPTABLE is the comment above it in
  `cell-api`. The rule applied: a cell is the harness's orchestration, so it
  legitimately drives the model, the run record, the task board and the tool
  path — through the same higher-level functions native code uses, each of
  which does one kind of thing. What it never gets is a raw capability: no
  process spawn (samizdat.engine.proc), no raw SQL (samizdat.store.db), no
  file IO beyond the confined readers listed, no eval of host code, no FFI,
  no host classes. The one command a cell may run is the one the operator
  configured for the run (verify/run-configured)."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [mycelium.cell :as cell]
            [samizdat.prompt :as prompt]
            [sci.core :as sci]))

;; --- the allowlists -----------------------------------------------------------

(def cell-api
  "Namespace -> the vars a cell may call from it. WHY each is safe to hand
  a cell is the comment above its entry.

  `:effects` is what the entry can do beyond computing a value: :net (a
  provider call), :db (the run record through the store's own functions),
  :proc (a subprocess), :fs (a file read), :run (runs cells, tools or
  fibers). An entry with none is pure. Inventoried from the shipped cells
  (every alias-qualified symbol in resources/cells/*.clj) — cells-sci-test
  loads them all against this table, so a cell that needs more fails there."
  '{;; JSON text <-> data. Pure.
    clojure.data.json
    {:vars [read-str write-str]}

    ;; Log lines. Exposed as functions over print-str (the macros they shadow take
    ;; the same arguments); logging is not a capability.
    clojure.tools.logging
    {:vars [debug info warn error]}

    ;; Structured concurrency: `join` runs tasks and `?` parks on one (exposed as
    ;; ebb's run-task, which the `?` macro expands to). A task is a cell's own
    ;; function; it gains nothing it did not have.
    ebb.core
    {:vars [? join]
     :effects #{:run}}

    ;; Registering a cell is what a cell file is FOR. Exposed through a shim that
    ;; unwraps SCI's error wrapper at the handler boundary.
    mycelium.cell
    {:vars [defcell]}

    ;; Runs a compiled sub-workflow — more cells, under the same list.
    mycelium.core
    {:vars [run-compiled]
     :effects #{:run}}

    ;; Acceptance criteria. Spawns nothing itself: `check` runs each :check
    ;; criterion through the :run-check fn the CALLER passes, and a cell has only
    ;; verify/run-configured to pass.
    samizdat.agent.acceptance
    {:vars [all-passed? check failed normalize table]}

    ;; The beam driver's steps. Each takes branches through turns, which reach
    ;; tools only through the tool path's policy and approval.
    samizdat.agent.beam
    {:vars [advance-all await-resume! drain-directives! ensure-scored finish-now?
            record-inactive! round-max-turns select-done-branch spawn-children! spent-tokens]
     :effects #{:net :db :run}}

    ;; Context-window arithmetic over message vectors. Pure.
    samizdat.agent.compaction
    {:vars [aggressive? apply-summary cap-oversized-results compress-window estimate-tokens
            fold-pays? fold-task fold? pressure prune-tool-outputs result-cap
            snip-bought-enough? summary-budget task-span tier turn-range unpinned-in
            validate-summary]}

    ;; Scoring over branch maps. Pure.
    samizdat.agent.critic
    {:vars [dominated? fitness-objective survival-objectives]}

    ;; Prompt text, reply parsing, and the decomposition solver. Pure.
    samizdat.agent.decompose
    {:vars [architect-prompt parse-decision solve]}

    ;; Reads the Clojure sources among `paths` under `root`, through
    ;; resolve-under-root (an escaping path is skipped). Read-only and Clojure
    ;; files only; the code-quality gate's input.
    samizdat.agent.files
    {:vars [read-sources]
     :effects #{:fs}}

    ;; Reads of the policy tables. Pure lookups.
    samizdat.agent.gates
    {:vars [threshold tool-vocab]}

    ;; Fixed git subcommands (stash create, write-tree, diff, ls-files) in `root`,
    ;; under the scrubbed environment. No caller-chosen command; a baseline that
    ;; looks like an option is refused (gitdiff/rev?).
    samizdat.agent.gitdiff
    {:vars [baseline changed-files diff max-diff-chars]
     :effects #{:proc}}

    ;; The probe's model calls and its journal line.
    samizdat.agent.infer
    {:vars [ab complete-fn log-probe! of-branch trampoline]
     :effects #{:net :db}}

    ;; Which instruction files a branch owes a read. Pure over the branch.
    samizdat.agent.instructions
    {:vars [pending pin-message touched-paths]}

    ;; The reviewer and critic: prompt text, parsing, and (review, review-plan,
    ;; review-rubric) a provider call.
    samizdat.agent.judge
    {:vars [blocking-findings critic-prompt critique-message deterministic-block evidence
            findings focus-sources focused-diff for-the-record parse-criteria parse-verdict
            parse-yesno review review-plan review-rubric section-bullets yesno-prompt]
     :effects #{:net}}

    ;; A run's in-memory model overrides (a supervisor's /model switch).
    samizdat.agent.live
    {:vars [set!]}

    ;; The turn's steps. `tool-step` runs the model's tool calls through the SAME
    ;; policy, approval and confinement as any tool call — the tool path is how a
    ;; cell reaches the machine, never around it.
    samizdat.agent.loop
    {:vars [absorb-response call-model initial-messages journal-step! no-call-step phase-valve
            provider-error-step settle-step steer-step tool-step]
     :effects #{:net :db :run}}

    ;; The phase table's withheld tools. Pure lookup.
    samizdat.agent.phases
    {:vars [withholds]}

    ;; Plan prompt text and parsing. Pure.
    samizdat.agent.planner
    {:vars [default-max-parts parse-plan plan-prompt]}

    ;; Distils a finished task into a lesson: a model call and a store write.
    samizdat.agent.reflect
    {:vars [distil-task!]
     :effects #{:net :db}}

    ;; A skill's body BY NAME, from the discovered skill set — a lookup, not a
    ;; path.
    samizdat.agent.skills
    {:vars [load-skill]
     :effects #{:fs}}

    ;; Branch-map functions. Pure.
    samizdat.agent.state
    {:vars [active? add-message finish-planning planned? unwritten banked-in-last branch-id-for build-residual-report
            confirmed-artifacts new-branch own-turn-count parked? plan reframe-active?
            render-residual-report residual]}

    ;; A read-only digest of the run's own record.
    samizdat.agent.telemetry
    {:vars [digest]
     :effects #{:db}}

    ;; The tool vocabulary. Pure.
    samizdat.agent.tools
    {:vars [tool-names]}

    ;; Whether the phase table withholds a tool. Pure.
    samizdat.agent.tools.base
    {:vars [phase-refusal]}

    ;; A task row as prose. Pure.
    samizdat.agent.tools.tasks
    {:vars [task-statement]}

    ;; run-configured runs a command ONLY when it is one the run was sealed with
    ;; at start (its :verify-cmd or an :acceptance :check, from config the agent
    ;; cannot write — policy protected-paths), in the sealed root, scrubbed env,
    ;; output redacted. This replaced a raw engine.proc/run in cells/feature.clj.
    ;; unrun-tests reads test file names under root.
    samizdat.agent.verify
    {:vars [run-configured unrun-tests]
     :effects #{:proc :fs}}

    ;; Asks a person and waits. Widens nothing by itself.
    samizdat.approval
    {:vars [await! policy request!]
     :effects #{:db}}

    ;; Cancellation: whether an exception is the run being stopped, and a
    ;; cancellable fiber for a cell's own function.
    samizdat.cancel
    {:vars [control-signal? spawn]
     :effects #{:run}}

    ;; Resolves a role's provider settings from the config map. Pure.
    samizdat.config
    {:vars [provider-llm role-llm]}

    ;; Reads a policy table. Pure lookup.
    samizdat.lexicon
    {:vars [policy]}

    ;; A provider call with the run's adapter — the cells' main job.
    samizdat.llm.client
    {:vars [chat]
     :effects #{:net}}

    ;; String repair. Pure.
    samizdat.llm.fence
    {:vars [close-unbalanced fill-dangling-key repair-control-chars strip-trailing-commas]}

    ;; The adapter for a provider config. Constructs, calls nothing.
    samizdat.llm.registry
    {:vars [adapter-for]}

    ;; Code metrics over source text a cell already holds. Pure.
    samizdat.metrics
    {:vars [review]}

    ;; Prompt templates by NAME, rendered. Read-only lookups.
    samizdat.prompt
    {:vars [prompt render render-str]}

    ;; The session's findings and observations, through its own functions.
    samizdat.session
    {:vars [branch-fitness branch-fitnesses experiments findings mark! observe! render
            run-window]
     :effects #{:db}}

    ;; Recent failures, read.
    samizdat.store.failures
    {:vars [recent]
     :effects #{:db}}

    ;; The supervisor's queued directives: read, and resolved once applied.
    samizdat.store.interventions
    {:vars [payload pending resolve! text-of turns-asked workflow-kinds]
     :effects #{:db}}

    ;; The run journal: reads, and appending a note or a side call's usage.
    samizdat.store.journal
    {:vars [branch-turns gate-firings gate-tally last-note last-turn note! notes notes-since
            record-side-call! retirement-candidates run-usage turns]
     :effects #{:db}}

    ;; The project's lessons: read, and recording one.
    samizdat.store.knowledge
    {:vars [graduation-candidates remember! standing]
     :effects #{:db}}

    ;; Run and branch rows, through the store's own functions. get-branch replaced
    ;; a raw SELECT in cells/board.clj.
    samizdat.store.runs
    {:vars [close-branch! extend-budget! finish-run! get-branch nth-recent-start open-branch!]
     :effects #{:db}}

    ;; The task board, through its own functions.
    samizdat.store.tasks
    {:vars [attempted! board children-of claim! close! create! get-task held-by release! update!]
     :effects #{:db}}

    ;; Read-only reports over the project's userspace history.
    samizdat.store.userspace
    {:vars [drift pruning-candidates]
     :effects #{:db}}

    ;; Read-only reports: pending template offers and rejected edits.
    samizdat.userspace
    {:vars [offers prescription-mass rejections]}

    ;; Compiled manifests (under this same list), role prompt text, and a role's
    ;; provider settings.
    samizdat.workflow
    {:vars [compiled-manifest prompt-text render-catalog role-ctx worker-compiled]
     :effects #{:run}}})

(def form-api
  "Namespace -> the vars the policy tables' forms may call. Smaller than
  `cell-api`: a :when is a predicate over the branch, and nothing it names
  writes. Inventoried from resources/gates.edn and resources/phases.edn."
  '{samizdat.agent.checklist {:vars [refusal]}
    samizdat.agent.exam {:vars [refusal]}
    samizdat.agent.files {:vars [large-shell-print? large-untargeted-read?]}
    samizdat.agent.gates {:vars [digest-policy message-context prompt storm-policy threshold
                                 tool-vocab]}
    samizdat.agent.roles {:vars [may-use?]}
    samizdat.agent.state {:vars [active? confirmed-artifacts confirmed-in-last
                                 has-relevant-confirmed? last-failure orienting-too-long?
                                 own-turn-count plan-stale? planned? planning?
                                 safe-state-due? stated-goal turn-count unwritten]}
    samizdat.agent.storm {:vars [oscillation-blocked? repeat-blocked?]}
    samizdat.agent.supervisor {:vars [over-studying? repeating-one-failure? stall-nudge
                                      unchanged-failure]}
    samizdat.agent.tools.ship {:vars [asks-the-reader? engages-problem? unfinished-claim?]}
    samizdat.prompt {:vars [render-str]}})

(def form-homes
  "Where each table's forms are compiled: which `form-api` vars they may name
  bare (`:refer`) and which aliases they may use. The names are the ones the
  forms were written against when they were compiled in the namespace that
  reads them — gates forms in samizdat.agent.gates, the ship rungs in
  samizdat.agent.tools.ship — so the shipped tables read unchanged."
  '{:gates {:refer {samizdat.agent.gates [message-context prompt threshold tool-vocab]}
            :aliases {state samizdat.agent.state
                      supervisor samizdat.agent.supervisor
                      roles samizdat.agent.roles
                      sp samizdat.prompt
                      str clojure.string}}
    :ship {:refer {samizdat.agent.tools.ship [asks-the-reader? engages-problem? unfinished-claim?]}
           :aliases {checklist samizdat.agent.checklist
                     exam samizdat.agent.exam
                     str clojure.string}}
    ;; phases.edn names everything fully qualified.
    :phases {}
    :state {:refer {samizdat.agent.state [confirmed-artifacts]}}
    ;; A manifest's (fn [d] ...) dispatch: clojure.core over the data map.
    :dispatch {}})

;; --- exposing a var -----------------------------------------------------------

(defn- live
  "The value SCI sees for host var `v`: a function that derefs it at call
  time, or its value when it is not a function."
  [v]
  (let [x @v]
    (if (fn? x)
      (fn [& args] (apply @v args))
      x)))

(defn- log-fn [level]
  (fn [& args]
    (case level
      :debug (log/debug (apply print-str args))
      :info (log/info (apply print-str args))
      :warn (log/warn (apply print-str args))
      :error (log/error (apply print-str args)))
    nil))

(defn unwrap
  "The exception a native caller should see for `e` thrown out of SCI code:
  SCI wraps a runtime error in an ex-info of :type :sci/error carrying the
  original as its cause, and the harness dispatches on the original —
  cancel/control-signal?, mycelium's error routing, a test's ex-data."
  [e]
  (loop [e e]
    (let [c (ex-cause e)]
      (if (and c (= :sci/error (:type (ex-data e))))
        (recur c)
        e))))

(defn- defcell-shim
  "mycelium.cell/defcell, with the handler wrapped so an exception leaves it
  as the handler threw it rather than in SCI's wrapper."
  [cell-id opts handler]
  (cell/defcell cell-id opts
    (fn [& args]
      (try (apply handler args)
           (catch Throwable e (throw (unwrap e)))))))

(def ^:private shims
  "Entries that are not a plain var: a macro exposed as the function it
  expands to, or a wrapper."
  {'clojure.tools.logging {'debug (log-fn :debug) 'info (log-fn :info)
                           'warn (log-fn :warn) 'error (log-fn :error)}
   'ebb.core {'? (fn [task] ((requiring-resolve 'ebb.core/run-task) task))}
   'mycelium.cell {'defcell defcell-shim}})

(defn- expose
  "{var-sym value} for `api`'s entry for `ns-sym`. Requires the namespace
  first — the allowlist is data, so nothing here depends on a cell's
  namespaces at compile time (several of them require this one). A listed
  var that does not exist is an error: the table has drifted from the code."
  [ns-sym vars]
  (let [shim (get shims ns-sym)]
    (when-not (every? #(contains? shim %) vars)
      (require ns-sym))
    (into {}
          (for [v vars]
            [v (or (get shim v)
                   (if-let [hv (ns-resolve (the-ns ns-sym) v)]
                     (live hv)
                     (throw (ex-info (str "sandbox allowlist names " ns-sym "/" v
                                          ", which does not exist")
                                     {:ns ns-sym :var v}))))]))))

(defn- namespaces-for [api]
  (into {} (for [[ns-sym {:keys [vars]}] api] [ns-sym (expose ns-sym vars)])))

(def ^:private classes
  "Host classes code in the sandbox may name: exception types, for `catch`,
  `throw` and `instance?`, and nothing that does IO. SCI refuses a method
  call on any class not listed (its default set is strings, numbers and
  exceptions), so an object a cell is handed — a connection, an adapter —
  cannot be driven by interop."
  {'java.lang.Throwable Throwable
   'java.lang.Exception Exception
   'java.lang.RuntimeException RuntimeException
   'java.lang.IllegalArgumentException IllegalArgumentException
   'java.lang.IllegalStateException IllegalStateException
   'java.lang.NumberFormatException NumberFormatException
   'java.lang.ArithmeticException ArithmeticException
   'clojure.lang.ExceptionInfo clojure.lang.ExceptionInfo})

(def ^:private imports
  "The short names those classes go by, as in a host namespace."
  (into {} (for [full (keys classes)]
             [(symbol (last (str/split (str full) #"\."))) full])))

(def ^:private core-additions
  "Pure clojure.core functions (Clojure 1.11 / 1.12) the SCI fork's core map
  never lists, although jolt has every one — handed to sci/init here. Cells
  loaded with load-string had them, so a cell that parses a number with
  parse-long must still load. None touches anything outside its arguments."
  '[abs infinite? NaN? iteration parse-boolean parse-double parse-long parse-uuid
    partitionv partitionv-all splitv-at random-uuid update-keys update-vals])

(defn- with-core-additions [nss]
  (assoc nss 'clojure.core
         (into {} (for [s core-additions
                        :let [v (ns-resolve 'clojure.core s)]
                        :when v]
                    [s @v]))))

(def ^:private cell-namespaces (delay (with-core-additions (namespaces-for cell-api))))
(def ^:private form-namespaces (delay (with-core-additions (namespaces-for form-api))))

;; --- cells ---------------------------------------------------------------------

(defn cell-context
  "A fresh SCI context holding the cell allowlist. A fresh one per load: the
  cells of one load share it (a cell may require another cell's namespace
  loaded before it), and a load that fails is dropped whole — its half-made
  definitions never reach the handlers of the load before it."
  []
  (sci/init {:namespaces @cell-namespaces :classes classes :imports imports}))

(defn- outside-name
  "The namespace or qualified symbol an SCI resolution failure names when it
  is one the HARNESS has — a reach past the allowlist rather than a typo."
  [msg]
  (let [[_ ns-name] (re-find #"Could not find namespace ([\w.\-]+)" (str msg))
        [_ sym] (re-find #"Unable to resolve symbol: ([\w.\-]+/[^\s]+)" (str msg))]
    (cond
      ;; SCI ends the sentence with a period, which the capture takes.
      (and ns-name (find-ns (symbol (str/replace ns-name #"\.$" ""))))
      (str/replace ns-name #"\.$" "")
      sym sym)))

(defn- outside-message
  "SCI's message for `msg`, and when it names something the harness has, what
  that means. SCI says `Could not find namespace samizdat.store.db.` of a
  namespace that is right there on disk; a live supervisor read it as a
  missing file and spent thirty turns looking for one, never reaching the
  offered update that was the fix."
  [msg]
  (if-let [n (outside-name msg)]
    (try (prompt/render "cell-sandbox-refused" {:reason (str msg) :name n})
         (catch Throwable _ msg))
    msg))

(defn eval-source!
  "Evaluate cell source `content` in `ctx`. Throws an ex-info whose message
  names what failed — for a reach outside the allowlist, the symbol or
  namespace SCI could not resolve — with :line/:column in its data when SCI
  has them (userspace/problem reads them from there)."
  [ctx content]
  (try
    (sci/eval-string* ctx content)
    nil
    (catch Throwable e
      (let [e (if (= :sci/error (:type (ex-data e))) e (unwrap e))]
        (throw (ex-info (outside-message (ex-message e))
                        (merge (select-keys (ex-data e) [:line :column])
                               {:sandbox :sci})
                        e))))))

(defn var-in
  "The SCI var `ns-sym`/`sym` in `ctx`, private or not, or nil. Deref it for
  the value; a function value is callable natively."
  [ctx ns-sym sym]
  (when-let [n (sci/find-ns ctx ns-sym)]
    (let [v (get (sci/eval-form ctx (list 'ns-interns (list 'quote ns-sym))) sym)]
      (when (and v n) v))))

;; --- policy forms --------------------------------------------------------------

(defn- form-context [home]
  (let [{:keys [refer aliases]} (get form-homes home)
        nss @form-namespaces
        user (into {} (for [[ns-sym vars] refer v vars]
                        [v (get-in nss [ns-sym v])]))
        ctx (sci/init {:namespaces (assoc nss 'user user) :classes classes :imports imports})]
    (doseq [[a n] aliases]
      (sci/eval-form ctx (list 'require (list 'quote [n :as a]))))
    ctx))

(def ^:private form-contexts
  (memoize (fn [home]
             (when-not (contains? form-homes home)
               (throw (ex-info (str "no sandbox home " home) {:home home})))
             (form-context home))))

(defn form-fn
  "Compile a policy-table form in `home`'s context (a key of `form-homes`)
  into a function native code calls. With `params`, `body` is the fn's body
  — `(form-fn :gates '[ctx] body)` is `(fn [ctx] body)`; with nil, `body`
  must itself evaluate to a function. Throws, naming the symbol, when it
  reaches for anything the home does not hold."
  [home params body]
  (let [ctx (sci/fork (form-contexts home))
        form (if params (list 'fn params body) body)]
    (try
      (let [f (sci/eval-form ctx form)]
        (fn [& args]
          (try (apply f args)
               (catch Throwable e (throw (unwrap e))))))
      (catch Throwable e
        (throw (ex-info (ex-message e) {:home home :form body :sandbox :sci} e))))))

(defn unresolved
  "The symbols in `form` that `home` cannot resolve — every one, not just the
  first, for a refusal that lists what to fix. `locals` are the names the
  caller binds around the form (and the ones the form binds itself)."
  [home form locals]
  (let [ctx (form-contexts home)]
    (->> (tree-seq coll? seq form)
         (filter symbol?)
         (remove #(or (special-symbol? %) (contains? locals %) (= '& %)
                      (str/starts-with? (name %) ".")
                      (str/ends-with? (name %) ".")
                      (some? (try (sci/resolve ctx %) (catch Throwable _ nil)))))
         distinct
         sort
         vec)))
