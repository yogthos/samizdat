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

(ns samizdat.workflow
  "The loop definition's lifecycle: read from the db, compile through
  mycelium's checks, drive a run.

  This ns is the seam the mutation protocol (karamazov-ioo.11) grows on: an
  agent edit is a us/save! :manifest followed by the same compile-loop call the
  driver makes, and a failed compile means the previous version keeps
  driving. Activation is serialized by construction — each run loads and
  compiles once, at start.

  The beam drives this manifest too (karamazov-ioo.20, done): it compiles the
  per-turn SLICE of the run's loop — `turn-manifest` below — and runs one
  branch through it per scheduling round, owning the scheduling, culling,
  forking and finishing the manifest's :finish would otherwise do for a single
  branch. So there is one driver and one definition of a turn.

  It was not always so, and the gap was invisible: the beam called
  samizdat.agent.loop's steps directly, `run!` here was reached only from
  tests, and `:run :loop` was documented in config, parsed from HARNESS_LOOP,
  and read by nothing on the production path — the critic, team, feature and
  decompose manifests could not run outside the suite. `run!` remains as the
  single-branch driver a role's sub-loop uses (see `compiled-manifest`)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [mycelium.core :as myc]
            [mycelium.cell :as cell]
            [mycelium.compose :as compose]
            [mycelium.workflow :as wf]
            [samizdat.cells :as cells]
            [samizdat.config :as config]
            [samizdat.manifests :as manifests]
            [samizdat.llm.registry :as registry]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.loop :as branch-loop]
            [samizdat.repl :as repl]
            [samizdat.repl.route :as route]
            [samizdat.session :as session]
            [samizdat.watch :as watch]
            [samizdat.userspace :as userspace]
            [samizdat.agent.state :as state]
            [samizdat.store.journal :as journal]
            [samizdat.store.knowledge :as knowledge]
            [samizdat.store.runs :as runs]
            [samizdat.store.userspace :as us])
  (:refer-clojure :exclude [run!]))

(def loop-name "loop")
(def loop-resource "manifests/loop.edn")

;; Reading, validating and compiling a manifest moved to samizdat.manifests
;; (karamazov-blt.2/.3/.4/.6): the tools could not require THIS namespace (it
;; requires the branch loop, which requires the tool dispatcher), so each
;; re-implemented a slice of the pipeline and the slices drifted. The vars
;; below delegate so every existing caller and test keeps its name.
(def manifest-resource manifests/manifest-resource)

(defn active-loop-name
  "Which manifest a run should drive, in precedence order: the name the caller
  configured, then what selection chose, then the factory default.
  HARNESS_LOOP or a project's .samizdat/config.edn set :run :loop.

  THE CONFIGURED NAME ALWAYS WINS. `selected` is what samizdat.agent.select
  picked from the catalogue for a run that named no workflow of its own; a run
  that did name one is never overridden, because a caller who pinned a loop
  asked a question this has no business re-answering.

  Kept as one function with the precedence in it — rather than resolved at the
  call site — because it is the ONLY place a run decides what drives it, and
  that is worth having somewhere a reader can find."
  ([config] (active-loop-name config nil))
  ([config selected]
   (or (get-in config [:run :loop])
       selected
       loop-name)))

(def read-definition manifests/read-definition)
(def register-subworkflows! manifests/register-subworkflows!)

(def ctx-keys manifests/ctx-keys)
(def cell-requires manifests/cell-requires)
(def invariants manifests/invariants)
(def enforced-constraints manifests/enforced-constraints)
(def unenforced-invariants manifests/unenforced-invariants)

(def compile-loop manifests/compile-loop)

(defn load-loop!
  "The loop to drive a run: seed its factory resource on first use (if it has
  one), then load and compile the latest stored version. Named manifests let a
  sophisticated loop live in the workflows table beside the default; a name
  with no resource and no stored version is an error. Returns {:name :version
  :definition :compiled}."
  ([conn] (load-loop! conn loop-name))
  ([conn name]
   (let [res (manifest-resource name)
         row (if-let [r (io/resource res)]
               (us/seed! conn :manifest name (slurp r))
               (us/load-latest conn :manifest name))]
     (when-not row
       (throw (ex-info (str "no loop manifest named '" name
                            "' — no resource at " res " and nothing stored")
                       {:name name})))
     ;; `:body`, not `:edn`. store/workflows.clj used to rename the column on
     ;; the way out; reading the raw userspace row means the key is what the
     ;; table calls it. Read once and compiled from the same value, so a
     ;; caller cannot get a definition and a compiled FSM built from different
     ;; text.
     (let [definition (read-definition (:body row))]
       {:name name
        :version (:version row)
        :definition definition
        :compiled (compile-loop definition)}))))

;; --- the per-turn slice, for the beam ---------------------------------------
;;
;; A loop manifest describes a WHOLE run: the per-turn chain, a back edge from
;; :route to :start, and a :finish that closes the branch and run rows. The
;; beam needs the per-turn chain alone — it owns scheduling, culling, forking
;; and finishing across many branches, and a driver that runs one branch to
;; completion cannot be scheduled against four others.
;;
;; Rather than maintain a second per-turn manifest per loop (two files to keep
;; in agreement, which is how the two drivers drifted apart in the first
;; place), the slice is DERIVED: every edge that would loop back to the start
;; node or hand off to :loop/finish instead goes to :end. What remains is one
;; turn, and the beam does the rest. The cells, the dispatches and the
;; constraints are untouched, so a manifest edit reaches both drivers.

(def start-node manifests/start-node)
(def finish-nodes manifests/finish-nodes)
(def iterating? manifests/iterating?)
(def turn-manifest manifests/turn-manifest)

(defn compile-turn-loop
  "Load the named manifest and compile BOTH forms: the whole-run definition
  (for provenance and for `iterating?`) and its per-turn slice, which is what
  the beam drives. Returns {:name :version :definition :iterating? :compiled}."
  [conn name]
  (let [{:keys [version definition]} (load-loop! conn name)]
    {:name name
     :version version
     :definition definition
     :iterating? (iterating? definition)
     :compiled (compile-loop (turn-manifest definition))}))

(def compiled-manifest manifests/compiled-manifest)

(defn worker-compiled
  "The worker sub-loop, compiled — for a team cell that runs a worker per
  sub-task, each on its own branch. Compiled fresh (cells may have changed);
  the caller runs it N times."
  []
  (compiled-manifest "worker"))

(defn prompt-text
  "The text of a named prompt resource (resources/prompts/<name>.md), or nil if
  there is no such resource. The shared reader behind manifest :prompt injection
  and the team-worker roster."
  [name]
  ;; Through the userspace seam: a workflow's prompt is this project's prompt.
  ;; nil-tolerant, unlike prompt/prompt — a manifest declaring no :prompt and a
  ;; :prompt naming nothing are both "no suffix", not errors.
  (userspace/body :prompt name))

(def catalog manifests/catalog)

(defn render-catalog
  "The workflow catalog as a text menu — one `- name — description` line each —
  for injecting into the supervisor's context."
  [conn]
  (str/join "\n" (for [{:keys [name description]} (catalog conn)]
                   (str "- " name (when (seq description) (str " — " description))))))

(defn workflow-prompt
  "A manifest may declare `:prompt <name>`, naming a prompt resource
  (resources/prompts/<name>.md) that is appended to the base system prompt for
  that workflow — how a workflow injects its own instructions at the start. A
  review manifest points at review guidance; the default loop declares none and
  runs the base prompt. Returns the text, or nil."
  [definition]
  (when-let [p (:prompt definition)]
    (prompt-text p)))

(defn role-ctx
  "The ctx a role's sub-loop runs under, with its LLM adapter and config swapped
  to the model assigned to `role` under config :run :role-models — e.g.
  {:implementor {:provider \"deepseek\"} :supervisor {:provider \"glm\"}}. A role
  with no entry keeps the run's default model. `:provider` may be omitted to keep
  the run's provider and only change the model. This is how a cheap model can
  implement while a stronger one reviews or supervises."
  [ctx role]
  ;; :role rides the ctx from here on. It used to be consumed by prompt
  ;; assembly and dropped, which left the tool layer unable to tell a
  ;; supervisor from an implementor — and WHICH IMAGE AN EVAL LANDS IN is
  ;; exactly that question (samizdat.repl.route). A ctx with no role gets the
  ;; project image, which is the safe direction.
  (let [ctx (assoc ctx :role role)]
    (if-let [spec (get-in (:config ctx) [:run :role-models role])]
      (let [provider (or (some-> (:provider spec) name str/lower-case keyword)
                         (:provider (:llm-config ctx)))
            llm (config/provider-llm provider (dissoc spec :provider))]
        (assoc ctx :llm-adapter (registry/adapter-for provider) :llm-config llm))
      ctx)))

(defn note-schema-warnings!
  "Record any :mycelium/warnings the pass accumulated, and return `data`.

  Under gates.edn :schema-validation :warn a cell whose data does not match
  its declared shape leaves a warning on the data map and the run carries on.
  That is the right cost while the declarations are still being tightened, and
  it is worthless if nobody ever reads them — a warning nothing records is the
  same as :off with extra steps.

  Each warning carries :key-diff {:missing :extra}, which is what makes the
  row worth keeping: it names the rename rather than reporting that something
  somewhere did not match.

  Best effort and returns its input either way, so it can sit in a threading
  position on the run path. A journal that refuses must not fail a run that
  otherwise worked — the same rule compaction's note! follows."
  [{:keys [conn run-id]} data]
  (when-let [warnings (seq (:mycelium/warnings data))]
    (log/warn "schema warnings this pass:" (pr-str warnings))
    (when (and conn run-id)
      (try
        (journal/note! conn run-id :schema-warning {:data {:warnings (vec warnings)}})
        (catch Throwable e
          (log/warn "recording the schema warnings failed:" (ex-message e))))))
  data)

(defn run-turn
  "Advance one branch by one turn, through the manifest.

  THE ONE DEFINITION OF A TURN. samizdat.agent.loop composed the same steps in
  compiled Clojure until this replaced it, which meant there were two
  definitions and an edit to the loop manifest reached only one of them. That
  is the drift karamazov-ioo.20 found the first time — the beam called the
  compiled composition while the manifest driver ran the same steps as cells,
  and nothing in the production path ever reached the manifest, so four
  workflows existed only under the test suite. Unifying the call site left the
  duplicate standing; this removes it.

  Lives here rather than in samizdat.agent.loop because a turn is now defined
  by a manifest, and loading a manifest is this namespace's job — agent.loop
  cannot require it without a cycle.

  For a caller that wants one turn rather than a whole run: the benches, and
  the tests that assert what a single turn does to a branch. Compiles the named
  manifest's per-turn slice fresh, so a cell or manifest edit is picked up."
  ([ctx branch turn] (run-turn ctx branch turn loop-name))
  ([ctx branch turn manifest-name]
   ;; Through the userspace seam: the stored version when the project has one,
   ;; seeding the factory template on the way past — and a manifest the agent
   ;; authored, which has no factory resource at all, drives a turn too
   ;; (io/resource slurped unconditionally here and NPE'd on a store-only
   ;; name, karamazov-blt.38).
   (let [wf (compile-loop
             (turn-manifest
              (read-definition (manifests/manifest-body! manifest-name))))
         data (note-schema-warnings!
               ctx (myc/run-compiled wf ctx {:branch branch :turn turn}))]
     (when (myc/error? data)
       (throw (ex-info "the turn manifest failed structurally"
                       {:error (myc/workflow-error data)})))
     (:branch data))))

(defn run!
  "Run one branch to completion under the stored loop definition.
  Returns {:status :answer :branch :run-id (:residual)}."
  [{:keys [conn config llm-adapter llm-config problem max-turns]}]
  (let [max-turns (or max-turns (get-in config [:run :max-turns]) 40)
        loop-nm (active-loop-name config)
        {:keys [version compiled definition]} (load-loop! conn loop-nm)
        run-id (runs/start-run! conn {:problem problem
                                      :provider (:provider llm-config)
                                      :model (:model llm-config)
                                      :max-turns max-turns
                                      :beam-width 1
                                      :prompt-digest (branch-loop/prompt-digest)})
        branch (state/new-branch {:id "B1" :problem problem
                                  :messages (branch-loop/initial-messages
                                             problem (workflow-prompt definition))})
        ;; The project root the file tools are confined to, and the shell tool
        ;; runs in. Configurable so a run can target another checkout.
        root (or (get-in config [:run :root]) (System/getProperty "user.dir"))
        ;; Make the project's own namespaces requirable from `eval` before any
        ;; branch takes a turn. The system prompt's whole first section is
        ;; REPL-first against the project under work, and without this that
        ;; instruction is unreachable the moment :run :root is not the harness.
        _ (repl/ensure-project-roots! root)
        ctx {:conn conn :run-id run-id :config config
             :llm-adapter llm-adapter :llm-config llm-config
             :root root
             ;; A run-start git baseline: what this run changed, for a
             ;; finalization critic AND — the part this used to miss — for the
             ;; ship gate's test rung.
             ;;
             ;; This was `(when (not= loop-nm loop-name) …)`, on the reasoning
             ;; that the factory loop has no critic to read it and skipping it
             ;; keeps the common path off git entirely. That was true when the
             ;; critic was the only reader. The ship gate reads it too now, via
             ;; changed-files, and with no baseline `changed` is nil, no focused
             ;; command is built, no tests run, and verify-block falls through
             ;; its last clause — which trusts rather than deadlocks. So the
             ;; default loop verified NOTHING while reporting a successful ship:
             ;; observed live, a run that shipped `{:test 19 :pass 49 :error 5}`
             ;; with the gate silently inert.
             ;;
             ;; Captured whenever anything downstream could use it.
             :git-baseline (when (or (not= loop-nm loop-name)
                                     (get-in config [:run :verify-focused?])
                                     (not (str/blank? (str (get-in config [:run :verify-cmd])))))
                             (gitdiff/baseline root))
             ;; A per-run eval session, so defs the agent makes with `eval`
             ;; persist across its turns (define, then use) — REPL-first
             ;; development against the live image.
             :repl-session (repl/new-session)
             :max-turns max-turns}]
    (runs/open-branch! conn run-id {:branch-id "B1"})
    ;; The window findings are evaluated over.
    (session/mark-run! run-id)
    ;; The single-branch driver drains the same interventions queue the beam
    ;; does (loop/drain-directives!), so the watcher works here unchanged.

    ;; Which loop drove this run, durably: an agent reading a surprising run
    ;; back needs to know which version of itself produced it.
    (journal/note! conn run-id :loop-workflow
                   {:data {:name loop-nm :version version}})
    (let [stop-watch (watch/start! ctx)]
     (try
      (let [data (note-schema-warnings!
                  ctx
                  (myc/run-compiled compiled ctx
                                   (cond-> {:branch branch :turn 1}
                                     ;; A team workflow fans out over these — one
                                     ;; worker per sub-task. The single-branch
                                     ;; loops ignore the key.
                                     (seq (get-in config [:run :subtasks]))
                                     (assoc :subtasks (get-in config [:run :subtasks])))))]
        (when (myc/error? data)
          ;; A structural failure mid-run is a harness bug, not a branch
          ;; outcome; surface it rather than shipping a half-closed run.
          (throw (ex-info "loop workflow failed structurally"
                          {:run-id run-id :error (myc/workflow-error data)})))
        (-> (select-keys data [:status :answer :branch :residual])
            (assoc :run-id run-id)))
      (finally
        (stop-watch)
        ;; SHORT-TERM BECOMES LONG-TERM, here too. This driver runs the factory
        ;; loop, which is what most runs use; distilling only in the beam meant
        ;; the common path measured everything and remembered none of it.
        (try
          (knowledge/distil-session! conn {:run-id run-id
                                           :findings (session/findings
                                                            ;; THIS RUN's window, not the
                                                            ;; whole-process tally: counters
                                                            ;; never reset between runs, so
                                                            ;; run 1's parse-error rate kept
                                                            ;; "corroborating" a finding at
                                                            ;; the end of clean runs 2..n,
                                                            ;; each with a distinct run-id
                                                            ;; that defeated the guard
                                                            ;; (karamazov-blt.24).
                                                            (session/run-window run-id))
                                           :experiments (session/experiments)})
          (catch Throwable e
            (log/warn "distilling the session failed:" (ex-message e))))
        ;; The run's eval namespace does not outlive the run
        ;; (provenance CR1-6): one namespace per run, never removed, was
        ;; unbounded growth on a serve process.
        (repl/close-session (:repl-session ctx))
        ;; The project image outlives a session, because it is a PROCESS. Left
        ;; running it holds a port and a sandbox for the life of the harness.
        (route/release! (:root ctx)))))))
