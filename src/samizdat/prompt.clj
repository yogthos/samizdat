;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.prompt
  "Selmer is the template engine for every prompt seam — the gates' message
  files and suffixes, the beam's steer prose, the loop's valve message, the
  domain prompts (critic, judge, architect). Template files keep the
  {{...}} spelling the hand-rolled str/replace chains used; the move changed
  the renderer, not the templates.

  Escaping is OFF, globally, here: a prompt full of code must not have
  < > & turned into entities, and unlike an HTML page there is no injection
  surface to defend — the output feeds a model, not a browser. One semantic
  difference from str/replace chains, accepted: a missing key renders empty
  instead of surfacing a literal {{...}} to the model."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [selmer.parser :as selmer]
            [selmer.util :as selmer-util]
            [samizdat.userspace :as userspace]))

(selmer-util/turn-off-escaping!)

(def shipped-prompts
  "Every prompt name the harness ships, ENUMERATED not globbed.

  Same reason as `cells/shipped-cells`: `jolt build` bakes resources/ into the
  binary, and an embedded resource has no filesystem path for a glob to walk —
  a built binary run outside the project root would find nothing and report
  that the harness has no prompts. `prompt-test` pins this against the
  directory, so it cannot drift on its own.

  Used by the `prompt` tool to list what a project can edit. Reading one still
  goes through the userspace seam, which is what decides whether the project's
  version or the template answers."
  [
   "architect"
   "branch-cap"
   "branch-out"
   "cell-shadowed"
   "cell-tool"
   "compaction-marker"
   "compaction-summary"
   "critic"
   "critic-system"
   "crossover"
   "cull-reprieve"
   "directive-refused"
   "directive-rejected"
   "dispatch-order"
   "dispatch-refused"
   "emergency-review"
   "eval-calls-main"
   "eval-error"
   "eval-image"
   "eval-terminates-process"
   "eval-syntax"
   "experiment-tool"
   "explore-cap"
   "failure-log"
   "fetch-turn-miss"
   "file-thrash"
   "clojure-syntax"
   "file-tool"
   "grep-tool"
   "image-denied"
   "image-down"
   "image-off"
   "image-timeout"
   "fork-thesis"
   "kernel-write-refused"
   "judge"
   "judge-exemptions"
   "judge-user"
   "juvenile-grace"
   "last-call"
   "ledger"
   "manual-group"
   "mechanics-streak"
   "memory-tool"
   "memory-stale-completion"
   "memory-unverified"
   "milestone"
   "no-call-imitation"
   "no-edits"
   "outside-role-surface"
   "intervene-tool"
   "oversight-pass"
   "parked"
   "orienting"
   "parse-error-causes"
   "parse-error-repaired"
   "plan-tool"
   "planner"
   "policy-tool"
   "probe-candidates"
   "probe-steer"
   "problem"
   "progress-stalled"
   "plan-not-landed"
   "prologue-cap"
   "prompt-tool"
   "repl-needs-a-plan"
   "repopulate"
   "residual-report"
   "retry-diagnosis"
   "review"
   "roles/implementor"
   "roles/reviewer"
   "roles/supervisor"
   "run-health"
   "safe-state"
   "session-block"
   "shared-artifacts"
   "shared-tree"
   "shell-refused"
   "stale-write"
   "storm"
   "storm-force"
   "storm-oscillation"
   "stuck"
   "suspect-the-test"
   "system"
   "task-busy"
   "task-claimed"
   "task-current"
   "task-none"
   "task-reflection"
   "task-reflection-input"
   "trajectory-judge"
   "task-required"
   "task-tests"
   "thinking-runaway"
   "team-worker"
   "turn-deadline"
   "uncertain-effect"
   "verify-red"
   "workflow-select"
   "workflow-select-system"
   "verify-timeout"
   "verify-unknown"
   "watch-intervention"
   "websearch-tool"
   "wind-down"   ])

(defn prompt
  "The text of prompt `name` for the current project.

  Through the userspace seam rather than straight at io/resource: a prompt is
  userspace like a cell is, so this project's version is what the model sees
  and the shipped file is only where the project started. With no project
  bound (a test, a bare REPL) this is exactly the resource read it always was.

  Fails loud on a name that neither the project nor the harness has — a prompt
  seam rendering an empty string is how a whole instruction block goes missing
  without anyone noticing."
  [name]
  (userspace/body! :prompt name))

;; system.md documents {{env/NAME}} as RUNTIME syntax the shell tool
;; resolves at spawn — it must reach the model verbatim. Selmer would parse
;; it as a nested lookup and render it empty, so the braces are swapped for
;; private-use sentinels around the render and restored after. Values are
;; inserted as nodes and never re-parsed, so only the template text needs
;; the round-trip.
(def ^:private env-open "\uE000env/")
(defn render-str
  "Render an inline template string — the gates' :message-suffix forms."
  [template ctx]
  (-> template
      (str/replace "{{env/" env-open)
      (selmer/render ctx)
      (str/replace env-open "{{env/")))

(defn render
  "Render resources/prompts/<name>.md with selmer against `ctx`."
  [name ctx]
  (render-str (prompt name) ctx))

;; --- prompt chains (LR-7) ----------------------------------------------------
;;
;; Ported from llm-repl's roster/resolve-preamble, MIT licensed, (c) 2026
;; Michael Whitford — full notice in src/samizdat/tape.clj. The rule is
;; FIRST-PRESENT-WINS: a level REPLACES the text rather than adding to it, an
;; absent level inherits from the one below, and a level that is present but
;; blank means explicitly NONE and stops the walk.
;;
;; The chain itself is resources/prompt-chain.edn — data, so which layers exist
;; and in what order is editable at runtime.

(defn chains
  "The declared chains, {layer-key [entry …]}. Read fresh so an edit takes
  effect without a restart; nil when the resource is absent, which callers
  treat as 'no chain declared' rather than an error — a harness with no
  prompt-chain.edn still has its shipped prompts."
  []
  (userspace/edn-body :policy "prompt-chain"))

(defn- entry-value
  "One chain entry's value, or `::absent`.

  `::absent` ≡ inherit from the next level down. A present-but-blank value is
  NOT absent: it is the explicit \"none\", which is the distinction the whole
  trichotomy rests on, and collapsing the two would make it impossible to
  suppress a layer at all."
  [{:keys [project file text] :as entry}]
  (cond
    (contains? entry :project)
    (let [f (io/file project)]
      (if (.exists f) (slurp f) ::absent))

    (contains? entry :file)
    ;; The project's prompt, not the shipped file: a chain level naming a
    ;; prompt must resolve to whatever this project has made of it.
    (or (userspace/body :prompt file) ::absent)

    (contains? entry :text)
    text

    :else
    (throw (ex-info (str "unknown prompt-chain entry — want :project, :file or"
                         " :text") {:entry entry}))))

(defn resolve-chain
  "Walk `entries` and return the first PRESENT value's text, or nil.

  nil means one of two different things, and the caller does not need to tell
  them apart: either a level said \"explicitly none\", or no level was present.
  Both mean this layer contributes no text."
  [entries]
  (reduce (fn [_ entry]
            (let [v (entry-value entry)]
              (if (= ::absent v)
                nil                        ; inherit — keep walking
                (reduced (not-empty (str/trim (str v)))))))
          nil
          entries))

(defn layer
  "The text for the named chain layer, e.g. `(layer :system)`.

  Falls back to the prompt resource of the same name when no chain is declared
  for it, so adding a layer to prompt-chain.edn is opt-in and a harness with no
  chain file behaves exactly as it did before."
  [k]
  (if-let [entries (get (chains) k)]
    (resolve-chain entries)
    (userspace/body :prompt (name k))))
