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
            ;; The java.time.* host shim. selmer.filters imports
            ;; java.time.format.FormatStyle at load, and under jolt 0.8.1 that
            ;; class exists only once jolt.time has installed it — 0.8.0 had it
            ;; implicitly. system.clj loads the shim for tools.logging, which
            ;; is why every path through `system` was fine and the sandbox
            ;; battery (sandbox-test -> repl -> prompt, never touching system)
            ;; was not. The same precedent as db.jdbc before jdbc.core: the
            ;; shim is required where the library that needs it enters.
            [jolt.time]
            [selmer.parser :as selmer]
            [selmer.util :as selmer-util]
            [samizdat.userspace :as userspace]))

(selmer-util/turn-off-escaping!)

(defn shipped-prompts
  "Every prompt role the harness ships, from the shipped role map
  (resources/userspace.edn). What a project HAS is its own map —
  `userspace/roles` — and that is what a lister should read."
  []
  (userspace/shipped-roles :prompt))

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
    ;; Against the bound project root, not the process cwd: a served harness
    ;; with HARNESS_ROOT elsewhere never saw the project's own file. Unbound
    ;; (a test, a bare REPL) the cwd is the root, as it always was.
    (let [f (if-let [r (userspace/project-root)] (io/file r project) (io/file project))]
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

;; A prompt edit is checked before it is what renders (karamazov-1a51.8): the
;; template has to parse. Whether every variable it names is supplied is the
;; caller's business and not knowable from the text alone.
(userspace/register-validator!
 :prompt
 (fn [_ text]
   (try (render-str text {}) nil
        (catch Throwable e (userspace/problem :render e)))))
