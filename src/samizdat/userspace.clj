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

(ns samizdat.userspace
  "THE BASE / USERSPACE SEAM.

  The harness is two layers. `src/` is the BASE: how to talk to a provider,
  how to run a tool, how to reach the db, how to render a template, how to
  compile and validate a workflow. Lego pieces — capabilities with no
  opinions. Nothing in the base decides what the harness does.

  USERSPACE is how those pieces snap together into an agentic loop: the cells,
  the manifests that wire them, the policy tables they read, and the prompts
  they speak. It belongs to the PROJECT, not to the harness. `resources/`
  ships a template; a project seeds its own copy on first use and evolves it
  from there, so two projects running the same binary can work differently and
  neither can break the other.

  This namespace is the READ seam. Every loader in the base — the cell loader,
  the manifest loader, the gate and phase tables, the prompt renderer — comes
  through here instead of reaching for `io/resource` directly, and gets:

    the project's current version, if the project has one
    else the shipped template, seeded into the project as version 1

  WHY A BOUND CONNECTION rather than a threaded argument. The readers are
  called from everywhere — a selmer render deep inside a gate message, a cell
  reload from a tool, a threshold lookup inside a compiled predicate — and
  threading a conn through all of it would put the store in the signature of
  every function that reads a number. `bind!` is called once, by
  system/start!, with the project's connection.

  UNBOUND IS A VALID STATE, and it reads the template. A test, a REPL session,
  or a tool that has no run behind it gets exactly the behaviour the harness
  had before this existed. That is what keeps the seam addable without a flag
  day: nothing has to know whether a project store is present."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [samizdat.store.outcomes :as outcomes]
            [samizdat.store.userspace :as store]))

;; --- the bound project -------------------------------------------------------

(defonce ^:private project (atom nil))

;; {[kind name] body} for the bound project.
;;
;; Reads are HOT: a prompt is rendered on every gate message and every turn
;; assembly, and a threshold is read inside compiled predicates. One db query
;; per read would put SQLite in the path of string interpolation. The cache is
;; invalidated wholesale on any write and on any (un)bind — coarse on purpose,
;; because the alternative is reasoning about which read a write could have
;; affected, and a stale cell is the bug that looks like the supervisor's edit
;; silently not taking.
(defonce ^:private cache (atom {}))

;; Bumped by every invalidation. `body` records the generation BEFORE it reads
;; and refuses to cache a value read under an older one — otherwise a write
;; landing between the read and the cache fill re-installed the pre-edit body,
;; which then served until the next write: exactly the "supervisor's edit
;; silently not taking" failure the cache comment warns about
;; (karamazov-blt.8).
(defonce ^:private generation (atom 0))

;; {[kind name] text} — in file mode, the text the store is known to hold as
;; the latest version, so an unchanged file costs no query to confirm it is
;; unchanged. Dropped with the read cache.
(defonce ^:private synced (atom {}))

(declare reset-checks!)

(defn invalidate!
  "Drop the read cache. Called on every write; public so a caller that changed
  the store behind this namespace's back can say so."
  []
  (swap! generation inc)
  (reset! cache {})
  (reset! synced {})
  nil)

(defn bind!
  "Point userspace reads at this project's store. Called once by
  system/start! with the project's db connection.

  Returns the previous binding, so a caller that needs to restore it (a test,
  a tool operating on another project) can."
  [conn]
  (let [prev @project]
    (reset! project conn)
    (invalidate!)
    (reset-checks!)
    prev))

(defn unbind!
  "Detach from the project store — reads fall back to the shipped template.
  The state a test and a bare REPL run in."
  []
  (reset! project nil)
  (invalidate!)
  (reset-checks!)
  nil)

(defn conn
  "The bound project connection, or nil."
  []
  @project)

(defn bound? [] (some? @project))

;; --- the project DIRECTORY ---------------------------------------------------
;;
;; The connection above says which store userspace reads; this says which tree
;; the run is working IN. Prompt assembly needs it and cannot be handed it:
;; `initial-messages` is called from a dozen cells and drivers, and the shipped
;; system prompt has to know whether the project under work is the harness
;; itself. Bound once by `system/start!` from the same config the drivers take
;; their `:root` from.

(defonce ^:private root (atom nil))

(defn bind-root!
  "Point userspace at the directory this process works on. Returns the previous
  value."
  [dir]
  (let [prev @root]
    (reset! root (some-> dir str))
    (invalidate!)
    (reset-checks!)
    prev))

(defn project-root
  "The bound project directory, or nil when nothing has bound one — a test, a
  bare REPL."
  []
  @root)

(def ^:private harness-markers
  "Files that exist in a samizdat checkout and nowhere else. Two rather than
  one, from opposite layers, so a directory that merely vendored a copy of the
  loop manifest is not mistaken for the harness."
  ["src/samizdat/workflow.clj" "resources/manifests/loop.edn"])

;; --- the MODEL ---------------------------------------------------------------
;;
;; Which provider and model this run speaks to, for the prompt file layer
;; below: .samizdat/prompts/<provider>/<model>/ is consulted for the running
;; model and no other. Bound by system/start! from the run config, after the
;; endpoint probe has said what a local server actually loaded.

(defonce ^:private model (atom nil))

(defn bind-model!
  "Point the prompt file layer at this run's `{:provider kw :model string}`.
  nil unbinds — the provider/model directories are then not consulted at
  all, and only the plain project files and the store answer. Returns the
  previous value."
  [m]
  (let [prev @model]
    (reset! model (when (map? m) m))
    prev))

(defn model-context
  "The bound `{:provider :model}`, or nil."
  []
  @model)

(defn model-dir-matches?
  "Whether a `.samizdat/prompts/<provider>/<dir>/` directory applies to
  `model-id`: a case-insensitive PREFIX match, so `qwen3` covers
  `Qwen3.8-27B-Q8_0` and `glm` covers `glm-5.3`. A prefix rather than an
  exact id because the trait a per-model prompt accommodates is a family's
  training, and an exact key would need re-authoring on every point release."
  [dir model-id]
  (boolean
   (and (string? dir) (string? model-id)
        (not (str/blank? dir))
        (str/starts-with? (str/lower-case model-id) (str/lower-case dir)))))

(defn self-hosting?
  "Whether the project being worked on IS a samizdat checkout.

  The system prompt spends several sections on this harness's own
  architecture — cells and manifests, src-is-mechanism vs
  resources-are-behaviour, `use what you build`. All of it is load-bearing
  when the run's target is samizdat and all of it is standing instruction
  about the wrong codebase when the target is anything else, in the
  most-weighted part of the context (karamazov-8zk).

  UNKNOWN READS AS TRUE. With no root bound there is nothing to go on, and the
  shipped prompt is the harness's own; defaulting the other way would delete
  whole instruction blocks from every run whose driver forgot to bind, which
  is a silent failure rather than a loud one."
  ([] (if-let [r (project-root)] (self-hosting? r) true))
  ([dir]
   (boolean (and dir (every? #(.exists (io/file (str dir) %)) harness-markers)))))

;; --- the template ------------------------------------------------------------

(def ^:private resource-path
  "Where the shipped template for each kind lives on the classpath. The
  extension is part of the kind, because the kind is what says how to read a
  body: Clojure for a cell, EDN for a manifest or a policy table, markdown for
  a prompt."
  {:cell     (fn [name] (str "cells/" name ".clj"))
   :manifest (fn [name] (str "manifests/" name ".edn"))
   :policy   (fn [name] (str name ".edn"))
   :prompt   (fn [name] (str "prompts/" name ".md"))})

(defn template-path
  "The classpath resource holding the shipped template for `kind`/`name`."
  [kind name]
  (if-let [f (get resource-path kind)]
    (f name)
    (throw (ex-info (str "no template path for userspace kind " (pr-str kind))
                    {:kind kind :name name}))))

(declare role-path template-map)

(defn template
  "The shipped template body for `kind`/`name`, or nil when nothing ships
  under that name.

  nil is not an error here. A project may hold userspace the harness never
  shipped — a cell the supervisor wrote, a manifest it authored — and those
  have no template by definition."
  [kind name]
  (some-> (io/resource (or (role-path kind name (template-map))
                           (template-path kind name)))
          slurp))

;; --- prompt FILES ------------------------------------------------------------
;;
;; A project carries prompt overrides as files a human and the agent can both
;; read and edit in place, under <root>/.samizdat/prompts/:
;;
;;   <name>.md                       every provider, every model
;;   <provider>/<name>.md            every model on that provider
;;   <provider>/<model-dir>/<name>.md  one model family (prefix match)
;;
;; Most specific first; among model directories the LONGEST match wins, so a
;; project can special-case a sub-family beneath a general one. A file, when
;; present, IS the newest version — it beats a stored row, and `prompt-source`
;; says so, because two sources of truth that silently shadow each other is
;; the drift this project keeps finding. Read fresh on every call (a few
;; stats, against a render that costs far more): the point of a file is that
;; someone edits it in place, and a cache would pin the first content it saw.

(defn- prompts-dir
  "The project's prompt file directory, or nil when no root is bound."
  []
  (when-let [r (project-root)]
    (io/file r ".samizdat" "prompts")))

(defn- present-file
  "`f` when it is a readable file, else nil."
  [^java.io.File f]
  (when (and f (.isFile f)) f))

(defn- model-dirs
  "Subdirectories of <prompts>/<provider>/ that apply to `model-id`, most
  specific (longest name) first."
  [^java.io.File provider-dir model-id]
  (when (and provider-dir (.isDirectory provider-dir))
    (->> (.listFiles provider-dir)
         (filter #(.isDirectory ^java.io.File %))
         (filter #(model-dir-matches? (.getName ^java.io.File %) model-id))
         (sort-by #(- (count (.getName ^java.io.File %)))))))

(defn prompt-file
  "The file that overrides prompt `name` for the bound root and model, as
  `{:path :layer}` — :layer being :model, :provider or :project — or nil when
  no file applies."
  [name]
  (when-let [dir (prompts-dir)]
    (let [{:keys [provider model]} (model-context)
          pname (str name ".md")
          provider-dir (when provider (io/file dir (clojure.core/name provider)))]
      (or (some (fn [^java.io.File md]
                  (when-let [f (present-file (io/file md pname))]
                    {:path (.getPath f) :layer :model}))
                (model-dirs provider-dir model))
          (when-let [f (present-file (some-> provider-dir (io/file pname)))]
            {:path (.getPath f) :layer :provider})
          (when-let [f (present-file (io/file dir pname))]
            {:path (.getPath f) :layer :project})))))

(defn prompt-variants
  "Every prompt file the project holds, as {name [variant …]} — each variant
  `{:layer :path}` plus :provider and :model-dir where they apply. What the
  `prompt` tool lists, so a per-model wording is a thing the supervisor can
  see rather than a shadow it has to know to look for. {} with no root."
  []
  (if-let [^java.io.File dir (prompts-dir)]
    (if-not (.isDirectory dir)
      {}
      (let [md? (fn [^java.io.File f] (and (.isFile f) (.endsWith (.getName f) ".md")))
            nm (fn [^java.io.File f] (subs (.getName f) 0 (- (count (.getName f)) 3)))
            project (for [f (.listFiles dir) :when (md? f)]
                      [(nm f) {:layer :project :path (.getPath f)}])
            provider (for [^java.io.File p (.listFiles dir) :when (.isDirectory p)
                           f (.listFiles p) :when (md? ^java.io.File f)]
                       [(nm f) {:layer :provider :provider (.getName p) :path (.getPath ^java.io.File f)}])
            model (for [^java.io.File p (.listFiles dir) :when (.isDirectory p)
                        ^java.io.File m (.listFiles p) :when (.isDirectory m)
                        f (.listFiles m) :when (md? ^java.io.File f)]
                    [(nm f) {:layer :model :provider (.getName p) :model-dir (.getName m)
                             :path (.getPath ^java.io.File f)}])]
        (reduce (fn [acc [n v]] (update acc n (fnil conj []) v))
                {}
                (sort-by (fn [[n v]] [n (:path v)])
                         (concat project provider model)))))
    {}))

;; --- the project's FILES -----------------------------------------------------
;;
;; A project's workflow lives in files under <root>/.samizdat/, and WHICH files
;; is data too: .samizdat/userspace.edn is a ROLE MAP — {:manifests {role path}
;; :prompts {role path} :policies {role path} :cells [path …]} — and every read
;; asks it which file serves a role. Nothing in src/ lists what a project has;
;; the agent can point a role at another file, add roles, or drop them.
;; resources/userspace.edn is the shipped map, and the first run copies it
;; with every file it names (`seed-project!`). The templates are a generic
;; starting point, not a layer underneath: a role the project's map lacks is
;; refused, not quietly answered by the shipped copy.
;;
;; The STORE becomes the history. A save writes the file and appends a
;; version with its rationale; a revert rewrites the file from a version; an
;; edit made to the file directly — by a person, or by the agent's own file
;; tools — is appended as a version the next time it is read. Standing, drift
;; and prescription keep reading the same rows they always did.
;;
;; FILE MODE needs both halves bound, a root and a store: without a directory
;; there are no files, and without a store there is no history to keep. A
;; test or a bare REPL that binds only a store gets the store, as before.

(def layered-policies
  "Policy names that are LAYERED settings files (samizdat.layers), not part of
  the project's workflow: never in a role map and never copied into the
  project, because a full copy in .samizdat/ would sit above a person's
  ~/.config/samizdat/ file and hide it. A save still writes the project's
  <name>.edn — that is the project layer."
  #{"tui" "gui" "webui"})

(defn files?
  "Whether reads and writes go to the project's files."
  []
  (boolean (and (conn) (project-root))))

(defn project-dir
  "<root>/.samizdat, or nil with no root bound."
  []
  (some-> (project-root) (str "/.samizdat")))

;; {path {:stamp [mtime length] :text}} — so a hot read is a stat, not a read.
(defonce ^:private file-cache (atom {}))

(defn- file-text
  "The text at `path`, or nil when there is no such file."
  [path]
  (let [f (io/file (str path))]
    (when (.isFile f)
      (let [stamp [(.lastModified f) (.length f)]
            c (get @file-cache path)]
        (if (= stamp (:stamp c))
          (:text c)
          (let [t (slurp f)]
            (swap! file-cache assoc path {:stamp stamp :text t})
            t))))))

(defn- write-file!
  "Write `text` at `path` through a sibling temp file and a rename, so a
  reader never sees half of it."
  [path text]
  (let [f (io/file (str path))
        tmp (io/file (str path ".tmp"))]
    (.mkdirs (.getParentFile f))
    (spit tmp (str text))
    (when-not (.renameTo tmp f)
      (spit f (str text))
      (.delete tmp))
    (swap! file-cache dissoc (str path))
    nil))

;; --- validation --------------------------------------------------------------
;;
;; An edit to a project's workflow is CHECKED before it is what runs. Each kind
;; has a validator — registered by the namespace that owns the kind, since that
;; is the code that knows what "works" means for it — taking the role's name
;; and the candidate text and answering nil, or a PROBLEM:
;;
;;   {:stage :read|:compile|:render|:load|:shape|… :message "…" :line L :column C}
;;
;; A text that fails is never recorded and never runs: the last version that
;; passed keeps running, the file is left as written so it can be fixed in
;; place, and the rejection is held — `rejection`, `rejections` — until a text
;; that passes replaces it. Whoever made the edit is told: a file tool's write
;; hears it in its own result (`check-written!`); anything else is reported
;; through `on-reject!` and listed for the supervisor.

(defonce ^:private validators (atom {}))

(defn register-validator!
  "Install `f` — (fn [name text] -> nil | problem) — as `kind`'s validator.
  nil removes it."
  [kind f]
  (if f (swap! validators assoc kind f) (swap! validators dissoc kind))
  nil)

(def ^:private validator-owners
  "Which namespace registers each kind's validator. A validator is installed
  as its owner loads, and an owner nothing happened to load would let every
  edit of its kind through unchecked — so asking for a validator loads its
  owner first."
  {:manifest 'samizdat.manifests
   :prompt   'samizdat.prompt
   :cell     'samizdat.cells
   :policy   'samizdat.agent.tools.policy})

(defn validator
  "`kind`'s validator, or nil."
  [kind]
  (or (get @validators kind)
      (when-let [owner (get validator-owners kind)]
        (require owner)
        (get @validators kind))))

(defn problem
  "A throwable as a problem at `stage`, with the reader's line and column when
  it carried them."
  [stage ^Throwable e]
  (let [d (ex-data e)
        line (or (:clojure.error/line d) (:line d))
        col (or (:clojure.error/column d) (:column d))]
    (cond-> {:stage stage :message (or (ex-message e) (str e))}
      line (assoc :line line)
      col (assoc :column col))))

(defn read-edn
  "`text` as EDN: {:value v}, or {:problem …} at the :read stage."
  [text]
  (try {:value (edn/read-string (str text))}
       (catch Throwable e {:problem (problem :read e)})))

(defn- check
  "The problem with `text` as `kind`/`name`, or nil. A validator that throws
  is itself the problem, at :check — a validator bug must not wave an edit
  through."
  [kind name text]
  (when-let [f (validator kind)]
    (try (f (str name) text)
         (catch Throwable e (problem :check e)))))

;; {[kind name] {:text :problem}} — the verdict on the last text checked, so a
;; broken file read on every render is checked once.
(defonce ^:private verdicts (atom {}))

;; {[kind name] {:kind :name :path :text :problem :by}} — the edits currently
;; refused, until a passing text replaces them.
(defonce ^:private rejected (atom {}))

(def ^:dynamic *candidate*
  "{[kind name] text} — texts that `body` answers with in place of the
  project's, for the extent of a validator that has to exercise the LIVE
  reader (a policy table's reload) against a text not yet accepted."
  {})

(defn with-candidate
  "Call `f` with `kind`/`name` read as `text`, then refill whatever `refill`
  (a no-arg fn) refills from the text that should be live now — the last
  version that passed — so a rejected candidate does not linger in a cache."
  [kind name text f refill]
  (try
    (binding [*candidate* (assoc *candidate* [kind (str name)] text)]
      (f))
    (finally
      ;; Only with a text to refill FROM: with none, the refill would read the
      ;; project's file, and checking that file is what is running now.
      (when-let [good (some-> (conn) (store/load-latest kind (str name)) :body)]
        (binding [*candidate* (assoc *candidate* [kind (str name)] good)]
          (try (refill) (catch Throwable _ nil)))))))

(def ^:dynamic *writer*
  "Who is writing, while a file tool checks its own write: {:branch-id …}.
  A rejection made under it has been told to its writer already."
  nil)

(defn- default-on-reject [r]
  (log/warn "userspace: REJECTED" (:path r) "—"
            (name (get-in r [:problem :stage] :check))
            (get-in r [:problem :message])
            (if-let [l (get-in r [:problem :line])] (str "(line " l ")") "")))

(defonce ^:private on-reject (atom default-on-reject))

(defn on-reject!
  "Install `f`, called once with each NEW rejection. Returns the previous
  hook; nil restores the default, which logs."
  [f]
  (let [prev @on-reject]
    (reset! on-reject (or f default-on-reject))
    prev))

(defn- verdict
  "Check `text` as `kind`/`name` (once per text), holding or clearing the
  rejection. Returns the problem, or nil when it passes."
  [kind name path text]
  (let [k [kind name]
        v (get @verdicts k)
        ;; A failing verdict is not reused for the ROLE MAP: whether its
        ;; files exist is not a property of its text, and a map waiting on a
        ;; file being written must see the file the moment it lands.
        p (if (and v (= text (:text v))
                   (not (and (= :map kind) (:problem v))))
            (:problem v)
            (let [p (check kind name text)]
              (swap! verdicts assoc k {:text text :problem p})
              p))]
    (if p
      (let [prev (get @rejected k)]
        (when-not (= text (:text prev))
          (let [r {:kind kind :name (str name) :path (str path) :text text
                   :problem p :by *writer*}]
            (swap! rejected assoc k r)
            (@on-reject r))))
      (swap! rejected dissoc k))
    p))

;; The last project map that read, so a map broken mid-edit does not take
;; every role with it.
(defonce ^:private last-good-map (atom nil))

(defn- reset-checks!
  "Forget every verdict and rejection — another project is bound."
  []
  (reset! verdicts {})
  (reset! rejected {})
  (reset! last-good-map nil)
  nil)

(defn rejection
  "The edit of `kind`/`name` currently refused, or nil."
  [kind name]
  (some-> (get @rejected [kind (str name)]) (dissoc :text)))

(defn rejections
  "Every edit currently refused."
  []
  (mapv #(dissoc % :text) (vals @rejected)))

;; --- the role map ------------------------------------------------------------

(def ^:private map-key
  {:manifest :manifests :prompt :prompts :policy :policies :cell :cells})

;; {slot {:text :value}} — the last text each map was parsed from, so a map is
;; parsed once per change. Two slots, :shipped and :project; no bound needed.
(defonce ^:private parsed-maps (atom {}))

(defn- parse-map [slot text]
  (let [c (get @parsed-maps slot)]
    (if (and c (= text (:text c)))
      (:value c)
      (let [v (try (let [m (edn/read-string (str text))] (when (map? m) m))
                   (catch Throwable _ nil))]
        (swap! parsed-maps assoc slot {:text text :value v})
        v))))

(defn template-map
  "The shipped role map, resources/userspace.edn."
  []
  (or (some->> (io/resource "userspace.edn") slurp (parse-map :shipped)) {}))

(defn map-path
  "Where the project's role map is: <root>/.samizdat/userspace.edn."
  []
  (str (project-dir) "/userspace.edn"))

(declare map-files)

(defn role-map
  "The role map in force: the project's in file mode, the shipped one
  otherwise. A project map that does not read, or names a file that is not
  there, is rejected like any other edit and the last one that passed stays
  in force."
  []
  (if (files?)
    (if-let [t (file-text (map-path))]
      (let [m (parse-map :project t)
            p (verdict :map "userspace" (map-path) t)]
        (if (and m (nil? p))
          (do (reset! last-good-map m) m)
          (or @last-good-map m {})))
      {})
    (template-map)))

(defn- cell-role [path]
  (str/replace (last (str/split (str path) #"/")) #"\.clj$" ""))

(defn role-path
  "The file serving `kind`/`name` under `m` (the map in force by default),
  relative to .samizdat/ — or nil when the map has no such role."
  ([kind name] (role-path kind name (role-map)))
  ([kind name m]
   (let [name (str name)]
     (cond
       (and (= :policy kind) (contains? layered-policies name)) (str name ".edn")
       (= :cell kind) (some #(when (= name (cell-role %)) %) (:cells m))
       :else (get-in m [(map-key kind) (keyword name)])))))

(defn- roles-in [m kind]
  (if (= :cell kind)
    (mapv cell-role (:cells m))
    (->> (keys (get m (map-key kind)))
         (map #(subs (str %) 1))
         sort
         vec)))

(defn roles
  "Every role of `kind` in the map in force, as names. Cells in load order,
  the rest sorted."
  [kind]
  (roles-in (role-map) kind))

(defn shipped-roles
  "Every role of `kind` the shipped map has."
  [kind]
  (roles-in (template-map) kind))

(defn project-path
  "The absolute path of the file serving `kind`/`name` in the project, or
  nil when the project's map has no such role."
  [kind name]
  (some->> (role-path kind name) (str (project-dir) "/")))

(defn- render-map
  "`m` as the text of a userspace.edn: `header` (the comment lines a person
  or the agent wrote at the top) kept, then one role per line, so the file
  stays readable after a tool adds a role to it."
  [m header]
  (let [section (fn [k]
                  (let [v (get m k)]
                    (if (= :cells k)
                      (str " " k "\n  [" (str/join "\n   " (map pr-str v)) "]")
                      (str " " k "\n  {"
                           (str/join "\n   " (for [[r p] (sort-by (comp str key) v)]
                                                (str (pr-str r) " " (pr-str p))))
                           "}"))))
        ks (concat (filter #(contains? m %) [:manifests :policies :cells :prompts])
                   (remove #{:manifests :policies :cells :prompts} (keys m)))]
    (str header
         "{" (subs (str/join "\n\n" (for [k ks]
                                     (if (#{:manifests :policies :cells :prompts} k)
                                       (section k)
                                       (str " " k " " (pr-str (get m k))))))
                   1)
         "}\n")))

(defn- comment-header
  "The leading comment block of a map file, verbatim."
  [text]
  (let [ls (take-while #(or (str/starts-with? (str/trim %) ";") (str/blank? %))
                       (str/split-lines (str text)))]
    (if (seq ls) (str (str/join "\n" ls) "\n") "")))

(defn- map-role!
  "Add `kind`/`name` to the project's map at `rel`, and write the map."
  [kind name rel]
  (let [text (file-text (map-path))
        m (role-map)
        m (if (= :cell kind)
            (update m :cells (fnil conj []) rel)
            (assoc-in m [(map-key kind) (keyword (str name))] rel))]
    (write-file! (map-path) (render-map m (comment-header text)))
    (reset! last-good-map m)))

(defn- save-path
  "Where a save of `kind`/`name` goes: the file its role maps to, or — for a
  role the map does not have yet — a new file laid out like resources/, which
  is added to the map."
  [kind name]
  (or (project-path kind name)
      (let [rel (template-path kind name)]
        (map-role! kind name rel)
        (str (project-dir) "/" rel))))

;; --- reading the files -------------------------------------------------------

(defn- record-file-edit!
  "Append `text` as a version when it is not what the store already has as
  the latest — an edit made to the file outside the tools."
  [kind name text]
  (when-not (= text (get @synced [kind name]))
    (when-let [c (conn)]
      (let [latest (:body (store/load-latest c kind name))]
        (when (not= text latest)
          (store/save! c kind name text "file" nil))))
    (swap! synced assoc [kind name] text)))

(defn- last-good
  "The last text of `kind`/`name` that passed: the store's newest version,
  since only a passing text is ever recorded."
  [kind name]
  (some-> (conn) (store/load-latest kind name) :body))

(defn- read-project-file
  "`kind`/`name` from the project's files, or nil when the project's map has
  no such role or its file is gone. A text that does not pass its kind's
  validator is not what runs: the last version that did is returned instead.
  A prompt may be answered by a provider or model variant; only the role's
  own file is its history."
  [kind name]
  (when-let [own (project-path kind name)]
    (let [variant (when (= :prompt kind)
                    (let [f (prompt-file name)]
                      (when (#{:model :provider} (:layer f)) (:path f))))
          vt (some-> variant file-text)]
      (if (and vt (nil? (check kind name vt)))
        vt
        (when-let [t (file-text own)]
          (cond
            (= t (get @synced [kind name])) t
            (verdict kind name own t) (last-good kind name)
            :else (do (record-file-edit! kind name t) t)))))))

(defn check-written!
  "After a file tool wrote `path`: when it serves a role of the project's
  workflow, check it now and return the rejection if the edit was refused
  ({:kind :name :path :problem}), else nil. `writer` ({:branch-id …}) is
  recorded on the rejection — it has been told."
  ([path] (check-written! path nil))
  ([path writer]
   (when (files?)
     (let [abs (.getCanonicalPath (io/file (str path)))
           base (.getCanonicalPath (io/file (str (project-dir))))
           rel (when (str/starts-with? abs (str base "/")) (subs abs (inc (count base))))
           m (role-map)
           role (cond
                  (nil? rel) nil
                  (= rel "userspace.edn") [:map "userspace"]
                  :else (some (fn [[kind n r]] (when (= r rel) [kind n])) (map-files m)))]
       (when-let [[kind n] role]
         (binding [*writer* writer]
           (swap! rejected (fn [m] (if (contains? m [kind n])
                                     (assoc-in m [[kind n] :by] writer)
                                     m)))
           (if (= :map kind) (role-map) (read-project-file kind n)))
         (when-let [r (get @rejected [kind n])]
           (when (= (:text r) (file-text abs))
             (assoc (dissoc r :text) :path (str path)))))))))

(defn- adoption-path [] (str (project-dir) "/adoption.edn"))

(defn- offer-key [kind role] (str (clojure.core/name kind) "/" role))

(defn- map-files
  "Every file a role map names, as [kind role rel-path]."
  [m]
  (concat (for [[r p] (:manifests m)] [:manifest (subs (str r) 1) p])
          (for [[r p] (:policies m)] [:policy (subs (str r) 1) p])
          (for [p (:cells m)] [:cell (cell-role p) p])
          (for [[r p] (:prompts m)] [:prompt (subs (str r) 1) p])))

(defn seed-project!
  "Copy the shipped role map, and every file it names, into the project the
  first time samizdat runs there. `m` is the map to copy — the shipped one by
  default; a test hands in a slice.

  ONCE. While .samizdat/userspace.edn exists nothing is copied again: a
  project that dropped a role keeps it dropped, and a template a later
  release adds is offered to the supervisor rather than written behind its
  back. A file that already exists is never overwritten, first run or not.

  A project that ran before its workflow lived in files has its evolution in
  the store. Those versions are not applied: they are listed under :pending in
  .samizdat/adoption.edn, for the supervisor to adopt or not.

  Returns {:written [path …] :pending [{:kind :name :version} …]}, or nil
  outside file mode."
  ([] (seed-project! nil))
  ([m]
   (when (files?)
     (if (.isFile (io/file (map-path)))
       {:written [] :pending []}
       (let [c (conn)
             shipped? (nil? m)
             m (or m (template-map))
             ;; What the store held BEFORE any of this: a project's own
             ;; versions, which the templates written below would otherwise
             ;; bury under a newer factory row.
             pending (vec (for [kind (sort store/kinds)
                                {:keys [name version]} (store/names c kind)
                                :let [row (store/load-latest c kind name)]
                                :when (and (not= "factory" (:source row))
                                           (not (contains? layered-policies name))
                                           (not= (:body row) (template kind name)))]
                            {:kind kind :name name :version version}))
             written (vec (for [[kind role rel] (map-files m)
                                :let [t (some-> (io/resource rel) slurp)
                                      path (str (project-dir) "/" rel)]
                                :when (and t (not (.exists (io/file path))))]
                            (do (write-file! path t)
                                (store/seed! c kind role t)
                                path)))]
         (write-file! (map-path) (if shipped?
                                   (slurp (io/resource "userspace.edn"))
                                   (render-map m "")))
         ;; What this project has SEEN of each template, so a later release
         ;; that changes one can be told apart from the project changing it.
         (write-file! (adoption-path)
                      (pr-str {:seen (into {} (for [[kind role rel] (map-files m)
                                                    :let [t (some-> (io/resource rel) slurp)]
                                                    :when t]
                                                [(offer-key kind role) (hash t)]))
                               :pending pending
                               :declined []}))
         (invalidate!)
         (log/info "userspace: seeded" (count written) "file(s) into" (project-dir)
                   (if (seq pending)
                     (str "; " (count pending) " stored version(s) await the supervisor")
                     ""))
         {:written written :pending pending})))))

;; --- adoption ----------------------------------------------------------------
;;
;; What the project has not taken is OFFERED to the supervisor, never applied:
;;
;;   :new      a role the shipped map has and the project's does not
;;   :updated  a role both have, whose shipped template changed since the
;;             project last saw it, where the project's file does not already
;;             say what the template says (:edited? when the project changed
;;             its copy too)
;;   :pending  a version the store held before the project's workflow lived
;;             in files (seed-project!)
;;
;; .samizdat/adoption.edn remembers the answers: :seen maps each role to the
;; hash of the template the project last saw (seeded, adopted or declined),
;; :pending lists the store's versions not yet answered, :declined keeps each
;; refusal with its reason for the next supervisor. jolt's `hash` is stable
;; across processes; if a runtime changed it every template would be offered
;; again, which is loud rather than wrong.

(declare save!)

(defn- adoption []
  (or (some-> (file-text (adoption-path))
              (#(try (edn/read-string %) (catch Throwable _ nil))))
      {}))

(defn- save-adoption! [a]
  (write-file! (adoption-path) (pr-str a)))

(defn- factory-hash
  "The hash of the first template the store seeded for `kind`/`name` — what
  the project saw when adoption.edn has no record of it."
  [kind name]
  (when-let [c (conn)]
    (when-let [v (some #(when (= "factory" (:source %)) (:version %))
                       (store/versions c kind name))]
      (some-> (store/load-version c kind name v) :body hash))))

(defn offers
  "Everything on offer to this project, as {:offer :kind :name :path …}. See
  the section comment. Empty outside file mode."
  []
  (if-not (files?)
    []
    (let [a (adoption)
          pm (role-map)
          seen (fn [kind role] (or (get-in a [:seen (offer-key kind role)])
                                   (factory-hash kind role)))
          shipped (for [[kind role rel] (map-files (template-map))
                        :when (not (and (= :policy kind) (contains? layered-policies role)))
                        :let [t (template kind role)]
                        :when (and t (not= (hash t) (seen kind role)))
                        :let [own (role-path kind role pm)]]
                    (if-not own
                      {:offer :new :kind kind :name role :path rel}
                      (let [ft (file-text (str (project-dir) "/" own))]
                        (when (not= ft t)
                          {:offer :updated :kind kind :name role :path own
                           :edited? (not= (some-> ft hash) (seen kind role))}))))
          pending (for [{:keys [kind name version]} (:pending a)
                        :let [own (role-path kind name pm)
                              ft (some->> own (str (project-dir) "/") file-text)
                              b (some-> (conn) (store/load-version kind name version) :body)]
                        :when (and b (not= b ft))]
                    {:offer :pending :kind kind :name name :version version
                     :path (or own (template-path kind name))})]
      (vec (concat (remove nil? shipped) pending)))))

(defn offer-text
  "The text an offer would install."
  [{:keys [offer kind name version]}]
  (if (= :pending offer)
    (some-> (conn) (store/load-version kind name version) :body)
    (template kind name)))

(defn- find-offer [kind name]
  (some #(when (and (= kind (:kind %)) (= (str name) (:name %))) %) (offers)))

(defn- answered!
  "Record that `o` has been answered, so it is not offered again."
  [{:keys [offer kind name version]} a]
  (save-adoption!
   (if (= :pending offer)
     (update a :pending (fn [ps] (vec (remove #(and (= kind (:kind %)) (= name (:name %))
                                                    (= version (:version %)))
                                              ps))))
     (assoc-in a [:seen (offer-key kind name)] (hash (template kind name))))))

(defn adopt!
  "Take the offer for `kind`/`name`: its text is checked like any edit, then
  written to the project's file (a :new role is added to the map) and
  recorded as a version with `rationale`.

  Returns {:adopted offer :version v}, {:problem p :offer offer} when the
  text does not pass — nothing written, still on offer — or {:no-offer true}."
  [kind name rationale]
  (if-let [o (find-offer kind name)]
    (let [text (offer-text o)]
      (if-let [p (check kind (str name) text)]
        {:problem p :offer o}
        (let [a (adoption)]
          (when (= :new (:offer o))
            (write-file! (str (project-dir) "/" (:path o)) text)
            (map-role! kind (str name) (:path o)))
          (let [v (save! kind (str name) text rationale)]
            (answered! o a)
            (log/info "userspace: adopted" (clojure.core/name (:offer o))
                      (clojure.core/name kind) name)
            {:adopted o :version v}))))
    {:no-offer true}))

(defn decline!
  "Answer the offer for `kind`/`name` with no, keeping `reason` for the next
  supervisor. Writes nothing else. Returns the offer, or nil when there was
  none."
  [kind name reason]
  (when-let [o (find-offer kind name)]
    (let [a (adoption)]
      (answered! o (update a :declined (fnil conj [])
                           (assoc (select-keys o [:kind :name :offer :version])
                                  :reason (str reason)))))
    o))

;; --- reads -------------------------------------------------------------------

(declare cached-body body-of)

(defn- read-body
  [kind name]
  (if-let [c (conn)]
    (if-let [t (template kind name)]
      ;; Through seed! even when the project already has a row: seed! is what
      ;; carries a harness upgrade into a project whose copy is still the
      ;; factory one. Reading the row first and returning early is what pinned
      ;; a project to whatever shipped the day it first ran.
      (:body (store/seed! c kind name t))
      ;; No template — a cell or manifest the supervisor wrote, which has one
      ;; by definition only if the project holds it.
      (:body (store/load-latest c kind name)))
    (template kind name)))

(defn body
  "The body of `kind`/`name` for the current project.

  The project's newest version when it has one; otherwise the shipped
  template, seeded into the project as version 1 on the way past. Unbound,
  the template with no seeding.

  Returns nil when neither the project nor the template has it — the caller
  decides whether that is an error, because it is one for a cell the manifest
  references and not one for an optional prompt.

  A :prompt is first looked for as a FILE under the project's
  .samizdat/prompts/ (see `prompt-file`); the file layer is not cached, the
  store/template layer below it is.

  Cached per (kind, name) and invalidated on every write; see `cache`."
  [kind name]
  (if-let [t (get *candidate* [kind (str name)])]
    t
    (body-of kind name)))

(defn- body-of
  [kind name]
  (if (files?)
    ;; The project's file for the role, and nothing else: a role the map
    ;; lacks is nil here and refused by body!, not answered by the template.
    (read-project-file kind name)
    (or (when (= :prompt kind)
          (some-> (prompt-file name) :path slurp))
        (cached-body kind name))))

(defn- cached-body
  [kind name]
  (let [k [kind name]
        hit (get @cache k ::miss)]
    (if (= ::miss hit)
      (let [gen @generation
            v (read-body kind name)]
        ;; nil is cached too: an absent name is looked up on every render of a
        ;; prompt block that may not exist, and re-querying for a row that is
        ;; not there is the same cost as one that is.
        ;;
        ;; Cache only if no invalidation landed while we were reading — a
        ;; stale fill after a concurrent save! would serve the pre-edit body
        ;; until the NEXT write. The value itself is still returned: stale is
        ;; fine for the read that raced, poisonous for every read after.
        (swap! cache (fn [c] (if (= gen @generation) (assoc c k v) c)))
        v)
      hit)))

(defn body!
  "`body`, failing loud when it is absent. For a caller whose whole operation
  is meaningless without it — a manifest node's cell, the system prompt."
  [kind name]
  (or (body kind name)
      (if (files?)
        ;; The two ways a project can lack a role, each named with where to
        ;; fix it — the agent reads this and has to be able to act on it.
        (let [rel (role-path kind name)
              data {:kind kind :name (str name) :role (keyword (str name))
                    :map (map-path)}
              r (rejection kind name)]
          (throw (cond
                   r
                   (ex-info (str "the " (clojure.core/name kind) " role "
                                 (keyword (str name)) " (" rel ") was rejected and has"
                                 " no earlier version that passed: "
                                 (clojure.core/name (get-in r [:problem :stage] :check)) " — "
                                 (get-in r [:problem :message]))
                            (assoc data :missing :rejected :path rel :problem (:problem r)))
                   rel
                   (ex-info (str "the " (clojure.core/name kind) " role "
                                 (keyword (str name)) " maps to " rel
                                 ", which is not in " (project-dir))
                            (assoc data :missing :file :path rel))
                   :else
                   (ex-info (str "no " (clojure.core/name kind) " role "
                                 (keyword (str name)) " in " (map-path))
                            (assoc data :missing :role)))))
        (throw (ex-info (str "no userspace " (clojure.core/name kind) " named "
                             (pr-str name) ": the project has no version and"
                             " nothing ships at " (template-path kind name))
                        {:kind kind :name name})))))

(declare store-prompt-source)

(defn prompt-source
  "Where the text of prompt `name` comes from, for the `prompt` tool:
  `{:source :file :layer … :path …}` for a project file, `{:source :project
  :version n}` for a stored row, `{:source :template}` for the shipped file,
  nil for a name nobody has."
  [name]
  (if (files?)
    ;; The project's files are the whole story: a provider or model variant,
    ;; else the role's own file, else nothing.
    (let [f (prompt-file name)
          own (project-path :prompt name)]
      (cond
        (#{:model :provider} (:layer f)) {:source :file :layer (:layer f) :path (:path f)}
        (and own (.isFile (io/file own))) {:source :file :layer :project :path own}))
    (store-prompt-source name)))

(defn- store-prompt-source [name]
  (or (when-let [f (prompt-file name)]
        {:source :file :layer (:layer f) :path (:path f)})
      ;; A bound project SEEDS a shipped prompt as a factory row on first read,
      ;; so "there is a row" does not mean the project authored anything. A
      ;; row whose body is still the template's is reported as the template:
      ;; that is where the words come from, and it is what a reader deciding
      ;; whether to edit a file or `save` a version needs to know.
      (when-let [c (conn)]
        (when-let [row (store/load-latest c :prompt name)]
          (if (= (:body row) (template :prompt name))
            {:source :template}
            {:source :project :version (:version row)})))
      (when (template :prompt name)
        {:source :template})))

(defn edn-body
  "`body` parsed as EDN — a manifest or a policy table. nil stays nil."
  [kind name]
  (some-> (body kind name) edn/read-string))

(defn edn-body!
  [kind name]
  (edn/read-string (body! kind name)))

;; --- writes ------------------------------------------------------------------

(defn save!
  "Append a new version of `kind`/`name` for the current project. Returns the
  new version number, or nil when no project is bound.

  nil rather than a throw on an unbound write: a tool that edits userspace
  outside a run is a real situation (a REPL session, a test), and it should
  hear that nothing was stored rather than crash.

  `rationale` — why the edit was made — is stored with the version and shown
  in the history; see store/save!."
  ([kind name new-body] (save! kind name new-body nil))
  ([kind name new-body rationale]
   (if-let [c (conn)]
     (let [v (store/save! c kind name new-body "project" rationale)]
       ;; The file is what runs, so it is written; the version above is its
       ;; history, and marking it synced keeps the read that follows from
       ;; recording the same text a second time as a file edit.
       (when (files?)
         (write-file! (save-path kind name) new-body)
         (swap! synced assoc [kind name] (str new-body)))
       (invalidate!)
       (log/info "userspace" (clojure.core/name kind) name "saved as version" v)
       v)
     (do (log/warn "userspace save ignored — no project store is bound:"
                   (clojure.core/name kind) name)
         nil))))

(defn revert!
  "Re-append an older version as the newest — the rollback, recorded with the
  caller's stated reason. Returns the new version number, or nil."
  ([kind name version] (revert! kind name version nil))
  ([kind name version rationale]
   (when-let [c (conn)]
     (let [v (store/revert! c kind name version rationale)]
       (when (and v (files?))
         (let [b (:body (store/load-latest c kind name))]
           (write-file! (save-path kind name) b)
           (swap! synced assoc [kind name] (str b))))
       (invalidate!)
       v))))

(defn record-run-outcome!
  "Stamp a run's ending — :shipped, :failed or :error (samizdat.store.outcomes)
  — onto the project-authored versions that were current for it: their
  standing, read back through `versions`. A quiet nil when no project is
  bound, like every other unbound write; an outcome outside the vocabulary
  throws whether bound or not."
  [outcome]
  (outcomes/column outcome)
  (when-let [c (conn)]
    (store/record-run-outcome! c outcome)
    true))

(defn versions
  "The edit history of one piece of userspace, oldest first. Empty when
  unbound: the template has no history, which is the point of copying it."
  [kind name]
  (if-let [c (conn)] (store/versions c kind name) []))

(defn names
  "Every name the project holds at `kind`, with its latest version. Empty when
  unbound."
  [kind]
  (if-let [c (conn)] (store/names c kind) []))

(defn prescription-mass
  "This project's accumulated prescription: which kinds it has overridden and
  by how much. `{}` when unbound or when nothing has been overridden, which is
  the honest answer for a project still running the shipped template."
  []
  (if-let [c (conn)] (store/prescription c) {}))

(defn seed-all!
  "Seed every named template of `kind` into the project, and return the
  project's bodies for that kind as {name body}.

  `template-names` is enumerated by the caller rather than globbed, for the
  reason every other resource list in this codebase is: a classpath has no
  directory listing and an embedded resource has no filesystem path, so a glob
  finds nothing inside a built binary and the layer silently comes up empty.

  What comes back is the PROJECT's bodies, not the templates: a name the
  project has evolved returns its own version, and a name it has authored that
  no template covers is included too. Seeding and loading in one motion,
  because the only way to be sure a project has its copy is to try."
  [kind template-names]
  (if-let [c (conn)]
    (do (doseq [n template-names]
          (when-let [t (template kind n)]
            (store/seed! c kind n t)))
        (invalidate!)
        (store/latest-bodies c kind))
    ;; Unbound: the template IS the layer.
    (into {}
          (keep (fn [n] (when-let [t (template kind n)] [n t])))
          template-names)))

;; --- the role map's own validator --------------------------------------------

(defn- map-problem
  "What is wrong with a role-map text: it does not read, is not the shape a
  map has, or names files the project does not have."
  [_ text]
  (let [{:keys [value problem]} (read-edn text)
        role-map? (fn [m] (and (map? m) (every? keyword? (keys m)) (every? string? (vals m))))]
    (or problem
        (cond
          (not (map? value))
          {:stage :shape :message "not a map"}

          (not-every? #(or (nil? (get value %)) (role-map? (get value %)))
                      [:manifests :policies :prompts])
          {:stage :shape :message "not role -> path"
           :sections (vec (remove #(or (nil? (get value %)) (role-map? (get value %)))
                                  [:manifests :policies :prompts]))}

          (not (or (nil? (:cells value))
                   (and (vector? (:cells value)) (every? string? (:cells value)))))
          {:stage :shape :message ":cells not paths"})
        (let [missing (vec (for [[_ _ rel] (map-files value)
                                 :when (not (.isFile (io/file (str (project-dir) "/" rel))))]
                             rel))]
          (when (seq missing)
            {:stage :shape :message (str "missing files: " (str/join ", " missing))
             :missing missing})))))

(register-validator! :map map-problem)
