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

(ns samizdat.cells
  "The cell loader — kernel mechanism, cell-agnostic by design.

  Cells are the harness's plugins: not compiled into src, but loaded at
  runtime from resources/cells (and .samizdat/cells project overrides), each a
  small self-contained Clojure file that calls mycelium's `defcell`. This
  namespace knows how to find them, load them into the live image, register
  them, and reload them — it knows no specific cell. With no cell files, the
  kernel registers no cells and runs no loop; the loop is entirely user space.

  Loading is transactional (autolith's extension-registry pattern): the whole
  cell registry is snapshotted before a load and restored if any file fails, so
  a broken cell edit never leaves the registry half-loaded. That is also the
  reversible-load half of the mutation protocol (karamazov-ioo.11): the agent
  edits a cell, this reloads it, and a bad edit rolls back cleanly.

  Files are load-stringed rather than required, so they are dynamically loaded
  into the running image (dev filesystem or a built binary's resources alike)
  and never AOT-compiled into src."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [jolt.fs :as fs]
            [mycelium.cell :as cell]
            ;; Preload the namespaces the shipped cells load-string reach for, so
            ;; they compile the normal way first and the AOT cache stays sound
            ;; (samizdat.cell-prelude explains the -dirty-build failure it fixes).
            [samizdat.cell-prelude]
            [samizdat.prompt :as prompt]
            [clojure.tools.logging :as log]
            [samizdat.userspace :as userspace]))

(defn resource-dir
  "Resolve the shipped cells dir from the classpath, so a caller that is not
  running from the project root still finds resources/cells."
  []
  (when-let [url (io/resource "cells")]
    (.getPath url)))

(def default-dirs
  "Where cells live, lowest precedence first: the shipped library, then a
  project's own overrides. A later dir's cell of the same id wins (it loads
  last), so a project can replace a shipped cell without touching it.

  The shipped entry resolves through the classpath (provenance R3-11): a built
  binary started outside the project root must still find the cells it
  ships — a cwd-relative `resources/cells` there resolves to nothing and
  the kernel silently registers zero cells. The override dir stays
  cwd-relative: it belongs to the project being worked on."
  [(or (resource-dir) "resources/cells") ".samizdat/cells"])

;; Which cells this loader registered, and from which file — introspection for
;; the mutation protocol and for `dev`/debugging. {cell-id {:source path}}.
(defonce ^:private loaded-cells (atom {}))

;; The on-disk content of each file at the last SUCCESSFUL load — the known-good
;; snapshot the mutation protocol rolls a bad edit back to. {path content}.
(defonce ^:private loaded-content (atom {}))

(defn loaded [] @loaded-cells)

;; What the last SUCCESSFUL load put in the registry, for the skip in
;; load-cells!: the sources as [[id content] …] in load order, and the spec
;; object each loaded cell id resolved to once every source was in.
(defonce ^:private last-load (atom nil))

(defn- unchanged-since-last-load?
  "True when `sources` are the last successful load's sources — same ids, same
  content, same order — AND the registry still resolves every cell that load
  registered to the very spec object it registered.

  The first half is the files. The second is the registry, which is global
  mutable state: a test or another workflow may have registered its own cell
  under one of our ids, or removed one, and unchanged files are no proof the
  LOOP's cells are present. That was compile-loop's reason for loading before
  every compile; it is kept here as the guard, so the load happens exactly
  when it would change something. A defcell registers its spec through
  `constantly`, so the object the registry answers with is the one we
  recorded until somebody re-registers the id (karamazov-3n4n)."
  [sources]
  (when-let [{last-sources :sources specs :specs} @last-load]
    (and (= last-sources (mapv (juxt :id :content) sources))
         (let [live (:specs (cell/registry-snapshot))]
           (every? (fn [[id spec]] (identical? spec (get live id))) specs)))))

(defn loaded-file-content
  "The content of every cell file as it was at the last successful load — the
  last-good disk state, for the mutation protocol to restore on a rollback."
  []
  @loaded-content)

(defn- cell-files
  "The .clj files under `dir`, sorted, or nil when the dir is absent."
  [dir]
  (when (fs/exists? dir)
    (->> (fs/glob dir "**.clj")
         (map str)
         (filter #(str/ends-with? % ".clj"))
         sort)))

(defn shipped-cells
  "The cell files that ship with the harness, as RESOURCE paths, in load
  order — the :cells list of the shipped role map (resources/userspace.edn).

  Read through io/resource, so the shipped cells load from a built binary
  that has no resources/ on disk (deps.edn :jolt/build :embed bakes them
  in): a classpath has no directory listing and an embedded resource has no
  filesystem path, so a glob finds nothing there and the kernel registered
  ZERO cells — which surfaces as `Cell :loop/assemble not found in registry`
  the moment a run tries to compile its loop. userspace-files-test pins the
  map against resources/.

  Lowest precedence: a project's .samizdat/cells override still loads after
  these and wins."
  []
  (:cells (userspace/template-map)))

(defn- shipped-sources
  "The shipped cells as {:id :content} — read from the classpath (embedded or
  not). Skips any whose dir scan already produced it, so a source checkout
  loads each file once and from disk."
  [covered]
  (for [r (shipped-cells)
        :let [url (io/resource r)]
        :when (and url (not (contains? covered (last (str/split r #"/")))))]
    {:id r :content (slurp url) :file? false}))

(defn defcell-ids
  "The cell ids a cell file defines, by reading its `defcell` forms — so a
  reload attributes a cell to its file even though re-registration is not a
  'new' method, and so we never have to clear the shared registry to tell
  which cells are ours."
  [content]
  (->> (read-string (str "[" content "\n]"))
       (tree-seq coll? seq)
       (filter #(and (seq? %) (symbol? (first %))
                     (= "defcell" (name (first %)))))
       (map second)
       (filter keyword?)
       vec))

;; --- what a cell reads, against what it declares -------------------------------

(defn- ctx-reads
  "The ctx keys a handler `(fn [ctx data] …)` reads: what its first parameter
  destructures (`{:keys [a b]}`, `{x :x}`), and every `(:k ctx)`, `(get ctx :k)`
  and `(get-in ctx [:k …])` on the symbol it binds ctx to. A read through a
  function the ctx is handed to is not seen — that function declares its own
  needs where it is defined."
  [handler]
  (when (and (seq? handler) (= "fn" (name (first handler))))
    (let [params (first (filter vector? handler))
          binding (first params)
          ctx-sym (cond (symbol? binding) binding
                        (map? binding) (:as binding))
          destructured (when (map? binding)
                         (concat (map keyword (:keys binding))
                                 (keep (fn [[k v]] (when (and (symbol? k) (keyword? v)) v))
                                       binding)))
          body (rest (drop-while (complement vector?) handler))
          direct (when ctx-sym
                   (->> (tree-seq coll? seq body)
                        (keep (fn [f]
                                (when (seq? f)
                                  (let [[h a b] f]
                                    (cond
                                      (and (keyword? h) (= a ctx-sym) (= 2 (count f))) h
                                      (and (= 'get h) (= a ctx-sym) (keyword? b)) b
                                      (and (= 'get-in h) (= a ctx-sym) (vector? b)
                                           (keyword? (first b))) (first b))))))))]
      (set (concat destructured direct)))))

(defn ctx-problems
  "Every cell `text` defines whose `:requires` does not cover the ctx keys its
  handler reads, as [{:cell :undeclared :requires}] — :requires being what it
  should declare. A cell with no :requires at all is listed with :missing?.
  [] when every cell says what it reads.

  `:requires` is what compile-definition holds a manifest to: a cell asking
  for a key no driver provides is refused there. That is only worth anything
  if :requires is TRUE, and nothing checked it outside a test — :beam/escalate
  read :conn and :beam-width, declared neither, and would have loaded from an
  agent's edit."
  [text]
  (vec (for [form (read-string {:read-cond :allow} (str "[" text "\n]"))
             :when (and (seq? form) (symbol? (first form))
                        (= "defcell" (name (first form))))
             :let [[_ id meta-map handler] form
                   declared (set (:requires meta-map))
                   reads (ctx-reads handler)
                   undeclared (sort (remove declared reads))]
             :when (or (not (contains? meta-map :requires)) (seq undeclared))]
         (cond-> {:cell id :undeclared (vec undeclared)
                  :requires (vec (sort (into declared reads)))}
           (not (contains? meta-map :requires)) (assoc :missing? true)))))

(declare ctx-problem-message)

(defn requires-problem
  "Why `text`'s cells are not self-consistent about ctx — the sentence to
  refuse an edit with, naming each cell and the :requires to write — or nil.
  The cell validator and the mutation protocol both refuse through this."
  [text]
  ;; A text that does not READ is not this check's to report: the load step
  ;; says what is wrong with it, with a line and column.
  (when-let [ps (seq (try (ctx-problems text) (catch Throwable _ nil)))]
    (ctx-problem-message ps)))

(defn- ctx-problem-message
  "The refusal, from prompts/cell-requires.md."
  [problems]
  (str/trim
   (prompt/render "cell-requires"
                  {:cells (for [{:keys [cell undeclared requires missing?]} problems]
                            {:cell (pr-str cell)
                             :undeclared (str/join ", " undeclared)
                             :requires (pr-str requires)
                             :missing-only (and missing? (empty? undeclared))})})))

;; --- earned effect marks ------------------------------------------------------
;;
;; A cell's :pure / :effects mark is load-bearing: the mutation soak stubs
;; every non-pure cell to identity so a dry run does no IO, and mycelium's
;; validate-effects-declaration! checks the mark for SHAPE — :pure only true,
;; :effects a non-empty vector — and nothing else. A cell marked :pure that
;; calls slurp passed validate and ran its IO inside the soak; a cell marked
;; :effects [:fs] that called the provider was stubbed as if it only touched
;; the filesystem. BendTT's rule for the same situation is that a kind is
;; EARNED: the type declares it and the checker walks every constructor
;; (karamazov-viht.1). This is that walk for a cell file.
;;
;; The walk is over SOURCE, the same forms defcell-ids reads, so it runs before
;; the candidate is load-stringed into the image — a mis-marked cell is refused
;; before it is installed, like a shadowed id. What it reaches is decided by a
;; catalog that is data (gates.edn :effect-symbols), keyed by effect, each
;; entry a symbol: a bare name for a core fn (slurp), a namespace for one whose
;; every var is that effect (samizdat.store), or a qualified var for one in a
;; mixed namespace (samizdat.agent.loop/call-model). Anything the catalog does
;; not name is not a hit: a scan cannot be complete, so it errs toward
;; accepting, which is why the mark stays required rather than inferred.

(defn- source-forms
  [content]
  (read-string (str "[" content "\n]")))

(defn- ns-aliases
  "{alias full-ns} from the file's ns form, so `llm/chat` in a body reads as
  samizdat.llm.client/chat against the catalog."
  [forms]
  (let [ns-form (first (filter #(and (seq? %) (= 'ns (first %))) forms))]
    (into {}
          (for [clause (rest ns-form)
                :when (and (seq? clause) (= :require (first clause)))
                spec (rest clause)
                :when (vector? spec)
                :let [[nsym & {:keys [as]}] spec]
                :when as]
            [as nsym]))))

(defn- called-symbols
  "Every symbol in `form` outside a quoted subform — the calls a body can
  make. A quoted list is data."
  [form]
  (letfn [(walk [f acc]
            (cond (and (seq? f) (= 'quote (first f))) acc
                  (symbol? f) (conj acc f)
                  (coll? f) (reduce #(walk %2 %1) acc f)
                  :else acc))]
    (walk form #{})))

(defn- resolve-alias
  [aliases s]
  (if-let [full (some->> (namespace s) symbol (get aliases))]
    (symbol (str full) (name s))
    s))

(defn- catalog-hit?
  "Whether the resolved symbol `s` is what catalog entry `e` names: the same
  var, a var in that namespace or under it, or — for a bare entry — a bare
  call of that name (or its clojure.core spelling)."
  [e s]
  (let [ens (namespace e) sns (namespace s)]
    (cond
      ens (= e s)
      sns (or (= sns (str e)) (str/starts-with? sns (str e ".")))
      :else (= (name s) (str e)))))

(defn implied-effects
  "What each cell in `content` reaches that `catalog` names as an effect:
  {cell-id {:declared {:pure true} | {:effects #{…}} | {}
            :implied  {effect #{resolved-symbol …}}}}.
  A cell's symbols are its own plus those of every top-level defn in the
  file it reaches, transitively, so an effect cannot be laundered through a
  local helper. Only effects with a hit appear under :implied."
  [content catalog]
  (let [forms (source-forms content)
        aliases (ns-aliases forms)
        top? (fn [f heads] (and (seq? f) (symbol? (first f))
                                (contains? heads (name (first f)))))
        locals (into {}
                     (for [f forms :when (top? f #{"defn" "defn-" "def"})]
                       [(second f) (called-symbols (drop 2 f))]))
        ;; `seq` on every recur: concat is lazy and never nil, and a loop
        ;; that tests the queue for nil would spin forever once it empties.
        expand (fn [syms]
                 (loop [seen #{} todo (seq syms)]
                   (if-not todo
                     seen
                     (let [s (first todo)
                           more (when-not (seen s) (get locals s))]
                       (recur (conj seen s)
                              (seq (concat (rest todo) (remove seen more))))))))
        declared (fn [opts]
                   (cond (true? (:pure opts)) {:pure true}
                         (seq (:effects opts)) {:effects (set (:effects opts))}
                         :else {}))]
    (into {}
          (for [f forms :when (top? f #{"defcell"})
                :let [[_ id opts & body] f]
                :when (keyword? id)]
            (let [syms (->> (called-symbols body) expand (map #(resolve-alias aliases %)))]
              [id {:declared (declared (when (map? opts) opts))
                   :implied (into {}
                                  (for [[effect entries] catalog
                                        :let [hits (set (for [s syms e entries
                                                              :when (catalog-hit? e s)]
                                                          s))]
                                        :when (seq hits)]
                                    [effect hits]))}])))))

(defn effect-problems
  "The cells in `content` whose mark does not cover what their body implies,
  as [{:id :declared :missing #{effect} :evidence {effect #{symbol}}} …]:
  a :pure cell with any hit, or an :effects cell missing one. An undeclared
  cell is not this function's complaint — mycelium's :undeclared-effects
  warning already refuses it — and a clean file is []."
  [content catalog]
  (vec
   (for [[id {:keys [declared implied]}] (implied-effects content catalog)
         :let [covered (or (:effects declared) #{})
               missing (when (seq declared) (set (remove covered (keys implied))))]
         :when (seq missing)]
     {:id id :declared declared :missing missing
      :evidence (select-keys implied missing)})))

(defn- load-source!
  "Load one cell SOURCE into the live image; return the cell ids it defines.

  Takes {:id :content} rather than a path, because a shipped cell may come
  from an embedded resource with no file behind it.

  The load is wrapped in a *ns* binding: a cell file begins with an `(ns …)`
  form, and load-string's evaluation of it switches *ns* and does not restore
  it — leaking the cell namespace into whatever called the loader, and breaking
  a second load. Binding *ns* to itself reverts it on exit, so the loader is
  repeatable (the reload the mutation protocol needs) and leaves the caller's
  namespace untouched."
  [{:keys [content]}]
  (binding [*ns* *ns*]
    (load-string content))
  (defcell-ids content))

(defn- cell-names
  "The names the shipped cell templates are known by in the userspace store —
  the resource basename without its extension."
  []
  (mapv #(str/replace (last (str/split % #"/")) #"\.clj$" "") (shipped-cells)))

(defn- project-sources
  "The project's cells as {:id :content}, seeded from the shipped templates on
  first use and read back from the project's own store.

  This is what makes the cell layer USERSPACE rather than harness state. Before
  it, `reload_cells` edited the file in resources/ — the file every other
  project loads — so a supervisor 'changing its cells' was changing the
  harness. Now the template seeds a copy into the project and every later edit
  is a version of that copy.

  `dirs` are still read, and still win, but as an additional SEED source: a
  `.samizdat/cells` file is how a project starts with a cell the harness never
  shipped, and once seeded the store is authoritative for it too."
  [dirs]
  (let [files (mapcat cell-files dirs)
        ;; A dir file's name is its basename, so it seeds over a shipped
        ;; template of the same name — the documented override, now expressed
        ;; as "this project starts from a different template".
        from-dirs (into {} (for [p files]
                             [(str/replace (last (str/split (str p) #"/"))
                                           #"\.clj$" "")
                              (slurp p)]))]
    ;; A dir file that differs from the store's copy is recorded as the
    ;; project's newest version. It used to be saved only when the store had
    ;; NOTHING under the name — and a shipped cell always has its template
    ;; there — so an override of a shipped cell never landed and the store's
    ;; copy ran instead (karamazov-1a51.4).
    (doseq [[nm body] from-dirs]
      (when (and (userspace/bound?) (not= body (userspace/body :cell nm)))
        (userspace/save! :cell nm body)))
    (let [bodies (merge (userspace/seed-all! :cell (cell-names))
                        ;; And the dir content wins the load either way —
                        ;; unbound (a test, a bare REPL) it is the only source.
                        from-dirs)
          ;; Shipped templates load FIRST, in their shipped order; everything
          ;; the project added after them, sorted for determinism. Later
          ;; load-string wins in the registry, so this is what makes "a
          ;; project cell overrides a shipped cell-id" true by construction —
          ;; a plain (sort-by key) made precedence depend on how a project
          ;; name happened to sort against the template basenames
          ;; (karamazov-blt.8).
          shipped-order (into {} (map-indexed (fn [i n] [n i]) (cell-names)))]
      (for [[nm body] (sort-by (fn [[nm _]] [(get shipped-order nm 999999) nm])
                               bodies)]
        {:id nm :content body :file? false :store? true}))))

(defn project-cells-dir
  "Where a project keeps its cells: <root>/.samizdat/cells."
  []
  (some-> (userspace/project-dir) (str "/cells")))

(defn- file-sources
  "A project's cells in file mode: the :cells list of its role map, in that
  order — and nothing else. The project owns its whole set, so a cell it
  dropped from the map stays dropped and one it changed is its own. Each text
  comes through userspace, so an edit that does not pass its check is not
  loaded: the last version that passed is. Keyed by PATH, which is what the
  mutation protocol's file checkpoint restores."
  []
  (for [rel (:cells (userspace/role-map))
        :let [p (str (userspace/project-dir) "/" rel)
              content (userspace/body :cell (str/replace (last (str/split rel #"/")) #"\.clj$" ""))]
        :when (or content
                  (do (log/warn "cells: the role map lists" rel "but it has no text that passes")
                      false))]
    {:id p :content content :file? true}))

(defn current-dirs
  "The cell dirs the running image loads from: the project's own in file
  mode, the shipped library plus .samizdat/cells otherwise."
  []
  (if (userspace/files?) [(project-cells-dir)] default-dirs))

(defn- dir-sources
  "The legacy source set: shipped resources plus a scan of `dirs`, with no
  project store involved. What `(load-cells! dirs)` still does, and what a
  test loading a temp directory needs."
  [dirs]
  (let [files (mapcat cell-files dirs)]
    (concat (shipped-sources
             (set (map #(last (str/split (str %) #"/")) files)))
            (for [p files] {:id p :content (slurp p) :file? true}))))

(declare load-sources!)

(defn load-cells!
  "Load the project's cells into the live image, registering them.

  Two modes, and the difference is where the bodies come from:

  `(load-cells!)` — THE PROJECT's cells: seeded from the shipped templates
  into the project's userspace store on first use, then read from that store,
  so an edit the supervisor makes is a version of this project's copy and no
  other project sees it. `.samizdat/cells` files seed alongside the templates.
  With no project bound (a test, a bare REPL) this reads the templates
  directly, which is what the harness did before the store existed.

  `(load-cells! dirs)` — a literal source scan of `dirs` plus the shipped
  resources, with no store. The seam a test loading a temp directory needs,
  and deliberately not the production path.

  Transactional either way: on any error the registry is restored to its prior
  state and the error rethrown, so a bad cell never half-loads. Returns the
  loaded map {cell-id {:source name}}.

  Free when nothing changed: the same sources as the last successful load,
  with the registry still holding what that load registered, return that
  load's map without evaluating a file (unchanged-since-last-load?).
  compile-loop calls this before every compile, and re-evaluating twelve
  files cost about a second each time — most of the test suite's minutes."
  ([] (load-cells! nil))
  ([dirs]
   ;; nil means "the project"; an explicit dir list means the legacy scan.
   ;; Distinguishing on the ARGUMENT rather than on a flag keeps every
   ;; existing caller and test meaning exactly what it meant.
   (let [sources (cond
                   (some? dirs) (dir-sources dirs)
                   (userspace/files?) (file-sources)
                   ;; The project's override dir only for a bound project.
                   ;; It is cwd-relative, and unbound (a test, a bare REPL)
                   ;; the cwd is whatever checkout this runs in — this
                   ;; repository's own .samizdat/cells ran in place of the
                   ;; shipped cells its tests were testing.
                   (userspace/bound?) (project-sources default-dirs)
                   :else (project-sources (take 1 default-dirs)))]
     (if (unchanged-since-last-load? sources)
       @loaded-cells
       (load-sources! dirs sources)))))

(defn- load-sources!
  "The full load: every source evaluated in order, transactionally."
  [dirs sources]
   (let [snapshot (cell/registry-snapshot)]
     (try
       (let [loaded (reduce (fn [acc src]
                              (into acc (for [id (load-source! src)]
                                          [id {:source (:id src)
                                               :store? (boolean (:store? src))}])))
                            {}
                            sources)]
         ;; Drop any cell we loaded before that is gone from the new set (a
         ;; deleted cell / removed defcell), WITHOUT clearing the shared
         ;; registry — other code and tests hold cells here that are not ours.
         (doseq [id (remove (set (keys loaded)) (keys @loaded-cells))]
           (cell/remove-cell! id))
         (reset! loaded-cells loaded)
         ;; The known-good content of every source, for the mutation
         ;; protocol's rollback. Keyed by whatever identifies the source:
         ;; a path for the legacy scan, a store name for the project path.
         ;; Only reached on success, so it never records a half-loaded state.
         (reset! loaded-content
                 (into {} (for [src sources :when (or (:file? src) (:store? src))]
                            [(:id src) (:content src)])))
         (reset! last-load
                 {:sources (mapv (juxt :id :content) sources)
                  :specs (select-keys (:specs (cell/registry-snapshot))
                                      (keys loaded))})
         loaded)
       (catch Throwable e
         (cell/registry-restore! snapshot)
         (throw (ex-info (str "cell load failed; registry rolled back: "
                              (ex-message e))
                         {:dirs dirs} e))))))

;; A cell edit is checked before it is loaded for real (karamazov-1a51.8): it
;; has to read, it has to define a cell, and it has to LOAD — tried inside a
;; registry snapshot that is put back whatever happens, so a candidate that
;; fails half way registers nothing. Whether the manifests still compile
;; against it is the manifest check's, and the mutation protocol's for an edit
;; made through the cell tool.
(userspace/register-validator!
 :cell
 (fn [_ text]
   (let [forms (try {:ids (defcell-ids text)}
                    (catch Throwable e {:problem (userspace/problem :read e)}))]
     (cond
       (:problem forms) (:problem forms)
       (empty? (:ids forms)) {:stage :shape :message "defines no cell"}
       ;; Self-consistent before it loads: a cell's :requires is the promise
       ;; manifests are compiled against, so one that reads more than it
       ;; declares is refused with what to write instead.
       (requires-problem text)
       {:stage :requires :message (requires-problem text)}
       :else
       (let [snapshot (cell/registry-snapshot)]
         (try (binding [*ns* *ns*] (load-string text)) nil
              (catch Throwable e (userspace/problem :load e))
              (finally (cell/registry-restore! snapshot))))))))
