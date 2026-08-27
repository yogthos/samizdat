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

(def shipped-cells
  "The cell files that ship with the harness, as RESOURCE names.

  Enumerated rather than globbed, and read through io/resource, so the shipped
  cells load from a built binary that has no resources/ on disk (deps.edn
  :jolt/build :embed bakes them in). A classpath has no directory listing and
  an embedded resource has no filesystem path for `.getPath` to name, so the
  glob above finds nothing there and the kernel registered ZERO cells — which
  surfaces as `Cell :loop/assemble not found in registry` the moment a run
  tries to compile its loop. Same reasoning as workflow/factory-manifest-names;
  both are pinned against their directory by a test so the list cannot drift.

  Lowest precedence: a project's .samizdat/cells override still loads after
  these and wins, and in a source checkout the dir scan reads the very same
  files, so a shipped cell edited in place is picked up from disk."
  ["cells/beam.clj" "cells/board.clj" "cells/critic.clj" "cells/decompose.clj"
   "cells/feature.clj" "cells/loop.clj" "cells/probe.clj" "cells/repair.clj"
   "cells/team.clj"])

(defn- shipped-sources
  "The shipped cells as {:id :content} — read from the classpath (embedded or
  not). Skips any whose dir scan already produced it, so a source checkout
  loads each file once and from disk."
  [covered]
  (for [r shipped-cells
        :let [url (io/resource r)]
        :when (and url (not (contains? covered (last (str/split r #"/")))))]
    {:id r :content (slurp url) :file? false}))

(defn- defcell-ids
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

(def ^:private cell-names
  "The names the shipped cell templates are known by in the userspace store —
  the resource basename without its extension. `shipped-cells` stays the
  resource list; this is the same set as store keys."
  (mapv #(str/replace (last (str/split % #"/")) #"\.clj$" "") shipped-cells))

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
    (doseq [[nm body] from-dirs]
      (when-not (userspace/body :cell nm)
        (userspace/save! :cell nm body)))
    (let [bodies (merge (userspace/seed-all! :cell cell-names)
                        ;; Unbound (a test, a bare REPL) there is no store to
                        ;; seed into, so the dir content IS the source.
                        (when-not (userspace/bound?) from-dirs))
          ;; Shipped templates load FIRST, in their shipped order; everything
          ;; the project added after them, sorted for determinism. Later
          ;; load-string wins in the registry, so this is what makes "a
          ;; project cell overrides a shipped cell-id" true by construction —
          ;; a plain (sort-by key) made precedence depend on how a project
          ;; name happened to sort against the template basenames
          ;; (karamazov-blt.8).
          shipped-order (into {} (map-indexed (fn [i n] [n i]) cell-names))]
      (for [[nm body] (sort-by (fn [[nm _]] [(get shipped-order nm 999999) nm])
                               bodies)]
        {:id nm :content body :file? false :store? true}))))

(defn- dir-sources
  "The legacy source set: shipped resources plus a scan of `dirs`, with no
  project store involved. What `(load-cells! dirs)` still does, and what a
  test loading a temp directory needs."
  [dirs]
  (let [files (mapcat cell-files dirs)]
    (concat (shipped-sources
             (set (map #(last (str/split (str %) #"/")) files)))
            (for [p files] {:id p :content (slurp p) :file? true}))))

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
  loaded map {cell-id {:source name}}."
  ([] (load-cells! nil))
  ([dirs]
   (let [snapshot (cell/registry-snapshot)
         ;; nil means "the project"; an explicit dir list means the legacy
         ;; scan. Distinguishing on the ARGUMENT rather than on a flag keeps
         ;; every existing caller and test meaning exactly what it meant.
         sources (if (nil? dirs)
                   (project-sources default-dirs)
                   (dir-sources dirs))]
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
         loaded)
       (catch Throwable e
         (cell/registry-restore! snapshot)
         (throw (ex-info (str "cell load failed; registry rolled back: "
                              (ex-message e))
                         {:dirs dirs} e)))))))
