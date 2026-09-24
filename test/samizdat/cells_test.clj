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

(ns samizdat.cells-test
  "The cell loader: the kernel is cell-agnostic — it loads whatever cell
  definitions live in resources (and .samizdat overrides), registers them, and
  can reload them into the live image. No cell is baked into src."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [jolt.fs :as fs]
            [mycelium.cell :as cell]
            [samizdat.cells :as cells]
            [samizdat.store.db :as db]
            [samizdat.userspace :as userspace]))

(def ^:private tmp (atom nil))

(defn- cell-file! [dir id-kw body]
  (fs/create-dirs dir)
  (spit (str dir "/" (name id-kw) ".clj")
        (str "(ns cells.gen." (name id-kw)
             " (:require [mycelium.cell :as cell]))\n"
             "(cell/defcell " id-kw " {:doc \"a generated cell\" :pure true :requires []}\n"
             "  " body ")\n")))

(use-fixtures :each
  (fn [f]
    (cell/clear-registry!)
    (reset! tmp (str "/tmp/samizdat-cells-" (random-uuid)))
    (try (f) (finally (fs/delete-tree @tmp) (cell/clear-registry!)))))

;; --- loading ----------------------------------------------------------------

(deftest shipped-cells-match-what-ships
  ;; The shipped cells are enumerated as resource names rather than globbed:
  ;; `jolt build` bakes resources/ into the binary (deps.edn :jolt/build
  ;; :embed), and an embedded resource has no filesystem path for the glob to
  ;; walk — so a built binary run outside the project root registered zero
  ;; cells and every run died with "Cell :loop/assemble not found in
  ;; registry". An enumerated list cannot drift on its own, so pin it.
  (let [on-disk (->> (fs/glob "resources/cells" "**.clj")
                     (map #(str "cells/" (last (clojure.string/split (str %) #"/"))))
                     set)]
    (is (seq on-disk) "resources/cells is readable from the test's cwd")
    (is (= on-disk (set (cells/shipped-cells)))
        (str "(cells/shipped-cells) and resources/cells disagree; missing: "
             (sort (remove (set (cells/shipped-cells)) on-disk))
             ", listed but absent: "
             (sort (remove on-disk (set (cells/shipped-cells)))))))
  (testing "every shipped name resolves on the classpath"
    (doseq [r (cells/shipped-cells)]
      (is (some? (clojure.java.io/resource r)) (str r " does not resolve")))))

(deftest every-shipped-cell-require-is-reachable-without-load-string
  ;; A shipped cell is load-stringed, so a namespace it requires that NOTHING
  ;; in src reaches is absent from a `jolt build` image and the cell dies with
  ;; "Could not locate … on the source roots". samizdat.cell-prelude exists to
  ;; pull those onto the compile graph; samizdat.agent.decompose had fallen
  ;; off it. Walk the requires rather than trusting the list stays current.
  ;;
  ;; Reachability is FROM THE BINARY'S ENTRY: `jolt build -m samizdat.core`
  ;; embeds that namespace's require closure and nothing else, so that is what
  ;; must already have loaded these. Without this require the test only
  ;; passed after other namespaces in the suite had loaded the harness, and
  ;; failed 23 times on its own.
  (require 'samizdat.core)
  (let [required (->> (cells/shipped-cells)
                      (keep clojure.java.io/resource)
                      (mapcat #(re-seq #"\[(samizdat\.[a-z0-9.-]+)" (slurp %)))
                      (map second)
                      set)]
    (is (seq required) "the shipped cells were readable")
    (doseq [ns-name required]
      (is (some? (find-ns (symbol ns-name)))
          (str ns-name " is required by a shipped cell but is not loaded — add"
               " it to samizdat.cell-prelude or it will be missing from a"
               " built binary")))))

(deftest loads-every-cell-file-in-a-dir
  (let [d (str @tmp "/cells")]
    (cell-file! d :gen/a "(fn [_ data] (assoc data :a 1))")
    (cell-file! d :gen/b "(fn [_ data] (assoc data :b 2))")
    (cells/load-cells! [d])
    (is (some? (cell/get-cell :gen/a)))
    (is (some? (cell/get-cell :gen/b)))
    (testing "the handlers actually run"
      (is (= {:a 1} ((:handler (cell/get-cell :gen/a)) {} {})))))
  (testing "loaded reports what was registered and from where"
    (is (contains? (set (keys (cells/loaded))) :gen/a))))

(deftest a-later-dir-overrides-an-earlier-cell
  ;; builtin (resources/cells) then project (.samizdat/cells): a project cell
  ;; with the same id wins.
  (let [base (str @tmp "/base") proj (str @tmp "/proj")]
    (cell-file! base :ov/c "(fn [_ data] (assoc data :from :base))")
    (cell-file! proj :ov/c "(fn [_ data] (assoc data :from :proj))")
    (cells/load-cells! [base proj])
    (is (= {:from :proj} ((:handler (cell/get-cell :ov/c)) {} {})))))

;; --- transactional rollback -------------------------------------------------

(deftest a-broken-cell-file-rolls-the-whole-load-back
  (let [d (str @tmp "/cells")]
    ;; a good cell registered from a PRIOR load — must survive a failed reload
    (cell-file! d :keep/good "(fn [_ data] data)")
    (cells/load-cells! [d])
    (is (some? (cell/get-cell :keep/good)))
    ;; now add a broken file and a would-be new cell, then reload
    (cell-file! d :new/one "(fn [_ data] data)")
    (spit (str d "/broken.clj") "(ns cells.gen.broken)\n(this is not valid clojure")
    (is (thrown? Exception (cells/load-cells! [d])))
    (testing "the registry is restored to its pre-load state — no partial load"
      (is (some? (cell/get-cell :keep/good)) "the previously-good cell survives")
      (is (nil? (cell/get-cell :new/one)) "the new cell from the failed load is not left registered"))))

;; --- hot reload -------------------------------------------------------------

(deftest reload-picks-up-an-edited-cell
  (let [d (str @tmp "/cells")]
    (cell-file! d :hot/x "(fn [_ data] (assoc data :v 1))")
    (cells/load-cells! [d])
    (is (= 1 (:v ((:handler (cell/get-cell :hot/x)) {} {}))))
    ;; edit the cell on disk and reload — the live registry reflects it
    (cell-file! d :hot/x "(fn [_ data] (assoc data :v 2))")
    (cells/load-cells! [d])
    (is (= 2 (:v ((:handler (cell/get-cell :hot/x)) {} {}))))))

;; --- an unchanged load does no work ------------------------------------------

(defn- token-of
  "The per-load token a generated cell closes over: a load-string makes a new
  one, a skipped load keeps the old one — proof of a reload that no jolt
  version can fake by handing back the same fn object."
  [id]
  (:token ((:handler (cell/get-cell id)) {} {})))

(def ^:private tokened "(let [t (Object.)] (fn [_ data] (assoc data :token t)))")

(deftest an-unchanged-source-set-is-not-reloaded
  ;; compile-loop reloads the cells before EVERY compile, and a reload that
  ;; re-evaluated twelve files when nothing had changed cost about a second —
  ;; 168 times over in three test namespaces alone (karamazov-3n4n). The
  ;; unchanged case must be free.
  (let [d (str @tmp "/cells")]
    (cell-file! d :same/a tokened)
    (let [first-load (cells/load-cells! [d])
          t1 (token-of :same/a)]
      (is (= first-load (cells/load-cells! [d])) "the second load reports the same cells")
      (is (identical? t1 (token-of :same/a)) "and did not re-evaluate the file")
      (testing "an edit still reloads"
        (cell-file! d :same/a "(let [t (Object.)] (fn [_ data] (assoc data :token t :v 2)))")
        (cells/load-cells! [d])
        (is (not (identical? t1 (token-of :same/a))))
        (is (= 2 (:v ((:handler (cell/get-cell :same/a)) {} {}))))))))

(deftest a-registry-touched-by-someone-else-is-reloaded
  ;; The registry is global mutable state, and unchanged files are not proof
  ;; the LOOP's cells are present: a test or another workflow may have
  ;; registered its own under one of our ids, or removed one. Only a registry
  ;; still holding exactly what the last load registered may be skipped.
  (let [d (str @tmp "/cells")]
    (cell-file! d :ours/a tokened)
    (cell-file! d :ours/b tokened)
    (cells/load-cells! [d])
    (let [t1 (token-of :ours/a)]
      (testing "an id re-registered by another party"
        (cell/defcell :ours/a {:doc "an impostor" :pure true}
          (fn [_ data] (assoc data :who :them)))
        (is (= :them (:who ((:handler (cell/get-cell :ours/a)) {} {}))))
        (cells/load-cells! [d])
        (is (nil? (:who ((:handler (cell/get-cell :ours/a)) {} {}))) "ours is back")
        (is (not (identical? t1 (token-of :ours/a))) "by a real reload"))
      (testing "an id removed by another party"
        (cell/remove-cell! :ours/b)
        (is (nil? (cell/get-cell :ours/b)))
        (cells/load-cells! [d])
        (is (some? (cell/get-cell :ours/b)))))))

;; --- the shipped loop cells load from resources -----------------------------

(deftest the-shipped-cells-dir-follows-the-classpath
  ;; provenance R3-11: default-dirs carried the relative "resources/cells", so a
  ;; built binary started outside the project root found no cells and ran no
  ;; loop — silently, with zero registrations. The shipped entry must be the
  ;; classpath answer (which follows the binary), not a cwd-relative guess.
  (let [rdir (cells/resource-dir)]
    (is (some? rdir) "the classpath carries resources/cells")
    (is (= rdir (first cells/default-dirs))
        "default-dirs' shipped entry is the classpath-resolved dir")))

(deftest with-no-project-bound-the-shipped-cells-are-what-load
  ;; The override dir in default-dirs is cwd-relative, and a checkout of this
  ;; repository is itself a samizdat project: its .samizdat/cells held its
  ;; own evolved oversight.clj, so the suite ran THAT instead of the cell it
  ;; was testing, and a change to resources/cells was not what the tests saw.
  ;; A project's cells are read when a project is bound, not by whoever
  ;; happens to be standing in its directory.
  (let [stray (str @tmp "/stray")]
    (cell-file! stray :cwd/stray "(fn [_ d] d)")
    (with-redefs [cells/default-dirs [(cells/resource-dir) stray]]
      (cells/load-cells!)
      (is (some? (cell/get-cell :loop/route)) "the shipped cells load")
      (is (nil? (cell/get-cell :cwd/stray)) "the cwd's project cells do not"))))

(deftest the-loop-cells-load-from-resources
  ;; No cell is compiled into src: loading from resources/cells registers the
  ;; whole loop. This is the acceptance — the kernel is cell-agnostic.
  (cells/load-cells!)
  (doseq [id [:loop/assemble :llm/infer :llm/parse :tool/dispatch
              :journal/record :gate/settle :gate/arbiter :loop/route :loop/finish]]
    (is (some? (cell/get-cell id)) (str id " loaded from resources")))
  (testing "every loaded loop cell declares its effects (pure or a set)"
    (doseq [id (keys (cells/loaded))]
      (is (cell/effects-declared? (cell/get-cell id))
          (str id " must declare :pure or :effects")))))

(deftest a-project-cell-overrides-a-shipped-id-whatever-its-name-sorts-as
  ;; Store-mode loading sorted bodies alphabetically by store name, so whether
  ;; a project cell's redefinition of a shipped cell-id won depended on how
  ;; its name happened to sort against the template basenames — "aaa-custom"
  ;; loaded FIRST and the shipped template silently overrode it
  ;; (karamazov-blt.8). Shipped templates now load first, project extras
  ;; after, so the project wins by construction.
  (let [c (db/open! ":memory:")]
    (try
      (userspace/bind! c)
      (userspace/save! :cell "aaa-custom"
                       (str "(ns cells.custom (:require [mycelium.cell :as cell]))\n"
                            "(cell/defcell :gate/arbiter"
                            " {:doc \"overridden-by-project\" :pure true}\n"
                            "  (fn [_ d] d))\n"))
      (cells/load-cells!)
      (is (= "overridden-by-project" (:doc (cell/get-cell :gate/arbiter)))
          "the project's redefinition wins regardless of its store name")
      (finally
        (userspace/unbind!)
        (db/close c)
        ;; restore the template registry for whatever runs next
        (cells/load-cells!)))))

;; --- earned effect marks (karamazov-viht.1) ----------------------------------
;;
;; BendTT 2.4: a type declares its kind and the checker EARNS it by walking
;; every constructor. A cell's :pure / :effects mark is the thing the mutation
;; soak stubs by, and until now it was checked for shape and never against
;; the body — a cell marked :pure that calls slurp ran its IO inside the soak.
;; `implied-effects` walks the body and says what it reaches, by a catalog
;; that is data (gates.edn :effect-symbols), so the mark can be checked.

(def ^:private catalog
  '{:fs   [slurp spit clojure.java.io samizdat.agent.files]
    :net  [samizdat.llm.client samizdat.agent.loop/call-model]
    :db   [samizdat.store samizdat.userspace/save!]
    :proc [samizdat.engine.proc clojure.java.shell]})

(defn- one-cell [decl body & [requires]]
  (str "(ns cells.gen.t (:require [mycelium.cell :as cell]" requires "))\n"
       "(cell/defcell :t/x {:doc \"t\" " decl "}\n  " body ")\n"))

(deftest a-bare-core-call-implies-its-effect
  (let [r (cells/implied-effects (one-cell ":pure true" "(fn [_ d] (assoc d :s (slurp \"f\")))") catalog)]
    (is (= {:pure true} (get-in r [:t/x :declared])))
    (is (= {:fs #{'slurp}} (get-in r [:t/x :implied])))))

(deftest an-aliased-call-resolves-through-the-ns-form
  ;; The body says llm/chat; the catalog names samizdat.llm.client. The
  ;; ns form's :require is what connects them, so the walk reads it.
  (let [r (cells/implied-effects
           (one-cell ":effects [:net]" "(fn [ctx d] (llm/chat (:llm-adapter ctx) {} []))"
                     " [samizdat.llm.client :as llm]")
           catalog)]
    (is (= {:net #{'samizdat.llm.client/chat}} (get-in r [:t/x :implied])))))

(deftest a-namespace-entry-covers-its-sub-namespaces
  ;; samizdat.store in the catalog matches samizdat.store.journal/note!: the
  ;; store's namespaces are many and every one of them is the database.
  (let [r (cells/implied-effects
           (one-cell ":effects [:db]" "(fn [ctx d] (journal/note! (:conn ctx) 1 :k {}) d)"
                     " [samizdat.store.journal :as journal]")
           catalog)]
    (is (= {:db #{'samizdat.store.journal/note!}} (get-in r [:t/x :implied])))))

(deftest a-single-var-entry-does-not-taint-its-namespace
  ;; samizdat.agent.loop is mixed: call-model is the provider, absorb-response
  ;; is pure. Only the named var is a hit.
  (let [pure (cells/implied-effects
              (one-cell ":pure true" "(fn [ctx d] (turn/absorb-response d))"
                        " [samizdat.agent.loop :as turn]")
              catalog)
        net (cells/implied-effects
             (one-cell ":pure true" "(fn [ctx d] (turn/call-model ctx d))"
                       " [samizdat.agent.loop :as turn]")
             catalog)]
    (is (= {} (get-in pure [:t/x :implied])))
    (is (= {:net #{'samizdat.agent.loop/call-model}} (get-in net [:t/x :implied])))))

(deftest a-local-helper-hands-its-effects-to-the-cells-that-call-it
  ;; The IO is in a defn beside the cell; the cell calls the defn. Attributed
  ;; transitively within the file, or the mark could be laundered through one
  ;; indirection.
  (let [src (str "(ns cells.gen.t (:require [mycelium.cell :as cell]))\n"
                 "(defn- read-it [p] (slurp p))\n"
                 "(defn- via [p] (read-it p))\n"
                 "(cell/defcell :t/x {:doc \"t\" :pure true}\n  (fn [_ d] (assoc d :s (via \"f\"))))\n"
                 "(cell/defcell :t/y {:doc \"t\" :pure true}\n  (fn [_ d] d))\n")
        r (cells/implied-effects src catalog)]
    (is (= {:fs #{'slurp}} (get-in r [:t/x :implied])))
    (is (= {} (get-in r [:t/y :implied])) "the helper's effects reach only its callers")))

(deftest a-quoted-form-is-data-not-a-call
  (let [r (cells/implied-effects (one-cell ":pure true" "(fn [_ d] (assoc d :ops '(slurp spit)))") catalog)]
    (is (= {} (get-in r [:t/x :implied])))))

(deftest an-unknown-symbol-is-not-a-hit
  ;; A scan cannot be complete, so it errs toward accepting: that is why the
  ;; mark stays REQUIRED rather than inferred.
  (let [r (cells/implied-effects (one-cell ":pure true" "(fn [_ d] (frobnicate d))") catalog)]
    (is (= {} (get-in r [:t/x :implied])))))

(deftest effect-problems-names-what-the-mark-fails-to-cover
  (let [pure (cells/effect-problems (one-cell ":pure true" "(fn [_ d] (assoc d :s (slurp \"f\")))") catalog)
        under (cells/effect-problems
               (one-cell ":effects [:fs]" "(fn [ctx d] (llm/chat (:llm-adapter ctx) {} []))"
                         " [samizdat.llm.client :as llm]")
               catalog)
        honest (cells/effect-problems
                (one-cell ":effects [:fs :net]" "(fn [ctx d] (slurp \"f\") (llm/chat (:llm-adapter ctx) {} []))"
                          " [samizdat.llm.client :as llm]")
                catalog)
        undeclared (cells/effect-problems (one-cell "" "(fn [_ d] (slurp \"f\"))") catalog)]
    (is (= [{:id :t/x :declared {:pure true} :missing #{:fs} :evidence {:fs #{'slurp}}}] pure))
    (is (= [{:id :t/x :declared {:effects #{:fs}} :missing #{:net}
             :evidence {:net #{'samizdat.llm.client/chat}}}] under))
    (is (= [] honest))
    (is (= [] undeclared) "an undeclared cell is the existing warning's business, not a second complaint")))

(deftest every-shipped-cell-earns-its-mark-against-the-shipped-catalog
  ;; The pin that keeps the catalog and the cells agreeing: a shipped cell
  ;; whose body reaches an effect its mark does not name, or a catalog entry
  ;; too broad for a shipped pure cell, fails here and names both.
  (let [cat (samizdat.agent.gates/threshold :effect-symbols)]
    (is (map? cat) "the catalog is read from gates.edn, not a constant")
    (is (= #{:fs :net :db :proc} (set (keys cat))) "one entry per effect in the cell vocabulary")
    (doseq [n (cells/shipped-cells)
            :let [content (slurp (clojure.java.io/resource n))]]
      (is (= [] (cells/effect-problems content cat)) n))))

;; --- a project's cells are its files (karamazov-1a51.3, .4) ------------------

(deftest in-a-project-the-cells-are-the-ones-its-map-lists
  ;; The project owns its whole cell set: what is in .samizdat/cells is what
  ;; loads, keyed by PATH so the mutation protocol's file rollback applies,
  ;; and a shipped cell the project changed is the project's version.
  (let [root @tmp
        c (db/open! ":memory:")
        prev-root (userspace/bind-root! root)]
    (try
      (userspace/bind! c)
      (cell-file! (str root "/.samizdat/cells") :proj/one "(fn [_ d] (assoc d :one 1))")
      (cell-file! (str root "/.samizdat/cells") :proj/two "(fn [_ d] d)")
      (cell-file! (str root "/.samizdat/cells") :proj/unlisted "(fn [_ d] d)")
      (spit (str root "/.samizdat/userspace.edn")
            (pr-str {:cells ["cells/one.clj" "cells/two.clj"]}))
      (let [loaded (cells/load-cells!)]
        (is (= #{:proj/one :proj/two} (set (keys loaded)))
            "the role map's :cells list is the set — nothing shipped loads
             beside it, and a file the map does not list does not load")
        (is (= (str root "/.samizdat/cells/one.clj") (:source (loaded :proj/one))))
        (is (every? #(clojure.string/starts-with? % (str root "/.samizdat/cells/"))
                    (keys (cells/loaded-file-content)))
            "content is keyed by path, so a rollback can write it back"))
      (finally (userspace/unbind!) (userspace/bind-root! prev-root) (db/close c)))))

(deftest a-dir-cell-overrides-a-shipped-cell-in-a-store-bound-image
  ;; karamazov-1a51.4. The dir file was saved into the store only when the
  ;; store had NOTHING under that name — and a shipped cell always has its
  ;; template there — so a .samizdat/cells override of a shipped cell was
  ;; silently ignored in a bound image, though the docstring said it won.
  (let [dir (str @tmp "/cells")
        c (db/open! ":memory:")
        prev-root (userspace/bind-root! nil)]
    (try
      (userspace/bind! c)
      (fs/create-dirs dir)
      (spit (str dir "/critic.clj")
            (str "(ns cells.gen.critic (:require [mycelium.cell :as cell]))\n"
                 "(cell/defcell :proj/critic-override {:doc \"mine\" :pure true}\n"
                 "  (fn [_ d] d))\n"))
      (let [srcs (#'cells/project-sources [dir])
            critic (some #(when (= "critic" (:id %)) %) srcs)]
        (is (clojure.string/includes? (:content critic) ":proj/critic-override")))
      (finally (userspace/unbind!) (userspace/bind-root! prev-root) (db/close c)))))

;; --- a cell's :requires must say what it reads --------------------------------

(def ^:private reads-undeclared
  "(ns cells.gen.stray (:require [mycelium.cell :as cell]))
   (cell/defcell :gen/stray {:doc \"reads more than it says\" :requires [:run-id]}
     (fn [{:keys [conn run-id] :as ctx} data]
       (assoc data :w (:beam-width ctx) :c (get-in ctx [:config :run]))))")

(deftest a-cell-that-reads-ctx-keys-it-does-not-declare-is-refused
  ;; :requires is what compile-definition holds a manifest to — a cell whose
  ;; key no driver provides is refused there — but only if :requires is TRUE.
  ;; :beam/escalate read :conn and :beam-width and declared neither, and the
  ;; only thing that noticed was a test; an agent's edit would have loaded.
  (is (= [{:cell :gen/stray :undeclared [:beam-width :config :conn]
           :requires [:beam-width :config :conn :run-id]}]
         (cells/ctx-problems reads-undeclared)))
  (let [p ((userspace/validator :cell) "stray" reads-undeclared)]
    (is (= :requires (:stage p)))
    (is (str/includes? (:message p) ":gen/stray"))
    (is (str/includes? (:message p) ":beam-width"))
    (is (str/includes? (:message p) ":requires [:beam-width :config :conn :run-id]")
        "and says exactly what to write"))
  (testing "a cell with no :requires at all is told to declare one"
    (let [p ((userspace/validator :cell) "bare"
             "(ns cells.gen.bare (:require [mycelium.cell :as cell]))
              (cell/defcell :gen/bare {:doc \"d\"} (fn [ctx data] data))")]
      (is (= :requires (:stage p)))
      (is (str/includes? (:message p) ":requires []"))))
  (testing "one that says what it reads loads"
    (is (nil? ((userspace/validator :cell) "ok"
               "(ns cells.gen.ok (:require [mycelium.cell :as cell]))
                (cell/defcell :gen/ok {:doc \"d\" :requires [:run-id]}
                  (fn [{:keys [run-id]} data] (assoc data :r run-id)))")))))
