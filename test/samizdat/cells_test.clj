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
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
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
             "(cell/defcell " id-kw " {:doc \"a generated cell\" :pure true}\n"
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
    (is (= on-disk (set cells/shipped-cells))
        (str "cells/shipped-cells and resources/cells disagree; missing: "
             (sort (remove (set cells/shipped-cells) on-disk))
             ", listed but absent: "
             (sort (remove on-disk (set cells/shipped-cells))))))
  (testing "every shipped name resolves on the classpath"
    (doseq [r cells/shipped-cells]
      (is (some? (clojure.java.io/resource r)) (str r " does not resolve")))))

(deftest every-shipped-cell-require-is-reachable-without-load-string
  ;; A shipped cell is load-stringed, so a namespace it requires that NOTHING
  ;; in src reaches is absent from a `jolt build` image and the cell dies with
  ;; "Could not locate … on the source roots". samizdat.cell-prelude exists to
  ;; pull those onto the compile graph; samizdat.agent.decompose had fallen
  ;; off it. Walk the requires rather than trusting the list stays current.
  (let [required (->> cells/shipped-cells
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
