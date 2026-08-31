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

(ns samizdat.cell-schema-test
  "The cells declare the shape of the data map they read and write, so
  mycelium's schema-chain check has something to check.

  WHY THIS EXISTS. mycelium walks every path from :start accumulating the keys
  each cell's :output declares, and refuses a manifest where a cell's required
  :input is not among them (mycelium.workflow/validate-schema-chain!). With no
  schemas anywhere the walk accumulates nothing, compares nothing, and passes
  everything — 59 cells declared none, so the check had been running and
  finding nothing since it was written. Measured on the same two-cell graph:
  with schemas mycelium refuses `:demo/b requires keys #{:y} but only #{:x}
  available`; without them it compiles.

  That is the one thing propose-cell! could not catch. A cell edit that renames
  a key the next node reads compiles, survives the soak — which dry-runs ONE
  path with the effectful cells stubbed to identity — and reaches a run."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [mycelium.dev :as dev]
            [samizdat.cells :as cells]
            [samizdat.manifests :as manifests]))

(defn- loop-def []
  (edn/read-string (slurp (io/resource "manifests/loop.edn"))))

(def ^:private round-1
  "The manifests whose every cell declares a schema. Grows a manifest at a
  time; the general form — every cell any shipped manifest references — is
  what this becomes when the rollout finishes (karamazov-6y7.3)."
  ["loop"])

(deftest round-1-cells-declare-their-shape
  (cells/load-cells!)
  (doseq [nm round-1]
    (let [d (edn/read-string (slurp (io/resource (manifests/manifest-resource nm))))]
      (doseq [[node cell-id] (sort-by key (:cells d))]
        (testing (str nm " " node " (" cell-id ")")
          (let [spec (cell/get-cell cell-id)]
            (is (some? spec) "the cell is registered")
            (is (some? (get-in spec [:schema :input]))
                (str cell-id " declares no :input, so nothing upstream is"
                     " required to produce anything for it"))
            (is (some? (get-in spec [:schema :output]))
                (str cell-id " declares no :output, so it contributes nothing"
                     " to what downstream cells may rely on"))))))))

(deftest the-turn-accumulates-keys-rather-than-nothing
  ;; The direct measurement of the bug: dev/infer-workflow-schema walks the
  ;; graph reporting what each node adds. Every node reported #{} — which reads
  ;; as "this loop moves no data", and was true only of the declarations.
  (cells/load-cells!)
  (let [inferred (dev/infer-workflow-schema (loop-def))]
    (doseq [node [:infer :parse :dispatch :arbiter :route]]
      (testing node
        (is (seq (:adds (get inferred node)))
            (str node " adds no keys any downstream cell can rely on"))))
    (testing "and the keys reach the end of the turn"
      (is (contains? (:available-after (get inferred :route)) :verdict)
          ":verdict is what :finish routes on"))))

(deftest a-cell-reading-a-key-nothing-produces-is-refused
  ;; The check earning its keep. :gate/arbiter reads :before, which only
  ;; :loop/assemble writes; a manifest that routes around assemble is a
  ;; manifest whose arbiter steers on nil. Compiled through the loader's own
  ;; pipeline, so this is the error an agent-authored manifest would get from
  ;; `manifest save`.
  (cells/load-cells!)
  (let [broken '{:cells {:start :llm/infer
                         :arbiter :gate/arbiter}
                 :edges {:start :arbiter
                         :arbiter :end}}
        e (try (manifests/compile-definition broken) nil
               (catch Throwable t t))]
    (is (some? e) "compiled a manifest whose arbiter reads a key nothing wrote")
    (is (re-find #"(?i)schema chain" (str (ex-message e)))
        (str "the refusal names the schema chain rather than failing later: "
             (ex-message e)))))

(deftest every-shipped-manifest-still-compiles-with-partial-schemas
  ;; THE ROLLOUT HAZARD, pinned. Accumulation is monotonic — a cell with no
  ;; schema adds nothing to what is available but takes nothing away — so a
  ;; schema'd cell sitting downstream of unschema'd ones is refused for keys
  ;; that ARE produced, just not declared.
  ;;
  ;; Which is why :loop/finish declares :verdict optional: feature, team,
  ;; decompose and orchestrator reach it without :loop/route, and their
  ;; routers are not schema'd yet. Tighten it in karamazov-6y7.3, when they
  ;; are — and until then this test is what says the rollout is still safe
  ;; mid-flight rather than only at the end.
  (cells/load-cells!)
  (doseq [nm manifests/shipped-manifests]
    (testing nm
      (is (some? (manifests/compiled-manifest nm))
          (str nm " no longer compiles — a cell schema requires a key that"
               " something upstream produces but does not yet declare")))))
