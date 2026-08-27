;; samizdat - a claim-first verification harness
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

(ns samizdat.repair-test
  "The repair ladder as a composition: rungs are core fns, cells wrap them,
  the `repair` manifest wires them, and fence's seam runs the installed
  composition with the built-in chain as the fail-open fallback."
  (:require [clojure.test :refer [deftest testing is]]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.cells :as cells]
            [samizdat.llm.fence :as fence]
            [samizdat.manifests :as manifests]))

(defn- manifest-repair
  "Run one body through the compiled repair manifest, as the installed seam
  does."
  [body]
  (when (nil? (cell/get-cell :repair/control-chars))
    (cells/load-cells!))
  (:body (myc/run-compiled (manifests/compiled-manifest "repair")
                           {} {:body body})))

(def ^:private fixtures
  ["{\"name\": \"t\", \"args\": {\"code\": \"(+ 1\n2)\"}}"     ; raw newline
   "{\"name\": \"t\", \"args\": {\"a\": [1, 2,], \"b\": 3,}}"  ; trailing commas
   "{\"name\": \"t\", \"args\": {\"op\":"                      ; dangling key
   "{\"name\": \"t\", \"args\": {\"a\": 1}"                    ; missing closer
   "{\"name\": \"t\", \"args\": {\"s\": \"half"])              ; ends in string

(deftest the-manifest-composition-matches-the-built-in-chain
  ;; Same rungs, same order — the manifest is the same ladder expressed as
  ;; data. If this ever diverges, either the manifest was edited (fine, and
  ;; this test is the notice that the factory default moved) or a rung
  ;; changed without its cell (a bug).
  (doseq [f fixtures]
    (is (= (fence/default-repair f) (manifest-repair f))
        (pr-str f))))

(deftest every-rung-cell-is-pure
  ;; A repair rung that declared effects could be soaked, journalled, and
  ;; retried — none of which a string transform needs, and purity is what
  ;; lets the mutation protocol validate an edited rung cheaply.
  (when (nil? (cell/get-cell :repair/control-chars))
    (cells/load-cells!))
  (doseq [id [:repair/control-chars :repair/trailing-commas
              :repair/dangling-key :repair/close-unbalanced]]
    (is (:pure (cell/get-cell id)) (str id))))

(deftest the-seam-prefers-the-installed-composition-and-fails-open
  (try
    (testing "an installed composition is what repair-json runs"
      (fence/install-repair! (fn [body] (str body "]")))
      (is (= "x]" (fence/repair-json "x"))))
    (testing "a broken composition falls back to the built-in chain"
      (fence/install-repair! (fn [_] (throw (ex-info "boom" {}))))
      (is (= (fence/default-repair "{\"a\": 1,}")
             (fence/repair-json "{\"a\": 1,}"))))
    (testing "a composition returning a non-string falls back too"
      (fence/install-repair! (fn [_] nil))
      (is (= (fence/default-repair "{\"a\": 1")
             (fence/repair-json "{\"a\": 1"))))
    (testing "uninstalled, the default chain runs"
      (fence/install-repair! nil)
      (is (= (fence/default-repair "{\"a\": 1,}")
             (fence/repair-json "{\"a\": 1,}"))))
    (finally
      (fence/install-repair! nil))))
