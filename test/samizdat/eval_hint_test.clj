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

(ns samizdat.eval-hint-test
  "An eval error says what works instead (karamazov-0e2c.18, after xi's
  interop-hint): a model with JVM habits in jolt loses a turn to each one
  unless the error names the move that works."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.repl :as repl-tool]
            [samizdat.repl.route :as route]))

(deftest the-error-names-what-works
  (is (re-find #"(?i)require" (str (repl-tool/eval-hint "Unknown class gates"))))
  (is (re-find #"(?i)shell" (str (repl-tool/eval-hint "Could not locate clojure/java/shell.jolt (or .clj/.cljc) on the source roots"))))
  (is (re-find #"(?i)deps.edn|:paths" (str (repl-tool/eval-hint "Could not locate flight/core.jolt (or .clj/.cljc) on the source roots"))))
  (is (nil? (repl-tool/eval-hint "Divide by zero")) "an ordinary error carries no hint"))

(deftest an-eval-error-carries-its-hint
  (with-redefs [route/eval-for (fn [& _] {:ok false :error "Unknown class str"})]
    (let [r (tools/run-tool {:tool-name "eval" :branch {:id "B1"} :args {:code "(str/join [1])"}})]
      (is (str/includes? (:result r) "Unknown class str"))
      (is (re-find #"(?i)require" (:result r)) (:result r)))))
