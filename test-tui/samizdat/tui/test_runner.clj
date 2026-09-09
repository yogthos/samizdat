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

(ns samizdat.tui.test-runner
  "`jolt tui-test` — the TUI's toolkit-bound tests.

  Separate from `samizdat.test-runner` because these load ftxui, which needs
  a compiled C++ shim. The main suite stays buildable with nothing but jolt;
  everything about the TUI that can be checked as data is over there."
  (:require [clojure.test :as t]
            [samizdat.tui.mouse-test]))

(def namespaces '[samizdat.tui.mouse-test])

(defn run []
  (apply t/run-tests namespaces))

(defn -main [& _]
  (let [{:keys [fail error]} (run)]
    (println "----")
    (System/exit (if (pos? (+ (or fail 0) (or error 0))) 1 0))))
