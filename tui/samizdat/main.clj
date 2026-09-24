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

(ns samizdat.main
  "The application's entry point, and the binary's: the server and the
  terminal UI in one process by default.

      samizdat                   # server + TUI
      samizdat --headless        # the server alone (what `jolt serve` runs)
      samizdat --connect [URL]   # the TUI alone, against a running server

  The TUI stays a strict HTTP client even here — it reaches the server over
  loopback exactly as it would a remote one, so nothing about it changes
  with where the server runs. What sharing a process does change is the
  terminal: the server's log goes to `.samizdat/samizdat.log` under the
  project root while the TUI owns the screen.

  Lives under tui/ rather than src/ because it requires the toolkit, and
  `jolt serve` and `jolt test` must never load one."
  (:require [samizdat.core :as core]
            [samizdat.launch :as launch]
            [samizdat.tui.core :as tui]))

(defn- with-tui!
  "Start the server, run the TUI against it until the user quits, exit."
  []
  (let [log (launch/log-path (or (System/getenv "HARNESS_ROOT")
                                      (System/getProperty "user.dir")))]
    (println "samizdat: starting the server; its log is" log)
    ;; Before anything logs, so the boot itself stays off the screen.
    (launch/log-to-file! log)
    (launch/redirect-stderr! log)
    (let [started (core/start!)]
      (tui/run-ui! (core/local-url (:config started)))
      ;; The shutdown hooks stop the system and record anything still running.
      (System/exit 0))))

(defn -main [& args]
  (let [{:keys [mode url message]} (launch/parse-args args)]
    (case mode
      :tui (with-tui!)
      :headless (core/-main)
      :connect (tui/-main url)
      :help (println launch/usage)
      :error (do (binding [*out* *err*]
                   (println "samizdat:" message)
                   (println launch/usage))
                 (System/exit 2)))))
