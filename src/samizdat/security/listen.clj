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

(ns samizdat.security.listen
  "The loopback ports this harness listens on, as the servers that bound them
  recorded them.

  The HTTP API answers approvals, grants and the approval mode, and the nREPL
  IS the harness process, and both sit on loopback with no credentials. What
  the agent runs has to be kept off them (karamazov-3vu1.1), and the confining
  profile can only name ports someone wrote down — so the servers write them
  down here as they bind, rather than the profile guessing from defaults.")

(defonce ^:private bound (atom {}))

(defn register!
  "Record that the server `k` (:http, :nrepl) listens on `port`. A server that
  restarts replaces its entry."
  [k port]
  (when (pos-int? port)
    (swap! bound assoc k port))
  port)

(defn ports
  "Every port recorded, in no particular order."
  []
  (vec (distinct (vals @bound))))

(defn reset! [] (clojure.core/reset! bound {}))
