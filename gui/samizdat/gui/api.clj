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

(ns samizdat.gui.api
  "The GUI's names for the run API client.

  The client itself is `samizdat.api.client`, in the base: how to reach the
  server is mechanism, and it is the same question for every front end. This
  namespace was that code until the TUI arrived and would have had to copy
  it — two poll loops and two cursor implementations, drifting.

  Kept as a namespace rather than deleted so the GUI and its tests read the
  same as they did. New front ends call `samizdat.api.client` directly."
  (:require [samizdat.api.client :as client]))

(def list-runs      client/list-runs)
(def models         client/models)
(def journal-since  client/journal-since)
(def branch-detail  client/branch-detail)
(def start-run!     client/start-run!)
(def intervene!     client/intervene!)
(def abort!         client/abort!)
(def resume!        client/resume!)
(def poll-step      client/poll-step)
(def start-poller!  client/start-poller!)

(def start-timeout-ms client/start-timeout-ms)
(def base-interval-ms client/base-interval-ms)
(def max-backoff-ms   client/max-backoff-ms)
