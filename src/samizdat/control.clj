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

(ns samizdat.control
  "Steering a running agent from the REPL.

  The harness is a live image: a run started in one thread (or another process
  sharing the db) can be redirected from a connected REPL while it works. This
  is the human-in-the-loop seam the whole design turns on — you watch the
  journal, and when the agent needs a nudge (\"wire that in\", \"write a test
  first\", \"stop and ship what you have\") you steer it without stopping it.

      (require '[samizdat.control :as ctl] '[samizdat.store.db :as db])
      (def conn (db/open! \".samizdat/samizdat.sqlite3\"))
      (ctl/runs conn)                         ; which runs are live
      (ctl/steer! conn run-id \"wire truncate-middle into the shell tool\")
      (ctl/watch conn run-id)                 ; the last few turns

  A directive lands at the next turn boundary, never mid-turn, and the arbiter
  puts it above every machine gate (priority zero). It is delivered through the
  same interventions queue the HTTP control surface uses, so a REPL steer and a
  UI steer are the same event."
  (:require [samizdat.store.db :as db]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn steer!
  "Inject an instruction into a running run. It reaches the agent on its next
  turn, ahead of anything the harness would otherwise say. Optionally scope it
  to one branch with {:branch-id \"B2\"}; unscoped, it applies to whichever
  branch reaches the next boundary.

  Returns the directive id."
  ([conn run-id text] (steer! conn run-id text {}))
  ([conn run-id text {:keys [branch-id]}]
   (interventions/submit! conn run-id
                          {:kind "message" :payload text
                           :branch-id branch-id :issued-by "repl"})))

(defn review!
  "Tell the agent to cross-check and ship what it has, rather than opening a
  new line of work. A distinct kind so the record shows a review nudge as what
  it was."
  ([conn run-id] (review! conn run-id {}))
  ([conn run-id {:keys [branch-id]}]
   (interventions/submit! conn run-id
                          {:kind "review" :payload "Review what you have and ship it."
                           :branch-id branch-id :issued-by "repl"})))

(defn extend!
  "Raise a run's turn cap. For a run that is close but out of budget."
  [conn run-id max-turns]
  (runs/extend-budget! conn run-id max-turns))

(defn pending
  "The directives waiting to be drained at the next boundary."
  [conn run-id]
  (interventions/pending conn run-id))

(defn history
  "Every directive submitted to a run and what became of it."
  [conn run-id]
  (interventions/history conn run-id))

(defn runs
  "Recent runs, newest first — to find the id of the one to steer."
  ([conn] (runs conn 10))
  ([conn n]
   (mapv #(select-keys % [:id :status :problem :model :started_at])
         (runs/list-runs conn n))))

(defn watch
  "The last `n` turns of a run: what the agent did and how each landed. The
  REPL supervisor's window into a live run.

  Defaults to the run's last active branch — a beam run is several branches
  wide, and the window is for watching the work, not one named arm (review3
  #13: this hardcoded \"B1\", which made the other branches invisible). An
  explicit branch id still narrows."
  ([conn run-id] (watch conn run-id 8))
  ([conn run-id n]
   (let [bs (runs/branches conn run-id)]
     (watch conn run-id n
            ;; last branch still active, else the last branch there was
            (when (seq bs)
              (:id (or (last (filter #(= "active" (:status %)) bs))
                       (last bs)))))))
  ([conn run-id n branch-id]
   (when branch-id
     (->> (journal/branch-turns conn run-id branch-id)
          (take-last n)
          (mapv (fn [t] {:turn (:turn t)
                         :tool (:tool_name t)
                         :category (:category t)
                         :result (let [r (str (:result t))]
                                  (subs r 0 (min (count r) 160)))}))))))
