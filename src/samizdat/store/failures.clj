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

(ns samizdat.store.failures
  "The cross-branch failure log.

  In the TypeScript harness this is a vector re-rendered into every branch's
  context on every turn, so its cost grows with the run and every branch pays
  for every other branch's history whether or not it is relevant. Backed by
  FTS5 it becomes a question instead: give this branch the failures most like
  what it is about to try. Smaller context and a better one.

  The FTS table is standalone rather than external-content, because what gets
  indexed is a projection of claim plus reason and external-content deletes
  require the exact indexed values back. Sync is app-managed here, no triggers."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            ;; db.jdbc registers the java.sql shim clojure.jdbc compiles against and
            ;; points connection construction at the native driver; it has to load
            ;; before jdbc.core.
            [db.jdbc]
            [jdbc.core :as jdbc]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]))

(defn record!
  [conn run-id {:keys [branch-id turn tool-name claim reason]}]
  (db/with-writer
    (db/execute! conn
                   ["INSERT INTO failures (run_id, branch_id, turn, tool_name, claim,
                                           reason, created_at)
                     VALUES (?, ?, ?, ?, ?, ?, ?)"
                    run-id branch-id turn (str tool-name) (str claim) (str reason) (db/now)])
    (let [id (db/last-insert-id conn)]
      (db/execute! conn
                     ["INSERT INTO failures_fts (rowid, claim, reason) VALUES (?, ?, ?)"
                      id (str claim) (str reason)])))
  (journal/note! conn run-id :failure
                 {:branch-id branch-id :turn turn
                  :data {:tool tool-name :claim claim :reason reason}}))

(defn- fts-query
  "Turn free text into an FTS5 OR query.

  FTS5's query language treats several characters as operators, so raw model
  prose is not a safe query string. Words are extracted and quoted; anything
  below the lexicon's search-token floor is dropped as noise."
  [text]
  (->> (str/split (str/lower-case (or text "")) #"[^a-z0-9]+")
       (filter #(>= (count %) (lexicon/tuning :claim-matching :min-search-token-length)))
       distinct
       (take 12)
       (map #(str "\"" % "\""))
       (str/join " OR ")))

(defn similar
  "The failures most like `text`, best match first.

  Returns [] rather than throwing when the query has no usable terms, because
  an empty failure log is a normal state and a branch should not lose its turn
  to a search that found nothing."
  ([conn run-id text] (similar conn run-id text 5))
  ([conn run-id text limit]
   (let [q (fts-query text)]
     (if (str/blank? q)
       []
       (try
         (db/fetch conn
                     ["SELECT f.branch_id, f.turn, f.tool_name, f.claim, f.reason
                       FROM failures_fts fts
                       JOIN failures f ON f.id = fts.rowid
                       WHERE failures_fts MATCH ? AND f.run_id = ?
                        ORDER BY bm25(failures_fts) LIMIT ?"
                       q run-id limit])
          (catch Throwable e
            ;; Empty stays the contract (a quiet FTS miss must not cost a
            ;; turn), but a persistent fault must leave a trace (provenance R2-15).
            (log/warn "failures/similar failed; returning empty:" (ex-message e))
            []))))))

(defn recent
  ([conn run-id] (recent conn run-id 10))
  ([conn run-id limit]
   (db/fetch conn ["SELECT branch_id, turn, tool_name, claim, reason FROM failures
                      WHERE run_id = ? ORDER BY id DESC LIMIT ?" run-id limit])))

(defn render
  "Failures as the block that goes into a branch's next-turn context."
  [entries]
  (when (seq entries)
    (prompt/render
     "failure-log"
     {:entries (str/join "\n"
                         (for [{:keys [branch_id turn tool_name claim reason]} entries]
                           (str "- [" branch_id " t" turn " " tool_name "] " claim
                                "\n  → " reason)))})))
