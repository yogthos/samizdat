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

(ns samizdat.schemacheck-test
  "A database older than the code says so, instead of failing at the first write.

  While nothing is in production the schema is edited in place rather than
  migrated (karamazov-fxf), which means a db from before an edit is simply
  missing columns: `user_version` already counts the migration as applied, so
  `migrate!` skips it and the absence surfaces as `no such column: lineage_id`
  on whatever happens to write first. That is a stale schema presenting as a
  broken feature, and it costs whoever hits it a debugging session in the wrong
  namespace before they think to check the file's age.

  Same rule the harness applies everywhere else: an error beats a wrong answer
  that looks right."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.store.db :as db]))

(deftest a-current-database-passes
  (let [conn (db/open! ":memory:")]
    (is (nil? (db/schema-drift conn)))))

(deftest a-column-the-code-needs-and-the-file-lacks-is-drift
  ;; Exactly the karamazov-oov case: the table exists, the version says it is
  ;; up to date, and a column added by editing the CREATE is simply absent.
  (let [conn (db/connect ":memory:")]
    (db/execute! conn ["CREATE TABLE knowledge (id TEXT PRIMARY KEY, content TEXT NOT NULL,
                                                kind TEXT, created_at TEXT)"])
    (let [drift (db/schema-drift conn)]
      (is (some? drift) "a table missing the columns the code writes read as healthy")
      (is (contains? drift "knowledge"))
      (is (contains? (get drift "knowledge") "lineage_id")
          "the report did not name the column that is actually missing"))))

(deftest the-message-says-what-to-do-about-it
  ;; A diagnosis nobody can act on costs the same as no diagnosis. The wording
  ;; lives at the throw, not in schema-drift: it is what a developer reads off
  ;; a stack trace, and it cannot come from resources/prompts because prompt
  ;; bodies resolve through the userspace TABLE — rendering one to report a
  ;; broken database would query the database being reported on.
  (let [conn (db/connect ":memory:")]
    (db/execute! conn ["CREATE TABLE knowledge (id TEXT PRIMARY KEY, content TEXT NOT NULL)"])
    (db/execute! conn ["PRAGMA user_version = 99"])
    (try
      (db/migrate! conn)
      (is false "a stale database migrated without complaint")
      (catch Exception e
        (is (str/includes? (ex-message e) "delete")
            "the message did not say the database should be recreated")
        (is (str/includes? (ex-message e) "lineage_id")
            "the message did not name what is missing")))))

(deftest a-missing-table-is-not-drift
  ;; An empty file is a NEW database, and migrate! is about to build it. Only a
  ;; table that exists and is the wrong shape is evidence of an edited schema.
  (let [conn (db/connect ":memory:")]
    (is (nil? (db/schema-drift conn)))))

(deftest opening-a-stale-database-fails-loudly
  ;; The point of the check: it fires where the database is opened, not where
  ;; the first write happens to be.
  (let [conn (db/connect ":memory:")]
    (db/execute! conn ["CREATE TABLE knowledge (id TEXT PRIMARY KEY, content TEXT NOT NULL)"])
    ;; Pretend the migration that would have created it already ran.
    (db/execute! conn ["PRAGMA user_version = 99"])
    (is (thrown-with-msg? Exception #"(?i)delete|recreate|stale"
                          (db/migrate! conn)))))

(deftest a-fresh-open-is-unaffected
  ;; The guard must not cost the ordinary path anything.
  (let [conn (db/open! ":memory:")]
    (is (pos? (db/schema-version conn)))
    (is (nil? (db/schema-drift conn)))))
