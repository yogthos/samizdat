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

(ns samizdat.store.db
  "SQLite connection and migration runner (jolt-lang/db over libsqlite3).

  Writes funnel through one connection on purpose. Five branches appending to
  the journal concurrently means concurrent FFI calls into libsqlite3, and
  while libsqlite3's default threading mode is serialized, the FFI binding's
  own safety under Chez threads is unproven. A single writer sidesteps the
  question; readers open their own connections."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            ;; db.jdbc registers the java.sql shim clojure.jdbc compiles against and
            ;; points connection construction at the native driver; it has to load
            ;; before jdbc.core.
            [db.jdbc]
            [jdbc.core :as jdbc]
            [samizdat.store.migrations :as migrations]))

(defn connect
  "Open `path` (a file, or \":memory:\") and return a connection.

  The two pragmas are about EXTERNAL readers. In-process access is serialized
  through one lock (see with-conn), so the beam's own branches never collide;
  but the database is a file on disk and the first thing anyone does with a
  run is point sqlite3 or a viewer at it. Under the defaults — rollback
  journal, zero busy timeout — such a reader holds a shared lock, the harness's
  next write gets SQLITE_BUSY immediately, and the branch that was writing dies
  with \"branch error: sqlite step failed: database is locked\".

  That is not a hypothetical: it happened while watching a live knights-3 run
  on 2026-08-18, and the branch it killed was mid-proof. A branch lost to a
  monitoring query is the vf-jki mistake in a different subsystem — a
  non-mathematical event ending a line of mathematical work — and it is
  invisible afterwards, because the reason recorded is true and says nothing
  about what to fix.

  WAL is the actual fix: readers and the writer stop excluding each other, so
  the case cannot arise. busy_timeout is the belt to its braces, covering the
  writer-versus-writer contention WAL does not (a second harness process on
  the same file). Both are set per connection; WAL additionally persists in
  the file, which is why it is safe to set here and pointless to set twice.

  On \":memory:\" the journal_mode pragma is a documented no-op returning
  \"memory\" rather than an error, so tests need no special case."
  [path]
  (let [conn (jdbc/connection (str "sqlite:" path))]
    (jdbc/execute! conn "PRAGMA journal_mode = WAL")
    (jdbc/execute! conn "PRAGMA busy_timeout = 5000")
    conn))

(def ^:private conn-lock (Object.))

(defmacro with-conn
  "Serialize ALL access to the connection, reads included.

  Locking only writes was wrong and the full benchmark sweep is what proved it:
  every read also calls sqlite3_prepare_v2 on the same connection handle, and
  four concurrent problems times three branches is twelve threads preparing at
  once. The result was `sqlite prepare failed: not an error` on every run and
  then an invalid memory reference on close. The Phase 0 probe passed because
  it only exercised writers, which were the half already serialized.

  One lock for one handle. Concurrency lives in the branches and the engines,
  not in the store.

  This looks heavier than it is, and the obvious next move — a write queue
  drained by one thread, so reads stop waiting behind writes — was measured and
  declined. A journal write is ~2.5ms, an FTS read ~0.7ms under twelve
  concurrent writers, and a real run does about 3.6 writes per turn against a
  turn that averages 37 seconds because it contains a provider call. The store
  is roughly 0.027% of a turn.

  Buffering would trade the property this system leads with — everything
  appended as it happens, so a crashed run stays inspectable — plus a drainer
  that stops the journal silently if it dies, for a saving nothing can observe.
  See samizdat-clj-dpj for the numbers. Reopen it if the store ever appears in
  a profile, which realistically means dropping this lock first."
  [& body]
  `(locking conn-lock ~@body))

;; Retained so existing call sites keep meaning what they say; a write is just
;; an access that mutates.
(defmacro with-writer [& body] `(with-conn ~@body))

(defn fetch
  "A serialized read. Everything that reads the shared connection goes through
  here rather than calling jdbc directly."
  [conn q]
  (with-conn (jdbc/fetch conn q)))

(defn fetch-one [conn q]
  (with-conn (jdbc/fetch-one conn q)))

(defn execute!
  ([conn q] (with-conn (jdbc/execute! conn q)))
  ([conn q opts] (with-conn (jdbc/execute! conn q opts))))

(defn last-insert-id
  "clojure.jdbc has no last-insert-id — it was jolt-lang/db's own helper, and it
  called sqlite's last_insert_rowid(). Asking for that directly keeps every call
  site here meaning what it said."
  [conn]
  (with-conn (:id (jdbc/fetch-one conn "select last_insert_rowid() as id"))))

(defn now
  "An ISO-8601 timestamp. One function so every table sorts the same way."
  []
  (str (java.time.Instant/now)))

(defn close [conn]
  ;; jdbc.core's connection is a map carrying a :close thunk, not an object.
  (when-let [f (:close conn)] (f)))

(defn schema-version [conn]
  (or (-> (jdbc/fetch-one conn "PRAGMA user_version") vals first) 0))

(defn- set-schema-version! [conn n]
  ;; PRAGMA does not take bound parameters, so this is interpolated. n is an
  ;; index into a compiled-in vector, never user input.
  (jdbc/execute! conn (str "PRAGMA user_version = " (long n))))

(defn change-count
  "Rows affected by the statement just executed on this connection — how a
  guarded UPDATE reports whether it won (provenance R2-4). Read before anything
  else runs on the connection."
  [conn]
  (-> (jdbc/fetch-one conn "SELECT changes()") vals first))

(defn id-collision?
  "Whether e is a UNIQUE-constraint failure — the only insert error that
  retrying with a fresh short id can fix. The id-retry loops used to catch
  everything, converting disk and lock failures into five blind retries and
  then 'could not allocate an id' (provenance R2-15)."
  [e]
  (str/includes? (str e) "UNIQUE"))

(def required-columns
  "Columns the code writes that a database from before an in-place schema edit
  will not have. Table -> the columns whose absence means the FILE is stale.

  NOT the whole schema. This is a tripwire for one specific failure, and a
  full mirror of every column would be a second schema to keep in step with
  the first — which is how a check starts lying. Add a column here only when
  its absence would otherwise surface as an unrelated-looking error.

  IN src/ AND NOT IN resources, like the other guards: a check the agent can
  edit is not a check."
  {"knowledge" #{"lineage_id" "current" "cause"}})

(defn- columns-of
  "Column names of `table`, or nil when the table does not exist."
  [conn table]
  (some->> (seq (jdbc/fetch conn (str "PRAGMA table_info(" table ")")))
           (map :name)
           set))

(defn schema-drift
  "Which required columns the file lacks, as `{table #{col …}}`, or nil when it
  matches the code.

  DATA, NOT A SENTENCE. The wording belongs at the throw site — an exception
  message is what a developer reads off a stack trace, and it cannot come from
  `resources/prompts` here for a reason worth stating: prompt bodies resolve
  through the userspace TABLE, so rendering one to report a broken database
  would query the database being reported on.

  WHILE THE SCHEMA IS EDITED IN PLACE rather than migrated (karamazov-gku
  carries the decision about when that stops), a file from before an edit is
  missing columns while `user_version` still counts the migration as applied —
  so `migrate!` skips it and the absence turns up as `no such column:
  lineage_id` at whatever writes first. A stale schema presenting as a broken
  feature costs a debugging session in the wrong namespace.

  A table that does not exist is not drift: that is a NEW database and
  `migrate!` is about to create it."
  [conn]
  (not-empty
   (into {}
         (for [[table cols] required-columns
               :let [present (columns-of conn table)]
               :when present
               :let [gone (set (remove present cols))]
               :when (seq gone)]
           [table gone]))))

(defn migrate!
  "Apply every migration past the current user_version. Idempotent: running it
  twice is a no-op. Each migration is all-or-nothing with its version bump
  INSIDE the transaction (provenance R2-3): v2/v4/v5 are non-idempotent ALTERs, and
  running them as autocommitted statements with the bump after the last one
  meant a crash in between left user_version stale — every later boot died on
  `duplicate column name` forever. Returns the version now in effect.

  Throws on schema DRIFT afterwards — a file whose tables exist but lack
  columns the code writes. Loud here beats a `no such column` from whichever
  namespace happens to write first."
  [conn]
  (let [applied (schema-version conn)
        total (count migrations/migrations)]
    (when (> applied total)
      (log/warn "db schema version" applied "exceeds this binary's" total
                "migrations — a newer database on an older binary; applying nothing"))
    (doseq [[i statements] (map-indexed vector (subvec migrations/migrations (min applied total)))]
      (let [version (+ applied i 1)]
        (jdbc/execute! conn "BEGIN IMMEDIATE")
        (try
          (doseq [sql statements]
            (try
              (jdbc/execute! conn sql)
              (catch Throwable e
                (throw (ex-info (str "Migration v" version " failed: " (ex-message e))
                                {:version version
                                 :statement (subs sql 0 (min 120 (count sql)))}
                                e)))))
          (set-schema-version! conn version)
          (jdbc/execute! conn "COMMIT")
          (catch Throwable e
            (try (jdbc/execute! conn "ROLLBACK") (catch Throwable _ nil))
            (throw e)))))
    ;; AFTER migrating, because the question is whether the file matches the
    ;; code once everything that was going to run has run.
    (when-let [drift (schema-drift conn)]
      (throw (ex-info (str "this database predates the current schema — "
                           (str/join "; "
                                     (for [[table cols] drift]
                                       (str table " is missing "
                                            (str/join ", " (sort cols)))))
                           ". The schema is edited in place until the first"
                           " release is tagged (karamazov-gku), so delete the"
                           " database file and let it rebuild.")
                      {:drift drift})))
    (schema-version conn)))

(defn open!
  "Connect, migrate, return the connection."
  [path]
  (doto (connect path) (migrate!)))

(defn table-names
  "Every user table and virtual table in the database, sorted. Used by the
  migration test to check that a multi-statement migration string did not
  silently execute only its first statement."
  [conn]
  (->> (jdbc/fetch conn "SELECT name FROM sqlite_master WHERE type IN ('table','view') AND name NOT LIKE 'sqlite_%' ORDER BY name")
       (map :name)
       (remove #(str/includes? % "_fts_"))
       vec))

(defn fts5-available?
  "Whether the libsqlite3 the FFI binding loaded has FTS5 compiled in. This is
  a different question from whether the sqlite3 CLI has it, and the failure
  mode is a migration that throws at startup, so it is probed explicitly."
  [conn]
  (try
    (jdbc/execute! conn "CREATE VIRTUAL TABLE IF NOT EXISTS __fts5_probe USING fts5(x)")
    (jdbc/execute! conn "DROP TABLE IF EXISTS __fts5_probe")
    true
    (catch Throwable _ false)))
