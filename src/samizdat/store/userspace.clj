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

(ns samizdat.store.userspace
  "The userspace layer as durable, versioned, PER-PROJECT rows.

  The harness ships a userspace TEMPLATE in resources/: the cells, the
  manifests, the policy tables, the prompts. This table is a project's own
  copy of it. A project seeds from the template on first use and evolves from
  there — every edit the supervisor makes is a new version here, in this
  project's database, and the shipped files are never written.

  That is what makes two projects running the same harness able to diverge in
  how they work, which is the point of the base/userspace split: src/ is
  capability, userspace is the loop that assembles capabilities, and the loop
  belongs to the project rather than to the harness.

  Four kinds, one lifecycle — seed, load latest, append a version, roll back
  by pointing at an older row:

    :cell      Clojure source, load-stringed into the live image
    :manifest  EDN, a mycelium workflow definition
    :policy    EDN, a table of thresholds/rules (gates, phases, retention)
    :prompt    markdown, text the model reads

  `body` is TEXT for all of them. The kind says how to read it; nothing here
  parses it, which is what keeps this namespace a store rather than a loader.

  APPEND-ONLY, never an UPDATE. The edit history of a system that rewrites
  itself is the most valuable thing in its database: it is how a surprising
  run is explained, and how a bad edit is undone without a backup."
  (:require [clojure.string :as str]
            [samizdat.store.db :as db]))

(def kinds
  "The kinds this store holds. Enumerated so a typo'd kind is a loud failure
  rather than a row nobody will ever read again."
  #{:cell :manifest :policy :prompt})

(defn- kind-str [kind]
  (let [k (if (keyword? kind) (name kind) (str kind))]
    (when-not (contains? kinds (keyword k))
      (throw (ex-info (str "unknown userspace kind " (pr-str kind)
                           " — expected one of " (str/join ", " (sort (map name kinds))))
                      {:kind kind})))
    k))

(defn load-latest
  "The newest version of `name` at `kind`, or nil."
  [conn kind name]
  (db/fetch-one conn ["SELECT * FROM userspace
                       WHERE kind = ? AND name = ?
                       ORDER BY version DESC LIMIT 1"
                      (kind-str kind) (str name)]))

(defn load-version
  [conn kind name version]
  (db/fetch-one conn ["SELECT * FROM userspace
                       WHERE kind = ? AND name = ? AND version = ?"
                      (kind-str kind) (str name) (long version)]))

(defn versions
  "Every version of `name`, oldest first — the edit history of one piece of
  userspace, with each version's rationale and its standing (how many runs
  ended shipped / not while it was current)."
  [conn kind name]
  (db/fetch conn ["SELECT version, created_at, source, rationale,
                          success_count, failure_count
                   FROM userspace
                   WHERE kind = ? AND name = ? ORDER BY version"
                  (kind-str kind) (str name)]))

(defn save!
  "Append `body` as the next version of `name` at `kind`. Returns the new
  version number.

  No content comparison: saving a body identical to the current version still
  appends. An edit that turned out to be a no-op is itself a fact about what
  the supervisor tried, and suppressing it would make the history lie by
  omission.

  `source` marks WHOSE copy the row is — \"project\" for anyone editing, which
  is everyone but `seed!`. It is what lets a harness upgrade refresh a
  factory copy without ever touching the supervisor's work; the version
  number cannot say, because a save of a name that was never seeded also
  writes version 1.

  `rationale` is WHY — the commit message of self-modification, read by the
  next supervisor deciding whether to keep or revert the version
  (karamazov-c58). Nullable: seeding and mechanical writes have nothing to
  say, and inventing text would make the history lie. The mutation tools are
  where a reason is demanded."
  ([conn kind name body] (save! conn kind name body "project"))
  ([conn kind name body source] (save! conn kind name body source nil))
  ([conn kind name body source rationale]
   (let [k (kind-str kind)]
     (db/with-writer
       (let [v (inc (or (:version (load-latest conn k name)) 0))]
         (db/execute! conn ["INSERT INTO userspace (kind, name, version, body, created_at, source, rationale)
                             VALUES (?, ?, ?, ?, ?, ?, ?)"
                            k (str name) v (str body) (db/now) (str source)
                            (some-> rationale str str/trim not-empty)])
         v)))))

(defn seed!
  "Install `body` as version 1 of `name` when the project has no version yet,
  and REFRESH version 1 in place when the shipped template has moved on.
  Returns the latest row either way, so a caller can seed-and-load in one
  motion.

  Idempotent by construction and cheap enough to call on every load: seeding
  is how a project acquires its copy of the template, and the only way to be
  sure it has one is to try. A project that has edited the entry is untouched
  — the template never overwrites what the supervisor has done.

  WITHOUT THE REFRESH A HARNESS UPGRADE CANNOT REACH AN EXISTING PROJECT. The
  seeded row was authoritative from the moment it was written, so a project
  stayed on whatever shipped the day it first ran. Live: a project seeded
  gates.edn on its first read, a threshold added to the harness afterwards was
  missing from that project's table, and the rule reading it threw rather than
  finding the key absent. Because entries seed lazily at first USE, a
  long-lived project ends up running on a sediment of whatever harness version
  happened to touch each one first.

  In place rather than as version 2, because appending would make the entry
  look edited and stop it following the next upgrade. Keyed on `source`
  rather than on the version number: `save!` of a name that was never seeded
  also writes a version 1, and mistaking that for a factory copy would
  overwrite the supervisor's work with the template — the one thing userspace
  exists to prevent."
  [conn kind name body]
  ;; THE READ AND THE DECISION UNDER ONE LOCK. `save!` takes the writer lock
  ;; for its own insert, but the `load-latest` that decides whether to seed at
  ;; all sat outside it — so two branches reaching an unseeded entry in the
  ;; same instant both read nil, both seeded, and the second appended a
  ;; version 2 whose body was byte-identical to version 1.
  ;;
  ;; Live in run a3ba69bb: roles/implementor v1 and v2, created 1ms apart at
  ;; run start, when four fan-out workers each triggered the first read. Not a
  ;; content problem — both bodies were the template — but it puts a version in
  ;; the history that changed nothing, which is the one thing a version history
  ;; must not contain, and it breaks any first-write-wins assumption reading
  ;; it. The lock is reentrant (`locking` on the connection monitor), so the
  ;; nested take inside `save!` is free (karamazov-cuv).
  (db/with-writer
    (let [row (load-latest conn kind name)]
      (cond
        (nil? row)
        (save! conn kind name body "factory")

        (and (= "factory" (:source row)) (not= (str body) (str (:body row))))
        (db/execute! conn ["UPDATE userspace SET body = ?, created_at = ?
                             WHERE kind = ? AND name = ? AND version = ?"
                           (str body) (db/now)
                           (kind-str kind) (str name) (long (:version row))]))
      (load-latest conn kind name))))

(defn revert!
  "Re-append the body of `version` as a NEW latest version — the rollback.

  Not a delete and not a pointer move: the failed edit stays in the history
  where it can be read, and the revert is itself an edit. Returns the new
  version number, or nil when the named version does not exist.

  The new row's rationale always says it was a revert and to what, with the
  caller's stated reason appended — a revert with no account of itself is
  exactly the oscillation karamazov-c58 records."
  ([conn kind name version] (revert! conn kind name version nil))
  ([conn kind name version rationale]
   (when-let [row (load-version conn kind name version)]
     (save! conn kind name (:body row) "project"
            (str "revert to v" (long version)
                 (when-let [r (some-> rationale str str/trim not-empty)]
                   (str ": " r)))))))

(defn record-run-outcome!
  "Stamp how a run ended onto every project-authored version that is current
  as it ends: shipped bumps success_count, anything else failure_count.

  Standing, for the next supervisor: a version that has survived N green runs
  has evidence behind it that a fresh reader's unfamiliarity does not
  outweigh (karamazov-c58). Only `source = 'project'` rows accrue it —
  factory copies are the baseline, not a tuning, and crediting them would
  drown the signal in the default.

  The latest version at run END is an approximation of \"was current while
  it ran\": an edit landed mid-run was live for the tail of the run, and the
  run's outcome is the first evidence it has."
  [conn shipped?]
  (db/with-writer
    (db/execute! conn [(if shipped?
                         "UPDATE userspace SET success_count = success_count + 1
                          WHERE source = 'project'
                            AND version = (SELECT MAX(v.version) FROM userspace v
                                           WHERE v.kind = userspace.kind
                                             AND v.name = userspace.name)"
                         "UPDATE userspace SET failure_count = failure_count + 1
                          WHERE source = 'project'
                            AND version = (SELECT MAX(v.version) FROM userspace v
                                           WHERE v.kind = userspace.kind
                                             AND v.name = userspace.name)")])))

(defn names
  "Every name at `kind`, with its latest version and how many versions it
  has — the catalogue a tool lists."
  [conn kind]
  (db/fetch conn ["SELECT name, MAX(version) AS version, COUNT(*) AS versions
                   FROM userspace WHERE kind = ? GROUP BY name ORDER BY name"
                  (kind-str kind)]))

(defn latest-bodies
  "{name body} for every name at `kind`, at its newest version — one query for
  a loader that needs the whole kind (the cell loader does).

  The correlated MAX rather than a GROUP BY on the row: SQLite would let a
  bare GROUP BY pick any row's body, and picking the wrong version of a cell
  is the kind of bug that looks like the supervisor's edit silently not
  taking."
  [conn kind]
  (let [k (kind-str kind)]
    (into {}
          (map (juxt :name :body))
          (db/fetch conn
                    ["SELECT u.name, u.body FROM userspace u
                      WHERE u.kind = ?
                        AND u.version = (SELECT MAX(v.version) FROM userspace v
                                         WHERE v.kind = u.kind AND v.name = u.name)
                      ORDER BY u.name"
                     k]))))
