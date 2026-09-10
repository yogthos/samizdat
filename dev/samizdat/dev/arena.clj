;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.dev.arena
  "Paired-arm measurement rig for the Procedural Graph epic (karamazov-ylte.1).

  WHY IT EXISTS. Every claim in 2609.09153v1 is a measurement, so the epic
  cannot be judged by unit tests. A round that cannot show its number on live
  runs is not done. This is the thing that produces the number.

  THE DESIGN CONSTRAINT, and the only one that matters: two arms must see the
  SAME TREE. samizdat resolves both the project root and the database from
  `:run :root` (config.clj:318, config.clj:110), so a git worktree of a pinned
  sha gives isolation of both for free — a fresh checkout AND a fresh
  `.samizdat/samizdat.sqlite3` per run, with nothing shared but the harness.
  Runs are therefore never comparable across different `:sha`, and `row` stamps
  it so a later reader cannot mistake one sweep for another.

  WHAT IS STAMPED ON EVERY ROW, per karamazov-7mo.4's rule that a score without
  its fixture, revision, model and scorer beside it is only a number: the
  subject sha, the harness git revision, the harness working-tree cleanliness,
  the model id, and the arm. A dirty harness tree is recorded rather than
  refused — a spike is a legitimate arm — but it is recorded, so a sweep run
  against uncommitted work can never be quoted as if it were run against a
  commit.

  EACH ARM IS A SUBPROCESS, WITH ITS CWD AT THE WORKTREE. This is not a
  packaging preference, it is the difference between measuring the harness and
  measuring the rig. repl.clj:60 is explicit that jolt resolves relative paths
  against the REAL cwd and offers no chdir, so an in-process rig driven from
  the samizdat checkout leaves every arm's `(slurp \"README.md\")` reading
  SAMIZDAT'S README — observed live at turn 4 of a real run, where a branch
  went on to read samizdat's deps.edn believing it was the project's. The
  harness's own advice is 'run the harness FROM the project root', so the rig
  does, and `-main` is the child end of that.

  ARMS. An arm is a name plus an optional `:setup` form — quoted EDN evaluated
  in the child before `beam/run!`, so an arm can install userspace, flip a
  gate, or do nothing (`:baseline`). It is a FORM and not a function because
  it has to cross a process boundary. Keeping the arm's effect out of this
  namespace is deliberate: the rig knows how to measure and nothing about what
  is being measured, the same mechanism/behaviour split the project applies
  everywhere else.

  WHAT IT DOES NOT DO. It does not decide anything. It writes rows; reading
  them is the analysis step and it is a separate pass, because a rig that both
  produced and judged its own numbers would be the confounded comparison
  RFC-010 already warns about."
  (:require [clojure.edn :as edn]
            [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [samizdat.agent.beam :as beam]
            [samizdat.server :as server]
            [samizdat.session :as session]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.knowledge :as knowledge]
            [samizdat.store.runs :as runs]
            [samizdat.system :as system]))

;;; ------------------------------------------------------------------ shell

(defn- sh
  "Run a command, returning {:out :exit}, with stderr merged into `:out`.
  Nothing here is interactive, and every destructive call passes its force
  flag explicitly (AGENTS.md).

  ONE STREAM, READ TO THE END, and both halves of that matter. Reading two
  pipes in sequence from one thread deadlocks the moment the one not being
  read fills its buffer, and the chattiest command this runs is `jolt -A:dev
  -Spath`, which writes dependency-resolution progress to stderr on a cold
  cache — so the sweep's classpath probe was one slow download away from
  hanging the whole sweep before its first run.

  The obvious repair — send stderr to a file — does not work on jolt 0.8.6:
  `.redirectError` is accepted and ignored, exactly like `.redirectOutput`
  (see `drain-to!`), so the file is never written AND the pipe is still there
  to fill. Measured: a child writing 4000 stderr lines with `.redirectError`
  set to a file never finished. Merging is the only form that needs no second
  reader at all.

  Callers that used to read `:err` read `:out` now; the only one is
  `worktree!`, reporting why a checkout failed, and a merged message says that
  no less well."
  [& args]
  (let [pb (doto (ProcessBuilder. ^java.util.List (vec args))
             (.redirectErrorStream true))
        p (.start pb)
        out (slurp (.getInputStream p))]
    {:out out :err out :exit (.waitFor p)}))

(defn- git [dir & args] (apply sh "git" "-C" (str dir) args))

;;; -------------------------------------------------------------- worktrees

(defn worktree!
  "A clean checkout of `sha` from `repo` at `dest`. Detached, so the subject
  repo's branches are untouched and two arms cannot race over one HEAD."
  [repo sha dest]
  (let [dest (str dest)]
    (when (.exists (io/file dest))
      (git repo "worktree" "remove" "--force" dest))
    (let [r (git repo "worktree" "add" "--detach" dest sha)]
      (when-not (zero? (:exit r))
        (throw (ex-info "worktree add failed" {:sha sha :dest dest :err (:err r)})))
      dest)))

(defn drop-worktree!
  "Remove a worktree. Best effort: a sweep must not die cleaning up after a
  run whose numbers it already has."
  [repo dest]
  (try (git repo "worktree" "remove" "--force" (str dest)) (catch Throwable _ nil)))

;;; -------------------------------------------------- waiting on the child

(defn- drain-to!
  "Copy `p`'s output into `file` on a plain OS thread, until the stream ends.

  THE CHILD'S STDOUT MUST BE READ BY SOMEBODY. jolt 0.8.6 accepts
  `ProcessBuilder.redirectOutput` and does nothing with it — the file is never
  created, in either the `File` or the `Redirect/to` form — so the process
  keeps the default PIPE and a rig that only asked for the redirect is a rig
  that never reads it. Once the child has written a pipe buffer's worth (64 KB
  on this platform) its next write blocks forever.

  THIS IS THE 33-MINUTE WEDGE, and it was the rig's own doing rather than the
  harness's. Run dbe64eea journalled turns for half an hour, filled the pipe
  mid-log, and stopped: process alive, nothing progressing, no error anywhere,
  and the log that would have shown it never existed. Measured directly — a
  child writing 200 KB with `.redirectOutput(File)` set had not exited after
  10 seconds, while the same child with the redirect merely *dropped* exits in
  207 ms.

  A PLAIN `Thread`, not a `future`. jolt's futures are fibers, and a fiber
  parked in a blocking read is delicate here (karamazov-p3jo); a host thread
  blocking on a read is just a thread blocking on a read. It is a daemon so it
  can never hold the sweep open, and it ends on its own when the child's
  stream closes.

  FLUSHED PER CHUNK rather than left to the stream's buffer. The log's whole
  job is to be readable WHILE a run is in trouble — a sweep is watched by
  tailing it — and a daemon thread holding the last few kilobytes when the
  parent exits would lose exactly the end, which is the part that says what
  went wrong."
  [^java.lang.Process p ^java.io.File file]
  (doto (Thread.
         (fn []
           (try
             (with-open [in (.getInputStream p)
                         out (java.io.FileOutputStream. file)]
               (let [buf (byte-array 8192)]
                 (loop []
                   (let [n (.read in buf)]
                     (when (pos? n)
                       (.write out buf 0 n)
                       (.flush out)
                       (recur))))))
             (catch Throwable _ nil))))
    (.setDaemon true)
    (.start)))

(defn- last-activity
  "The newest timestamp in the child's journal, or nil if it has not written
  one yet. The child's own record of being alive.

  ALL THREE TABLES, because a run can be busy without taking a turn. `turns`
  and `events` miss the reviewer, the critic and the supervisor's own calls
  entirely — those land in `side_calls` — and a feature round spends real
  minutes there between one owner ending and the next beginning. Watching
  turns alone would call a working review a wedge."
  [conn]
  (->> ["SELECT MAX(t) AS t FROM (SELECT MAX(created_at) AS t FROM turns
                                  UNION ALL
                                  SELECT MAX(created_at) AS t FROM events
                                  UNION ALL
                                  SELECT MAX(created_at) AS t FROM side_calls)"]
       (db/fetch conn)
       first
       :t))

(defn- open-child-db
  "A read connection to the child's database, once it exists.

  Safe from another process because db/connect sets WAL, which is exactly the
  case its docstring is about: before WAL, pointing a reader at a live run's
  file killed the branch that was writing. nil until the child has created the
  file, which it does at system/start!."
  [root]
  (let [f (io/file (str root "/.samizdat/samizdat.sqlite3"))]
    (when (and (.exists f) (pos? (.length f)))
      (try (db/connect (.getPath f)) (catch Throwable _ nil)))))

(defn await-child!
  "Wait for `p` to exit. Returns {:ended-by :loop|:clock|:stall …}.

  OUR OWN CLOCK, NOT `.waitFor(n, unit)`. Measured on jolt 0.8.6: a timed
  `.waitFor` waits about 1.24x what it is asked for — 1000 ms came back at
  1233, 20000 at 24800, 60000 at 74183, 120000 at 148904, a stable ~24%
  overshoot — and asked in `TimeUnit/SECONDS` it returns immediately instead
  of waiting at all. So the sweep's only stop handle was inaccurate at the
  unit we used and absent at the other: the 45-minute cap on the one recorded
  run actually fired at 55.6 minutes. `Thread/sleep` is accurate to a few
  milliseconds at every scale tested, so a poll against
  `System/currentTimeMillis` is.

  THE STALL CHECK IS THE ONE THAT EARNS ITS KEEP. That same run's last
  journalled activity was at 03:00:31 and the rig killed it at 03:33:49: 33
  minutes in which the child took no turn, fired no gate, wrote no event and
  cost nothing but the sweep's wall clock. Doubling a timeout over a wedge
  buys twice the waiting. The child's own journal is the liveness signal, so
  a wedge is named — `:stall`, with the last activity and the idle span — and
  the sweep moves on to the next run instead of paying out the whole cap.

  `stall-ms` must stay well above the slowest legitimate quiet period, which
  is a provider call: :llm :max-response-ms is 600000 and the retry ladder can
  spend that more than once on one turn."
  [p {:keys [root timeout-ms stall-ms poll-ms]
      :or {poll-ms 15000}}]
  (let [t0 (System/currentTimeMillis)
        conn (atom nil)]
    (try
      (loop [seen nil
             seen-at t0]
        (let [now (System/currentTimeMillis)]
          (cond
            (not (.isAlive p))
            {:ended-by :loop :waited-ms (- now t0) :last-activity-at seen}

            (and timeout-ms (>= (- now t0) timeout-ms))
            {:ended-by :clock :waited-ms (- now t0) :last-activity-at seen}

            (and stall-ms (>= (- now seen-at) stall-ms))
            {:ended-by :stall :waited-ms (- now t0) :last-activity-at seen
             :idle-ms (- now seen-at)}

            :else
            (do
              (Thread/sleep (long poll-ms))
              (when-not @conn (reset! conn (open-child-db root)))
              (let [t (try (some-> @conn last-activity) (catch Throwable _ seen))]
                (if (and t (not= t seen))
                  (recur t (System/currentTimeMillis))
                  (recur seen seen-at)))))))
      (finally
        (when-let [c @conn] (try (db/close c) (catch Throwable _ nil)))))))

(defn- kill!
  "Stop the child and everything under it, returning whether anything survived.

  `.destroyForcibly` then a bounded confirmation, because a sweep that
  believed a run had ended while its process was still working ran two arms at
  once and contaminated both. The confirmation polls rather than calling the
  timed `.waitFor` for the reason await-child! gives."
  [p]
  (.destroyForcibly p)
  (let [deadline (+ (System/currentTimeMillis) 15000)]
    (loop []
      (cond
        (not (.isAlive p)) false
        (>= (System/currentTimeMillis) deadline) true
        :else (do (Thread/sleep 200) (recur))))))

(defn keep-recording!
  "Copy the child's database out of the worktree as ONE file, with nothing
  left behind in a write-ahead log.

  A PLAIN FILE COPY LOSES THE RUN. db/connect puts every database in WAL mode,
  and a child we killed with `.destroyForcibly` never got to check its log
  back in — so the pages sit in `<db>-wal` and the file named `.sqlite3` holds
  only whatever the last automatic checkpoint had folded in. Measured on a
  short run killed the way the rig kills one: main file 4096 bytes, WAL
  988,832, and a copy of the main file alone could not even find the `events`
  table. On a long run the automatic checkpoint saves most of it and loses the
  TAIL — which is precisely the part that says how the run ended.

  Checkpointing the SOURCE is sound for the same reason reconciling it is: the
  process that owned it is confirmed dead, so there is no writer to race."
  [src dest]
  (let [c (db/connect (str src))]
    (try (db/execute! c "PRAGMA wal_checkpoint(TRUNCATE)")
         (finally (db/close c))))
  (io/copy (io/file (str src)) (io/file (str dest))))

(defn reconcile-recording!
  "Mark a killed child's run row for what it is, in the copy that is kept.

  A run row says `running` from the moment the beam opens until it finishes,
  so a child we killed leaves the preserved recording asserting forever that
  it is still going. `runs/reconcile-orphans!` is the harness's own answer and
  it is sound here for exactly its own reason: it is only ever right when
  nothing can be running, and we have just confirmed the process is dead.

  `interrupted` is deliberately a status `battery/draft-case` REFUSES. A run
  the clock ended is not a case — the loop did not choose that ending — and
  the point of writing the honest status is that the refusal happens for the
  true reason rather than because the row still claims to be running."
  [path]
  (try
    (let [conn (db/connect (str path))]
      (try (runs/reconcile-orphans! conn)
           (finally (db/close conn))))
    (catch Throwable _ nil)))

;;; ------------------------------------------------------------ memory carry

(def ^:private memory-tables
  "The tables a run's LEARNING lives in, as opposed to its record.

  `knowledge` only, for now, and the restraint is the point: `runs`, `turns`,
  `events` and the rest are what THIS run did, and carrying them forward would
  hand the next run a history it did not live. What crosses the boundary is
  what the harness came to believe, which is exactly the thing the epic is
  about (karamazov-ei6t)."
  ["knowledge"])

(defn- copy-table!
  "Copy every row of `table` from `from` to `to`, replacing on id collision.

  INSERT OR REPLACE rather than INSERT: a memory carried forward and then
  corroborated comes back with the same id and a higher count, and the newer
  row is the one worth keeping. Column names are read from the source rather
  than listed, so a migration that adds a column does not silently stop being
  carried — which is the failure mode a hand-written column list has, and it
  fails in the direction of losing exactly the newest field."
  [from to table]
  (let [rows (db/fetch from [(str "SELECT * FROM " table)])]
    (when (seq rows)
      (let [cols (mapv name (keys (first rows)))
            placeholders (str/join ", " (repeat (count cols) "?"))
            sql (str "INSERT OR REPLACE INTO " table
                     " (" (str/join ", " cols) ") VALUES (" placeholders ")")]
        (doseq [r rows]
          (db/with-writer
            (db/execute! to (into [sql] (map #(get r (keyword %))) cols))))))
    (count rows)))

(defn carry-memory!
  "Seed the worktree at `root` with what this ARM has learned so far.

  WHY THE ARENA NEEDS THIS AT ALL (karamazov-ei6t.1). Every run gets a fresh
  worktree, and samizdat resolves its database from `:run :root`, so every run
  also got a fresh knowledge store. Measured across three preserved runs:
  30-35 memories written per run and ZERO ever recalled — which is structural,
  not a defect. Memories are written at the END by distil-session! and
  distil-project!, and the two recall calls a run makes happen mid-run against
  an empty store. So the rig could not measure memory at all, and every later
  round of the epic would have shown the same zero before and after.

  PER ARM, NEVER PER SWEEP, and that is the line that keeps the comparison
  honest: two arms sharing a store means arm B inherits arm A's learning and
  the delta is no longer the arm. Across TASKS within an arm is deliberate —
  the subject is one repository, and an agent learning where its tests live is
  the realistic shape. Tasks are never compared to each other anyway.

  The database is created and migrated here, before the child starts, because
  the child's own `system/start!` opens whatever is already there."
  [carry-db root]
  (when (and carry-db (.exists (io/file (str carry-db))))
    (let [dest (str root "/.samizdat/samizdat.sqlite3")
          _ (io/make-parents (io/file dest))
          to (db/open! dest)
          from (db/connect (str carry-db))]
      (try (reduce + 0 (map #(copy-table! from to %) memory-tables))
           (finally (db/close from) (db/close to))))))

(defn harvest-memory!
  "Fold what this run learned back into the arm's carry database.

  Taken from the PRESERVED recording rather than the worktree, so a run the
  rig had to kill still contributes what it had learned by then — and so this
  reads the same checkpointed file everything else does (`keep-recording!`)."
  [recording carry-db]
  (when (and carry-db recording (.exists (io/file (str recording))))
    (let [to (db/open! (str carry-db))
          from (db/connect (str recording))]
      (try (reduce + 0 (map #(copy-table! from to %) memory-tables))
           (finally (db/close from) (db/close to))))))

;;; ---------------------------------------------------------- the measurement

(defn- harness-revision
  "The harness's own git revision and whether its tree was clean. Both, because
  a sweep run against uncommitted work is legitimate and must never be quotable
  as if it had been run against a commit."
  []
  (let [root (System/getProperty "user.dir")]
    {:harness-rev (str/trim (:out (git root "rev-parse" "--short" "HEAD")))
     :harness-clean? (str/blank? (str/trim (:out (git root "status" "--porcelain"))))}))

(defn load-average
  "The machine's 1-minute load average, or nil if it cannot be read.

  STAMPED ON EVERY ROW because a wall-clock measurement on a contended box is
  not a measurement of the harness. Run dbe64eea-successor's retry attempt
  measured 0.62 minutes a turn against the first attempt's 0.37 in the SAME
  RUN, and the difference was three of this repository's own test suites
  running beside it — load average 26 on a 10-core machine. Nothing in the row
  said so, so the number read as 'the retry is slower', and the budget was
  very nearly re-tuned around it.

  karamazov-7mo.4's rule is that a score without its fixture, revision, model
  and scorer beside it is only a number. Contention belongs on that list: it
  is the one confound a rig sharing a machine with its own development cannot
  avoid and can always record.

  Read from `uptime` rather than a management bean: jolt has no
  java.lang.management, and the bean returned nil here — measured, before this
  read like a machine with no load rather than a call that does not work."
  []
  (try
    (some-> (re-find #"load averages?:\s+([0-9.]+)" (str (:out (sh "uptime"))))
            second
            Double/parseDouble)
    (catch Throwable _ nil)))

(defn suite-green?
  "Whether the subject's own verify command passes in `root`. The scorer, named
  and recorded, not inferred from the harness's opinion of itself.

  nil, never false, when the command could not be run: `sh` failing to launch
  and a suite failing are different facts, and only the second is a red test.

  Waits on its own clock for the reason `await-child!` gives: jolt's timed
  `.waitFor` overshoots by about a quarter, so a ten-minute verify bound was
  really twelve and a half."
  [root verify-cmd timeout-ms]
  (try
    (let [args (str/split (str verify-cmd) #"\s+")
          pb (doto (ProcessBuilder. ^java.util.List (vec args))
               (.directory (io/file (str root)))
               (.redirectErrorStream true))
          p (.start pb)
          deadline (+ (System/currentTimeMillis) (long timeout-ms))
          done? (loop []
                  (cond (not (.isAlive p)) true
                        (>= (System/currentTimeMillis) deadline) false
                        :else (do (Thread/sleep 500) (recur))))]
      (if done?
        (zero? (.exitValue p))
        (do (kill! p) false)))
    (catch Throwable _ nil)))

(defn edits
  "What the SUPERVISOR changed about the loop during this run, and what became
  of each change.

  THE POINT OF THIS READER. A run's outcome says whether the harness solved
  the task; this says what the harness decided was wrong with ITSELF while
  trying. Across a sweep of independent runs seeded from the same factory
  userspace, an edit that recurs is not this run's opinion — it is a standing
  defect in the shipped template, and the fix belongs there rather than being
  re-derived per project. That is the question `recurring-edits` asks, and it
  is the one thing the paper's loop cannot ask of itself: it keeps the evolved
  graph and never compares independent evolutions.

  PROJECT-AUTHORED ONLY, on drift's argument (userspace.clj:228): the 55-odd
  factory rows are the baseline every run starts from, not a tuning.

  Both halves are kept. A COMMITTED edit says what stuck; a ROLLED-BACK one
  says what the supervisor kept trying that the protocol refused, which is
  either a persistent bad idea or a validator that is wrong — and both are
  actionable. `:attempt` is what mutation.clj:193 already records for exactly
  this purpose and nothing has ever read (karamazov-mpd, ylte.2)."
  [conn run-id]
  (let [rows (db/fetch conn ["SELECT kind, name, version, rationale,
                                    success_count, failure_count, length(body) AS body_len
                             FROM userspace WHERE source = 'project'
                             ORDER BY kind, name, version"])
        ;; The factory seed for each name, to size the edit against the
        ;; TEMPLATE rather than against the previous edit: three small saves
        ;; that walk a cell a long way from the shipped one is the shape drift
        ;; was built to catch, and a per-save delta hides it.
        seed (into {} (for [r (db/fetch conn ["SELECT kind, name, length(body) AS body_len
                                              FROM userspace WHERE source = 'factory'"])]
                        [[(:kind r) (:name r)] (:body_len r)]))]
    {:saves (mapv (fn [r]
                    {:kind (:kind r) :name (:name r) :version (:version r)
                     :rationale (:rationale r)
                     :revert? (str/starts-with? (str (:rationale r)) "revert to v")
                     :standing [(:success_count r) (:failure_count r)]
                     :chars-vs-seed (when-let [b (get seed [(:kind r) (:name r)])]
                                      (- (:body_len r) b))})
                  rows)
     ;; journal/notes returns the note's DATA map already parsed; wrapping it
     ;; in (mapv :data ...) yields a vector of nils, which reads as 'edits
     ;; happened and said nothing' rather than as a bug.
     :committed (vec (journal/notes conn run-id :mutation-committed))
     :refused (vec (journal/notes conn run-id :mutation-rolled-back))}))

(defn recall-report
  "What memory was ASKED and whether the answer was used (karamazov-ei6t.2).

  The question this exists to settle: recall was called exactly twice in each
  of three live runs of 167, 203 and 219 turns, while 30-35 memories were
  written per run. Three explanations need different fixes — the model does
  not know the tool is there; it calls it, gets nothing, and stops; or it
  genuinely does not need memory on a small tree it can read directly. A count
  cannot tell them apart.

  So this reports, per call: what was asked, how many rows came back, how many
  memories were live at the time, and whether the branch's NEXT turn touched
  one of the ids it was handed. That last column is the one that matters and
  the one nothing recorded before: a memory returned and ignored looked
  exactly like a memory never returned.

  `:used?` is a weak signal on purpose — it asks whether a returned id was
  touched again, not whether the model was influenced, which no record can
  answer. Read it as a floor."
  [conn run-id]
  (let [calls (vec (journal/notes conn run-id :recall))
        touched (into #{} (map :id)
                      (db/fetch conn ["SELECT id FROM knowledge WHERE use_count > 0"]))]
    {:calls (count calls)
     :turns (count (journal/turns conn run-id))
     :hits (count (filter #(pos? (long (or (:returned %) 0))) calls))
     :empty-store (count (filter #(zero? (long (or (:live %) 0))) calls))
     :detail (mapv (fn [c]
                     {:turn (:turn c)
                      :query (:query c)
                      :returned (:returned c)
                      :live (:live c)
                      :used? (boolean (some touched (:ids c)))})
                   calls)}))

(defn memory-eval
  "Score a knowledge store against questions whose answers are known.

  karamazov-ei6t.14, the shape of lemmalog's scenario::run_eval — accuracy
  against ground truth rather than an argument about whether recall feels
  better. What is NOT copied is its benchmarks: LongMemEval and LoCoMo are
  long-conversation QA over prose, and samizdat's memory is about a codebase
  and a harness, written mostly by mechanical distillers from its own journal.

  THE FIXTURE IS BETTER THAN SYNTHETIC AND IT IS ALREADY HERE. A preserved
  recording holds memories real runs actually wrote, so a question with a
  checkable answer falls straight out of one — `what is the test command`,
  `is python3 allowed`. A synthetic corpus would measure how well recall
  ranks text somebody invented for it to rank.

  `cases` are {:ask :expect} where :expect is a substring the right memory
  must contain. Substring rather than a judge on purpose: this scores
  RETRIEVAL, and grading it with a model would fold the model's reading into
  the number the retrieval is being measured by.

  Reports :recall@k as well as :hit — a store that has the answer at rank 5
  and a store that does not have it at all are different failures, and only
  the first is a ranking problem."
  [conn cases k]
  (let [scored (mapv (fn [{:keys [ask expect]}]
                       (let [rows (knowledge/recall conn ask k)
                             pos (first (keep-indexed
                                         (fn [i r]
                                           (when (str/includes?
                                                  (str/lower-case (str (:content r)))
                                                  (str/lower-case (str expect)))
                                             (inc i)))
                                         rows))]
                         {:ask ask :expect expect :rank pos
                          :returned (count rows) :hit? (some? pos)}))
                     cases)
        n (count scored)]
    {:n n
     :hits (count (filter :hit? scored))
     :accuracy (if (pos? n) (double (/ (count (filter :hit? scored)) n)) 0.0)
     :mean-rank (let [rs (keep :rank scored)]
                  (when (seq rs) (double (/ (reduce + rs) (count rs)))))
     :empty (count (filter #(zero? (:returned %)) scored))
     :detail scored}))

(defn measure
  "One run's row, read off the journal after it ends.

  Everything here is already collected by the harness; nothing is instrumented
  for the rig. That is deliberate — a metric that only exists while the rig is
  watching measures the rig."
  [conn run-id]
  (let [turns (journal/turns conn run-id)
        usage (journal/run-usage conn run-id)
        tally (journal/gate-tally conn run-id)
        arts (journal/artifacts conn run-id)
        ;; Turns to the first artifact: the paper's steps-to-progress, and the
        ;; number every stalled run in this project's history moves first.
        ;; nil when nothing was ever banked — absent, not zero.
        first-art (when (seq arts)
                    (let [ids (set (map :branch_id arts))]
                      (->> turns
                           (keep-indexed (fn [i t] (when (ids (:branch_id t)) (inc i))))
                           first)))]
    {:turns (count turns)
     :artifacts (count arts)
     :turns-to-first-artifact first-art
     :fitness (session/fitness)
     :tokens (:total-tokens usage)
     :prompt-tokens (:prompt-tokens usage)
     :completion-tokens (:completion-tokens usage)
     :cache-hit-rate (:cache-hit-rate usage)
     :provider-calls (+ (:turns usage) (:side-calls usage))
     :gates (into {} (for [g tally]
                       [(:gate g) (select-keys g [:fired :met :met_late :unmet :open])]))
     :tools (frequencies (keep :tool_name turns))
     :edits (edits conn run-id)}))

;;; --------------------------------------------------------------- one run

(def harness-classpath
  "samizdat's own resolved classpath, ABSOLUTE, memoized.

  The child runs with its cwd at the worktree, so it cannot resolve samizdat's
  deps.edn — `jolt` reads the deps.edn of the directory it is run from, and
  that directory is the SUBJECT's. `-Scp` supplies the roots instead and
  expands no deps.

  Absolutised because `-Spath` emits the project roots relative (./src,
  ./resources, ./dev, ./test) and those would resolve against the worktree,
  silently giving the child a classpath with no samizdat on it at all."
  (memoize
   (fn []
     (let [harness (System/getProperty "user.dir")
           ;; The LAST line that looks like a classpath, not simply the last
           ;; line. `sh` merges stderr in now (it has to; see its docstring),
           ;; so dependency-resolution chatter shares the stream and a cold
           ;; cache can put a progress line after the answer.
           raw (->> (str/split-lines (:out (sh "jolt" "-A:dev" "-Spath")))
                    (map str/trim)
                    (filter #(and (str/includes? % ":")
                                  (or (str/starts-with? % "/")
                                      (str/starts-with? % "./"))))
                    last)]
       (when (str/blank? (str raw))
         (throw (ex-info "could not read the harness classpath from jolt -Spath" {})))
       (->> (str/split raw #":")
            (map (fn [e] (if (str/starts-with? e "./") (str harness (subs e 1)) e)))
            (str/join ":"))))))



(def ^:private child-main
  "The form the child process evaluates. Reads its params from `:in`, runs, and
  writes the row to `:out` as EDN.

  A FORM, built here and passed with -e, so the child and the parent can never
  drift: there is one definition of what a measured run is."
  '(do
     (require '[samizdat.dev.arena :as arena]
              '[samizdat.server :as server]
              '[samizdat.session :as session]
              '[samizdat.system :as system]
              '[samizdat.agent.beam :as beam]
              '[clojure.edn :as edn])
     (let [in (edn/read-string (slurp (System/getenv "ARENA_IN")))
           out (System/getenv "ARENA_OUT")
           started (System/currentTimeMillis)
           row (try
                 (system/start! #'server/handler
                                {:run (cond-> {:root (:root in)}
                                        ;; THE RUNAWAY GUARD, opted into
                                        ;; because a sweep is unattended.
                                        ;; :feature/route has no numeric
                                        ;; abandon by default — it keeps
                                        ;; solving until a supervisor says
                                        ;; STOP — and there is no supervisor
                                        ;; watching a sweep at 3am.
                                        (:max-revisions-hard in)
                                        (assoc :max-revisions-hard
                                               (:max-revisions-hard in)))
                                 :http {:port (:http-port in)}})
                 (try
                   (session/reset!)
                   (when-let [f (:setup in)] (eval f))
                   (let [cfg (system/config)
                         conn (system/conn)
                         result (beam/run! {:conn conn :config cfg
                                            :llm-adapter (system/adapter)
                                            :llm-config (:llm cfg)
                                            :root (:root in)
                                            :problem (:problem in)
                                            :max-turns (:max-turns in)
                                            :beam-width (:beam-width in)
                                            :token-budget (:token-budget in)})
                         run-id (:run-id result)]
                     (merge {:run-id run-id
                             :status (:status result)
                             :verify-cmd (get-in cfg [:run :verify-cmd])
                             :model (get-in cfg [:llm :model])
                             :provider (get-in cfg [:llm :provider])
                             :cwd (str (.getCanonicalFile (clojure.java.io/file ".")))}
                            (when run-id (arena/measure conn run-id))))
                   (finally (try (system/stop!) (catch Throwable _ nil))))
                 (catch Throwable e
                   {:status :rig-error :error (str (ex-message e))}))]
       (spit out (pr-str (assoc row :child-wall-ms
                                (- (System/currentTimeMillis) started))))
       (System/exit 0))))

(defn run-once!
  "One measured run of one arm, in its own worktree, in its OWN PROCESS whose
  working directory is that worktree.

  The subprocess is the whole point — see the namespace docstring. An
  in-process version of this leaves every arm's relative-path `eval` reading
  the samizdat checkout instead of the project, which is a bias that varies
  with what each arm happens to do and so cannot be cancelled by pairing.

  opts:
    :repo :sha        — the subject repository and the pinned baseline
    :arm              — {:name kw :setup <quoted form, optional>}
    :problem          — the task text
    :max-turns        — the cap on ONE BRANCH, not on the run; see below
    :beam-width :token-budget
    :max-revisions-hard — the feature loop's runaway guard; see below
    :dest             — where to put the worktree
    :http-port        — the child's server port
    :timeout-ms       — wall-clock cap on the child
    :stall-ms         — how long the child may journal nothing before it is
                        called wedged
    :verify-timeout-ms
    :keep?            — leave the worktree behind for inspection
    :recordings       — directory to keep each run's database (and log) in
    :carry-db         — this ARM's accumulated memory, seeded in before the
                        child starts and folded back after it ends

  WHAT BOUNDS A RUN, measured rather than assumed, because the first sweep was
  bounded by nothing and the rig's clock had to end every run:

    `max-turns` is PER BRANCH and never the run's total. On a board-driven
    round it is the OWNER's cap (gates.edn :board-owner-turns takes the min of
    the two), and the feature loop multiplies it by every revision and every
    task in a round. On the one recorded run it was 60, the owner spent
    exactly 60 and came back exhausted, and the loop then started revision 1
    from nothing — so the number that looked like a work budget was in fact
    the thing that threw the work away. It is also what the steering gates key
    off (:wind-down-fraction 0.85, :last-call-window 2), which is the reason
    it cannot simply be raised to infinity.

    `token-budget` does NOT bound one of these runs, and the row says so.
    :beam/round-open is the only place it is checked, and a non-iterating
    manifest — feature, team, decompose — has one round for the whole job.
    Filed as its own defect; not worked around here, because a rig that
    quietly patched the harness would stop measuring it.

    `max-revisions-hard` is the one that closes the loop. :feature/route is
    deliberately supervisor-driven with no numeric abandon, and a run nobody
    is watching is precisely the case its `:run :max-revisions-hard` guard
    exists for. An unattended sweep opts in; that is what turns 'the clock
    killed it' into an ending the loop chose.

  Returns the row. Never throws for a run that failed: a failure IS a
  measurement, and a sweep that died on arm B's third run would silently
  report arm A as better."
  [{:keys [repo sha arm problem max-turns beam-width token-budget dest
           max-revisions-hard verify-timeout-ms keep? http-port timeout-ms
           stall-ms recordings carry-db]
    :or {verify-timeout-ms 600000 http-port 3997 timeout-ms 3600000
         stall-ms 1800000}}]
  (let [started (System/currentTimeMillis)
        root (worktree! repo sha dest)
        in-f (str root "/.arena-in.edn")
        out-f (str dest "-row.edn")
        log-f (io/file (str dest "-child.log"))
        ;; SETUP IS TIMED SEPARATELY. It used to sit inside :wall-ms with
        ;; nothing to separate it from the run, and on the first sweep that
        ;; hid several minutes of cold `jolt -Spath` inside a number read as
        ;; the run's duration. A budget cannot be sized against a figure that
        ;; silently includes the rig.
        budget {:turns-per-branch max-turns
                :beam-width beam-width
                :token-budget token-budget
                :max-revisions-hard max-revisions-hard
                :timeout-ms timeout-ms
                :stall-ms stall-ms}
        load-at-start (load-average)
        fail (fn [status extra]
               (merge {:arm (:name arm) :sha sha :status status
                       :wall-ms (- (System/currentTimeMillis) started)
                       :budget budget
                       :load [load-at-start (load-average)]
                       :at (str (java.time.Instant/now))}
                      (harness-revision) extra))]
    (try
      ;; WHAT THIS ARM ALREADY KNOWS, before the child opens the database.
      ;; Without it every run starts amnesiac and the memory rounds of
      ;; karamazov-ei6t cannot be measured at all.
      (let [n (try (carry-memory! carry-db root) (catch Throwable _ nil))]
        (when (and n (pos? (long n)))
          (println (format "   carried %d memor%s into %s"
                           n (if (= 1 n) "y" "ies") (.getName (io/file dest))))))
      (spit in-f (pr-str {:root root :problem problem
                          :max-turns max-turns :beam-width beam-width
                          :token-budget token-budget
                          :max-revisions-hard max-revisions-hard
                          :http-port http-port
                          :setup (:setup arm)}))
      (let [pb (doto (ProcessBuilder.
                      ^java.util.List
                      ["jolt" "-Scp" (harness-classpath) "-e" (pr-str child-main)])
                 ;; THE LINE THIS NAMESPACE EXISTS FOR.
                 (.directory (io/file root))
                 ;; Both streams on one pipe, and `drain-to!` empties it.
                 ;; Asking ProcessBuilder to write the file instead is what
                 ;; the first sweep did, and jolt honours neither redirect
                 ;; form — see drain-to!, which is also the wedge that ended
                 ;; that sweep.
                 (.redirectErrorStream true))
            _ (doto (.environment pb)
                (.put "ARENA_IN" in-f)
                (.put "ARENA_OUT" out-f)
                (.put "HARNESS_ROOT" root))
            setup-ms (- (System/currentTimeMillis) started)
            child-at (System/currentTimeMillis)
            p (.start pb)
            drain (drain-to! p log-f)
            pid (try (.pid p) (catch Throwable _ nil))
            waited (await-child! p {:root root :timeout-ms timeout-ms
                                    :stall-ms stall-ms})
            ended-by (:ended-by waited)
            ;; TRUST NOTHING. A sweep that believed a run had ended while its
            ;; process was still working ran two arms at once and contaminated
            ;; both — observed, and the reason this check exists. The row
            ;; carries :left-alive? so an overlap can never again be invisible
            ;; in the record.
            stray? (kill! p)
            ;; LET THE LOG CATCH UP before anything reads or copies it. The
            ;; last lines a killed child wrote are the ones that say what it
            ;; was doing, and they are still in flight in the drain thread the
            ;; moment the process dies. Bounded, because a hung reader must
            ;; not become a new way for the sweep to wedge.
            _ (try (.join drain 5000) (catch Throwable _ nil))
            span {:pid pid :left-alive? stray?
                  :ended-by ended-by
                  :setup-ms setup-ms
                  :child-ms (- (System/currentTimeMillis) child-at)
                  :last-activity-at (:last-activity-at waited)
                  :idle-ms (:idle-ms waited)
                  :ended-at (str (java.time.Instant/now))}]
        (cond
          ;; A WEDGE AND A LONG RUN ARE DIFFERENT RESULTS and the status says
          ;; which. :stalled means the child was alive and journalling
          ;; nothing; :timeout means it was working and ran out of clock. The
          ;; first is a bug to chase and the second is a budget to raise, and
          ;; reporting both as :timeout is how 33 idle minutes read as work.
          (= :stall ended-by) (fail :stalled span)
          (= :clock ended-by) (fail :timeout span)

          (not (.exists (io/file out-f)))
          (fail :no-row (merge span
                               {:exit (try (.exitValue p) (catch Throwable _ nil))
                                :log-tail (when (.exists log-f)
                                            (->> (slurp log-f) str/split-lines (take-last 25)
                                                 (str/join "\n")))}))
          :else
          (let [row (edn/read-string (slurp out-f))]
            (merge {:arm (:name arm) :sha sha
                    :green? (suite-green? root (:verify-cmd row) verify-timeout-ms)
                    :wall-ms (- (System/currentTimeMillis) started)
                    :budget budget
                    ;; Both ends, because a sweep that started quiet and ended
                    ;; contended is exactly the shape that misleads.
                    :load [load-at-start (load-average)]
                    :at (str (java.time.Instant/now))}
                   span
                   (harness-revision)
                   row))))
      (catch Throwable e (fail :rig-error {:error (ex-message e)}))
      (finally
        ;; KEEP THE RECORDING BEFORE THE WORKTREE GOES. The db lives INSIDE the
        ;; worktree (config.clj resolves it from :run :root), so dropping the
        ;; worktree destroyed the only copy of the conversation — and a
        ;; conversation is exactly what karamazov-ylte.4 needs to seed the
        ;; battery. The sweep was producing metric rows and silently throwing
        ;; away the thing the metrics were meant to enable.
        ;;
        ;; The db, not a derived case: what expectations a case should carry is
        ;; a judgement made later, and re-deriving them from a db is possible
        ;; while re-deriving a db from a case is not.
        ;;
        ;; AND THE CHILD'S LOG, which the first sweep dropped with the
        ;; worktree. The one wedge it produced could not be diagnosed
        ;; afterwards for exactly that reason: the journal shows a run going
        ;; quiet and only the log can say what it went quiet inside.
        (when recordings
          (try
            (io/make-parents (io/file (str recordings "/x")))
            (let [nm (.getName (io/file dest))
                  src (io/file (str root "/.samizdat/samizdat.sqlite3"))
                  kept (io/file (str recordings "/" nm ".sqlite3"))]
              (when (.exists src)
                ;; The process is confirmed dead by now, which is the one
                ;; condition that makes both of these sound: nothing can be
                ;; writing the log we are folding in, and no run can be
                ;; running that we are about to mark interrupted.
                (keep-recording! src kept)
                (reconcile-recording! (.getPath kept)))
              (when (.exists log-f)
                (io/copy log-f (io/file (str recordings "/" nm ".log"))))
              ;; FROM THE PRESERVED COPY, so a run the rig had to kill still
              ;; contributes what it had learned by then.
              (when (.exists kept)
                (harvest-memory! (.getPath kept) carry-db)))
            (catch Throwable _ nil)))
        (when-not keep? (drop-worktree! repo dest))))))


;;; ----------------------------------------------------------------- sweeps

(defn append-row!
  "One row per line, appended. EDN and not JSON because a row carries keywords
  and the reader is Clojure; append-only because a sweep that crashes on run 4
  must not cost the three rows it already has."
  [path row]
  (io/make-parents (io/file (str path)))
  (spit (str path) (str (pr-str row) "\n") :append true))

(defn rows
  "Every row written to `path`, in order."
  [path]
  (if (.exists (io/file (str path)))
    (->> (str/split-lines (slurp (str path)))
         (remove str/blank?)
         (mapv edn/read-string))
    []))

(defn sweep!
  "Run `arms` `n` times each, interleaved, writing a row per run.

  INTERLEAVED, not arm-at-a-time: a provider degrading over the hour would
  otherwise land entirely on whichever arm ran second and read as that arm
  being worse. Same reason RFC-010's verdict carries `:confounded`.

  SEQUENTIAL, because `run-once!` takes the process's system over for the
  duration. Two arms in parallel would share one `bind-project!` and neither
  would be running its own userspace."
  [{:keys [arms n out work] :as opts}]
  (doseq [i (range n)
          arm arms]
    (let [dest (str work "/wt-" (name (:name arm)) "-" i)
          row (run-once! (assoc opts :arm arm :dest dest))]
      (append-row! out (assoc row :n i))
      (println (format "%-10s n=%d  %-11s green=%-5s turns=%-4s tok=%-8s fit=%s"
                       (name (:name arm)) i
                       (str (:status row)) (str (:green? row))
                       (str (:turns row)) (str (:tokens row))
                       (some-> (:fitness row) (->> (format "%.2f")))))))
  (rows out))

(defn summarize
  "Per arm: n, ship rate, green rate, and the median of each numeric column.

  MEDIAN, not mean, and no significance test. With the run counts this rig can
  afford, an accept/reject turning on one or two runs is a search trace and not
  a result — the paper says so of its own 20-episode splits and it is truer
  here. Report the spread; let a person read it."
  [rows]
  (let [med (fn [xs] (let [v (vec (sort (remove nil? xs)))
                           c (count v)]
                       (when (pos? c) (nth v (quot c 2)))))]
    (into {}
          (for [[arm rs] (group-by :arm rows)]
            [arm {:n (count rs)
                  :shipped (count (filter #(= :completed (:status %)) rs))
                  :green (count (filter :green? rs))
                  :rig-errors (count (filter #(= :rig-error (:status %)) rs))
                  ;; HOW EACH RUN ENDED, and it is the first thing to read.
                  ;; A sweep whose runs the rig ended is not measuring the
                  ;; loop: :loop means the run reached its own ending,
                  ;; :clock that it ran out of wall time, :stall that it went
                  ;; quiet and was killed. Only :loop rows can seed a battery
                  ;; case, because only those have a status the loop chose.
                  :ended-by (frequencies (map #(or (:ended-by %) :?) rs))
                  :median {:turns (med (map :turns rs))
                           :tokens (med (map :tokens rs))
                           :fitness (med (map :fitness rs))
                           :wall-ms (med (map :wall-ms rs))
                           :turns-to-first-artifact (med (map :turns-to-first-artifact rs))}
                  :spread {:turns [(med (map :turns rs))
                                   (apply min (or (seq (keep :turns rs)) [nil]))
                                   (apply max (or (seq (keep :turns rs)) [nil]))]}}]))))

(defn recurring-edits
  "Across a sweep: which loop fixes the supervisor arrives at INDEPENDENTLY.

  Each run in a sweep gets a fresh worktree, so a fresh db, so a userspace
  seeded fresh from the factory template. N runs are therefore N independent
  evolutions from one baseline — which is exactly the corpus this question
  needs, and it is a free consequence of the isolation the rig already has for
  arm comparison.

  Grouped by [kind name] and reported with the runs it appeared in, because a
  fix derived once is an opinion and the same fix derived in four runs out of
  five is a defect in the shipped template. `:refused` is grouped the same way
  and read the same way, inverted: a rejection the supervisor keeps re-earning
  is either a standing bad idea worth naming in the prompt or a validator
  that is wrong.

  Reports counts and leaves the judgement alone, like graduation-candidates:
  promoting a project's edit into resources/ changes every project, and that
  is not a decision a reader of rows gets to make."
  [rows]
  (let [n (count rows)
        tally (fn [f]
                (->> rows
                     (mapcat (fn [r] (->> (f (:edits r))
                                          (map #(vector (:kind %) (:name %)))
                                          distinct
                                          (map #(vector % (:run-id r))))))
                     (reduce (fn [m [k rid]] (update m k (fnil conj #{}) rid)) {})
                     (sort-by (comp - count val))
                     (mapv (fn [[k rids]]
                             {:surface k :runs (count rids) :of n
                              :rate (double (/ (count rids) (max 1 n)))
                              :run-ids (vec rids)}))))]
    {:runs n
     :saved (tally :saves)
     :refused (tally (fn [e] (map (fn [d] {:kind "attempt" :name (str (:reason d))})
                                  (:refused e))))
     :rationales (->> rows
                      (mapcat (comp :saves :edits))
                      (keep :rationale)
                      frequencies
                      (sort-by (comp - val))
                      vec)}))

(defn tasks
  "The sweep task set, with `:standing-requirements` already appended to each
  problem. One place builds the prompt so two runs of the same task id can
  never differ by a paragraph."
  ([] (tasks "dev/samizdat/dev/arena_tasks.edn"))
  ([path]
   (let [t (edn/read-string (slurp (str (System/getProperty "user.dir") "/" path)))
         std (:standing-requirements t)]
     (assoc t :tasks
            (mapv #(assoc % :problem (str (:problem %) "\n\n" std)) (:tasks t))))))

(defn sweep-tasks!
  "The reference sweep: every task x every arm x n, one row per run.

  TASK-MAJOR, ARM-INTERLEAVED WITHIN A TASK. Arms must be compared on the same
  task, so they alternate inside it; tasks do not need to be interleaved with
  each other because no claim is made across them. Ports step per run so a
  child's listener cannot land in a predecessor's TIME_WAIT.

  `memory?` (default true) carries each ARM's knowledge store forward across
  its runs. Off gives the old amnesiac behaviour, which is the right control
  arm for measuring whether memory helps at all — the question the epic it
  serves has to be able to answer both ways (karamazov-ei6t.1)."
  [{:keys [task-ids arms n out work base-port memory?] :as opts
    :or {base-port 3990 n 3 memory? true}}]
  (let [t (tasks)
        subject (:subject t)
        chosen (if (seq task-ids)
                 (filterv #(contains? (set task-ids) (:id %)) (:tasks t))
                 (:tasks t))
        total (* (count chosen) (count arms) n)]
    (println (format "sweep: %d task(s) x %d arm(s) x n=%d = %d runs, subject %s @ %s"
                     (count chosen) (count arms) n total
                     (:repo subject) (:sha subject)))
    (loop [i 0
           todo (for [task chosen, k (range n), arm arms] [task k arm])]
      (if-let [[task k arm] (first todo)]
        (let [dest (str work "/wt-" (name (:id task)) "-" (name (:name arm)) "-" k)
              ;; PER-TASK OVERRIDES WIN over the sweep's defaults. max-turns
              ;; drives the steering gates as well as the work budget
              ;; (wind-down at 0.85 of it, last-call in the final 2), so a task
              ;; whose scope needs more turns needs its own number or its gate
              ;; profile measures the cap. Runs are compared within a task and
              ;; never across, so unequal budgets cost nothing.
              ;; ONE CARRY DATABASE PER ARM, and the per-arm part is what
              ;; keeps an A/B honest: two arms sharing a store means arm B
              ;; inherits arm A's learning and the delta stops being the arm.
              ;; Across TASKS within an arm is deliberate — the subject is one
              ;; repository, an agent learning where its tests live is the
              ;; realistic shape, and tasks are never compared to each other.
              carry (when memory? (str work "/memory-" (name (:name arm)) ".sqlite3"))
              row (run-once! (merge opts
                                    {:repo (:repo subject) :sha (:sha subject)
                                     :arm arm :problem (:problem task)
                                     :dest dest :carry-db carry
                                     :http-port (+ base-port (mod i 90))}
                                    (select-keys task [:max-turns :timeout-ms
                                                       :stall-ms :beam-width
                                                       :token-budget
                                                       :max-revisions-hard])))
              ;; :budget comes back on the row from run-once!, which is the
              ;; one place that knows every default that was applied. Writing
              ;; a second copy here re-derived it from the task and the opts
              ;; and silently omitted whatever run-once! defaulted — which is
              ;; how a row came to name a turn cap and nothing else while the
              ;; only bound that actually fired was the rig's clock.
              row (assoc row :task (:id task) :difficulty (:difficulty task) :n k
                         :memory-carried?  (boolean carry))]
          (append-row! out row)
          (println (format "[%d/%d] %-16s %-9s n=%d  %-11s by=%-7s green=%-5s turns=%-4s tok=%-9s edits=%s  %.1fmin"
                           (inc i) total (name (:id task)) (name (:name arm)) k
                           (str (:status row)) (str (name (or (:ended-by row) :?)))
                           (str (:green? row))
                           (str (:turns row)) (str (:tokens row))
                           (str (count (:saves (:edits row))))
                           (/ (or (:wall-ms row) 0) 60000.0)))
          (recur (inc i) (rest todo)))
        (rows out)))))

(defn -main
  "Run a sweep as its OWN PROCESS. `jolt -A:dev -m samizdat.dev.arena <out> [n]`.

  NOT THROUGH THE nREPL IMAGE, and this is the whole reason -main exists. An
  eval sent with samizdat.dev.nrepl-eval runs SERVER-SIDE, inside the image;
  killing the nrepl-eval client kills the client and leaves the sweep running.
  Observed the hard way: a stopped-and-relaunched sweep left its predecessor
  still spawning children inside the image, three arms ran at once against one
  provider and one machine, and every timing in the rows was contaminated. A
  sweep must be killable by killing one process, so it gets one.

  Trailing arguments name tasks to run, so a budget change can be proved on
  one task before nine runs are committed to it.

  ARENA_WORK and ARENA_RECORDINGS override the scratch and keep directories.
  The recordings are the point of the sweep — they are what seeds the battery
  (karamazov-ylte.4) — and the default lands them under the platform temp
  directory, which macOS eventually sweeps. A sweep whose evidence expires is
  the same failure as the one that deleted its worktrees, one clock later."
  [& [out n & task-ids]]
  (let [tmp (System/getProperty "java.io.tmpdir")
        rows (sweep-tasks! {:arms [{:name :baseline}]
                            :n (if n (parse-long (str n)) 3)
                            :task-ids (mapv keyword task-ids)
                            :out (or out "arena-rows.edn")
                            :work (or (System/getenv "ARENA_WORK") (str tmp "/arena"))
                            :base-port 3990
                            :beam-width 1
                            :recordings (or (System/getenv "ARENA_RECORDINGS")
                                            (str tmp "/arena-recordings"))
                            :verify-timeout-ms 600000})]
    (println "\n=== SUMMARY ===")
    (clojure.pprint/pprint (summarize rows))
    (println "\n=== RECURRING EDITS ===")
    (clojure.pprint/pprint (recurring-edits rows))
    (flush)
    (System/exit 0)))
