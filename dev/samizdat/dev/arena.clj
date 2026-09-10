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
            [samizdat.system :as system]))

;;; ------------------------------------------------------------------ shell

(defn- sh
  "Run a command, returning {:out :err :exit}. Nothing here is interactive, and
  every destructive call passes its force flag explicitly (AGENTS.md)."
  [& args]
  (let [pb (doto (ProcessBuilder. ^java.util.List (vec args)) (.redirectErrorStream false))
        p (.start pb)
        out (slurp (.getInputStream p))
        err (slurp (.getErrorStream p))]
    {:out out :err err :exit (.waitFor p)}))

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

;;; ---------------------------------------------------------- the measurement

(defn- harness-revision
  "The harness's own git revision and whether its tree was clean. Both, because
  a sweep run against uncommitted work is legitimate and must never be quotable
  as if it had been run against a commit."
  []
  (let [root (System/getProperty "user.dir")]
    {:harness-rev (str/trim (:out (git root "rev-parse" "--short" "HEAD")))
     :harness-clean? (str/blank? (str/trim (:out (git root "status" "--porcelain"))))}))

(defn suite-green?
  "Whether the subject's own verify command passes in `root`. The scorer, named
  and recorded, not inferred from the harness's opinion of itself.

  nil, never false, when the command could not be run: `sh` failing to launch
  and a suite failing are different facts, and only the second is a red test."
  [root verify-cmd timeout-ms]
  (try
    (let [args (str/split (str verify-cmd) #"\s+")
          pb (doto (ProcessBuilder. ^java.util.List (vec args))
               (.directory (io/file (str root)))
               (.redirectErrorStream true))
          p (.start pb)
          done? (.waitFor p timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)]
      (if done?
        (zero? (.exitValue p))
        (do (.destroyForcibly p) false)))
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
           raw (str/trim (last (str/split-lines (:out (sh "jolt" "-A:dev" "-Spath")))))]
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
                                {:run {:root (:root in)}
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
    :max-turns :beam-width :token-budget
    :dest             — where to put the worktree
    :http-port        — the child's server port
    :timeout-ms       — hard cap on the child
    :verify-timeout-ms
    :keep?            — leave the worktree behind for inspection

  THE CHILD PROCESS IS THE ABORT UNIT, and it has to be. `beam/run!` called
  directly never registers in `api.control/active` — that registration happens
  in `control/start-run!`'s `on-start`, which the rig does not go through — so
  a rig run cannot be stopped through the control API, and the in-process
  version of this had no way to stop a wedged run at all (observed: a trivial
  task churning past 40 turns on a 6-turn budget through a done / __no_call__ /
  last-call loop, with nothing able to reach it). `timeout-ms` and
  `.destroyForcibly` are that handle now, and a killed child is recorded as
  `:timeout` rather than being lost.

  Returns the row. Never throws for a run that failed: a failure IS a
  measurement, and a sweep that died on arm B's third run would silently
  report arm A as better."
  [{:keys [repo sha arm problem max-turns beam-width token-budget dest
           verify-timeout-ms keep? http-port timeout-ms recordings]
    :or {verify-timeout-ms 600000 http-port 3997 timeout-ms 3600000}}]
  (let [started (System/currentTimeMillis)
        root (worktree! repo sha dest)
        in-f (str root "/.arena-in.edn")
        out-f (str dest "-row.edn")
        fail (fn [status extra]
               (merge {:arm (:name arm) :sha sha :status status
                       :wall-ms (- (System/currentTimeMillis) started)
                       :at (str (java.time.Instant/now))}
                      (harness-revision) extra))]
    (try
      (spit in-f (pr-str {:root root :problem problem
                          :max-turns max-turns :beam-width beam-width
                          :token-budget token-budget :http-port http-port
                          :setup (:setup arm)}))
      (let [log-f (io/file (str dest "-child.log"))
            pb (doto (ProcessBuilder.
                      ^java.util.List
                      ["jolt" "-Scp" (harness-classpath) "-e" (pr-str child-main)])
                 ;; THE LINE THIS NAMESPACE EXISTS FOR.
                 (.directory (io/file root))
                 (.redirectErrorStream true)
                 ;; REDIRECT TO A FILE rather than draining the pipe from a
                 ;; future. Two failure classes go away with it: a child that
                 ;; blocks forever because nobody emptied a full pipe buffer,
                 ;; and a reader future parked on a fiber (jolt's futures are
                 ;; fibers, and parking inside one is delicate here —
                 ;; karamazov-p3jo). The OS writes the file; nothing in this
                 ;; process has to keep up.
                 (.redirectOutput log-f))
            _ (doto (.environment pb)
                (.put "ARENA_IN" in-f)
                (.put "ARENA_OUT" out-f)
                (.put "HARNESS_ROOT" root))
            p (.start pb)
            pid (try (.pid p) (catch Throwable _ nil))
            done? (.waitFor p timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
            ;; TRUST NOTHING. A sweep that believed a run had ended while its
            ;; process was still working ran two arms at once and contaminated
            ;; both — observed, and the reason this check exists. `alive?` is
            ;; the ground truth, and the row carries it so an overlap can
            ;; never again be invisible in the record.
            _ (when-not done? (.destroyForcibly p))
            _ (when (.isAlive p)
                (.destroyForcibly p)
                (.waitFor p 15000 java.util.concurrent.TimeUnit/MILLISECONDS))
            stray? (.isAlive p)
            span {:pid pid :left-alive? stray?
                  :ended-at (str (java.time.Instant/now))}]
        (cond
          (not done?) (fail :timeout (merge span {:timeout-ms timeout-ms}))
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
        (when recordings
          (try
            (let [src (io/file (str root "/.samizdat/samizdat.sqlite3"))]
              (when (.exists src)
                (io/make-parents (io/file (str recordings "/x")))
                (io/copy src (io/file (str recordings "/"
                                          (.getName (io/file dest)) ".sqlite3")))))
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
  child's listener cannot land in a predecessor's TIME_WAIT."
  [{:keys [task-ids arms n out work base-port] :as opts
    :or {base-port 3990 n 3}}]
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
              row (run-once! (merge opts
                                    {:repo (:repo subject) :sha (:sha subject)
                                     :arm arm :problem (:problem task)
                                     :dest dest
                                     :http-port (+ base-port (mod i 90))}
                                    (select-keys task [:max-turns :timeout-ms
                                                       :beam-width :token-budget])))
              row (assoc row :task (:id task) :difficulty (:difficulty task) :n k
                         :budget {:max-turns (or (:max-turns task) (:max-turns opts))
                                  :beam-width (or (:beam-width task) (:beam-width opts))})]
          (append-row! out row)
          (println (format "[%d/%d] %-16s %-9s n=%d  %-11s green=%-5s turns=%-4s tok=%-9s edits=%s  %.1fmin"
                           (inc i) total (name (:id task)) (name (:name arm)) k
                           (str (:status row)) (str (:green? row))
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
  sweep must be killable by killing one process, so it gets one."
  [& [out n]]
  (let [rows (sweep-tasks! {:arms [{:name :baseline}]
                            :n (if n (parse-long (str n)) 3)
                            :out (or out "arena-rows.edn")
                            :work (str (System/getProperty "java.io.tmpdir") "/arena")
                            :base-port 3990
                            :beam-width 1
                            :recordings (str (System/getProperty "java.io.tmpdir")
                                             "/arena-recordings")
                            :verify-timeout-ms 600000})]
    (println "\n=== SUMMARY ===")
    (clojure.pprint/pprint (summarize rows))
    (println "\n=== RECURRING EDITS ===")
    (clojure.pprint/pprint (recurring-edits rows))
    (flush)
    (System/exit 0)))
