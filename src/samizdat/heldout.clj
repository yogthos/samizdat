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

(ns samizdat.heldout
  "The held-out gate, stage 1 (karamazov-7mo.4 / ylte.4): replay the project's
  frozen battery against its userspace AS IT IS and AS A CANDIDATE EDIT WOULD
  MAKE IT, and refuse the edit when a target that passed before fails after.
  MECHANISM ONLY — whether it runs, and its budgets, are gates.edn :heldout;
  its words are prompts/heldout-*.md.

  WHY A CHILD PROCESS PER CASE. A replay has to see the candidate as the
  project's userspace, everywhere a run reads it: the cells, the manifests,
  the policy tables, from every branch fiber. A dynamic binding does not
  cross a fiber, and a global override would leak the candidate into the live
  run that is proposing it. So the candidate is MATERIALIZED: the recorded
  run's fixture (the git baseline it started from) is unpacked into a temp
  directory, the project's .samizdat/ is copied beside it with the candidate
  file written in, and a fresh process (samizdat.heldout.child) starts the
  harness on that directory and replays the case into an in-memory database.
  Nothing it does touches the project or the live run.

  WHAT IS COMPARED. Per case, per target (battery/check), the baseline's
  verdict against the candidate's. The rule is RRSI's non-compensatory one:
  no target that passed before fails after — fixing one thing does not buy
  breaking another — and the two sides must cover the same targets
  (battery/accept?). Ties are accepted, deliberately: see samizdat.battery.
  A case that cannot run at baseline is not this edit's doing and is set
  aside, named; a case that ran at baseline and cannot run on the candidate
  is a regression.

  THE BATTERY MAY GROW AND MAY NOT BE WEAKENED. The authority is the
  battery_cases table, not the files: a case file not yet stored is added, a
  stored case whose file was edited runs AS STORED and the edit is named, and
  one whose file was deleted still runs. The agent may add a case from an
  observed failure (the `battery` tool); nothing here lets it remove one.

  WHAT IT CANNOT JUDGE. The replies are fixed, so a prompt edit cannot change
  what the model says under replay; such an edit is not sent here (the prompt
  tool says so). Replay measures what the harness DOES with a given
  conversation — the arena's live arms (stage 2) measure the rest."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [samizdat.agent.gates :as gates]
            [samizdat.battery :as battery]
            [samizdat.engine.proc :as proc]
            [samizdat.prompt :as prompt]
            [samizdat.security.secrets :as secrets]
            [samizdat.store.db :as db]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.userspace :as userspace]
            [samizdat.util :as util]))

(defn- policy [] (gates/threshold :heldout))

;;; ------------------------------------------------------------- the battery

(defn- sh-run
  [timeout-ms cmd]
  (proc/run {:timeout-ms timeout-ms :env (secrets/scrubbed-process-env)} "sh" "-c" cmd))

(defn case-dir
  "Where this project's case files live, or nil unbound."
  []
  (some-> (userspace/project-dir) (str "/" (:dir (policy)))))

(defn- case-id
  "A case's id: its own :id, else its file's name without .edn."
  [c]
  (str (or (:id c)
           (some-> (:path c) io/file .getName (str/replace #"\.edn$" "")))))

(defn- stored-form
  "What the table keeps of a case: the case as data, without where it was
  read from (which the row already says)."
  [c]
  (dissoc c :path :subject))

(defn add-case!
  "Store case `c` under `subject`, unless a case with its id is stored already
  — an id is written once, which is what makes the battery something that can
  grow and cannot be rewritten. Returns true when it was added."
  [conn subject c]
  (let [id (case-id c)]
    (when-not (seq (db/fetch conn ["SELECT id FROM battery_cases WHERE id = ?" id]))
      (db/execute! conn ["INSERT INTO battery_cases (id, subject, body, added_at)
                          VALUES (?, ?, ?, ?)"
                         id (str subject) (pr-str (stored-form (assoc c :id id))) (db/now)])
      true)))

(defn cases
  "The battery: every stored case, after adding each case file not yet stored.

  Returns {:cases [case …] :altered [path …]}. A file whose content differs
  from its stored case is not what runs — the stored case is — and its path is
  under :altered so whoever reads the verdict can see the attempt. A stored
  case whose file is gone runs all the same."
  [conn]
  (let [files (battery/load-cases (case-dir))
        by-id (fn [] (into {} (map (juxt :id identity))
                           (db/fetch conn ["SELECT id, subject, body FROM battery_cases ORDER BY id"])))
        _ (doseq [c files :when (not (contains? (by-id) (case-id c)))]
            (add-case! conn (:subject c) c))
        stored (by-id)
        altered (vec (for [c files
                           :let [row (get stored (case-id c))]
                           :when (and row (not= (edn/read-string (:body row))
                                                (stored-form (assoc c :id (case-id c)))))]
                       (:path c)))]
    {:cases (vec (for [[id row] (sort-by key stored)]
                   (assoc (edn/read-string (:body row)) :id id :subject (:subject row))))
     :altered altered}))

;;; ------------------------------------------------------------- adding one

(defn draft!
  "A case from finished run `run-id` of this project: battery/draft-case's
  replay and expectations, plus what the replay needs to reproduce it — the
  fixture (the run's :git-baseline, pinned under refs/samizdat/battery/ so
  git's collector keeps it), the manifest that drove it, the model that
  answered. Written to the battery directory and stored, and returned as
  {:id :path :targets}. Throws when the run has no baseline to replay from."
  [conn run-id]
  (let [c (battery/draft-case conn run-id)
        sha (some :ref (journal/notes conn run-id :git-baseline))
        loop-nm (some :name (journal/notes conn run-id :loop-workflow))
        ;; The ids the run's tasks were given, in the order it made them: a
        ;; recorded reply that names one (`task show sz-ffd83f`) only means
        ;; the same thing under replay if the replay hands out the same ids.
        task-ids (mapv :id (db/fetch conn ["SELECT id FROM tasks WHERE run_id = ?
                                            ORDER BY created_at, rowid" run-id]))
        root (userspace/project-root)]
    (when (str/blank? (str sha))
      (throw (ex-info (str "run " run-id " recorded no git baseline to replay from")
                      {:error :no-baseline :run-id run-id})))
    (sh-run (:stage-timeout-ms (policy))
            (str "git -C " (util/sh-quote root) " update-ref "
                 (util/sh-quote (str "refs/samizdat/battery/" (:id c))) " " (util/sh-quote sha)))
    (let [c (cond-> (assoc c :fixture {:sha sha}
                           :model (:model (runs/get-run conn run-id))
                           :task-ids task-ids)
              loop-nm (assoc :loop loop-nm))
          subject (.getName (io/file root))
          f (io/file (case-dir) subject (str (:id c) ".edn"))]
      (.mkdirs (.getParentFile f))
      (spit f (pr-str c))
      (add-case! conn subject c)
      {:id (:id c) :path (str f) :targets (count (:expect c))})))

;;; ------------------------------------------------------------- staging

(defn- delete-tree!
  [f]
  (let [f (io/file f)]
    (when (.isDirectory f)
      (doseq [c (.listFiles f)] (delete-tree! c)))
    (.delete f)))

(defn- relative
  [root f]
  (let [r (str (.getCanonicalPath (io/file root)) "/")
        p (.getCanonicalPath (io/file f))]
    (when (str/starts-with? p r) (subs p (count r)))))

(defn- userspace-files
  "The project's .samizdat files a replay reads, as [relative-path file]:
  everything but its databases and the battery itself."
  []
  (let [dir (userspace/project-dir)
        battery (:dir (policy))]
    (vec (for [f (file-seq (io/file dir))
               :when (.isFile f)
               :let [rel (relative dir f)]
               :when (and rel
                          (not (re-find (re-pattern (:skip (policy))) rel))
                          (not (str/starts-with? rel (str battery "/"))))]
           [rel f]))))

(defn candidate-path
  "The path, relative to .samizdat/, that `kind`/`name` is served from, or
  nil when the project's map has no such role."
  [kind name]
  (some->> (userspace/project-path kind name) (relative (userspace/project-dir))))

(defn stage!
  "Lay case `c` out under `dest` as a project: its fixture (the tree the
  recorded run started from, out of the project's git) with this project's
  .samizdat/ over it, and `candidate` ({:kind :name :text}) written in when
  given. Returns nil, or {:error …} saying what could not be staged."
  [c dest candidate]
  (let [root (userspace/project-root)
        sha (get-in c [:fixture :sha])]
    (.mkdirs (io/file dest))
    (cond
      (str/blank? (str sha))
      {:error :no-fixture}

      :else
      (let [r (sh-run (:stage-timeout-ms (policy))
                      (str "git -C " (util/sh-quote root) " archive " (util/sh-quote sha)
                           " | tar -x -C " (util/sh-quote dest)))]
        (if (or (:timeout r) (not (zero? (long (or (:exit r) 1)))))
          {:error :fixture :detail (str/trim (str (:err r)))}
          (let [us (io/file dest ".samizdat")]
            ;; The fixture's own .samizdat, if it committed one, is not this
            ;; project's workflow: the userspace under test is.
            (delete-tree! us)
            ;; A REPOSITORY, as the recorded run had: the write ledger, the
            ;; critic's diff and the run's own git calls all read the tree
            ;; through git, and an unpacked archive has none — so a replay's
            ;; edits went unseen and `done` was withheld on files it had
            ;; written. Committed before the userspace goes in, which is kept
            ;; out of it as the project keeps it out of its own.
            (sh-run (:stage-timeout-ms (policy))
                    (str "cd " (util/sh-quote dest)
                         " && git init -q && git add -A"
                         " && git -c user.name=heldout -c user.email=heldout@localhost"
                         " commit -q --no-verify -m fixture"
                         " && echo .samizdat/ >> .git/info/exclude"))
            (doseq [[rel f] (userspace-files)
                    :let [to (io/file us rel)]]
              (.mkdirs (.getParentFile to))
              (io/copy f to))
            (when-let [rel (and candidate (candidate-path (:kind candidate) (:name candidate)))]
              (let [to (io/file us rel)]
                (.mkdirs (.getParentFile to))
                (spit to (:text candidate))))
            nil))))))

;;; ------------------------------------------------------------- one replay

(defn- sh-lines [out] (some->> out str/split-lines (map str/trim) (remove str/blank?)))

(def ^:private classpath
  (memoize
   (fn [dir]
     ;; The last line that is a classpath: dependency chatter shares the
     ;; stream on a cold cache. Relative roots are made absolute, since the
     ;; child runs in the fixture and ./src would resolve against it.
     (let [r (sh-run (:stage-timeout-ms (policy))
                     (str "cd " (util/sh-quote dir) " && " (util/sh-quote (:command (policy))) " -Spath"))
           raw (->> (sh-lines (:out r))
                    (filter #(and (str/includes? % ":")
                                  (or (str/starts-with? % "/") (str/starts-with? % "./"))))
                    last)]
       (when raw
         (->> (str/split raw #":")
              (map (fn [e] (if (str/starts-with? e "./") (str dir (subs e 1)) e)))
              (str/join ":")))))))

(defn harness-dir
  "The samizdat checkout a child is started from: policy's :harness-dir, else
  this process's working directory."
  []
  (or (:harness-dir (policy)) (System/getProperty "user.dir")))

(defn harness-revision
  "The commit of the harness a verdict was measured on, or nil."
  []
  (some-> (sh-run 15000 (str "git -C " (util/sh-quote (harness-dir)) " rev-parse HEAD"))
          :out str/trim not-empty))

(defn- free-port []
  (let [s (java.net.ServerSocket. 0)]
    (try (.getLocalPort s) (finally (.close s)))))

(defn- temp-dir []
  (let [f (java.io.File/createTempFile "samizdat-heldout" "")]
    (.delete f)
    (.mkdirs f)
    f))

(defn run-case!
  "Replay case `c` against this project's userspace with `candidate` written
  in (nil for the userspace as it is), in a child process. Returns the
  child's {:run-id :status :result} (:result is battery/check's) or
  {:error …}. The staging directory is removed either way."
  [c candidate]
  (let [tmp (temp-dir)
        root (str tmp "/root")
        in-f (str tmp "/in.edn")
        out-f (str tmp "/out.edn")
        log-f (str tmp "/child.log")]
    (try
      (or (stage! c root candidate)
          (if-let [cp (classpath (harness-dir))]
            (let [_ (spit in-f (pr-str {:case c :root root :http-port (free-port)
                                        :slack (:turns-slack (policy))
                                        :trail-turns (:trail-turns (policy))
                                        :trail-chars (:trail-chars (policy))}))
                  form (pr-str (list 'do (list 'require (list 'quote 'samizdat.heldout.child))
                                     (list 'samizdat.heldout.child/-main)))
                  r (proc/run {:timeout-ms (:case-timeout-ms (policy))
                               :env (assoc (secrets/scrubbed-process-env)
                                           "HELDOUT_IN" in-f
                                           "HELDOUT_OUT" out-f
                                           "HARNESS_ROOT" root)}
                              "sh" "-c" (str "cd " (util/sh-quote root)
                                             " && exec " (util/sh-quote (:command (policy)))
                                             " -Scp " (util/sh-quote cp)
                                             " -e " (util/sh-quote form)
                                             " > " (util/sh-quote log-f) " 2>&1"))]
              (cond
                (:timeout r) {:error :timeout :ms (:ms r)}
                (.exists (io/file out-f)) (edn/read-string (slurp out-f))
                :else {:error :no-result
                       :detail (when (.exists (io/file log-f))
                                 (->> (slurp log-f) str/split-lines (take-last 12)
                                      (str/join "\n")))}))
            {:error :no-classpath :detail (harness-dir)}))
      (catch Throwable e {:error :rig :detail (or (ex-message e) (str e))})
      (finally (delete-tree! tmp)))))

;;; ------------------------------------------------------------- the gate

(defn- fingerprint
  "A key for the userspace a measurement was taken on: the project, and every
  file's path and CONTENT with `candidate` in place of its file. By content rather than
  mtime, so the candidate's own measurement is the baseline of the state a
  commit of it produces — one replay per edit, not two."
  [candidate]
  (let [rel (when candidate (candidate-path (:kind candidate) (:name candidate)))
        files (into {} (map (fn [[r f]] [r (slurp f)])) (userspace-files))
        files (cond-> files rel (assoc rel (:text candidate)))]
    (hash [(userspace/project-root) (vec (sort-by key files))])))

(defonce ^:private measured (atom {}))

(defn- measure
  "{case-id result} for `cases` on the userspace with `candidate`, from the
  cache when this exact userspace was measured before — unless `fresh?`."
  [cases candidate run-case & [fresh?]]
  (let [fp (fingerprint candidate)]
    (into {}
          (for [c cases]
            (let [k [fp (:id c) (hash c)]]
              [(:id c)
               (or (when-not fresh? (get @measured k))
                   (let [r (run-case c candidate)]
                     ;; Only a result is kept. A failure may be the machine
                     ;; (a killed child, a cold cache), and a kept failure
                     ;; would stand for this userspace until it changed.
                     (when-not (:error r)
                       (swap! measured assoc k r))
                     r))])))))

(defn- named
  "`result`'s targets, each named with its case so two cases' targets of the
  same name stay two targets."
  [id result]
  (update result :targets
          (fn [ts] (mapv #(update % :name (fn [n] (str id " — " n))) ts))))

(defn compare-results
  "The verdict on `before` and `after` ({case-id child-result}): {:ok? …}.

  Per case: one that did not run at baseline is set aside under :unmeasured;
  one that ran at baseline and did not run on the candidate is a regression
  by itself; otherwise its targets are compared — every target that passed
  before must pass after, over the same set of targets."
  [cases before after]
  (let [per (for [c cases
                  :let [id (:id c)
                        b (get before id)
                        a (get after id)]]
              (cond
                (:error b) {:id id :unmeasured (:error b) :detail (:detail b)}
                (:error a) {:id id :broke [(str id " — no longer runs (" (name (:error a)) ")")]}
                :else
                (let [b (named id (:result b))
                      a (named id (:result a))]
                  {:id id
                   :before b :after a
                   :broke (vec (concat (battery/regressions b a)
                                       (when-not (battery/accept? b a)
                                         (when (empty? (battery/regressions b a))
                                           [(str id " — measured different targets")]))))})))
        broke (vec (mapcat :broke per))]
    {:ok? (empty? broke)
     :regressions broke
     :cases (count cases)
     :unmeasured (vec (for [p per :when (:unmeasured p)] (:id p)))
     ;; Why each of those did not run, so a battery that measured nothing
     ;; says what to fix rather than only that it measured nothing.
     :unmeasured-why (into {} (for [p per :when (:unmeasured p)]
                                [(:id p) (str (name (:unmeasured p))
                                              (when (:detail p) (str ": " (:detail p))))]))
     :targets (vec (for [p per :when (:after p)
                         t (:targets (:after p))
                         :let [was (some #(when (= (:name %) (:name t)) %) (:targets (:before p)))]]
                     {:case (:id p) :name (:name t)
                      :before (boolean (:ok? was)) :after (boolean (:ok? t))}))
     :passed-before (reduce + 0 (keep #(get-in % [:before :passed]) per))
     :passed-after (reduce + 0 (keep #(get-in % [:after :passed]) per))
     :total (reduce + 0 (keep #(get-in % [:after :total]) per))}))

(defn record!
  "One heldout_checks row per target measured, with what the number was
  measured ON beside it: the case's fixture, the harness revision, the
  recorded model, the scorer. A score without those is only a number."
  [conn candidate cases verdict]
  (let [rev (harness-revision)
        by-id (into {} (map (juxt :id identity)) cases)]
    (doseq [{target :name :keys [case before after]} (:targets verdict)
            :let [c (get by-id case)]]
      (db/execute! conn ["INSERT INTO heldout_checks
                          (edit_kind, edit_name, case_id, target, ok_before, ok_after,
                           accepted, fixture, revision, model, scorer, created_at)
                          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                         (some-> (:kind candidate) name) (str (:name candidate))
                         case target (if before 1 0) (if after 1 0) (if (:ok? verdict) 1 0)
                         (str (get-in c [:fixture :sha])) (str rev) (str (:model c))
                         "battery/check" (db/now)]))))

(defn checks
  "The recorded per-target measurements for edits of `kind`/`name`, newest
  first."
  [conn kind name]
  (db/fetch conn ["SELECT * FROM heldout_checks WHERE edit_kind = ? AND edit_name = ?
                    ORDER BY id DESC"
                  (clojure.core/name kind) (str name)]))

(defn gate!
  "Measure `candidate` ({:kind :name :text}) against the battery. nil when the
  gate does not apply — disabled in policy, no project files, or no cases (a
  project with no battery tunes itself as before; RFC-014's opt-in). Else
  {:ok? …} per compare-results, plus :altered (case files edited against the
  stored battery), recorded per target.

  `opts` may carry :run-case (fn [case candidate] -> child result), which a
  test injects in place of the child process."
  ([conn candidate] (gate! conn candidate nil))
  ([conn candidate {:keys [run-case]}]
   (let [p (policy)]
     (when (and (:enabled? p) (userspace/files?) conn)
       (let [{:keys [cases altered]} (cases conn)
             cases (vec (take (:max-cases p) cases))]
         (when (seq cases)
           (let [run-case (or run-case run-case!)
                 before (measure cases nil run-case)
                 after (measure cases candidate run-case)
                 first-read (compare-results cases before after)
                 ;; A REPLAY IS NOT EXACT. A recording that forked replays
                 ;; its branches in whatever order their turns land, and the
                 ;; gates that read time follow, so one measurement can show
                 ;; a flip that is noise (measured on endless-flight: the
                 ;; same baseline read 11/14 and 13/14 minutes apart). A
                 ;; refusal is therefore confirmed: both sides measured once
                 ;; more, fresh, and only a flip seen both times refuses.
                 ;; Accepting costs nothing extra.
                 verdict (if (:ok? first-read)
                           first-read
                           (let [again (compare-results cases
                                                        (measure cases nil run-case true)
                                                        (measure cases candidate run-case true))
                                 seen (set (:regressions again))
                                 held (filterv seen (:regressions first-read))]
                             (assoc first-read
                                    :ok? (empty? held)
                                    :regressions held
                                    :unconfirmed (filterv (complement seen)
                                                          (:regressions first-read)))))
                 verdict (assoc verdict :altered altered)]
             (try (record! conn candidate cases verdict)
                  (catch Throwable e (log/warn "heldout: recording the verdict failed:" (ex-message e))))
             verdict)))))))

(defn refusal
  "What an edit's author reads when the battery refused it: the targets that
  broke by name, so the next attempt does not re-derive the same edit."
  [verdict]
  (prompt/render "heldout-refused"
                 {:broke (str/join "\n" (map #(str "  " %) (:regressions verdict)))
                  :cases (:cases verdict)
                  :cases-s (if (= 1 (:cases verdict)) "" "s")
                  :passed-before (:passed-before verdict)
                  :passed-after (:passed-after verdict)
                  :total (:total verdict)
                  :counted (pos? (long (or (:total verdict) 0)))
                  :unmeasured (str/join ", " (:unmeasured verdict))
                  :altered (str/join ", " (:altered verdict))}))

(defn check-edit
  "The refusal an edit's author reads, or nil when the edit may go live — the
  battery passed it, or there is no battery to consult. The one call a tool
  makes before it saves a cell, manifest or policy table."
  ([conn candidate] (check-edit conn candidate nil))
  ([conn candidate opts]
   (let [v (gate! conn candidate opts)]
     (when (and v (seq (:unmeasured v)) (= (count (:unmeasured v)) (:cases v)))
       (log/warn "heldout: no case ran at baseline, so" (:kind candidate) (:name candidate)
                 "went unmeasured:" (pr-str (:unmeasured-why v))))
     (when (and v (not (:ok? v)))
       (refusal v)))))

;;; ------------------------------------------------------------- off the turn

;; ONE LANE. Edits are measured and committed in the order they were made, so
;; each one's baseline is the userspace every earlier one left.
(defonce ^:private lane (Object.))

;; The verdicts not yet reached, so a caller that must not leave one behind —
;; a test, a shutdown — can wait for them.
(defonce ^:private outstanding (atom #{}))

(defn await-pending!
  "Wait for every deferred edit to reach its verdict."
  []
  (doseq [f @outstanding] (deref f)))

(defn defer!
  "Take `candidate` ({:kind :name :text}) off the turn: when the battery
  applies, measure it on a background thread and then either run `commit!` or
  refuse, and tell the branch that made the edit either way. Returns
  {:pending true :done <future>}, or nil when the gate does not apply —
  disabled, no project files, no cases — and the caller commits as before.

  WHY NOT IN THE TURN. A replay costs minutes, a refusal is confirmed by a
  second one, and the turn deadline is 900 s: proving this gate on
  endless-flight, one case's refusal took 733 s (karamazov-nha1). An edit
  checked inside the tool call would be cancelled mid-gate once a battery had
  a few cases. The edit that is pending is not live; what runs is what ran
  before, until the verdict.

  `opts`: :commit! (no-arg fn, returns the saved version, throws to refuse),
  :run-id and :branch-id (who is told, via a `message` for the branch's next
  turn, and where the verdict is journalled), :run-case (a test's stand-in for
  the child)."
  [conn candidate {:keys [commit! run-id branch-id run-case]}]
  (let [p (policy)]
    (when (and (:enabled? p) (userspace/files?) conn
               (seq (:cases (cases conn))))
      (let [what {:kind (some-> (:kind candidate) name) :name (str (:name candidate))}
            note! (fn [kind data]
                    (when run-id
                      (try (journal/note! conn run-id kind {:branch-id branch-id :data (merge what data)})
                           (catch Throwable _ nil))))
            tell! (fn [vars]
                    (when (and run-id branch-id)
                      (try (interventions/submit! conn run-id
                                                  {:branch-id branch-id :kind "message"
                                                   :issued-by "heldout"
                                                   :payload (prompt/render "heldout-decided"
                                                                           (merge what vars))})
                           (catch Throwable e
                             (log/warn "heldout: telling" branch-id "failed:" (ex-message e))))))]
        (note! :heldout-pending {})
        (let [done (promise)
              fut
         (future
           (try
           (locking lane
             (try
               (let [v (gate! conn candidate {:run-case run-case})]
                 (if (or (nil? v) (:ok? v))
                   (let [saved (try {:version (commit!)}
                                    (catch Throwable e {:error (or (ex-message e) (str e))}))]
                     (note! :heldout-verdict (merge {:accepted (nil? (:error saved))}
                                                    saved
                                                    (select-keys v [:passed-before :passed-after :total])))
                     (tell! (if (:error saved)
                              {:save-failed true :reason (:error saved)}
                              {:accepted true :version (:version saved)
                               :passed (:passed-after v) :total (:total v)})))
                   (let [r (refusal v)]
                     (note! :heldout-verdict {:accepted false :regressions (:regressions v)})
                     (tell! {:refused true :refusal r}))))
               (catch Throwable e
                 (log/warn "heldout: measuring" (:name what) "failed:" (ex-message e))
                 (note! :heldout-verdict {:accepted false :error (ex-message e)})
                 (tell! {:save-failed true :reason (ex-message e)}))))
             (finally (swap! outstanding disj @done))))]
          (swap! outstanding conj fut)
          (deliver done fut)
          {:pending true :done fut})))))

;;; ------------------------------------------------------------- stage 2: live arms

(defn- median [xs]
  (let [v (vec (sort (remove nil? xs)))]
    (when (seq v) (nth v (quot (count v) 2)))))

(defn- sd [xs]
  (let [v (vec (remove nil? xs)) n (count v)]
    (when (> n 1)
      (let [m (/ (reduce + v) n)]
        (Math/sqrt (/ (reduce + (map #(let [d (- % m)] (* d d)) v)) (dec n)))))))

(defn- criteria-held
  "Criterion names that passed in every row of `rows` that decided them."
  [rows]
  (let [decided (for [r rows, res (get-in r [:acceptance :results])
                      :when (some? (:passed? res))]
                  [(:name res) (:passed? res)])]
    (set (for [[nm vs] (group-by first decided)
               :when (every? second vs)]
           nm))))

(defn- criteria-failed [rows]
  (set (for [r rows, res (get-in r [:acceptance :results])
             :when (false? (:passed? res))]
         (:name res))))

(defn live-verdict
  "Stage 2 of the held-out gate: the arena's live arms read against each other
  (karamazov-7mo.4's design, from RRSI 2609.24972 and GEPA). Replay cannot
  judge what a changed prompt makes the model SAY; interleaved live runs of a
  baseline arm and a candidate arm on the same pinned tree can.

  `rows` are arena rows ({:arm :task :fitness :tokens :green? :acceptance}).
  Per task, RRSI's non-compensatory order, each a refusal on its own:

    1. floor — median fitness(candidate) >= S* - delta. S* is the best any
       kept version reached on the task (`best`, a {task score} map, and the
       baseline's own median if higher); delta is z standard deviations of the
       baseline's fitness on that task, never under min-band.
    2. cost — a gain beyond delta must pay for its tokens: relative token
       change dC <= base + per-fitness * dS. Inside the band only a cost cut
       (dC < -base) or a change declared `structural?` (a new component) is
       admissible — a neutral edit that adds tokens is refused.
    3. guards — no acceptance criterion that passed in every baseline row
       fails in a candidate row, and no candidate row loses the suite where
       every baseline row had it green.

  Returns {:accept? bool :tasks {task {:accept? :refused [kw …] :dS :dC
  :delta :floor}}}. A task either arm has no rows for is not judged."
  [rows {:keys [baseline candidate best structural? cost-rule noise]}]
  (let [{:keys [z min-band]} noise
        {:keys [base per-fitness]} cost-rule
        tasks (into {}
                    (for [[task rs] (group-by :task rows)
                          :let [b (filter #(= baseline (:arm %)) rs)
                                c (filter #(= candidate (:arm %)) rs)]
                          :when (and (seq b) (seq c))]
                      (let [sb (median (map :fitness b))
                            sc (median (map :fitness c))
                            tb (median (map :tokens b))
                            tc (median (map :tokens c))
                            delta (max (or min-band 0.0)
                                       (* (or z 0.0) (or (sd (map :fitness b)) 0.0)))
                            s* (max (or sb 0.0) (or (get best task) Double/NEGATIVE_INFINITY))
                            ds (when (and sb sc) (- sc sb))
                            dc (when (and tb tc (pos? tb)) (/ (- tc tb) (double tb)))
                            floor? (and sc (< sc (- s* delta)))
                            cost? (and ds dc base
                                       (if (> ds delta)
                                         (> dc (+ base (* (or per-fitness 0.0) ds)))
                                         (and (<= (Math/abs (double ds)) delta)
                                              (not structural?)
                                              (>= dc (- base)))))
                            lost-criteria (seq (filter (criteria-failed c) (criteria-held b)))
                            lost-suite? (and (every? :green? b) (some #(false? (:green? %)) c))
                            refused (cond-> []
                                      floor? (conj :floor)
                                      cost? (conj :cost)
                                      lost-criteria (conj :criterion)
                                      lost-suite? (conj :suite))]
                        [task {:accept? (empty? refused) :refused refused
                               :dS ds :dC dc :delta delta :floor (- s* delta)
                               :lost-criteria (vec (sort lost-criteria))}])))]
    {:accept? (boolean (and (seq tasks) (every? :accept? (vals tasks))))
     :tasks tasks}))
