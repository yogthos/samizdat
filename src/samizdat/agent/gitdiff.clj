;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.gitdiff
  "The run's own changes, as a diff, for the finalization critic to review.

  A baseline is captured when a run starts — a commit object of the working
  tree at that moment (git stash create), which does not touch the tree — so
  the diff at finalization is exactly what the RUN changed, not whatever was
  already uncommitted. Everything here fails soft: no git, no repo, or any
  error yields an empty diff, and the critic simply reviews completeness only."
  (:require [clojure.string :as str]
            [samizdat.engine.proc :as proc]
            [samizdat.sdiff.core :as sdiff]
            [samizdat.sdiff.group :as group]
            [samizdat.sdiff.names :as names]
            [samizdat.sdiff.text :as sdiff-text]
            [samizdat.lexicon :as lexicon]
            [samizdat.security.secrets :as secrets]))

(defn max-diff-chars
  "How much of the working-tree diff a branch and the judge are shown.
  gates.edn `:context-budget :diff-chars` — how much the model gets to see is
  one table, and this was a constant outside it."
  []
  (lexicon/budget :diff-chars))

(defn- git* [env root args]
  (let [r (apply proc/run {:timeout-ms 15000 :env env}
                 "git" "-C" (str root) args)]
    (when (and (not (:timeout r)) (zero? (or (:exit r) 1)))
      (:out r))))

(defn- git [root & args]
  (git* (secrets/scrubbed-process-env) root args))

(defn- lines [out]
  (some->> out str/split-lines (remove str/blank?) vec))

(defn- whole-tree-commit
  "A commit of the working tree as it stands, untracked files included
  (.gitignore respected), parented on `base`. Built in a scratch index, so
  the tree, the real index and the stash are not touched. nil when git
  refuses any step."
  [root base]
  (let [idx (java.io.File/createTempFile "samizdat-baseline" ".index")
        env (assoc (secrets/scrubbed-process-env) "GIT_INDEX_FILE" (.getPath idx))]
    (try
      (.delete idx)
      (when (and (git* env root ["read-tree" base])
                 (git* env root ["add" "-A"]))
        (when-let [tree (some-> (git* env root ["write-tree"]) str/trim not-empty)]
          (some-> (git root "commit-tree" tree "-p" base "-m" "samizdat baseline")
                  str/trim not-empty)))
      (finally (.delete idx)))))

(defn baseline
  "A ref the run's changes are diffed against: a commit of the working tree
  now, UNTRACKED FILES INCLUDED, so later edits show as the diff and a file
  that was already lying in the tree is not one the run wrote. `git stash
  create` alone captures tracked edits only: run 74ddebb8 answered a
  question, wrote nothing, and passed `done` on the untracked README.md,
  NOTES.md and EXPLAINED.md an earlier run had left (karamazov-9554). Falls
  back to the stash commit, then \"HEAD\". nil when git or the repo is
  unavailable — the critic then reviews completeness only."
  [root]
  (when (and root (proc/available? "git")
             (git root "rev-parse" "--is-inside-work-tree"))
    (let [base (or (some-> (git root "stash" "create") str/trim not-empty) "HEAD")]
      (or (whole-tree-commit root base) base))))

(defn- untracked-split
  "The untracked paths, split by what `baseline` holds of them: :stale — in
  the baseline with the same content, so not the run's; :created — not in
  the baseline at all. One that is in it with different content is neither:
  `git diff` against the baseline already reports it."
  [root baseline]
  (let [untracked (lines (git root "ls-files" "--others" "--exclude-standard"))]
    (if (empty? untracked)
      {:stale #{} :created (or untracked [])}
      (let [held (into {}
                       (keep (fn [l] (let [[meta path] (str/split l #"\t" 2)]
                                       (when path [path (nth (str/split meta #" ") 2 nil)]))))
                       (lines (apply git root "ls-tree" "-r" baseline "--" untracked)))
            now (zipmap untracked (lines (apply git root "hash-object" "--" untracked)))]
        {:stale (set (filter #(and (held %) (= (held %) (now %))) untracked))
         :created (vec (remove held untracked))}))))

(defn- porcelain-counts
  "Split `git status --porcelain` into git's own three kinds.

  Two status columns per line, X and Y: X is the index against HEAD, Y the
  working tree against the index, and `??` is a path git has never seen. A
  path can count on BOTH sides — staged once and edited again since — so
  these are three counts and not a partition, which is also why one \"dirty\"
  number would be the wrong thing to show."
  [out]
  (let [lines (remove str/blank? (str/split-lines (str out)))]
    (reduce (fn [acc line]
              (let [x (get line 0) y (get line 1)]
                (if (and (= \? x) (= \? y))
                  (update acc :untracked inc)
                  (cond-> acc
                    (not (contains? #{\space \?} x)) (update :staged inc)
                    (not (contains? #{\space \?} y)) (update :unstaged inc)))))
            {:staged 0 :unstaged 0 :untracked 0}
            lines)))

(defn snapshot
  "The working tree at a glance: `{:branch :staged :unstaged :untracked
  :last-commit}`. nil when `root` is not a git working tree.

  For a front end to show, not for the model — the TUI holds no filesystem
  knowledge of the project it is watching, so the server reads this and
  serves it. Ported from dirge, whose status line carries `project:branch`
  and whose left panel carries the counts.

  `:branch` is nil on a DETACHED HEAD (rebase, bisect, a CI checkout), where
  there is no branch to name and the counts still matter; `:last-commit` is
  nil in a repo with no commits yet. Fails soft like everything else here."
  [root]
  (when (and root (proc/available? "git")
             (git root "rev-parse" "--is-inside-work-tree"))
    (merge {:branch (some-> (git root "symbolic-ref" "--quiet" "--short" "HEAD")
                            str/trim not-empty)
            :last-commit (some-> (git root "log" "-1" "--format=%s")
                                 str/trim not-empty)}
           (porcelain-counts (git root "status" "--porcelain")))))

;; A baseline is a revision, and it reaches the tool as an argument: one that
;; begins with "-" is an OPTION there (`diff --output=<file>` writes a file).
;; Every baseline these functions take comes from the run's ctx, which a cell
;; can build, so one that looks like an option is not a revision and the call
;; answers as if the repository could not (karamazov-3vu1.9).
(defn rev?
  "Whether `baseline` can be passed on as a revision."
  [baseline]
  (and (some? baseline) (not (str/starts-with? (str baseline) "-"))))

(defn file-at
  "`path`'s content in the `baseline` commit, or nil when it was not there
  (or there is no git). The baseline commit holds untracked files too, so
  this is the file as the run found it — what the exam ratchet compares a
  test file against (karamazov-fgsb)."
  [root baseline path]
  (when (and root (rev? baseline) path)
    ;; `./` makes the path the project root's, as git -C runs from there.
    (git root "show" (str baseline ":./" path))))

(defn changed-files
  "The paths the run changed since `baseline`: tracked edits (git diff
  --name-only) UNION new files (git ls-files --others). The union matters —
  `git diff` is blind to untracked files, so a run that CREATES a namespace (the
  common case, and the one the prompt actively encourages) would otherwise read
  as 'changed nothing' and be judged hollow. nil when git or the repo is
  unavailable — 'cannot tell', distinct from [] which means 'genuinely nothing
  changed'. Ground truth for whether a run that claims done actually produced
  anything."
  [root baseline]
  (when (and root (rev? baseline))
    ;; --relative: paths from the project root, and only the files under
    ;; it. Without it git names paths from the REPOSITORY top, so a project
    ;; in a subdirectory of a repo read its edits as paths it did not have.
    (let [tracked (lines (git root "diff" "--name-only" "--relative" baseline))
          {:keys [stale created]} (untracked-split root baseline)]
      ;; nil only when git could not answer (cannot tell); otherwise the
      ;; union, which may be empty (genuinely nothing changed). An untracked
      ;; file the baseline holds unchanged is in `git diff` — as a deletion,
      ;; since the real index never had it — and it is not the run's.
      (when (or (some? tracked) (seq created))
        (vec (distinct (concat (remove stale tracked) created)))))))

(defn changed-lines
  "How many lines the run has WRITTEN since `baseline`: added plus deleted,
  tracked and untracked together. nil when git cannot answer.

  THE UNTRACKED HALF IS THE POINT, and it is the same trap `changed-files`
  names one function up: `git diff --numstat` is blind to a file that was
  never added, and the prompt actively encourages creating namespaces. A
  budget that counted only tracked edits would read a run that wrote four new
  files as having spent nothing, which is exactly backwards — a new file is
  the largest thing a task can produce.

  ADDED PLUS DELETED, not net. A change that rewrites two hundred lines into
  two hundred different ones is not a small change, and a net count would
  score it zero. What the budget is asking about is how much work is in
  flight, and a rewrite is work.

  A binary file contributes nothing rather than failing the count: numstat
  reports `-` for it, and a budget is about code the model wrote."
  [root baseline]
  (when (and root (rev? baseline))
    (let [num (fn [s] (or (parse-long (str s)) 0))
          {:keys [stale created]} (untracked-split root baseline)
          tracked (some->> (git root "diff" "--numstat" "--relative" baseline)
                           str/split-lines
                           (remove str/blank?)
                           (map #(str/split % #"\t"))
                           (remove (fn [[_ _ path]] (contains? stale path)))
                           (map (fn [[a d & _]] (+ (num a) (num d))))
                           (reduce + 0))
          ;; Only the files the baseline never had: one it had and the run
          ;; edited is already in the numstat above.
          untracked (some->> (seq created)
                             (map (fn [rel]
                                    (try (-> (java.io.File. (str root) (str rel))
                                             slurp str/split-lines count)
                                         (catch Throwable _ 0))))
                             (reduce + 0))]
      (when (or (some? tracked) (some? untracked))
        (+ (or tracked 0) (or untracked 0))))))

(defn diff
  "The unified diff of the run's changes since `baseline`, bounded to keep it
  out of a runaway prompt. Empty string when there is nothing to show."
  ([root baseline] (diff root baseline (max-diff-chars)))
  ;; `cap` explicit: the epic rubric fetches under its own, larger budget
  ;; (gates.edn :rubric :diff-fetch-chars) and cuts per question afterwards
  ;; (karamazov-0way).
  ([root baseline cap] (diff root baseline cap #{}))
  ;; `excluded`: paths left out — a file whose change is whitespace or
  ;; comments only, which review-diff names instead of showing.
  ([root baseline cap excluded]
   (or (when (and root (rev? baseline))
         ;; Without the untracked files the baseline holds unchanged, which
         ;; `git diff` would show as deleted — the run did not delete them.
         ;; --no-ext-diff --no-color: the unified diff whatever the
         ;; operator's git config says. A diff.external (difftastic, here)
         ;; replaced it with a side-by-side the judge's `diff --git`
         ;; chunking could not read.
         (some-> (apply git root "diff" "--no-ext-diff" "--no-color" "--relative" baseline "--" "."
                        (map #(str ":(exclude)" %)
                             (concat (:stale (untracked-split root baseline)) excluded)))
                 (as-> d (if (> (count d) (long cap))
                           (str (subs d 0 (long cap))
                                "\n… (diff truncated at " cap " chars)")
                           d))))
       "")))

(defn- structural-reports
  "sdiff's report for each Clojure file changed since `baseline`, renames
  rolled up across them: {:files :renames}."
  [root baseline]
  (let [paths (filter sdiff/clj? (or (changed-files root baseline) []))
        now (fn [p] (try (slurp (java.io.File. (str root) (str p))) (catch Throwable _ "")))]
    (sdiff/rollup-renames
     (vec (for [p paths]
            (try (sdiff/file-report p (or (file-at root baseline p) "") (now p))
                 ;; A file sdiff cannot parse is left to the line diff.
                 (catch Throwable _ nil)))))))

(defn review-diff
  "What a critic reads of the run's changes (karamazov-0e2c.1): sdiff's
  structural report of the changed Clojure files — each change named by the
  binding, clause or arity it lives in, renames rolled up, and a file whose
  change is whitespace or comments only said to be so — and then the line
  diff of everything else, those cosmetic files left out.

  gates.edn `:review-diff {:structural? true :structural-chars n}`. A table
  without the key, or the switch off, is the plain `diff`: the feature
  absent, not a value made up here."
  [root baseline cap]
  (let [{:keys [structural? structural-chars]}
        (try (lexicon/policy :review-diff) (catch Throwable _ nil))]
    (if-not (and structural? root (rev? baseline))
      (diff root baseline cap)
      (let [{:keys [files renames]} (structural-reports root baseline)
            files (vec (remove nil? files))
            cosmetic (set (keep #(when (#{:whitespace-only :comments-only} (:verdict %)) (:path %))
                                files))
            report (when (seq files)
                     (let [t (str/trim (sdiff-text/report-text {:clj files :renames renames}))
                           ;; Which changed forms call one another, ahead of
                           ;; the per-file report: what to read together
                           ;; (sdiff group, karamazov-0e2c.17).
                           grouping (try (let [g (group/by-calls {:clj files})]
                                           (when (or (seq (:groups g)) (seq (:shared g)))
                                             (group/outline-text g)))
                                         (catch Throwable _ nil))
                           t (if grouping (str (str/trim grouping) "\n\n" t) t)
                           ;; Long paths and namespaces shortened, with a
                           ;; legend, only where it saves more than the legend
                           ;; costs (sdiff names, karamazov-0e2c.16). The
                           ;; critic reads this and calls no tool with it.
                           names (try (names/used (names/table {:clj files :other []}) t)
                                      (catch Throwable _ nil))
                           t (if (or (seq (:paths names)) (seq (:nss names)))
                               (str (names/legend-text names) (names/shorten-text names t))
                               t)
                           n (long (or structural-chars cap))]
                       (if (> (count t) n) (str (subs t 0 n) "\n…") t)))
            lines (diff root baseline cap cosmetic)]
        (str/join "\n\n" (remove str/blank? [report lines]))))))
