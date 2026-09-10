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
            [samizdat.lexicon :as lexicon]
            [samizdat.security.secrets :as secrets]))

(defn max-diff-chars
  "How much of the working-tree diff a branch and the judge are shown.
  gates.edn `:context-budget :diff-chars` — how much the model gets to see is
  one table, and this was a constant outside it."
  []
  (lexicon/budget :diff-chars))

(defn- git [root & args]
  (let [r (apply proc/run {:timeout-ms 15000 :env (secrets/scrubbed-process-env)}
                 "git" "-C" (str root) args)]
    (when (and (not (:timeout r)) (zero? (or (:exit r) 1)))
      (:out r))))

(defn baseline
  "A ref the run's changes are diffed against: a commit object capturing the
  working tree now (so later edits show as the diff), or \"HEAD\" when the tree
  is clean. nil when git or the repo is unavailable — the critic then reviews
  completeness only."
  [root]
  (when (and root (proc/available? "git")
             (git root "rev-parse" "--is-inside-work-tree"))
    (or (some-> (git root "stash" "create") str/trim not-empty)
        "HEAD")))

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
  (when (and root baseline)
    (let [lines (fn [out] (some->> out str/split-lines (remove str/blank?)))
          tracked (lines (git root "diff" "--name-only" baseline))
          untracked (lines (git root "ls-files" "--others" "--exclude-standard"))]
      ;; nil only when BOTH git calls failed (cannot tell); otherwise the union,
      ;; which may be empty (genuinely nothing changed).
      (when (or (some? tracked) (some? untracked))
        (vec (distinct (concat (or tracked []) (or untracked []))))))))

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
  (when (and root baseline)
    (let [num (fn [s] (or (parse-long (str s)) 0))
          tracked (some->> (git root "diff" "--numstat" baseline)
                           str/split-lines
                           (remove str/blank?)
                           (map #(str/split % #"\t"))
                           (map (fn [[a d & _]] (+ (num a) (num d))))
                           (reduce + 0))
          untracked (some->> (git root "ls-files" "--others" "--exclude-standard")
                             str/split-lines
                             (remove str/blank?)
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
  [root baseline]
  (let [cap (max-diff-chars)]
    (or (when (and root baseline)
          (some-> (git root "diff" baseline)
                  (as-> d (if (> (count d) cap)
                            (str (subs d 0 cap)
                                 "\n… (diff truncated at " cap " chars)")
                            d))))
        "")))
