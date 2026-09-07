;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.board-test
  "The BOARD loop: one owner per task, worked to a finish, then a critic reads
  the diff that task produced.

  This replaces the fan-out as the default way several agents share a feature.
  The fan-out split a problem into parts nobody owned on a board and ran them
  simultaneously in one tree; what it produced was four workers negotiating
  over the same files and a planner's musing mistaken for a task list. The
  board keeps the collaboration and drops the simultaneity: work is a queue of
  owned tasks, an owner splits its task when it is really several, and nothing
  closes until a critic has read what it changed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.judge :as judge]
            [samizdat.cells :as cells]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.store.runs :as runs]
            [samizdat.store.tasks :as tasks]
            [samizdat.workflow :as workflow]))

(defn- ships-its-task
  "An owner that ships immediately, with an answer that engages its own task —
  the done-gate requires the answer to cover the words of the problem, so a
  fixed string is refused (which is the gate working)."
  [_ _ messages & _]
  (let [content (str/join " " (map :content messages))
        prob (str/trim (or (second (re-find #"## Problem\s+(.+)" content)) "task"))]
    {:content (str "```tool-call\n{\"name\":\"done\",\"args\":{\"answer\":\"handled "
                   prob "\"}}\n```")
     :finish-reason "stop"}))

(defn- judge-call?
  "Whether this provider call is the critic's judge rather than an owner's
  turn — the judge is a single user message carrying the judge preamble."
  [messages]
  (str/includes? (str/join " " (map :content messages))
                 "## The answer it wants to ship"))

(defn- run-board
  [conn opts]
  (workflow/run! (merge {:conn conn
                         :config {:run {:loop "board"}}
                         :llm-adapter :a :llm-config {:max-tokens 16384}
                         :problem "the feature" :max-turns 6}
                        opts)))

;; --- the board is the unit of work ------------------------------------------

(deftest a-run-with-no-board-opens-one-task-for-its-problem
  ;; No planner call, no invented parts: the run's problem IS the first task,
  ;; and its owner splits it if it turns out to be several (the claim prompt
  ;; asks). karamazov-6a3 was the planner's reasoning preamble becoming the
  ;; task list; the board removes that step from the critical path entirely.
  (with-redefs [llm/chat ships-its-task]
    (let [conn (db/open! ":memory:")]
      (run-board conn {})
      (let [rows (db/fetch conn ["SELECT * FROM tasks ORDER BY created_at, id"])]
        (is (= 1 (count rows)) "one task, made from the problem")
        (is (str/includes? (:title (first rows)) "the feature"))
        (is (= "done" (:status (first rows)))
            "and it closed when its owner finished and the critic passed")))))

(deftest every-task-on-the-board-gets-worked-by-one-owner
  (with-redefs [llm/chat ships-its-task]
    (let [conn (db/open! ":memory:")
          a (tasks/create! conn {:title "storage"})
          b (tasks/create! conn {:title "handlers"})]
      (run-board conn {})
      (testing "each task was worked on its own branch, and closed"
        (is (= "done" (:status (tasks/get-task conn a))))
        (is (= "done" (:status (tasks/get-task conn b))))
        (let [branches (map :branch_id (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"]))]
          (is (= 2 (count (remove nil? branches)))
              "two tasks, two owners — never two owners on one task"))
        (let [rows (db/fetch conn ["SELECT prompt_suffix FROM branches
                                    WHERE id IN (SELECT DISTINCT branch_id FROM turns)"])]
          (is (= 2 (count rows)) "the two owners; the driver's own B1 took no turn")
          (is (every? #(str/includes? (str (:prompt_suffix %))
                                      (workflow/prompt-text "roles/implementor"))
                      rows)
              "each owner's row records the owner prompt it opened on (v24)"))))))

(deftest a-task-with-open-children-is-not-workable-until-they-are-done
  ;; The owner of a composite task splits it; the parent is then a container,
  ;; and picking it up would mean working a task whose parts are the real work.
  (with-redefs [llm/chat ships-its-task]
    (let [conn (db/open! ":memory:")
          parent (tasks/create! conn {:title "the whole feature"})
          child (tasks/create! conn {:title "one part" :parent-id parent})]
      (run-board conn {})
      (is (= "done" (:status (tasks/get-task conn child))))
      (testing "the parent closes only after its children"
        (is (= "done" (:status (tasks/get-task conn parent))))
        (let [worked (set (map :branch_id (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"])))]
          (is (= 1 (count worked))
              "only the leaf was ever worked — the parent was never claimed"))))))

;; --- the critic reads what the task changed ---------------------------------

(deftest the-critic-sees-the-diff-of-this-task-not-the-whole-run
  ;; The point of reviewing per task: an owner is answerable for what IT
  ;; changed. Reviewing the run's whole diff makes the last task's review a
  ;; review of everything, and every earlier defect somebody else's problem.
  (let [seen (atom [])]
    (with-redefs [llm/chat ships-its-task
                  gitdiff/diff (fn [_ baseline] (swap! seen conj baseline) "")
                  ;; a distinct baseline per claim, without needing a git repo
                  gitdiff/baseline (let [n (atom 0)]
                                     (fn [_] (str "base-" (swap! n inc))))]
      (let [conn (db/open! ":memory:")]
        (tasks/create! conn {:title "first"})
        (tasks/create! conn {:title "second"})
        (run-board conn {})
        (is (= 2 (count @seen)) "one diff per task")
        (is (= 2 (count (distinct @seen)))
            "each against its OWN baseline, taken when that task was claimed")))))

(deftest a-critic-that-finds-defects-sends-the-task-back-to-its-owner
  (let [attempts (atom 0)
        critic-says (atom "REVISE\nthe handler ignores its error branch")]
    (with-redefs [llm/chat
                  (fn [a c messages & rest]
                    (if (judge-call? messages)
                      (let [reply @critic-says]
                        (reset! critic-says "COMPLETE")
                        {:content reply :finish-reason "stop"})
                      (do (swap! attempts inc)
                          (apply ships-its-task a c messages rest))))]
      (let [conn (db/open! ":memory:")
            id (tasks/create! conn {:title "the handler"})]
        (run-board conn {})
        (is (= 2 @attempts) "the owner worked it again with the findings")
        (is (= "done" (:status (tasks/get-task conn id)))
            "and it closed once the critic was satisfied")))))

(deftest the-board-works-its-own-tree-and-the-backlog-not-role-housekeeping
  ;; A role branch (supervisor, reviewer) creates run-scoped tasks for its own
  ;; bookkeeping — the task tool practically requires it. Those are not feature
  ;; work: the board works the tree rooted at tasks IT opened (type "feature")
  ;; and the unclaimed human backlog, and nothing else.
  (cells/load-cells!)
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        root (tasks/create! conn {:title "the feature" :type "feature" :run-id rid})
        child (tasks/create! conn {:title "a part" :parent-id root :run-id rid})
        _meta (tasks/create! conn {:title "Diagnose harness bug" :run-id rid})
        backlog (tasks/create! conn {:title "human-added work"})]
    (let [workable @(ns-resolve 'cells.board 'workable)
          ids (set (map :id (workable conn rid)))]
      (is (contains? ids child) "a leaf of the board's own tree is workable")
      (is (contains? ids backlog) "the unclaimed backlog is workable")
      (is (not (contains? ids root)) "the root has an open child")
      (is (not (contains? ids _meta))
          "a role's run-scoped housekeeping task is not the board's work"))))

(deftest a-claim-held-by-a-dead-branch-is-released-not-stranded
  ;; The pre-fix live run left 'Round 2 web layer ...' claimed by a finished
  ;; branch forever — invisible to the unclaimed-only board, so the run's real
  ;; work was stranded. A claim on a branch that is no longer active is
  ;; released back to open (and then scoping decides whether it is the
  ;; board's to work).
  (cells/load-cells!)
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        root (tasks/create! conn {:title "the feature" :type "feature" :run-id rid})
        child (tasks/create! conn {:title "a part" :parent-id root :run-id rid})]
    (runs/open-branch! conn rid {:branch-id "T9"})
    (tasks/claim! conn child rid "T9")
    (runs/close-branch! conn rid "T9" :exhausted "cap")
    (let [release-stale! @(ns-resolve 'cells.board 'release-stale-claims!)
          workable @(ns-resolve 'cells.board 'workable)]
      (release-stale! conn rid)
      (is (= "open" (:status (tasks/get-task conn child)))
          "the dead branch's claim was released")
      (is (contains? (set (map :id (workable conn rid))) child)
          "and the task is workable again"))))

(deftest a-task-claimed-by-another-branch-is-not-workable
  ;; Observed live (run e1491f04): the supervisor's task tool made it claim a
  ;; housekeeping task of its own; the board counted that in_progress row as
  ;; workable, re-claimed it from the finished supervisor branch, and handed a
  ;; feature implementor "Diagnose STAGE CRASHED harness bug". A claim is a
  ;; claim: the board works UNCLAIMED tasks, and what another branch holds is
  ;; that branch's business.
  (cells/load-cells!)
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        free (tasks/create! conn {:title "unclaimed feature work" :type "feature" :run-id rid})
        held (tasks/create! conn {:title "claimed feature work" :type "feature" :run-id rid})]
    (tasks/claim! conn held rid "S0")
    (let [workable @(ns-resolve 'cells.board 'workable)
          ids (set (map :id (workable conn rid)))]
      (is (contains? ids free))
      (is (not (contains? ids held))
          "an in_progress claim is never the board's to take"))))

(deftest an-owner-that-splits-and-switches-is-reviewed-on-what-it-held
  ;; karamazov-bf2: the claim prompt tells a composite task's owner to split
  ;; and SWITCH to the first child. The board loop tracked only the task it
  ;; claimed FOR the branch, so the review judged the untouched parent while
  ;; the child the owner actually worked stayed claimed to a finished branch.
  ;; The review reads what the branch actually held at the end.
  (let [step (atom 0)]
    (with-redefs [llm/chat
                  (fn [_ _ messages & _]
                    (let [c (str/join " " (map :content messages))
                          parent (second (re-find #"working on \*\*(sz-\w+)" c))]
                      (case (swap! step inc)
                        1 {:content (str "```tool-call\n{\"name\":\"task\",\"args\":"
                                         "{\"action\":\"create\",\"title\":\"the real part\","
                                         "\"parentId\":\"" parent "\"}}\n```")
                           :finish-reason "stop"}
                        2 {:content (str "```tool-call\n{\"name\":\"task\",\"args\":"
                                         "{\"action\":\"list\"}}\n```")
                           :finish-reason "stop"}
                        3 (let [child (second (re-find #"(sz-\w+) \[open" c))]
                            {:content (str "```tool-call\n{\"name\":\"task\",\"args\":"
                                           "{\"action\":\"switch\",\"id\":\"" child "\","
                                           "\"reason\":\"split; working the part\"}}\n```")
                             :finish-reason "stop"})
                        (ships-its-task nil nil messages))))]
      (let [conn (db/open! ":memory:")
            parent (tasks/create! conn {:title "a composite task"})]
        (run-board conn {:max-turns 8})
        (let [kids (tasks/children-of conn parent)]
          (is (= 1 (count kids)) "the owner split one child out")
          (is (= "done" (:status (first kids)))
              "the child the owner switched to and shipped is what closed")
          (is (= "done" (:status (tasks/get-task conn parent)))
              "and the parent closed when its children were all done"))))))

(deftest the-review-hands-the-judge-parsed-rows-not-json-strings
  ;; Run c2260271: every landed task whose answer mentioned testing was
  ;; deterministically bounced with "the run shows no test was run" — twice,
  ;; then left open — while ship-verify had genuinely run the suite green.
  ;; The claim gate reads (get-in row [:args :command]), and board/review was
  ;; handing it rows whose :args were still raw JSON strings, so no shell
  ;; command ever counted as a test run.
  (let [seen (atom nil)]
    (with-redefs [llm/chat ships-its-task
                  judge/deterministic-block (fn [_ rows _] (reset! seen (vec rows)) nil)]
      (let [conn (db/open! ":memory:")]
        (tasks/create! conn {:title "checked work"})
        (run-board conn {})
        (is (seq @seen) "the deterministic gate ran")
        (is (every? #(or (nil? (:args %)) (map? (:args %))) @seen)
            "every row's :args reaches the judge parsed, so its evidence predicates can read them")))))

(deftest a-task-its-owner-could-not-finish-does-not-close
  ;; Honesty over tidiness: a task nobody landed stays open on the board, so
  ;; the next round (or a human) can see what is actually left.
  (with-redefs [llm/chat (fn [_ _ _ & _]
                           {:content (str "```tool-call\n{\"name\":\"give_up\",\"args\":"
                                          "{\"reason\":\"cannot\"}}\n```")
                            :finish-reason "stop"})]
    (let [conn (db/open! ":memory:")
          id (tasks/create! conn {:title "the hard one"})
          r (run-board conn {})]
      (is (not= "done" (:status (tasks/get-task conn id))))
      (is (not= :completed (:status r))
          "and a run that landed nothing does not report success"))))
