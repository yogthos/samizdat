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
            [samizdat.agent.gates :as gates]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.judge :as judge]
            [samizdat.cells :as cells]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
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

(deftest the-critic-verifies-its-own-findings-before-they-reach-the-owner
  ;; PASS 2, running in the real board — not judge/verified-findings called
  ;; directly. That distinction is the point: an earlier test in this epic was
  ;; named for a real loop and only ever called the function underneath it,
  ;; and a 684,076-token bill was how that came out. The stub answers pass 1
  ;; and pass 2 DIFFERENTLY, so a board that never made the second call would
  ;; fail here rather than pass quietly.
  (let [calls (atom [])
        speculative "- [high] The retry loop leaks a connection on the error path."]
    (with-redefs [llm/chat
                  (fn [a c messages & rest]
                    (let [content (str/join " " (map :content messages))]
                      (cond
                        ;; pass 2 carries the candidates and the verify contract
                        (str/includes? content "candidate findings")
                        (do (swap! calls conj :verify)
                            {:content (str speculative
                                           " FALSE_POSITIVE — no such path in the diff.")
                             :finish-reason "stop"})

                        (judge-call? messages)
                        (do (swap! calls conj :review)
                            {:content (str "VERDICT: INCOMPLETE\n\nFINDINGS:\n" speculative)
                             :finish-reason "stop"})

                        :else (apply ships-its-task a c messages rest))))]
      (let [conn (db/open! ":memory:")]
        (tasks/create! conn {:title "the handler"})
        (run-board conn {})
        (testing "the board made both calls, in order"
          (is (= [:review :verify] (take 2 @calls))))
        (testing "both passes are counted as money the run spent"
          ;; sweep5 run 2 carried six real findings and one side_calls row, for
          ;; a reflection — the judge's calls were invisible, so nothing could
          ;; say whether verify had run or what it costs (karamazov-2rqb).
          ;; The board reviews once per ATTEMPT, so a task that is sent back
          ;; and re-reviewed bills two pairs — which is the cost of the second
          ;; pass made visible, and the number to read when deciding whether
          ;; :judge-verify? earns it.
          (let [rows (db/fetch conn ["SELECT kind, role FROM side_calls
                                       WHERE kind LIKE 'critic-%'"])
                by-kind (frequencies (map :kind rows))]
            (is (pos? (get by-kind "critic-review" 0)))
            (is (= (get by-kind "critic-review") (get by-kind "critic-verify"))
                "every review that found something is paired with its verify")
            (is (every? #(= "critic" (:role %)) rows))))
        (testing "and the finding the second pass rejected never reached the record"
          (let [note (journal/last-note conn
                                        (:id (first (db/fetch conn ["SELECT id FROM runs"])))
                                        :board-review)]
            (is (some? note))
            (is (str/includes? (str (:candidates note)) "leaks a connection")
                "pass 1 said it, and the record keeps that it was considered")
            (is (not (str/includes? (str (:findings note)) "leaks a connection"))
                "pass 2 rejected it, so it is not what the owner is told to fix")))))))

(deftest a-clean-review-does-not-pay-for-a-second-pass
  ;; The verify call is skipped when pass 1 found nothing, so the common case
  ;; — a review that passes — costs exactly one model call, as it did before.
  (let [calls (atom [])]
    (with-redefs [llm/chat
                  (fn [a c messages & rest]
                    (let [content (str/join " " (map :content messages))]
                      (cond
                        (str/includes? content "candidate findings")
                        (do (swap! calls conj :verify)
                            {:content "No issues found." :finish-reason "stop"})
                        (judge-call? messages)
                        (do (swap! calls conj :review)
                            {:content "VERDICT: COMPLETE" :finish-reason "stop"})
                        :else (apply ships-its-task a c messages rest))))]
      (let [conn (db/open! ":memory:")]
        (tasks/create! conn {:title "the handler"})
        (run-board conn {})
        (is (= [:review] (distinct @calls))
            "a clean review never reaches the verify pass")
        (is (= ["critic-review"]
               (mapv :kind (db/fetch conn ["SELECT kind FROM side_calls
                                             WHERE kind LIKE 'critic-%'"])))
            "and it is billed for one call, not two")))))

(deftest a-trivial-task-skips-the-plan-phase
  ;; karamazov-vale. The triage heuristic keeps the phase off obviously-small
  ;; tasks — a short title with no list — so it is not a tax on the common case.
  ;; This is also why every existing board test still passes unchanged: their
  ;; task titles are short.
  (with-redefs [llm/chat ships-its-task]
    (let [conn (db/open! ":memory:")]
      (tasks/create! conn {:title "the handler"})
      (run-board conn {})
      (let [notes (journal/notes conn
                                  (:id (first (db/fetch conn ["SELECT id FROM runs"])))
                                  :triage)]
        (is (seq notes))
        (is (every? #(= "skip" (name (:decision %))) notes)
            "a one-line task is one change; planning it is the tax triage avoids")
        (is (empty? (db/fetch conn ["SELECT id FROM events WHERE kind = 'design'"]))
            "and the design step never ran")))))

(deftest a-substantial-task-plans-and-the-plan-critic-reviews-it
  ;; The phase engages on a task that names several parts. The design step runs
  ;; the owner under the design brief to declare a plan; the plan critic reviews
  ;; it against the requirement BEFORE construction — the whole point, since
  ;; every other gate fires after the budget is spent.
  (let [calls (atom [])]
    (with-redefs [llm/chat
                  (fn [a c messages & rest]
                    (let [content (str/join " " (map :content messages))]
                      (cond
                        ;; the plan critic (pass 1 or verify) — it carries the plan prompt
                        (str/includes? content "reviewing this PLAN")
                        (do (swap! calls conj :plan-review)
                            {:content "VERDICT: COMPLETE" :finish-reason "stop"})
                        (str/includes? content "candidate findings")
                        (do (swap! calls conj :plan-verify)
                            {:content "No issues found." :finish-reason "stop"})
                        ;; the design owner: declare a plan and stop
                        (str/includes? content "PLANNING this task")
                        (do (swap! calls conj :design)
                            {:content (str "```tool-call\n{\"name\":\"plan\",\"args\":"
                                           "{\"files\":[\"src/x.clj\"],\"tests\":[\"test/x_test.clj\"],"
                                           "\"goal\":\"do the thing and cover the second part too\"}}\n```")
                             :finish-reason "stop"})
                        ;; the diff critic
                        (judge-call? messages)
                        (do (swap! calls conj :diff-critic)
                            {:content "VERDICT: COMPLETE" :finish-reason "stop"})
                        :else (apply ships-its-task a c messages rest))))]
      (let [conn (db/open! ":memory:")]
        (tasks/create! conn {:title "storage and handlers"
                             :body "Add the storage layer AND the handlers AND the templates. Three parts."})
        (run-board conn {})
        (let [rid (:id (first (db/fetch conn ["SELECT id FROM runs"])))]
          (testing "triage chose to plan, and design + plan-critic ran before work"
            (is (= "plan" (name (:decision (journal/last-note conn rid :triage)))))
            (is (some #{:design} @calls))
            (is (some #{:plan-review} @calls))
            (is (< (.indexOf @calls :plan-review) (or (some (fn [[i x]] (when (= x :diff-critic) i))
                                                            (map-indexed vector @calls)) 999))
                "the plan critic ran BEFORE the diff critic"))
          (testing "the approved plan is persisted as the task's contract"
            (let [t (first (db/fetch conn ["SELECT plan FROM tasks WHERE plan IS NOT NULL"]))]
              (is (some? t))
              (is (str/includes? (str (:plan t)) "second part"))))
          (testing "the plan-critic pass is priced under its own kind"
            ;; review-plan passes bare :review/:verify and the cell owns the
            ;; plan- prefix, like the diff critic — so the record reads
            ;; plan-review, not the double-prefixed plan-plan-review sweep8
            ;; recorded. (Pass 1 is clean here, so verify is skipped and costs
            ;; nothing, exactly as the diff critic's critic-verify is.)
            (let [kinds (set (map :kind (db/fetch conn ["SELECT kind FROM side_calls"])))]
              (is (contains? kinds "plan-review"))
              (is (not (contains? kinds "plan-plan-review"))))))))))

(deftest a-design-step-that-declares-no-plan-is-sent-back-once-then-fails-open
  ;; sweep8's remembers arm: the owner skipped the plan step, so design-review
  ;; had nothing to review and the pre-construction gate no-op'd straight to
  ;; :go. A blank plan now routes to :revise once — the owner is asked to
  ;; declare one — then fails open at :max-design-attempts rather than wedging
  ;; on a plan it could not produce. The diff critic stays downstream.
  (with-redefs [llm/chat
                (fn [a c messages & rest]
                  (if (judge-call? messages)
                    {:content "VERDICT: COMPLETE" :finish-reason "stop"}
                    ;; ships-its-task never declares a plan, so every design
                    ;; attempt ends with state/plan nil.
                    (apply ships-its-task a c messages rest)))]
    (let [conn (db/open! ":memory:")]
      (tasks/create! conn {:title "storage and handlers"
                           :body "Add the storage layer AND the handlers AND the templates. Three parts."})
      (run-board conn {})
      (let [rid (:id (first (db/fetch conn ["SELECT id FROM runs"])))
            reviews (journal/notes conn rid :design-review)
            designs (journal/notes conn rid :design)]
        (testing "the owner is sent back once to declare a plan, then it fails open"
          (is (= ["revise" "ok"] (mapv #(name (:decision %)) reviews))
              "first attempt revises the blank plan, the second fails open"))
        (testing "design ran twice and declared nothing either time"
          (is (= 2 (count designs)))
          (is (every? #(false? (:declared %)) designs)))
        (testing "the plan critic never ran on a blank plan — nothing to price"
          (let [kinds (set (map :kind (db/fetch conn ["SELECT kind FROM side_calls"])))]
            (is (not (contains? kinds "plan-review")))))
        (testing "fail-open held: construction still ran and the task closed"
          (is (= "done" (:status (first (db/fetch conn ["SELECT status FROM tasks"]))))))))))

(deftest triage-is-three-way-by-size
  ;; karamazov-dq1r: trivial skips, a multi-part task gets an RFC, an ordinary
  ;; single-change task gets the lightweight plan in between.
  (let [multipart? @(ns-resolve 'cells.board 'multipart?)
        looks-trivial? @(ns-resolve 'cells.board 'looks-trivial?)]
    (is (looks-trivial? {:title "fix the typo in the readme"})
        "short and no list — skip")
    (is (not (multipart? {:body "rename cfg to config across the ns"}))
        "an ordinary single change is not RFC-worthy")
    (is (multipart? {:body "Add these:\n- storage\n- handlers\n- templates"})
        "an explicit list is several things wearing one title — RFC")
    (is (multipart? {:body (apply str (repeat 130 "word "))})
        "a long statement is too big for one plan — RFC")))

(deftest an-rfc-child-does-not-open-its-own-rfc
  ;; A task decomposed from an RFC (its parent epic carries the plan) must not
  ;; recurse into another RFC — it gets a lightweight plan or skip.
  (cells/load-cells!)
  (let [conn (db/open! ":memory:")
        rfc-child? @(ns-resolve 'cells.board 'rfc-child?)
        epic (tasks/create! conn {:title "the feature" :type "feature"})
        child (tasks/create! conn {:title "storage and the handlers and templates"
                                   :parent-id epic})]
    (tasks/update! conn epic {:plan "# RFC\n## Work items\n- storage\n- handlers"})
    (is (rfc-child? conn (tasks/get-task conn child))
        "its parent carries a persisted RFC")
    (is (not (rfc-child? conn (tasks/get-task conn epic)))
        "the epic itself is not a child")))

(deftest a-multipart-task-writes-an-rfc-the-critic-reviews-and-it-persists
  ;; The RFC tier end to end: triage routes to :rfc, the owner writes an RFC
  ;; through the plan tool's :rfc field, the plan critic reviews it, and the
  ;; RFC is persisted as the task's contract (tasks.plan).
  (let [calls (atom [])
        rfc "# RFC: haze\n## Purpose\nfade the horizon.\n## Model\n```mermaid\nflowchart TD\n  a-->b\n```\n## Work items\n- pure flight.haze with tests\n- draw-terrain! blends toward sky\n## Acceptance criteria\ntests pass; keep complexity in the gates."]
    (with-redefs [llm/chat
                  (fn [a c messages & rest]
                    (let [content (str/join " " (map :content messages))]
                      (cond
                        (str/includes? content "reviewing this PLAN")
                        (do (swap! calls conj :plan-review)
                            {:content "VERDICT: COMPLETE" :finish-reason "stop"})
                        (str/includes? content "candidate findings")
                        (do (swap! calls conj :plan-verify)
                            {:content "No issues found." :finish-reason "stop"})
                        ;; the RFC design owner: declare a plan carrying the rfc
                        (str/includes? content "as an RFC")
                        (do (swap! calls conj :design-rfc)
                            {:content (str "```tool-call\n{\"name\":\"plan\",\"args\":"
                                           "{\"files\":[\"src/flight/haze.clj\"],"
                                           "\"tests\":[\"test/flight/haze_test.clj\"],"
                                           "\"goal\":\"fade the horizon and cover both parts\","
                                           "\"rfc\":" (pr-str rfc) "}}\n```")
                             :finish-reason "stop"})
                        (judge-call? messages)
                        (do (swap! calls conj :diff-critic)
                            {:content "VERDICT: COMPLETE" :finish-reason "stop"})
                        :else (apply ships-its-task a c messages rest))))]
      (let [conn (db/open! ":memory:")]
        ;; The board opens the epic from the run's problem itself, so it is a
        ;; proper feature root and its decompose children are in the tree.
        (run-board conn {:problem "Three parts to the flight's feel:\n- distance fade\n- tree clip\n- horizon blend"})
        (let [rid (:id (first (db/fetch conn ["SELECT id FROM runs"])))]
          (testing "triage routed to the RFC tier and the RFC design step ran"
            (is (some #(= "rfc" (name (:decision %))) (journal/notes conn rid :triage))
                "the epic triaged to the RFC tier (children triage to plan)")
            (is (some #{:design-rfc} @calls))
            (is (some #(true? (:rfc %)) (journal/notes conn rid :design))
                "the epic's design step ran in RFC mode")
            (is (some #{:plan-review} @calls) "the plan critic reviewed the RFC"))
          (testing "the RFC is persisted as the epic's contract, stamped rfc"
            (let [t (first (db/fetch conn ["SELECT id, plan, plan_kind FROM tasks WHERE plan IS NOT NULL"]))]
              (is (some? t))
              (is (= "rfc" (:plan_kind t)))
              (is (str/includes? (str (:plan t)) "Work items"))
              (is (str/includes? (str (:plan t)) "mermaid"))
              (testing "and it decomposed into child tasks under the epic"
                (is (<= 2 (count (db/fetch conn ["SELECT id FROM tasks WHERE parent_id = ?"
                                                 (:id t)])))
                    "the two work items became children"))
              (testing "the end-of-phase critic validated the whole change and closed the epic"
                (is (some? (journal/last-note conn rid :epic-review))
                    "epic-review ran once the children landed")
                (is (= "done" (:status (tasks/get-task conn (:id t))))
                    "and the epic closed, not by closable-parents! but through the RFC critic")))))))))

(deftest rfc-work-items-parses-the-breakdown-section
  (let [work-items @(ns-resolve 'cells.board 'rfc-work-items)]
    (is (= ["build storage" "build handlers" "wire templates"]
           (work-items (str "# RFC\n## Purpose\np\n## Work items\n"
                            "- build storage\n- build handlers\n* wire templates\n"
                            "## Acceptance\ntests pass\n- not a work item"))))
    (is (= [] (work-items "# RFC\n## Purpose\nno breakdown here")))))

(deftest an-rfc-epic-hands-its-children-the-rfc-for-context
  (cells/load-cells!)
  (let [epic-rfc @(ns-resolve 'cells.board 'epic-rfc)
        conn (db/open! ":memory:")
        epic (tasks/create! conn {:title "feature" :type "feature"})
        child (tasks/create! conn {:title "a part" :parent-id epic})
        loose (tasks/create! conn {:title "unparented"})]
    (tasks/update! conn epic {:plan "# RFC\n## Model\nthe design" :plan-kind "rfc"})
    (is (str/includes? (str (epic-rfc conn (tasks/get-task conn child))) "the design")
        "a child of an rfc epic gets the epic's RFC")
    (is (nil? (epic-rfc conn (tasks/get-task conn loose)))
        "a task with no rfc parent gets none")))

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

(deftest the-board-review-note-keeps-the-findings-the-owner-was-sent
  ;; karamazov-3htz: the review note recorded verdict and decision; the
  ;; findings went back to the owner and nowhere durable.
  (let [critic-says (atom "REVISE\nFINDINGS:\n- [high] the handler ignores its error branch")]
    (with-redefs [llm/chat
                  (fn [a c messages & rest]
                    (if (judge-call? messages)
                      (let [reply @critic-says]
                        (reset! critic-says "COMPLETE")
                        {:content reply :finish-reason "stop"})
                      (apply ships-its-task a c messages rest)))]
      (let [conn (db/open! ":memory:")]
        (tasks/create! conn {:title "the handler"})
        (run-board conn {})
        (let [rid (:id (first (db/fetch conn ["SELECT id FROM runs"])))
              notes (journal/notes conn rid :board-review)
              bounced (first (filter #(= "revise" (str (:decision %))) notes))]
          (is (some? bounced) "one review sent the task back")
          (is (str/includes? (str (:findings bounced)) "error branch")
              "and the note carries what it said"))))))

(deftest the-review-judge-is-told-what-the-task-asked-for
  ;; The judge's first live finding on the ghost-replay run (e1b765e7) was
  ;; "The requirement section is empty in the prompt": this cell still passed
  ;; the pre-requirement keys, so the judge was asked whether the work was
  ;; complete with the task nowhere in the message and the answer in it
  ;; twice. The requirement is the task's body or title — what the owner was
  ;; handed.
  (let [judged (atom nil)]
    (with-redefs [llm/chat (fn [a c messages & rest]
                             (if (judge-call? messages)
                               (do (reset! judged (str/join " " (map :content messages)))
                                   {:content "COMPLETE" :finish-reason "stop"})
                               (apply ships-its-task a c messages rest)))]
      (let [conn (db/open! ":memory:")]
        (tasks/create! conn {:title "wire the handler"
                             :body "Wire the error branch of the request handler to the logger"})
        (run-board conn {})
        (is (some? @judged) "the judge was called")
        (is (str/includes? (str @judged) "Wire the error branch of the request handler")
            "and read the task's own text as the requirement")))))

(deftest an-owner-that-spends-its-turn-budget-hands-the-task-back
  ;; karamazov-ghti: an owner's cap was the run's :max-turns, which on a
  ;; board-driven run bounds the beam's rounds and not any owner — so an
  ;; owner had the whole run. gates.edn :board-owner-turns bounds one owner
  ;; on one task; an exhausted owner is a give-up to the review and the task
  ;; stays open, unowned, for the next round or a human.
  (let [orig gates/threshold
        turns-taken (atom 0)]
    (with-redefs [gates/threshold (fn [k] (if (= k :board-owner-turns) 3 (orig k)))
                  llm/chat (fn [& _]
                             (swap! turns-taken inc)
                             {:content "```tool-call\n{\"name\":\"task\",\"args\":{\"action\":\"list\"}}\n```"
                              :finish-reason "stop"})]
      (let [conn (db/open! ":memory:")
            id (tasks/create! conn {:title "the endless one"})
            r (run-board conn {:max-turns 40})
            owner-turns (count (db/fetch conn ["SELECT id FROM turns WHERE branch_id LIKE 'T0%'"]))]
        (is (<= owner-turns 3) "the owner stopped at its own budget, not the run's")
        (is (not= "done" (:status (tasks/get-task conn id))) "the task is not closed")
        (is (not= :completed (:status r)) "and the run does not claim success")
        (let [rid (:id (first (db/fetch conn ["SELECT id FROM runs"])))
              reviews (journal/notes conn rid :board-review)]
          (is (some #(= "give-up" (str (:decision %))) reviews)
              "the review recorded the give-up rather than a pass"))))))

(deftest a-parent-that-delegated-comes-back-to-assemble-instead-of-being-closed
  ;; karamazov-ioo.15.4. `split` blocks the row it was called on and parks the
  ;; branch; the board's half of the wait is here. Two passes used to get this
  ;; wrong in the same direction: closable-parents! closed the parent the
  ;; moment its last piece landed, so the agent that designed the boundary
  ;; never composed it, and the composition it had written — the code that
  ;; calls the stubs, and its own tests — was never run against the real
  ;; pieces.
  ;;
  ;; The difference is what the parent OWES, and `stubs` on the children is the
  ;; evidence: an epic somebody opened to group work owes nothing once its
  ;; parts are done; a parent that split owes the assembly.
  (cells/load-cells!)
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        root (tasks/create! conn {:title "the feature" :type "feature" :run-id rid})
        split-parent (tasks/create! conn {:title "build the report" :parent-id root
                                          :run-id rid})
        piece (tasks/create! conn {:title "parse-line" :parent-id split-parent
                                   :run-id rid :stub-file "src/example/core.clj"
                                   :stubs ["parse-line"]})
        epic (tasks/create! conn {:title "a grouping" :parent-id root :run-id rid})
        grouped (tasks/create! conn {:title "a part" :parent-id epic :run-id rid})
        closable! @(ns-resolve 'cells.board 'closable-parents!)
        unblock! @(ns-resolve 'cells.board 'unblock-assembled!)
        workable @(ns-resolve 'cells.board 'workable)]
    ;; where split left it: parked branch, blocked row, one piece to build
    (tasks/claim! conn split-parent rid "T1")
    (tasks/update! conn split-parent {:status "blocked"})
    (testing "while its pieces are being built it is nobody's to take"
      (unblock! conn rid)
      (closable! conn rid)
      (is (= "blocked" (:status (tasks/get-task conn split-parent))))
      (is (not (contains? (set (map :id (workable conn rid))) split-parent))))
    ;; the pieces land
    (tasks/close! conn piece)
    (tasks/close! conn grouped)
    (unblock! conn rid)
    (closable! conn rid)
    (testing "a grouping with nothing of its own to do is closed"
      (is (= "done" (:status (tasks/get-task conn epic)))))
    (testing "a parent that delegated is handed back to be assembled"
      (is (= "open" (:status (tasks/get-task conn split-parent))))
      (is (nil? (:branch_id (tasks/get-task conn split-parent)))
          "unclaimed, so the board can hand it to an owner")
      (is (contains? (set (map :id (workable conn rid))) split-parent)))))

;; --- the durable attempt count is the board's too ---------------------------
;; karamazov-yjbp. Two counters are called `attempts`: the round's
;; :board/attempts, seeded to 0 at every claim, and tasks.attempts, the column
;; added in v21 so that "how many times has this been tried" survives the
;; round and the process. tasks/attempted! had one caller in the tree, in
;; decompose, so on a board run — the default — the column stayed 0 forever
;; and a released task came back indistinguishable from a fresh one.

(deftest what-board-review-attempts-counts-is-policy
  ;; karamazov-yjbp. The two scopes differ only for a task that was given up
  ;; and came back, which is why a single round never showed the difference.
  (with-redefs [llm/chat ships-its-task]
    (let [conn (db/open! ":memory:")
          id (tasks/create! conn {:title "hard one"})]
      (run-board conn {})
      ;; two prior claims on the record, one revision inside this claim
      (tasks/attempted! conn id)
      (is (= 2 (:attempts (tasks/get-task conn id))))
      (testing ":claim counts revisions inside this claim and ignores history"
        (is (= :claim (gates/threshold :board-attempts-scope))
            "the shipped default, until something measures the other"))
      (testing ":task counts what the task has cost across every claim"
        ;; the review reads the column under :task scope, so a task already at
        ;; the cap is spent the moment it is looked at again
        (is (>= (:attempts (tasks/get-task conn id))
                (gates/threshold :board-review-attempts))
            "which is what makes the bound span rounds instead of resetting")))))

(deftest claiming-a-task-records-the-attempt-on-the-task-itself
  (with-redefs [llm/chat ships-its-task]
    (let [conn (db/open! ":memory:")
          id (tasks/create! conn {:title "wire the thing"})]
      (is (= 0 (:attempts (tasks/get-task conn id))) "nothing has tried it yet")
      (run-board conn {})
      (is (= 1 (:attempts (tasks/get-task conn id)))
          "one claim, one attempt on the record")
      (testing "and the count accumulates, which the round's own counter cannot"
        ;; :board/attempts is seeded to 0 at every claim and dies with the
        ;; round; this one is the task's and survives both.
        (is (= 2 (tasks/attempted! conn id)))))))
