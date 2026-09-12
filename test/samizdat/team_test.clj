;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.team-test
  "Multi-agent fan-out: the team manifest runs a worker per sub-task in
  parallel and joins their answers."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.workflow :as workflow]))

(defn- worker-dones-its-task
  "A worker that immediately ships an answer engaging its own sub-task, so the
  done-gate accepts it."
  [_ _ messages & _]
  (let [content (str/join " " (map :content messages))
        prob (or (second (re-find #"## Problem\s+(\w+)" content)) "task")]
    {:content (str "```tool-call\n{\"name\":\"done\",\"args\":{\"answer\":\"handled "
                   prob "\"}}\n```")
     :finish-reason "stop"}))

(deftest team-fans-out-a-worker-per-subtask-and-joins
  (with-redefs [llm/chat worker-dones-its-task]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn
                            :config {:run {:loop "team" :subtasks ["alpha" "beta"]}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "the feature" :max-turns 6})]
      (is (= :completed (:status r)))
      (testing "each sub-task ran on its own worker branch on the shared run"
        (is (= #{"W0" "W1"}
               (set (map :branch_id
                         (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"]))))))
      (testing "the manager joins both workers' answers"
        (is (str/includes? (:answer r) "alpha"))
        (is (str/includes? (:answer r) "beta"))
        (is (str/includes? (:answer r) "2 workers"))))))

(defn- worker-gives-up
  "A worker that always gives up — nothing lands, retries included."
  [_ _ _ & _]
  {:content "```tool-call\n{\"name\":\"give_up\",\"args\":{\"reason\":\"cannot do it\"}}\n```"
   :finish-reason "stop"})

(deftest an-all-failed-team-run-does-not-report-completed
  ;; karamazov-blt.19: the fan-out marked the run :done unconditionally, so a
  ;; team where every worker failed still finished :completed with a summary
  ;; of the failures as its answer — upstream of the false-completion
  ;; memories in karamazov-mjb. The verdict now comes from :team/supervise,
  ;; after its retries, from what actually landed.
  (with-redefs [llm/chat worker-gives-up]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn
                            :config {:run {:loop "team" :subtasks ["alpha"]}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "the feature" :max-turns 4})]
      (is (not= :completed (:status r))
          "a team where every worker failed must not read as a success")
      (is (= "abandoned" (:status (db/fetch-one conn ["SELECT status FROM runs"])))
          "the run row records the honest ending"))))

(deftest team-with-no-subtasks-is-one-worker-on-the-whole-problem
  (with-redefs [llm/chat worker-dones-its-task]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn :config {:run {:loop "team"}}
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "solo" :max-turns 6})]
      (is (= :completed (:status r)))
      (is (str/includes? (:answer r) "1 worker")))))

(defn- planner-then-workers
  "One redef that plays two roles: the planner (returns a two-part split) on the
  plan call, and a worker that dones its own sub-task on the worker calls."
  [_ _ messages & _]
  (let [content (str/join " " (map :content messages))]
    (if (str/includes? content "splitting a coding task")
      {:content "- part one\n- part two" :finish-reason "stop"}
      (let [prob (str/trim (or (second (re-find #"## Problem\s+(.+)" content)) "task"))]
        {:content (str "```tool-call\n{\"name\":\"done\",\"args\":{\"answer\":\"handled "
                       prob "\"}}\n```")
         :finish-reason "stop"}))))

(deftest team-plans-its-own-split-when-no-subtasks-are-given
  (with-redefs [llm/chat planner-then-workers]
    (let [conn (db/open! ":memory:")
          r (workflow/run! {:conn conn
                            :config {:run {:loop "team"}} ; no :subtasks — the planner splits
                            :llm-adapter :a :llm-config {:max-tokens 16384}
                            :problem "build the thing" :max-turns 6})]
      (is (= :completed (:status r)))
      (testing "the planner's two parts each got their own worker branch"
        (is (= #{"W0" "W1"}
               (set (map :branch_id
                         (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"]))))))
      (testing "the manager joins both planned parts"
        (is (str/includes? (:answer r) "part one"))
        (is (str/includes? (:answer r) "part two"))
        (is (str/includes? (:answer r) "2 workers"))))))

(deftest explicit-subtasks-skip-the-planner
  ;; If config gives subtasks, :team/plan is a no-op — the planner LLM call must
  ;; never fire. A mock that throws on the planner prompt proves it.
  (let [planner-called (atom false)]
    (with-redefs [llm/chat (fn [a c messages & rest]
                             (when (str/includes? (str/join " " (map :content messages))
                                                  "splitting a coding task")
                               (reset! planner-called true))
                             (apply worker-dones-its-task a c messages rest))]
      (let [conn (db/open! ":memory:")
            r (workflow/run! {:conn conn
                              :config {:run {:loop "team" :subtasks ["alpha" "beta"]}}
                              :llm-adapter :a :llm-config {:max-tokens 16384}
                              :problem "the feature" :max-turns 6})]
        (is (= :completed (:status r)))
        (is (false? @planner-called) "planner must not run when subtasks are explicit")
        (is (str/includes? (:answer r) "alpha"))
        (is (str/includes? (:answer r) "beta"))))))

(deftest supervisor-retries-a-worker-that-gave-up
  (let [seen (atom #{})
        flaky (fn [_ _ messages & _]
                (let [content (str/join " " (map :content messages))
                      prob (str/trim (or (second (re-find #"## Problem\s+(.+)" content)) "task"))]
                  (if (contains? @seen prob)
                    ;; second sighting — the retry — succeeds
                    {:content (str "```tool-call\n{\"name\":\"done\",\"args\":{\"answer\":\"handled "
                                   prob "\"}}\n```")
                     :finish-reason "stop"}
                    ;; first sighting: give up, so the supervisor must re-task it.
                    ;;
                    ;; THE REASON HAS TO BE A REAL ACCOUNT. It said "stuck",
                    ;; which `give_up` now refuses (karamazov-ylte.1): of the
                    ;; four ways a branch can end that was the only ungated
                    ;; one, so it was the cheapest, and a loop teaches by what
                    ;; it makes cheap. The invariant this test pins is
                    ;; unchanged — a worker that gives up is re-tasked on its
                    ;; own retry branch — but what it takes to give up is not,
                    ;; and a stub that cannot get past the gate never reaches
                    ;; the supervisor this test is about.
                    (do (swap! seen conj prob)
                        {:content (str "```tool-call\n{\"name\":\"give_up\",\"args\":"
                                       "{\"reason\":\"I could not build " prob
                                       " — the module it needs is not on the "
                                       "path and nothing I tried put it there.\"}}"
                                       "\n```")
                         :finish-reason "stop"}))))]
    (with-redefs [llm/chat flaky]
      (let [conn (db/open! ":memory:")
            r (workflow/run! {:conn conn
                              :config {:run {:loop "team" :subtasks ["alpha" "beta"]}}
                              :llm-adapter :a :llm-config {:max-tokens 16384}
                              :problem "the feature" :max-turns 6})]
        (is (= :completed (:status r)))
        (testing "the supervisor re-ran each failed part on its own retry branch"
          (is (= #{"W0" "W1" "W0r1" "W1r1"}
                 (set (map :branch_id
                           (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"]))))))
        (testing "the retried answers replace the give-ups in the join"
          (is (str/includes? (:answer r) "handled alpha"))
          (is (str/includes? (:answer r) "handled beta")))))))

(deftest each-team-worker-gets-a-peer-roster-and-coordination-guide
  (let [systems (atom [])
        capturing (fn [a c messages & rest]
                    (swap! systems conj
                           (->> messages (filter #(= "system" (:role %))) first :content))
                    (apply worker-dones-its-task a c messages rest))]
    (with-redefs [llm/chat capturing]
      (let [conn (db/open! ":memory:")]
        (workflow/run! {:conn conn
                        :config {:run {:loop "team" :subtasks ["alpha" "beta"]}}
                        :llm-adapter :a :llm-config {:max-tokens 16384}
                        :problem "the feature" :max-turns 6})
        (let [seen @systems]
          (testing "a worker knows which one it is and sees its peer's part"
            ;; the worker on alpha is told it is W0 and that W1 has beta
            (is (some #(and (str/includes? % "You are worker W0")
                            (str/includes? % "W1: beta"))
                      seen))
            ;; and symmetrically the worker on beta sees W0: alpha
            (is (some #(and (str/includes? % "You are worker W1")
                            (str/includes? % "W0: alpha"))
                      seen)))
          (testing "the coordination guide (mailbox + remember) is injected"
            (is (some #(and (str/includes? % "message")
                            (str/includes? % "inbox")
                            (str/includes? % "remember"))
                      seen))))))))

(deftest implementors-get-the-repl-workflow-skill-in-context
  ;; karamazov-66k: implementors prototyped in eval and never wrote files. They
  ;; now get the repl-workflow skill in-context, which tells them the file on
  ;; disk is the deliverable.
  (let [systems (atom [])
        capturing (fn [a c messages & rest]
                    (swap! systems conj
                           (->> messages (filter #(= "system" (:role %))) first :content))
                    (apply worker-dones-its-task a c messages rest))]
    (with-redefs [llm/chat capturing]
      (let [conn (db/open! ":memory:")]
        (workflow/run! {:conn conn :config {:run {:loop "team" :subtasks ["alpha"]}}
                        :llm-adapter :a :llm-config {:max-tokens 16384}
                        :problem "the feature" :max-turns 6})
        (let [seen @systems]
          (is (some #(str/includes? % "REPL-driven development") seen)
              "implementors get the repl-workflow skill body")
          (is (some #(str/includes? % "edit_file") seen)
              "which tells them to write the change to the file, not leave it in eval")
          (is (some #(str/includes? % "Your role: implementor") seen)
              "alongside their role identity"))))))
