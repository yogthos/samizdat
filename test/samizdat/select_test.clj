;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.select-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.select :as select]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.store.knowledge :as knowledge]
            [samizdat.lexicon :as lexicon]
            [samizdat.workflow :as workflow]))

(deftest the-menu-is-narrowed-to-what-policy-admits
  ;; Several manifests say in their own descriptions that they are components
  ;; of the feature loop rather than run-level drivers. A menu that offers
  ;; those invites a run driven by half a workflow.
  (let [cands (select/candidates nil)
        names (set (map :name cands))
        allowed (set (:candidates (select/policy)))]
    (is (seq cands))
    (is (= names allowed) "every admitted name exists in the catalogue, and nothing else is offered")
    (testing "the components stay off the menu"
      (is (not (contains? names "worker")))
      (is (not (contains? names "reviewer")))
      (is (not (contains? names "supervisor"))))
    (testing "every candidate carries a description to choose on"
      (doseq [c cands]
        (is (not (str/blank? (:description c)))
            (str (:name c) " is on the menu with nothing to judge it by"))))))

(deftest every-candidate-reaches-the-prompt
  ;; What a model picks is not something a test can pin. A candidate that
  ;; never reached the menu is a harness bug, and this is where it shows.
  (let [cands (select/candidates nil)
        p (select/build-prompt "Add multi-user support to the todo app" cands)]
    (is (str/includes? p "Add multi-user support to the todo app"))
    (doseq [{:keys [name description]} cands]
      (is (str/includes? p name) (str name " is a candidate but is not in the prompt"))
      (is (str/includes? p (subs description 0 (min 40 (count description))))
          (str name "'s description did not reach the prompt")))))

(deftest a-choice-is-only-a-name-on-the-menu
  (let [cands (select/candidates nil)]
    (testing "a bare name"
      (is (= "team" (select/parse-choice "team" cands))))
    (testing "wrapped in the punctuation and markdown a model reaches for"
      (is (= "team" (select/parse-choice "  team  " cands)))
      (is (= "team" (select/parse-choice "`team`" cands)))
      (is (= "team" (select/parse-choice "team." cands)))
      (is (= "decompose" (select/parse-choice "**decompose**" cands))))
    (testing "anything not on the menu is no answer at all"
      (is (nil? (select/parse-choice "banana" cands)))
      (is (nil? (select/parse-choice "" cands)))
      (is (nil? (select/parse-choice nil cands))))
    (testing "a reasoning model's scratchpad is not its answer"
      ;; The first live selection this ever made returned exactly this shape,
      ;; and the strict match refused it — the model had chosen correctly and
      ;; the run fell back anyway.
      (is (= "critic" (select/parse-choice
                       "<think>loop has 0/4 here — evidence against. Answer: critic.</think>\ncritic"
                       cands)))
      (is (= "decompose" (select/parse-choice
                          "<think>weighing team and loop</think>\n\n**decompose**" cands)))
      (testing "and a think block naming everything still cannot vote on its own"
        (is (nil? (select/parse-choice
                   "<think>maybe team, maybe loop, maybe critic</think>\nbanana" cands)))))
    (testing "a name merely MENTIONED is not a vote"
      ;; This accepted a name found anywhere in the reply, and a test scripting
      ;; an unrelated model response had its run silently driven by a different
      ;; workflow. `loop` is an ordinary English word, and the reply chooses
      ;; the code path for a whole run — a loose match is worse than none,
      ;; because none falls back to the factory loop and says so.
      (is (nil? (select/parse-choice "I'd use the decompose workflow." cands)))
      (is (nil? (select/parse-choice "loop over the items" cands)))
      (is (nil? (select/parse-choice "solve the problem" cands))))))

(deftest a-short-question-is-still-triaged
  ;; The floor used to skip every problem under 120 characters as "the factory
  ;; loop's case by definition". "explain the project in the chat" is 31, so
  ;; it ran the code-change loop at width 5 with the plan and test gates, took
  ;; 50 minutes and wrote four files nobody asked for (karamazov-1wv9). A
  ;; short problem is where kind and size are least obvious from its length.
  (is (zero? (or (:min-problem-chars (select/policy)) 0)))
  (with-redefs [llm/chat (fn [& _] {:content "kind: answer\nsize: small\nworkflow: loop"
                                    :finish-reason "stop"})]
    (is (= {:kind :answer :size :small :workflow "loop"}
           (select/triage! {:conn nil :llm-adapter :fake :llm-config {}}
                           "explain the project in the chat")))))

(deftest a-triage-reply-is-three-lines-each-held-to-its-menu
  (let [cands [{:name "loop"} {:name "team"}]]
    (is (= {:kind :change :size :large :workflow "team"}
           (select/parse-triage "<think>parts</think>\nkind: change\nsize: large\nworkflow: team" cands)))
    (testing "a value off its menu is dropped, and the rest still stands"
      (is (= {:kind :answer :size nil :workflow "loop"}
             (select/parse-triage "kind: answer\nsize: huge\nworkflow: loop" cands)))
      (is (= {:kind :investigate :size :small :workflow nil}
             (select/parse-triage "kind: investigate\nsize: small\nworkflow: looping" cands))))
    (testing "a bare name, the old reply, is still a workflow"
      (is (= {:kind nil :size nil :workflow "team"} (select/parse-triage "team" cands))))
    (is (= {:kind nil :size nil :workflow nil} (select/parse-triage "no idea" cands)))))

(deftest the-effort-a-task-gets-is-policy
  ;; kind x size -> width, turns, effort, and for a question its workflow.
  ;; The numbers are gates.edn's; this pins the shape and the direction.
  (let [answer (select/effort {:kind :answer :size :small})
        large (select/effort {:kind :change :size :large})]
    (is (= 1 (:beam-width answer)) "a question is one line of work")
    (is (pos-int? (:max-turns answer)))
    (is (< (:max-turns answer) 100) "and a short one")
    (is (> (:beam-width large) 1) "a large change still gets the beam")
    (is (nil? (select/effort {:kind nil :size nil})) "no triage, no opinion")
    (is (= (select/effort {:kind :answer :size nil}) (select/effort {:kind :answer :size :large}))
        "a question's effort does not depend on its size")))

(deftest selection-never-stops-a-run-from-starting
  ;; nil on every uncertainty: the caller's fallback is the factory loop,
  ;; which is what the run would have used anyway.
  (testing "no adapter"
    (is (nil? (select/pick! {:conn nil} "a problem"))))
  (testing "a blank problem"
    (is (nil? (select/pick! {:conn nil :llm-adapter :fake} ""))))
  (testing "an adapter that throws"
    (is (nil? (select/pick! {:conn nil
                             :llm-adapter (reify Object)
                             :llm-config {}}
                            "a problem")))))

(deftest the-configured-loop-always-wins
  ;; A caller who pinned a loop asked a question selection has no business
  ;; re-answering.
  (is (= "critic" (workflow/active-loop-name {:run {:loop "critic"}} "team")))
  (is (= "team" (workflow/active-loop-name {} "team")))
  (is (= "team" (workflow/active-loop-name {:run {}} "team")))
  (testing "and with nothing to go on it is the factory default"
    (is (= "loop" (workflow/active-loop-name {} nil)))
    (is (= "loop" (workflow/active-loop-name {:run {:loop nil}} nil)))))

(deftest the-fallback-is-a-workflow-that-exists
  ;; A fallback naming a manifest nothing ships is a run that cannot start on
  ;; the one path that is supposed to be safe.
  (let [fb (:fallback (select/policy))
        catalogued (set (map :name (workflow/catalog nil)))]
    (is (contains? catalogued fb))
    (is (= fb (workflow/active-loop-name {} nil))
        "the documented fallback and the code's default are the same name")))

(deftest the-choice-reads-how-each-workflow-has-gone-here
  ;; A run sees only its own attempt, so "direct attempts on this project keep
  ;; getting stuck" is not something any single run can notice — it has to be
  ;; written down for the next one. This is decompose-on-stuck at the
  ;; granularity the harness actually operates at: the recursive solver splits
  ;; a unit that won't pass its tests, and this is what notices that the
  ;; unsplit approach keeps failing and chooses differently.
  (let [conn (db/open! ":memory:")]
    (knowledge/record-workflow-outcome! conn {:workflow "loop" :run-id "r1" :outcome :failed})
    (knowledge/record-workflow-outcome! conn {:workflow "loop" :run-id "r2" :outcome :failed})
    (knowledge/record-workflow-outcome! conn {:workflow "decompose" :run-id "r3" :outcome :shipped})
    (let [cands (select/candidates conn)
          lines (select/history-lines conn cands)]
      (testing "one line per workflow that has actually run, best first"
        (is (= 2 (count lines)))
        (is (str/includes? (first lines) "decompose"))
        (is (str/includes? (first lines) "shipped 1 of 1 run")))
      (testing "the failing one is reported as failing, not omitted"
        (is (some #(and (str/includes? % "loop") (str/includes? % "shipped 0 of 2 runs")) lines)))
      (testing "a workflow nobody has run says nothing rather than saying nothing at length"
        (is (not-any? #(str/includes? % "team") lines)))
      (testing "and all of it reaches the prompt"
        (let [p (select/build-prompt "a task" cands lines)]
          (doseq [l lines] (is (str/includes? p l))))))
    (testing "a run that never records leaves the prompt clean"
      (let [empty-conn (db/open! ":memory:")
            cands (select/candidates empty-conn)]
        (is (empty? (select/history-lines empty-conn cands)))
        (is (not (str/includes? (select/build-prompt "a task" cands nil)
                                "HOW THESE HAVE GONE")))))))

(deftest a-crash-is-neither-a-shipped-run-nor-a-failed-one
  ;; karamazov-a6mj.1. A Throwable escaping run-rounds — a provider outage, a
  ;; jolt bug, a hung turn — used to be recorded as :shipped? false, so the
  ;; chooser learned that the MANIFEST fails the task from a run the harness
  ;; could not finish. A crash is an outcome, and it has to be written down
  ;; (blt.38), but it is evidence about the harness, not about the workflow.
  (let [conn (db/open! ":memory:")]
    (knowledge/record-workflow-outcome! conn {:workflow "loop" :run-id "r1" :outcome :shipped})
    (knowledge/record-workflow-outcome! conn {:workflow "loop" :run-id "r2" :outcome :failed})
    (knowledge/record-workflow-outcome! conn {:workflow "loop" :run-id "r3" :outcome :error})
    (knowledge/record-workflow-outcome! conn {:workflow "loop" :run-id "r4" :outcome :error})
    (let [r (first (filter #(= "loop" (:workflow %)) (knowledge/workflow-record conn)))]
      (is (= 1 (:shipped r)))
      (is (= 1 (:failed r)) "a crash does not count as the workflow failing")
      (is (= 2 (:errors r)) "but it is counted, as its own thing")
      (is (= 2 (:runs r)) "the ratio the chooser reads is over FINISHED runs"))
    (testing "the history line tells a reader 1 of 2 with 2 crashes from 1 of 4"
      (let [lines (select/history-lines conn (select/candidates conn))
            line (first (filter #(str/includes? % "loop") lines))]
        (is (some? line))
        (is (str/includes? line "shipped 1 of 2 runs") line)
        (is (str/includes? line "2 crashed") line)))
    (testing "a workflow that has only ever crashed is still reported — as crashed, not as unrun"
      (knowledge/record-workflow-outcome! conn {:workflow "decompose" :run-id "r5" :outcome :error})
      (let [lines (select/history-lines conn (select/candidates conn))
            line (first (filter #(str/includes? % "decompose") lines))]
        (is (some? line) (pr-str lines))
        (is (str/includes? line "1 crashed") line)
        (is (not (str/includes? line "shipped 0 of 0")) line)))))

(deftest a-workflows-record-accumulates-across-runs
  (let [conn (db/open! ":memory:")]
    (dotimes [i 4]
      (knowledge/record-workflow-outcome! conn {:workflow "loop"
                                                :run-id (str "r" i) :outcome :failed}))
    (knowledge/record-workflow-outcome! conn {:workflow "loop" :run-id "r9" :outcome :shipped})
    (let [r (first (filter #(= "loop" (:workflow %)) (knowledge/workflow-record conn)))]
      (is (= 5 (:runs r)) "one row per workflow, the counts doing the accumulating")
      (is (= 1 (:shipped r)))
      (is (= 4 (:failed r))))
    (testing "and it is one row, not five"
      (is (= 1 (count (filter #(= "procedural" (:kind %)) (knowledge/recent conn 20))))))))


(deftest the-selection-call-is-billed-to-the-run-it-chooses-for
  ;; The one provider call the harness could not bill (karamazov-2rqb.1):
  ;; it ran before the run row existed. Now the row comes first and the
  ;; call records a side_calls row like every other side model.
  (let [c (db/open! ":memory:")]
    (try
      (let [rid (runs/start-run! c {:problem "p"})]
        (with-redefs [llm/chat (fn [& _] {:content "loop" :finish-reason "stop"
                                          :usage {:prompt-tokens 40 :completion-tokens 3}})]
          (select/pick! {:conn c :run-id rid :llm-adapter :fake :llm-config {:model "m"}}
                        (apply str (repeat 200 "a problem "))))
        (let [u (journal/run-usage c rid)]
          (is (= 1 (:side-calls u)))
          (is (= 43 (:total-tokens u)) "the run's bill includes it")))
      (testing "without a run id it still answers, unbilled, as before"
        (with-redefs [llm/chat (fn [& _] {:content "loop" :finish-reason "stop"})]
          (is (= "loop" (select/pick! {:conn c :llm-adapter :fake :llm-config {}}
                                      (apply str (repeat 200 "a problem ")))))))
      (finally (db/close c)))))
