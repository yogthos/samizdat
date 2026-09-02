;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.feature-test
  "The feature loop: an outer state machine (plan -> implement -> review ->
  critique -> supervise -> route) delegating each stage to a role. These tests
  drive the state machine with a role-dispatching mock and stub the judge's
  content heuristics (tested in judge-test), so they exercise the WIRING —
  ship, the reviewer's revise bounce, and the supervisor's directives landing
  at the stage that applies them (RFC-012)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.judge :as judge]
            [samizdat.agent.state :as ag-state]
            [samizdat.agent.tools :as ag-tools]
            [samizdat.engine.proc :as proc]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.workflow :as workflow]))

(defn- owner-ran?
  "Whether owner `n` ran, by prefix. Branch ids carry the task they worked now
  (`T0-get-the-suite-green`), so pinning the bare `T0` froze an id format that
  deliberately changed — the owner is the thing these tests mean."
  [ids n]
  (boolean (some #(str/starts-with? (str %) (str "T" n)) ids)))

(defn- round-ran?
  "Whether owner `n` ran a REVISE round `r`. The round suffix is still `v<r>`;
  what moved is the task slug between the owner and the suffix."
  [ids n r]
  (boolean (some #(and (str/starts-with? (str %) (str "T" n))
                       (str/ends-with? (str %) (str "v" r)))
                 ids)))

;; Ground truth (step 3): a done with no diff is not a completed feature. By
;; default these tests simulate a run that DID change files, so the wiring tests
;; below exercise the ship/revise paths; the hollow-path tests redef this to [].
(use-fixtures :each
  (fn [t]
    ;; A test file is in the change set: the ship gate's TDD rung refuses a
    ;; change with no test in it, so a mock world without one is a world where
    ;; no owner can ever land — which is a different test than these run.
    (with-redefs [gitdiff/changed-files (constantly ["src/example.clj"
                                                    "test/example_test.clj"])]
      (t))))

(defn- done-call [answer]
  {:content (str "```tool-call\n{\"name\":\"done\",\"args\":{\"answer\":\""
                 answer "\"}}\n```")
   :finish-reason "stop"})

(defn- review-answer
  "A substantive review verdict — PASS/REVISE on the first line, then reasons
  that name the feature/implementors so the done-gate accepts it as engaging the
  review problem (a one-word 'revise' gets blocked as engaging nothing)."
  [review]
  (case review
    :pass  "PASS: the implementors' changes implement the feature and the tests pass; nothing to send back."
    :revise "REVISE: the implementors' work does not satisfy the feature; the changes touch the wrong area and must be redone."))

(defn- roles
  "One redef playing every role by the prompt it sees: the reviewer ships
  PASS/REVISE, an implementor builds its part (or, when :exhaust, never calls a
  tool so it hits the turn cap), the critic's judge reply is ignored (stubbed).

  No supervisor branch: the feature loop no longer runs one. The supervisor
  is the stream beside the run, and its say arrives as directives — see
  `submitting`."
  [{:keys [review exhaust]}]
  (fn [_ _ messages & _]
    (let [c (str/join " " (map :content messages))]
      (cond
        (str/includes? c "Your role: reviewer")
        (done-call (review-answer review))        ; PASS/REVISE on the first line

        (str/includes? c "Your role: implementor")
        (if exhaust
          {:content "still working, no tool call yet" :finish-reason "stop"}
          (done-call (str "built " (str/trim (or (second (re-find #"## Problem\s+(\S+)" c))
                                                 "part")))))

        :else {:content "COMPLETE" :finish-reason "stop"}))))

(defn- branch-ids [conn]
  (set (map :branch_id (db/fetch conn ["SELECT DISTINCT branch_id FROM turns"]))))

(defn- run-feature [conn extra]
  (workflow/run! (merge {:conn conn
                         :llm-adapter :a :llm-config {:max-tokens 16384}
                         :problem "the feature" :max-turns 4}
                        extra)))

(defn- submitting
  "Run `f` with `directives` queued the moment the run row exists, as the
  supervisor's. The single-branch driver these tests use has no supervisor
  stream, so this stands in for the stream's hands: what it would have said
  through `intervene`, already on the queue when the loop reaches the stage
  that applies it."
  [directives f]
  (let [orig runs/start-run!]
    (with-redefs [runs/start-run! (fn [c & args]
                                    (let [id (apply orig c args)]
                                      (doseq [d directives]
                                        (interventions/submit! c id (assoc d :issued-by "supervisor")))
                                      id))]
      (f))))

(deftest feature-flows-plan-implement-review-critique-ship
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :complete)
                judge/blocking-findings (constantly nil)
                llm/chat (roles {:review :pass})]
    (let [conn (db/open! ":memory:")
          r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha" "beta"]}}})]
      (is (= :completed (:status r)))
      (testing "the join carries both implementors' parts"
        (is (str/includes? (:answer r) "alpha"))
        (is (str/includes? (:answer r) "beta")))
      (testing "each task got its own owner branch (T0/T1), and no round reviewer ran"
        ;; The default implement strategy is the BOARD: the two subtasks are two
        ;; owned tasks worked one at a time, not two workers in the tree at
        ;; once — and each was critic-reviewed on its own diff before closing,
        ;; so the round-level reviewer role is skipped (RFC-011).
        (let [b (branch-ids conn)]
          (is (owner-ran? b 0))
          (is (owner-ran? b 1))
          (is (not (contains? b "R0"))))))))

(deftest feature-critique-revise-loops-back-to-implement-bounded
  ;; On a board round the per-task critic replaced the round reviewer, so the
  ;; round-level bounce comes from CRITIQUE (the judge): an :incomplete verdict
  ;; sends the round back, bounded by the runaway guard.
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :incomplete) ; the judge always bounces
                judge/blocking-findings (constantly nil)
                llm/chat (roles {:review :pass})]
    (let [conn (db/open! ":memory:")
          ;; soft-cap above the hard cap so the strategy-escalation ladder does
          ;; not fire here — this test is about the revise mechanics.
          r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                              :max-revisions 9 :max-revisions-hard 2}}})]
      (testing "an unsatisfiable reviewer keeps the loop solving, then the runaway guard abandons honestly (it never claims a solution it didn't reach)"
        (is (= :abandoned (:status r))))
      (testing "each revise round re-implemented on a versioned branch"
        (let [b (branch-ids conn)]
          (is (owner-ran? b 0))     ; round 0
          (is (round-ran? b 0 1))   ; revise round 1
          (is (round-ran? b 0 2))   ; revise round 2, then the runaway guard trips
          (is (not (contains? b "T0v3"))))))))

(deftest a-crashing-stage-does-not-kill-the-run-and-is-on-the-record
  ;; critique used to throw an unbound-var and take the whole run down. Now a
  ;; stage that crashes is recorded, fails soft, and the run goes on. The
  ;; record is what the SUPERVISOR reads — the stream's gather picks up
  ;; :stage-error notes (oversight-test) — so the crash reaches the one
  ;; supervisor there is without this loop running a second one to show it to.
  (with-redefs [judge/deterministic-block (fn [& _] (throw (ex-info "boom in the judge" {})))
                judge/parse-verdict (constantly :complete)
                judge/blocking-findings (constantly nil)
                llm/chat (roles {:review :pass})]
    (let [conn (db/open! ":memory:")
          r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]}}})
          crashes (journal/notes conn (:run-id r) :stage-error)]
      (is (= :completed (:status r)) "the run survived the crashing critique")
      (is (seq crashes) "the crash is on the record for the supervisor stream")
      (is (str/includes? (str (:error (first crashes))) "boom in the judge")))))

(deftest a-stop-directive-means-give-up-and-abandons-unsolved
  ;; STOP is the supervisor's last resort — it concluded the loop can't solve
  ;; the task. It arrives as a `stop` directive through the queue, the stage
  ;; applies it, and the run ends UNSOLVED (abandoned): it does NOT ship the
  ;; work as done, and it stops iterating at once (no further revise round).
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :incomplete) ; the judge keeps bouncing
                judge/blocking-findings (constantly nil)
                llm/chat (roles {:review :pass})]
    (let [conn (db/open! ":memory:")
          r (submitting [{:kind "stop" :payload {:text "not converging after three approaches"}}]
                        ;; The hard cap is a guard for THIS TEST: a loop that
                        ;; ignored the stop would revise forever, and a hang
                        ;; is not a failure anyone can read.
                        #(run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                                           :max-revisions 3
                                                           :max-revisions-hard 4}}}))]
      (is (= :abandoned (:status r)) "STOP ends unsolved, not shipped")
      (is (nil? (:answer r)) "no answer is presented for an unsolved task")
      (testing "it gave up at once — no versioned revise branch"
        (is (not (contains? (branch-ids conn) "T0v1"))))
      (testing "the reason travels: the record says who stopped it and why"
        (is (str/includes? (str (get-in r [:branch :inactive-reason])) "not converging"))
        (let [note (journal/last-note conn (:run-id r) :supervise)]
          (is (true? (:stop note)))
          (is (= "supervisor" (:issued-by (first (:applied note))))))))))

(deftest a-budget-directive-extends-the-owner-turn-budget
  ;; Self-healing is ADJUSTING the loop, not just voting on it. The binding
  ;; constraint observed across every dogfood round was the per-owner turn
  ;; budget: owners spend their opening turns orienting and exhaust mid-fix,
  ;; and the supervisor could see that and do nothing about it. A `budget`
  ;; directive is the lever: the next round's owners run under it.
  (let [owner-turns (atom {})]
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  llm/chat (fn [_ _ messages & _]
                             (let [c (str/join " " (map :content messages))]
                               (cond
                                 (str/includes? c "Your role: implementor")
                                 ;; never calls a tool -> runs to ITS turn cap,
                                 ;; which is what the test measures
                                 {:content "thinking, no call" :finish-reason "stop"}

                                 :else {:content "COMPLETE" :finish-reason "stop"})))]
      (let [conn (db/open! ":memory:")]
        (submitting [{:kind "budget" :payload {:text "9"}}]
                    #(run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                                       :max-revisions 9 :max-revisions-hard 1}}
                                        :max-turns 3}))
        (let [turns (into {} (map (juxt :branch_id :t)
                                  (db/fetch conn ["SELECT branch_id, MAX(turn) t FROM turns
                                                   WHERE branch_id LIKE 'T0%' GROUP BY branch_id"])))]
          (is (= 3 (val (first (filter #(str/starts-with? (key %) "T0") turns))))
              "round 0 ran under the run's own budget")
          (is (= 9 (some (fn [[k v]] (when (and (str/starts-with? k "T0")
                                                   (str/ends-with? k "v1")) v))
                             turns))
              "after budget 9, the revise round's owner ran under the extended budget"))))))

(deftest an-advisory-branch-ships-its-verdict-without-the-evidence-rungs
  ;; karamazov-t86, the supervisor half. A reviewer or supervisor's done IS its
  ;; deliverable — a verdict about the run, quoting the run's own figures
  ;; ("19 tests, 0 failures") and, on a red tree, describing the redness. The
  ;; figure rung demanded artifacts for those numbers and the verify rung
  ;; demanded green tests, so the advisory roles ground out their budgets
  ;; unable to say what they had concluded (S0 in runs 3b8d2af5, e1491f04,
  ;; 7857c6e7 — every one). Advisory branches skip the evidence rungs.
  (let [ctx {:branch (assoc (ag-state/new-branch
                             {:id "S9" :problem "supervise the run"})
                            :advisory? true)
             :config {:run {:verify-cmd "false" :verify-focused? true}}
             :root "/nonexistent"
             :tool-name "done"
             :args {:answer "REVISE — 19 tests ran, 7 failed; the owners keep exhausting at turn 40."}}
        r (ag-tools/run-tool ctx)]
    (is (= :success (:category r)) (str (:result r)))
    (is (= :done (get-in r [:branch :status]))
        "the verdict lands — figures, red tree and all")))

(deftest per-role-models-reach-each-role
  ;; karamazov-reo: implementor on one model, critic on another. The captured
  ;; :provider per role proves each role's sub-loop ran on its assigned model.
  ;; (The supervisor is not a role this loop runs any more — it is the stream
  ;; beside the run — and a board round skips the reviewer, so the critic is
  ;; the second role a board run can show.)
  (let [seen (atom {})
        base (roles {:review :pass})]
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  llm/chat (fn [adapter cfg messages & r]
                             (let [c (str/join " " (map :content messages))
                                   role (cond
                                          (str/includes? c "Your role: implementor") :implementor
                                          :else :critic)]
                               (swap! seen update role (fnil conj #{}) (:provider cfg)))
                             (apply base adapter cfg messages r))]
      (let [conn (db/open! ":memory:")]
        (workflow/run! {:conn conn
                        :config {:run {:loop "feature" :subtasks ["alpha"]
                                       :role-models {:implementor {:provider "deepseek"}
                                                     :critic {:provider "glm"}}}}
                        :llm-adapter :a
                        :llm-config {:provider :openai :model "gpt-4o" :max-tokens 16384}
                        :problem "the feature" :max-turns 4})
        (is (contains? (:implementor @seen) :deepseek) "implementor ran on its assigned model")
        (is (contains? (:critic @seen) :glm) "critic ran on its assigned model")
        (is (not (contains? (:implementor @seen) :openai)) "and not on the run default")))))

(deftest hollow-work-is-never-shipped-completed-it-keeps-solving
  ;; step 3: the DeepSeek dogfood shipped an empty diff as "completed" (reviewer
  ;; passed, supervisor STOP). Ground truth: a done that changed no files is not
  ;; a solution. The loop does NOT ship it — it keeps solving (revising with a
  ;; "you changed no files" nudge). Here the workers never edit anything, so it
  ;; eventually hits the safety backstop and abandons honestly — never completed.
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :complete)
                judge/blocking-findings (constantly nil)
                gitdiff/changed-files (constantly [])       ; nothing ever changes
                llm/chat (roles {:review :pass})]           ; reviewer would pass, but ground truth overrides
    (let [conn (db/open! ":memory:")
          ;; soft-cap above the hard cap so escalation doesn't fire — this test
          ;; is about hollow work never shipping, via the board.
          r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                              :max-revisions 9 :max-revisions-hard 2}}})]
      (is (not= :completed (:status r)) "an empty diff is never reported completed")
      (testing "it kept solving before giving up (revised, did not abandon on the first empty round)"
        (is (round-ran? (branch-ids conn) 0 1)))
      (is (= :abandoned (:status r)) "only the runaway guard ends it, honestly unsolved"))))

(deftest a-ship-carries-an-answer-even-when-the-last-round-landed-nothing
  ;; Runs 3b8d2af5 and e1491f04 both ended with route decision SHIP on green
  ;; gates while the FINAL round's board had landed nothing (the work landed
  ;; in earlier rounds) — so the branch carried no :final-answer. Under the
  ;; beam driver the turn slice cuts the :finish node out of a whole-run
  ;; manifest, and the beam's own ending reads :final-answer: nil there turned
  ;; a shipped feature into finish-run! :failed, which then taught
  ;; record-workflow-outcome! that the loop never ships. Ship WRITES the
  ;; answer on the branch.
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :complete)
                judge/blocking-findings (constantly nil)
                proc/run (constantly {:exit 0 :out "ok"})
                llm/chat (fn [_ _ messages & _]
                           (let [c (str/join " " (map :content messages))]
                             (cond
                               (str/includes? c "Your role: supervisor")
                               (done-call "CONTINUE: the gates are green; nothing to adjust.")

                               ;; owners never call a tool -> the round lands nothing
                               (str/includes? c "Your role: implementor")
                               {:content "still thinking" :finish-reason "stop"}

                               :else {:content "COMPLETE" :finish-reason "stop"})))]
    (let [conn (db/open! ":memory:")
          r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                              :verify-cmd "run-tests"}}
                               :max-turns 3})]
      (is (= :completed (:status r))
          "green review + green tests ship, whatever the last round landed")
      (is (not (str/blank? (str (:answer r))))
          "and the shipped run carries an answer for the record"))))

(deftest a-real-diff-still-ships-as-completed
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :complete)
                judge/blocking-findings (constantly nil)
                gitdiff/changed-files (constantly ["src/store/knowledge.clj"])
                llm/chat (roles {:review :pass})]
    (let [conn (db/open! ":memory:")
          r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]}}})]
      (is (= :completed (:status r)) "real changes + a pass ships completed"))))

(deftest the-soft-cap-is-on-the-record-and-does-not-abandon
  ;; The cap is a SOFT stop: at it the loop keeps solving — auto-advancing the
  ;; strategy ladder — and the record says so, which is what the supervisor
  ;; stream reads to decide whether to switch, re-budget or stop
  ;; (oversight-test covers that it does look).
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :incomplete) ; keeps bouncing, so the loop revises
                judge/blocking-findings (constantly nil)
                llm/chat (roles {:review :pass})]
    (let [conn (db/open! ":memory:")
          r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                              :max-revisions 1 :max-revisions-hard 4}}})
          routes (journal/notes conn (:run-id r) :route)]
      (is (some #(and (= 1 (:soft-cap %)) (>= (:revision %) 1)) routes)
          "a round at the soft cap is on the record with the cap beside it")
      (is (contains? (branch-ids conn) "DT")
          "and the loop continued PAST the soft cap (escalating to decompose) rather than abandoning at it"))))

(deftest tests-gate-must-pass-to-complete
  (testing "failing tests block completion — gate 2 is real"
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  gitdiff/changed-files (constantly ["src/x.clj" "test/x_test.clj"])
                  proc/run (constantly {:exit 1 :out "1 test FAILED"})
                  llm/chat (roles {:review :pass})]
      (let [conn (db/open! ":memory:")
            r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                               :verify-cmd "run-tests"
                                               :max-revisions 1 :max-revisions-hard 1}}})]
        (is (not= :completed (:status r)) "review passed but the tests fail, so not completed"))))
  (testing "both gates green completes"
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  gitdiff/changed-files (constantly ["src/x.clj" "test/x_test.clj"])
                  proc/run (constantly {:exit 0 :out "ok"})
                  llm/chat (roles {:review :pass})]
      (let [conn (db/open! ":memory:")
            r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                               :verify-cmd "run-tests"}}})]
        (is (= :completed (:status r)) "real diff + review pass + tests pass = completed")))))

(deftest a-switch-directive-changes-the-implement-approach-mid-run
  ;; self-healing: the supervisor decides the board isn't working and switches
  ;; this run's implement stage to the decompose loop with a `switch`
  ;; directive. The next round routes through :decompose/run instead.
  (let [architect-json (str "{\"decision\":\"decompose\",\"subtasks\":"
                            "[{\"name\":\"a\",\"description\":\"do a\"}]}")
        mock (fn [_ _ messages & _]
               (let [c (str/join " " (map :content messages))]
                 (cond
                   (str/includes? c "Your role: reviewer")
                   (done-call "PASS: the implementors covered the feature; nothing to send back")

                   (str/includes? c "architect diagnosing")
                   {:content architect-json :finish-reason "stop"}

                   (str/includes? c "## Problem")
                   (done-call (str "handled " (str/trim (or (second (re-find #"## Problem\s+(.+)" c)) "it"))))

                   :else {:content "COMPLETE" :finish-reason "stop"})))]
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  gitdiff/baseline (constantly "HEAD")
                  gitdiff/changed-files (constantly ["src/x.clj" "test/x_test.clj"])
                  llm/chat mock]
      (let [conn (db/open! ":memory:")
            r (submitting [{:kind "switch" :payload {:text "decompose"}}]
                          #(run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                                             :max-revisions-hard 3}}}))
            branches (branch-ids conn)]
        (testing "round 0 ran the board (the default strategy)"
          (is (owner-ran? branches 0)))
        (testing "after the switch, the next round ran the decompose loop"
          (is (contains? branches "DT") "the decompose root attempt ran"))
        (testing "the directive was resolved at the stage, applied, with the record naming it"
          (let [[d] (interventions/history conn (:run-id r))]
            (is (= "applied" (:status d)))
            (is (some #(= "decompose" (:switch %)) (journal/notes conn (:run-id r) :supervise))
                "the round that applied it says so")))))))

(deftest a-failing-strategy-auto-escalates-even-without-a-supervisor-switch
  ;; iteration must not hinge on the LLM supervisor deciding to switch (it may
  ;; revise passively or exhaust). When a strategy keeps failing its soft-cap
  ;; rounds, the loop advances the ladder on its own — here team -> decompose.
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :complete)
                judge/blocking-findings (constantly nil)
                gitdiff/baseline (constantly "HEAD")
                gitdiff/changed-files (constantly [])          ; everything hollow -> keeps failing
                llm/chat (roles {:review :pass})]              ; no directive ever arrives
    (let [conn (db/open! ":memory:")]
      (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                        :max-revisions 1 :max-revisions-hard 3}}})
      (let [branches (branch-ids conn)]
        (testing "round 0 ran the board (the default strategy)"
          (is (owner-ran? branches 0)))
        (testing "the loop auto-advanced along the ladder on its own"
          ;; board -> team -> decompose: a strategy that keeps failing its
          ;; soft-cap rounds hands over without waiting for the supervisor.
          (is (contains? branches "W0v1") "the fan-out, the next rung (at revision 1)")
          (is (contains? branches "DT") "and then decompose"))))))

;; --- one run, two supervisors (karamazov-poe) --------------------------------

(deftest the-feature-loop-runs-no-supervisor-of-its-own
  ;; RFC-012 F1/F4. The stage used to run the supervisor ROLE on a branch of
  ;; its own — S<revision> — once per round: a second supervisor, with a
  ;; second identity and a second context, that could only act when the graph
  ;; reached it. There is one supervisor now, the stream on `SUP`, and the
  ;; stage is where its directives about the outer loop LAND. So a feature
  ;; run makes no supervisor model call and opens no S-branch.
  (let [prompts (atom [])
        base (roles {:review :pass})]
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  llm/chat (fn [a b messages & more]
                             (swap! prompts conj (str/join " " (map :content messages)))
                             (apply base a b messages more))]
      (let [conn (db/open! ":memory:")
            r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                                :max-revisions 9 :max-revisions-hard 1}}
                                 :max-turns 3})]
        (is (= :completed (:status r)))
        (is (not-any? #(str/includes? % "Your role: supervisor") @prompts)
            "no supervisor turn was spent inside the loop")
        (is (not-any? #(re-matches #"S\d+" (str %)) (branch-ids conn))
            "and no S<revision> branch was opened")))))

(deftest a-malformed-outer-loop-directive-is-refused-with-a-reason-and-the-round-goes-on
  ;; A directive is never silently dropped and never wedges the loop: a switch
  ;; to a strategy that does not exist, or a budget with no number in it, is
  ;; resolved :rejected with a reason a person can read, and the round routes
  ;; as if nothing had been said.
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :complete)
                judge/blocking-findings (constantly nil)
                llm/chat (roles {:review :pass})]
    (let [conn (db/open! ":memory:")
          r (submitting [{:kind "switch" :payload {:text "banana"}}
                         {:kind "budget" :payload {:text "lots"}}]
                        #(run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]}}}))
          by-kind (into {} (map (juxt :kind identity)) (interventions/history conn (:run-id r)))]
      (is (= :completed (:status r)) "the round shipped on its own gates")
      (is (= "rejected" (:status (by-kind "switch"))))
      (is (every? #(str/includes? (str (:disposition (by-kind "switch"))) %)
                  ["board" "team" "decompose"])
          "the refusal names what IS available")
      (is (= "rejected" (:status (by-kind "budget"))))
      (is (str/includes? (str (:disposition (by-kind "budget"))) "turn count")))))

(deftest the-journal-hands-back-the-last-note-of-a-kind
  ;; The stream hands its output to nobody, so the journal is the only place a
  ;; pipeline stage can meet it.
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})]
    (is (nil? (journal/last-note conn rid :oversight))
        "no note yet is nil, not a throw")
    (journal/note! conn rid :oversight {:data {:notes "first" :verdict "done"}})
    (journal/note! conn rid :oversight {:data {:notes "second" :verdict "done"}})
    (journal/note! conn rid :supervise {:data {:directive "revise"}})
    (is (= "second" (:notes (journal/last-note conn rid :oversight)))
        "the LAST note of that kind, not the last note")
    (is (= "revise" (:directive (journal/last-note conn rid :supervise))))
    (is (= ["first" "second"] (mapv :notes (journal/notes conn rid :oversight)))
        "and every note of a kind, oldest first")))
