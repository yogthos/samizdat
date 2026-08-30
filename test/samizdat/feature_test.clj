;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.feature-test
  "The feature loop: an outer state machine (plan -> implement -> review ->
  critique -> supervise -> route) delegating each stage to a role. These tests
  drive the state machine with a role-dispatching mock and stub the judge's
  content heuristics (tested in judge-test), so they exercise the WIRING —
  ship, the reviewer's revise bounce, and the supervisor's escalation."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.judge :as judge]
            [samizdat.agent.state :as ag-state]
            [samizdat.agent.tools :as ag-tools]
            [samizdat.engine.proc :as proc]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
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
  tool so it hits the turn cap), the critic's judge reply is ignored (stubbed)."
  [{:keys [review exhaust stop]}]
  (fn [_ _ messages & _]
    (let [c (str/join " " (map :content messages))]
      (cond
        (str/includes? c "Your role: reviewer")
        (done-call (review-answer review))        ; PASS/REVISE on the first line

        (str/includes? c "Your role: supervisor")
        ;; the supervisor READS the telemetry and DECIDES — it revises when the
        ;; digest flags that nothing shipped, else it lets the loop proceed.
        (cond
          stop (done-call "STOP: further revise rounds are not converging; ship what the implementors produced and end.")
          (str/includes? c "NO IMPLEMENTOR SHIPPED")
          (done-call "REVISE: no implementor shipped; re-run the implement round with tighter guidance.")
          :else (done-call "CONTINUE: the implementors shipped and the reviewer passed; the loop is converging, no adjustment needed."))

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

(deftest supervisor-reasons-over-telemetry-and-forces-a-round
  ;; Reviewer PASSes, so without the supervisor the run would ship round 0. The
  ;; implementors exhaust (ship nothing); the supervisor reads that in the
  ;; run-health digest ("NO IMPLEMENTOR SHIPPED") and DECIDES to REVISE — the
  ;; loop introspecting and steering itself, not a hard-coded rule.
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :complete)
                judge/blocking-findings (constantly nil)
                llm/chat (roles {:review :pass :exhaust true})]
    (let [conn (db/open! ":memory:")
          r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                              :max-revisions 9 :max-revisions-hard 1}}
                               :max-turns 3})]
      (testing "a revise round happened despite the reviewer passing"
        (is (round-ran? (branch-ids conn) 0 1)))
      (testing "and since the implementors never shipped, it ends unsolved, not falsely completed"
        (is (= :abandoned (:status r)))))))

(deftest a-crashing-stage-does-not-kill-the-run-and-surfaces-to-the-supervisor
  ;; critique used to throw an unbound-var and take the whole run down before the
  ;; supervisor stage ran. Now a stage that crashes is recorded, fails soft, and
  ;; the run reaches the supervisor with the crash in its telemetry to plan on.
  (let [seen-digest (atom nil)
        base (roles {:review :pass})]
    (with-redefs [judge/deterministic-block (fn [& _] (throw (ex-info "boom in the judge" {})))
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  llm/chat (fn [a b messages & r]
                             (when (str/includes? (str/join " " (map :content messages))
                                                  "Your role: supervisor")
                               (reset! seen-digest (str/join " " (map :content messages))))
                             (apply base a b messages r))]
      (let [conn (db/open! ":memory:")
            r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]}}})]
        (is (= :completed (:status r)) "the run survived the crashing critique")
        (is (some? @seen-digest) "it reached the supervisor despite the crash")
        (is (str/includes? @seen-digest "STAGE CRASHED")
            "the supervisor was shown the crash to plan around")))))

(deftest supervisor-stop-means-give-up-and-abandons-unsolved
  ;; STOP is the supervisor's last resort — it concluded the loop can't solve the
  ;; task. The run ends UNSOLVED (abandoned), it does NOT ship the work as done,
  ;; and it stops iterating at once (no further revise round).
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :incomplete) ; the judge keeps bouncing
                judge/blocking-findings (constantly nil)
                llm/chat (roles {:review :pass :stop true})]
    (let [conn (db/open! ":memory:")
          r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                              :max-revisions 3}}})]
      (is (= :abandoned (:status r)) "STOP ends unsolved, not shipped")
      (is (nil? (:answer r)) "no answer is presented for an unsolved task")
      (testing "it gave up at once — no versioned revise branch"
        (is (not (contains? (branch-ids conn) "T0v1")))))))

(deftest supervisor-extends-the-owner-turn-budget
  ;; Self-healing is ADJUSTING the loop, not just voting on it. The binding
  ;; constraint observed across every dogfood round was the per-owner turn
  ;; budget: owners spend their opening turns orienting and exhaust mid-fix,
  ;; and the supervisor could see that and do nothing about it. `EXTEND: <n>`
  ;; is the lever: the next round's owners run under the extended budget.
  (let [owner-turns (atom {})]
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  llm/chat (fn [_ _ messages & _]
                             (let [c (str/join " " (map :content messages))]
                               (cond
                                 (str/includes? c "Your role: supervisor")
                                 (done-call "REVISE — the owners keep exhausting mid-task.\nEXTEND: 9")

                                 (str/includes? c "Your role: implementor")
                                 ;; never calls a tool -> runs to ITS turn cap,
                                 ;; which is what the test measures
                                 (let [b (second (re-find #"branch (\S+)" c))]
                                   {:content "thinking, no call" :finish-reason "stop"})

                                 :else {:content "COMPLETE" :finish-reason "stop"})))]
      (let [conn (db/open! ":memory:")]
        (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                          :max-revisions 9 :max-revisions-hard 1}}
                           :max-turns 3})
        (let [turns (into {} (map (juxt :branch_id :t)
                                  (db/fetch conn ["SELECT branch_id, MAX(turn) t FROM turns
                                                   WHERE branch_id LIKE 'T0%' GROUP BY branch_id"])))]
          (is (= 3 (val (first (filter #(str/starts-with? (key %) "T0") turns))))
              "round 0 ran under the run's own budget")
          (is (= 9 (some (fn [[k v]] (when (and (str/starts-with? k "T0")
                                                   (str/ends-with? k "v1")) v))
                             turns))
              "after EXTEND: 9, the revise round's owner ran under the extended budget"))))))

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
  ;; karamazov-reo: implementor on one model, supervisor on another, reviewer on
  ;; the run default. The captured :provider per role proves each role's sub-loop
  ;; ran on its assigned model.
  (let [seen (atom {})
        base (roles {:review :pass})]
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  llm/chat (fn [adapter cfg messages & r]
                             (let [c (str/join " " (map :content messages))
                                   role (cond
                                          (str/includes? c "Your role: supervisor") :supervisor
                                          (str/includes? c "Your role: implementor") :implementor
                                          :else :critic)]
                               (swap! seen update role (fnil conj #{}) (:provider cfg)))
                             (apply base adapter cfg messages r))]
      (let [conn (db/open! ":memory:")]
        (workflow/run! {:conn conn
                        :config {:run {:loop "feature" :subtasks ["alpha"]
                                       :role-models {:implementor {:provider "deepseek"}
                                                     :supervisor {:provider "glm"}}}}
                        :llm-adapter :a
                        :llm-config {:provider :openai :model "gpt-4o" :max-tokens 16384}
                        :problem "the feature" :max-turns 4})
        (is (contains? (:implementor @seen) :deepseek) "implementor ran on its assigned model")
        (is (contains? (:supervisor @seen) :glm) "supervisor ran on its assigned model")
        (is (contains? (:critic @seen) :openai) "the unconfigured critic kept the run default")))))

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

(deftest soft-cap-notifies-the-supervisor-not-auto-abandon
  ;; The cap is a SOFT stop: at it the supervisor is notified via telemetry and
  ;; decides for itself — the loop does not abandon just for reaching it.
  (let [caps (atom [])
        base (roles {:review :pass})]
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :incomplete) ; keeps bouncing, so the loop revises
                  judge/blocking-findings (constantly nil)
                  llm/chat (fn [a b messages & r]
                             (when (str/includes? (str/join " " (map :content messages))
                                                  "Your role: supervisor")
                               (swap! caps conj (str/join " " (map :content messages))))
                             (apply base a b messages r))]
      (let [conn (db/open! ":memory:")]
        (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                          :max-revisions 1 :max-revisions-hard 4}}})
        (is (some #(str/includes? % "REVISION CAP REACHED") @caps)
            "at the soft cap the supervisor is told, and asked to decide")
        (is (contains? (branch-ids conn) "DT")
            "and the loop continued PAST the soft cap (escalating to decompose) rather than abandoning at it")))))

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

(deftest supervisor-can-switch-the-implement-approach-mid-run
  ;; self-healing: the supervisor decides the fan-out isn't working and switches
  ;; this run's implement stage to the decompose loop with a SWITCH: line. The
  ;; next round routes through :decompose/run instead of the fan-out.
  (let [architect-json (str "{\"decision\":\"decompose\",\"subtasks\":"
                            "[{\"name\":\"a\",\"description\":\"do a\"}]}")
        mock (fn [_ _ messages & _]
               (let [c (str/join " " (map :content messages))]
                 (cond
                   (str/includes? c "Your role: reviewer")
                   (done-call "PASS: the implementors covered the feature; nothing to send back")

                   (str/includes? c "Your role: supervisor")
                   (if (str/includes? c "revision 0")
                     (done-call "SWITCH: decompose\nthe board is stuck; try decompose")
                     (done-call "CONTINUE: the decompose approach is converging, nothing to change"))

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
            r (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                               :max-revisions-hard 3}}})
            branches (branch-ids conn)]
        (testing "round 0 ran the board (the default strategy)"
          (is (owner-ran? branches 0)))
        (testing "after SWITCH: decompose, the next round ran the decompose loop"
          (is (contains? branches "DT") "the decompose root attempt ran"))))))

(deftest a-failing-strategy-auto-escalates-even-without-a-supervisor-switch
  ;; iteration must not hinge on the LLM supervisor deciding to switch (it may
  ;; revise passively or exhaust). When a strategy keeps failing its soft-cap
  ;; rounds, the loop advances the ladder on its own — here team -> decompose.
  (with-redefs [judge/deterministic-block (constantly nil)
                judge/parse-verdict (constantly :complete)
                judge/blocking-findings (constantly nil)
                gitdiff/baseline (constantly "HEAD")
                gitdiff/changed-files (constantly [])          ; everything hollow -> keeps failing
                llm/chat (roles {:review :pass})]              ; supervisor CONTINUEs, never switches
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

(deftest the-routing-stage-reads-the-streams-conclusion-instead-of-a-second-one
  ;; The supervision STREAM watches the whole run continuously on its own
  ;; branch; :feature/supervise sees one round at one moment. Both are
  ;; supervisors with the full tool surface and separate contexts, so left
  ;; alone they reach two conclusions about the same run and neither knows the
  ;; other exists. The stage now quotes the stream and routes; diagnosis and
  ;; harness tuning belong to the stream, which has more to go on.
  (let [seen (atom nil)
        base (roles {:review :pass})]
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  llm/chat (fn [a b messages & more]
                             (let [c (str/join " " (map :content messages))]
                               (when (str/includes? c "Your role: supervisor")
                                 (reset! seen c))
                               (apply base a b messages more)))]
      (let [conn (db/open! ":memory:")]
        ;; The stream's last word, written the way the stream writes it.
        (with-redefs [journal/last-note
                      (constantly {:verdict "done"
                                   :notes "the classpath is wrong: eval cannot load the project's own FFI namespace"})]
          (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                            :max-revisions 9 :max-revisions-hard 1}}
                             :max-turns 3}))
        (is (str/includes? (str @seen) "the classpath is wrong")
            "the stage was not shown what the stream already concluded")
        (is (str/includes? (str @seen) "do not re-derive the same diagnosis")
            "and is told to route rather than diagnose a second time")))))

(deftest with-no-stream-conclusion-the-routing-stage-still-routes
  ;; The stream is budgeted and most passes are quiet, so a round can easily
  ;; arrive with nothing on record. That must not leave the stage with a
  ;; dangling quote or a missing instruction.
  (let [seen (atom nil)
        base (roles {:review :pass})]
    (with-redefs [judge/deterministic-block (constantly nil)
                  judge/parse-verdict (constantly :complete)
                  judge/blocking-findings (constantly nil)
                  llm/chat (fn [a b messages & more]
                             (let [c (str/join " " (map :content messages))]
                               (when (str/includes? c "Your role: supervisor")
                                 (reset! seen c))
                               (apply base a b messages more)))]
      (let [conn (db/open! ":memory:")]
        (run-feature conn {:config {:run {:loop "feature" :subtasks ["alpha"]
                                          :max-revisions 9 :max-revisions-hard 1}}
                           :max-turns 3})
        (is (str/includes? (str @seen) "No supervision-stream conclusion"))
        (is (str/includes? (str @seen) "CONTINUE, REVISE, or STOP"))))))

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
    (is (= "revise" (:directive (journal/last-note conn rid :supervise))))))
