;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.session-test
  "Short-term memory: the live session tally, and its distillation into the
  long-term store.

  The supervisor's job is to notice what is going wrong and change it, and it
  had two blind spots this closes. It could see THAT a run was going badly and
  not WHERE — a branch losing a third of its turns to unparseable calls and one
  losing them to a failing test look identical at the outcome level, and want
  opposite repairs. And it could not tell whether its own last change helped."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [samizdat.lexicon :as lexicon]
            [samizdat.session :as session]
            [samizdat.store.db :as db]
            [samizdat.store.knowledge :as knowledge]))

(use-fixtures :each (fn [f] (session/reset!) (f) (session/reset!)))

(deftest the-tally-counts-tools-by-outcome-and-mechanics-separately
  ;; Both axes matter. WHICH tool and HOW it went are different diagnoses, and
  ;; the harness's own failure modes (a fence that did not parse) are the ones
  ;; a supervisor is least able to infer from outcomes.
  (session/observe-turn! {:tool "eval" :category :success :signals {}})
  (session/observe-turn! {:tool "eval" :category :failure :signals {}})
  (session/observe-turn! {:tool "shell" :category :success
                          :signals {:parse-error true :auto-repaired true}})
  (let [snap (session/snapshot)]
    (is (= 3 (:turns snap)))
    (is (= {:success 1 :failure 1} (get-in snap [:tools "eval"])))
    (is (= 1 (get-in snap [:signals :parse-error])))
    (is (= 1 (get-in snap [:signals :auto-repaired])))))

(deftest a-mark-turns-the-tally-into-an-experiment
  ;; The whole point. A supervisor marks when it intervenes, and the delta is
  ;; the evidence for whether the change helped — rather than a feeling.
  (dotimes [_ 4] (session/observe-turn! {:tool "eval" :category :failure :signals {}}))
  (session/mark! "before-fix")
  (session/observe-turn! {:tool "eval" :category :success :signals {}})
  (let [d (session/since "before-fix")]
    (is (= 1 (:turns d)))
    (is (= {:success 1} (get-in d [:tools "eval"]))
        "only what changed — the four earlier failures are not the delta")
    (is (nil? (get-in d [:tools "eval" :failure]))
        "a report of everything that did NOT change is how a signal gets lost")))

(deftest an-unchanged-counter-is-absent-from-the-delta-not-zero
  (session/observe-turn! {:tool "eval" :category :success :signals {}})
  (session/mark! "m")
  (is (nil? (session/since "m")) "nothing happened, so there is nothing to report")
  (is (nil? (session/since "never-marked"))))

(deftest re-marking-compares-against-the-most-recent-intervention
  ;; A supervisor marking each round wants the delta since its LAST change, not
  ;; since its first — otherwise every round looks like progress.
  (session/observe-turn! {:tool "eval" :category :failure :signals {}})
  (session/mark! "sup")
  (session/observe-turn! {:tool "eval" :category :failure :signals {}})
  (session/mark! "sup")
  (session/observe-turn! {:tool "eval" :category :success :signals {}})
  (is (= {:success 1} (get-in (session/since "sup") [:tools "eval"]))))

(deftest findings-fire-on-thresholds-and-name-what-they-saw
  (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :success
                                         :signals {:parse-error true}}))
  (let [fs (session/findings)
        kinds (set (map :kind fs))]
    (is (contains? kinds :calls-not-parsing))
    (is (every? #(seq (:detail %)) fs) "a finding says what it saw")
    (is (every? #(seq (:evidence %)) fs) "and shows the numbers behind it")))

(deftest a-healthy-session-produces-no-findings
  ;; Not an empty page of zeroes: a supervisor shown noise every turn will stop
  ;; reading the block.
  (dotimes [_ 10] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (is (empty? (session/findings))))

(deftest successes-are-reported-too
  ;; A supervisor shown only what is broken will keep changing things that work.
  (dotimes [_ 5] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (dotimes [_ 3] (session/observe! [:verify :green]))
  (is (some #(= :verification-working (:kind %)) (session/findings))))

(deftest the-block-is-empty-until-something-has-happened
  (is (nil? (session/render "m")))
  (session/observe-turn! {:tool "eval" :category :success :signals {}})
  (is (str/includes? (session/render nil) "1 turns")))

(deftest an-unmeasured-change-is-reported-as-such-not-as-success
  ;; The case a supervisor most needs stated plainly, because the temptation is
  ;; to stack another change on an unmeasured one.
  ;;
  ;; This used to assert a bare `since the mark` delta. The experiment block
  ;; replaced it and subsumes it: it carries the same before/after numbers AND
  ;; what was changed and what it was expected to do, which is the difference
  ;; between a measurement and an experiment.
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (session/experiment! "fresh" {:change "c" :hypothesis "h"})
  (session/observe-turn! {:tool "eval" :category :success :signals {}})
  (let [block (session/render nil)]
    (is (str/includes? block "too early"))
    (is (str/includes? block "do not stack another change"))))

;; --- distillation -----------------------------------------------------------

(deftest a-finding-becomes-a-memory-once-not-once-per-run
  ;; The same pattern recurring is not new knowledge — it is the same
  ;; knowledge, confirmed. Recurrence should show up as a RECORD, not volume.
  (let [c (db/open! ":memory:")]
    (try
      (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :success
                                             :signals {:parse-error true}}))
      (let [first-pass (knowledge/distill! c (session/findings) {:run-id "r1"})
            second-pass (knowledge/distill! c (session/findings) {:run-id "r2"})]
        (is (seq first-pass))
        (is (every? (complement :repeat?) first-pass))
        (is (every? :repeat? second-pass) "the second run confirms, it does not duplicate")
        (is (= (set (map :id first-pass)) (set (map :id second-pass))))
        (is (= 1 (count (knowledge/recent c 20)))))
      (finally (db/close c)))))

(deftest a-distilled-finding-is-episodic-not-a-rule
  ;; What a session measured is a thing that happened, not a rule. Promoting an
  ;; episode to a standing rule is a judgement, and judgement is the
  ;; supervisor's — it has `remember` for that.
  (let [c (db/open! ":memory:")]
    (try
      (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :success
                                             :signals {:truncated true}}))
      (knowledge/distill! c (session/findings) {:run-id "r1"})
      (is (every? #(= "episodic" (:kind %)) (knowledge/recent c 20)))
      (finally (db/close c)))))

;; --- selection: fitness, experiments, verdicts -------------------------------

(deftest fitness-scores-a-tally-per-turn-so-runs-of-different-lengths-compare
  (dotimes [_ 4] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (let [good (session/fitness)]
    (session/reset!)
    (dotimes [_ 20] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
    (is (< (Math/abs (- good (session/fitness))) 1e-9)
        "a longer run of the same quality scores the same — otherwise a long
         bad run would outscore a short good one")))

(deftest an-empty-tally-has-no-fitness-rather-than-a-neutral-one
  ;; No turns is the ABSENCE of a measurement. A supervisor shown 0.0 would
  ;; read it as neutral and act on it.
  (is (nil? (session/fitness))))

(deftest the-weights-encode-the-judgements-worth-defending
  (let [w (:weights (lexicon/policy :fitness))]
    (is (< (:tool-mechanics w) (:tool-failure w))
        "a malformed call is worse than a failed one: the failure TESTED
         something and came back negative, the malformed call produced no
         evidence and cost the same turn")
    (is (< (:verify-skipped w) (:verify-red w))
        "a red test is the gate WORKING; skipped means it was asked, could not
         answer, and the work shipped anyway")
    (is (< (:parse-error w) (:truncated w))
        "truncation is lighter and separate — the repair is more tokens, not
         more steering, so it should push a different lever")))

(deftest an-experiment-binds-a-change-to-what-happened-after-it
  ;; The selection step. Variation and measurement both existed; what was
  ;; missing was the binding, without which a change is made and never judged.
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :mechanics
                                         :signals {:parse-error true}}))
  (session/experiment! "widen" {:change "budget 50k -> 80k"
                                :hypothesis "fewer calls will fail to parse"})
  (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (let [v (session/verdict "widen")]
    (is (= :better (:verdict v)))
    (is (< (:before v) (:after v)))
    (is (= "budget 50k -> 80k" (:change v)))
    (is (seq (:hypothesis v)) "a change with no stated expectation cannot be wrong")))

(deftest a-change-that-moved-nothing-is-unchanged-not-better
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (session/experiment! "noop" {:change "reworded a prompt" :hypothesis "nothing"})
  (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (is (= :unchanged (:verdict (session/verdict "noop")))))

(deftest a-change-that-made-things-worse-says-so
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (session/experiment! "bad" {:change "narrowed the budget" :hypothesis "cheaper"})
  (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :mechanics
                                         :signals {:parse-error true}}))
  (is (= :worse (:verdict (session/verdict "bad")))))

(deftest too-early-is-a-real-verdict
  ;; A supervisor that reads three turns of noise as a result will keep
  ;; changing things on the strength of nothing.
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (session/experiment! "fresh" {:change "x" :hypothesis "y"})
  (session/observe-turn! {:tool "eval" :category :success :signals {}})
  (is (= :too-early (:verdict (session/verdict "fresh")))))

(deftest the-block-tells-the-supervisor-what-each-verdict-obliges
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (session/experiment! "noop" {:change "c" :hypothesis "h"})
  (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (let [block (session/render nil)]
    (is (str/includes? block "fitness"))
    (is (str/includes? block "noop"))
    (is (str/includes? block "revert")
        "an unjustified change is debt, and the block has to say so")))

(deftest a-losing-change-keeps-being-raised-until-it-is-settled
  ;; The one thing a supervisor under selection pressure must not be able to
  ;; quietly skip. A change measured and found wanting, then left in place, is
  ;; worse than one nobody measured: the loop carries a modification the
  ;; evidence says is not helping, and the next supervisor inherits it with no
  ;; sign it was ever questioned.
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (session/experiment! "narrow" {:change "width 5 -> 2" :hypothesis "cheaper"})
  (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :mechanics
                                         :signals {:parse-error true}}))
  (is (= :worse (:verdict (session/verdict "narrow"))))
  (is (= 1 (count (session/unsettled-losses))))
  (is (str/includes? (session/render nil) "have not acted on them"))

  (testing "settling it stops the nag — a block that repeats itself forever
            trains a reader to skip it, which is the opposite of the point"
    (session/reverted! "narrow" false)
    (is (empty? (session/unsettled-losses)))
    (is (str/includes? (session/render nil) "(reverted)"))))

(deftest a-winning-change-is-never-nagged-about
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :mechanics
                                         :signals {:parse-error true}}))
  (session/experiment! "fix" {:change "widened the budget" :hypothesis "fewer failures"})
  (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (is (= :better (:verdict (session/verdict "fix"))))
  (is (empty? (session/unsettled-losses))))

(deftest an-unfinished-experiment-is-not-nagged-about-either
  ;; `too-early` has concluded nothing, and demanding action on it would push
  ;; the supervisor to decide before the evidence exists.
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (session/experiment! "fresh" {:change "c" :hypothesis "h"})
  (session/observe-turn! {:tool "eval" :category :success :signals {}})
  (is (empty? (session/unsettled-losses))))

(deftest culling-a-branch-that-held-evidence-is-a-sharpening-failure
  ;; papers/2608.17981v1 §4.5.4, after Yue et al. on pass@k. A beam fails two
  ;; ways and they want opposite fixes: no branch ever held the answer
  ;; (expansion — widen, diversify), or one did and the harness threw it away
  ;; (sharpening — fix the rubric and the cull thresholds). End-to-end success
  ;; cannot tell them apart, and widening a beam that is losing to selection
  ;; buys nothing but cost.
  (dotimes [_ 10] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (dotimes [_ 3] (session/observe! [:beam :culled-with-evidence]))
  (let [f (first (filter #(= :selection-losing-evidence (:kind %)) (session/findings)))]
    (is (some? f) "a branch culled while holding confirmed artifacts is the signal")
    (is (str/includes? (:detail f) "SHARPENING"))
    (is (str/includes? (:detail f) "widening the beam will not help")
        "the finding has to name the fix it rules OUT, or it will be read as
         an argument for a wider beam")))

(deftest a-verdict-becomes-a-lever-fact-that-outlives-the-session
  ;; The heredity of selection. Without it a lever that was tried and made
  ;; things worse is forgotten by the next session, which is free to try it
  ;; again — variation and measurement without inheritance is thrashing with
  ;; statistics.
  (let [c (db/open! ":memory:")]
    (try
      (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
      (session/experiment! "narrow" {:change "beam width 5 -> 2" :hypothesis "cheaper"})
      (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :mechanics
                                             :signals {:parse-error true}}))
      (let [written (knowledge/distill-verdicts! c (session/experiments) {:run-id "r1"})]
        (is (= 1 (count written)))
        (is (= :worse (:verdict (first written))))
        (let [row (knowledge/get-by-id c (:id (first written)))]
          (is (= "procedural" (:kind row))
              "a verdict is a fact about a LEVER and holds beyond the run that
               found it — unlike a session finding, which is an episode")
          (is (= 1 (:failure_count row)) "the verdict IS the record")))

      (testing "trying the same lever again confirms rather than duplicates"
        (session/reset!)
        (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
        (session/experiment! "narrow-again" {:change "beam width 5 -> 2"
                                             :hypothesis "maybe this time"})
        (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :mechanics
                                               :signals {:parse-error true}}))
        (knowledge/distill-verdicts! c (session/experiments) {:run-id "r2"})
        (let [rows (filter #(= "procedural" (:kind %)) (knowledge/recent c 20))]
          (is (= 1 (count rows)) "one lever, one memory")
          (is (= 2 (:failure_count (first rows)))
              "and a lever that keeps failing sinks in the ranking on its own")))
      (finally (db/close c)))))

(deftest an-unfinished-experiment-teaches-nothing-and-is-not-written
  (let [c (db/open! ":memory:")]
    (try
      (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
      (session/experiment! "fresh" {:change "c" :hypothesis "h"})
      (session/observe-turn! {:tool "eval" :category :success :signals {}})
      (is (empty? (knowledge/distill-verdicts! c (session/experiments) {:run-id "r1"}))
          "recording it would teach the next session that the lever was tested
           when it was not")
      (finally (db/close c)))))

(deftest only-one-change-may-be-in-flight-and-that-is-enforced
  ;; The supervisor prompt has always said "one change per round". backpass's
  ;; VISION puts the general principle sharply: a rule the model can decline is
  ;; not a rule. Two changes measured over the same interval tell you nothing
  ;; about either, so letting them stack quietly destroys the measurement the
  ;; whole mechanism exists for.
  (dotimes [_ 6] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (session/experiment! "one" {:change "a" :hypothesis "h"})
  (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :mechanics
                                         :signals {:parse-error true}}))
  (is (thrown? clojure.lang.ExceptionInfo
               (session/experiment! "two" {:change "b" :hypothesis "h"})))
  (testing "settling the first frees the slot — the cap paces changes, it does
            not forbid them"
    (session/reverted! "one" false)
    (session/experiment! "two" {:change "b" :hypothesis "h"})
    (is (some #(= "two" (:name %)) (session/experiments)))))

(deftest an-unfinished-experiment-does-not-hold-the-slot
  ;; `too-early` has concluded nothing. Blocking on it would leave the
  ;; supervisor unable to act for as long as the run is short.
  (session/experiment! "fresh" {:change "a" :hypothesis "h"})
  (session/observe-turn! {:tool "eval" :category :success :signals {}})
  (session/experiment! "another" {:change "b" :hypothesis "h"})
  (is (some #(= "another" (:name %)) (session/experiments))
      "blocking on an unfinished experiment would leave the supervisor unable
       to act for as long as the run is short"))

(deftest both-drivers-distil-the-session
  ;; Found live. The beam distilled at run end and workflow/run! — the
  ;; single-branch driver, which is what the factory loop uses and therefore
  ;; what most runs are — did not. A run completed, produced a finding, and
  ;; formed no memory at all: the short-term half worked, the long-term half
  ;; was never reached, and nothing said so.
  ;;
  ;; The bridge existing in one driver is the same as not existing, for every
  ;; run that uses the other. So it is one function now, and this pins that it
  ;; writes both halves.
  (let [c (db/open! ":memory:")]
    (try
      (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :mechanics
                                             :signals {:parse-error true}}))
      (session/experiment! "x" {:change "a lever" :hypothesis "h"})
      (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
      (let [{:keys [findings verdicts]}
            (knowledge/distil-session! c {:run-id "r1"
                                          :findings (session/findings)
                                          :experiments (session/experiments)})]
        (is (seq findings) "the patterns it measured")
        (is (seq verdicts) "and the verdict on what the supervisor changed"))
      (finally (db/close c)))))

(deftest a-finding-is-evaluated-over-the-run-not-the-whole-session
  ;; Found live, and it is the reason the starved run produced no finding: a
  ;; run given 48 max-tokens returned an empty provider reply, the counter
  ;; recorded it correctly, and nothing fired — one bad turn in thirty-six
  ;; cumulative is under every threshold. The session had been healthy for two
  ;; runs and the arithmetic said so.
  ;;
  ;; Rates over an unbounded window under-react to recent change. The RUN is
  ;; the natural window, because "this run is going badly" is the actionable
  ;; statement and "this process has been fine on average" is not.
  (dotimes [_ 30] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
  (session/mark-run! "r3")
  (dotimes [_ 4] (session/observe! [:provider :empty-reply])
                 (session/observe-turn! {:tool "__provider_error__" :category :neutral
                                         :signals {}}))
  (is (empty? (session/findings))
      "diluted across a long healthy session — correct for the supervisor's
       cross-run view, useless for noticing that things just broke")
  (is (some #(= :provider-empty-replies (:kind %))
            (session/findings (session/run-window "r3")))
      "and visible immediately over the run that is actually going wrong"))

(deftest an-unmarked-run-falls-back-to-the-whole-session
  ;; A caller that never marked — a test, a REPL call — must still get an
  ;; answer rather than an empty window that reads as "nothing wrong".
  (dotimes [_ 8] (session/observe-turn! {:tool "eval" :category :mechanics
                                         :signals {:parse-error true}}))
  (is (seq (session/findings (session/run-window "never-marked")))))

(deftest steering-ignored-counts-across-gates-not-within-one
  ;; gate-dead needs ONE gate to fire :min-gate-firings times. A branch
  ;; ignoring five different gates twice each trips none of them, which is the
  ;; shape a live run took: 38 turns, five gates, eight firings, not one file
  ;; written, and no finding at all — the harness counted the nags separately
  ;; while the branch ignored them equally.
  (testing "the live shape now raises a finding"
    (let [fs (session/findings
              {:turns 38
               :gates {:progress-stalled {:fired 2} :reflection {:fired 2}
                       :turn-budget {:fired 2} :wind-down {:fired 1}
                       :last-call {:fired 1}}})
          f (first (filter #(= :steering-ignored (:kind %)) fs))]
      (is (some? f))
      (is (= :high (:severity f)))
      (is (= {:fired 8 :met 0 :turns 38} (:evidence f)))))
  (testing "a branch that acts on what it is told raises nothing"
    (is (empty? (filter #(= :steering-ignored (:kind %))
                        (session/findings {:turns 20 :gates {:a {:fired 6 :met 5}}})))))
  (testing "met-late still counts as met — acting a turn later is acting"
    (is (empty? (filter #(= :steering-ignored (:kind %))
                        (session/findings {:turns 20 :gates {:a {:fired 6 :met-late 5}}})))))
  (testing "below the firing floor nothing is claimed either way"
    (is (empty? (filter #(= :steering-ignored (:kind %))
                        (session/findings {:turns 8 :gates {:a {:fired 2}}}))))))

(deftest run-end-distillation-reads-the-run-window-not-the-process-tally
  ;; karamazov-blt.24: both drivers passed (session/findings) — the whole-
  ;; process snapshot — to distil-session!, so run 1's parse-error rate kept
  ;; a finding above threshold at the end of clean runs 2..n, and each end-of-
  ;; run distillation corroborated it with a DISTINCT run-id, defeating the
  ;; distinct-run guard. They pass (session/run-window run-id) now; this pins
  ;; the window semantics that fix relies on.
  (session/reset!)
  (dotimes [_ 10]
    (session/observe-turn! {:tool "eval" :category :mechanics
                            :signals {:parse-error true}}))
  (is (seq (session/findings))
      "the whole-process tally reports the bad old run's pattern")
  (session/mark-run! "clean-run")
  (is (empty? (session/findings (session/run-window "clean-run")))
      "a clean later run inherits none of it"))

;; --- per-branch fitness (RFC-012 F3, karamazov-ts3o.2) ------------------------

(deftest a-branch-has-its-own-tally-scored-by-the-same-fitness
  ;; The number selection and evaluation share: the same counters cut per
  ;; branch, the same function, the same weights. Keyed by run AND branch,
  ;; because branch ids repeat across runs.
  (session/observe-turn! {:tool "eval" :category :success :signals {} :branch ["r1" "B1"]})
  (session/observe-turn! {:tool "eval" :category :success :signals {} :branch ["r1" "B1"]})
  (session/observe-turn! {:tool "eval" :category :failure
                          :signals {:parse-error true} :branch ["r1" "B2"]})
  (session/observe! [:verify :green] ["r1" "B1"])
  (session/observe! [:provider :empty-reply] ["r1" "B2"])
  (session/observe-turn! {:tool "eval" :category :failure :signals {} :branch ["r2" "B1"]})
  (testing "each branch's tally is the tally shape, cut to what it did"
    (is (= 2 (:turns (session/branch-tally "r1" "B1"))))
    (is (= {:success 2} (get-in (session/branch-tally "r1" "B1") [:tools "eval"])))
    (is (= 1 (get-in (session/branch-tally "r1" "B1") [:verify :green])))
    (is (= 1 (get-in (session/branch-tally "r1" "B2") [:signals :parse-error])))
    (is (= 1 (get-in (session/branch-tally "r1" "B2") [:provider :empty-reply]))))
  (testing "fitness is fitness-of over that tally, so it is comparable to the session's"
    (is (= (session/fitness-of (session/branch-tally "r1" "B1"))
           (session/branch-fitness "r1" "B1")))
    (is (> (session/branch-fitness "r1" "B1") (session/branch-fitness "r1" "B2")))
    (is (= ["B1" "B2"] (keys (session/branch-fitnesses "r1")))))
  (testing "the same branch id in another run is another branch"
    (is (= 1 (:turns (session/branch-tally "r2" "B1"))))
    (is (= ["B1"] (keys (session/branch-fitnesses "r2")))))
  (testing "a branch nothing was counted for has no fitness, not a neutral one"
    (is (nil? (session/branch-fitness "r1" "B9")))
    (is (nil? (session/branch-tally "r1" "B9"))))
  (testing "the process-wide tally counts everything once and shows no branches"
    (is (= 4 (:turns (session/snapshot))))
    (is (nil? (:branches (session/snapshot))))
    (session/mark! "m")
    (session/observe-turn! {:tool "eval" :category :success :signals {} :branch ["r1" "B1"]})
    (is (= {:turns 1 :tools {"eval" {:success 1}}} (session/since "m"))
        "a delta is the process-wide counts and nothing per branch"))
  (testing "a finished run's branch tallies are dropped, the session's counts stay"
    (session/forget-run! "r1")
    (is (nil? (session/branch-tally "r1" "B1")))
    (is (= 1 (:turns (session/branch-tally "r2" "B1"))))
    (is (= 5 (:turns (session/snapshot))))))

(deftest a-count-with-no-branch-lands-in-the-process-tally-only
  (session/observe-turn! {:tool "eval" :category :success :signals {}})
  (session/observe! [:verify :green])
  (is (= 1 (:turns (session/snapshot))))
  (is (empty? (session/branch-fitnesses "r1"))))
