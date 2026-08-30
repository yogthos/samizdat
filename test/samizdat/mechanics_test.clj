;; samizdat - a claim-first verification harness
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

(ns samizdat.mechanics-test
  "The no-call guards (karamazov-068): a branch that produces no usable tool
  call, and the particular way a long-running one learns to produce none.

  The live failure these pin: a supervisor with 119 turns of history, whose
  context was by then almost entirely `[unloaded]` digests standing in for
  its own past turns, started emitting digests instead of calls — eight in a
  row, answered each time with a generic 'emit a tool call' that produced
  another digest, with nothing able to stop it because no call ever reached
  dispatch."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.loop :as aloop]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.telemetry :as telemetry]
            [cells.board :as board]
            [samizdat.agent.supervisor :as supervisor]
            [samizdat.llm.fence :as fence]
            [samizdat.llm.message :as message]
            [samizdat.store.journal :as journal]))

;; --- the marker announces itself before it says anything else ---------------

(deftest the-unloaded-marker-leads-the-line
  (let [msgs [{:role "system" :content "sys"}
              {:role "user" :content "problem"}
              {:role "assistant" :content (apply str (repeat 400 "a"))}
              {:role "user" :content (apply str (repeat 400 "b")) :turn 1}
              {:role "assistant" :content "recent" :turn 9}
              {:role "user" :content "newest" :turn 9}]
        out (message/compact msgs [{:turn 1 :tool "grep" :category :neutral}]
                             {:keep-pairs 1 :threshold-chars 10})
        compacted (filter message/unloaded? (map :content out))]
    (is (seq compacted) "something was unloaded at this threshold")
    (doseq [c compacted]
      (is (str/starts-with? c "[unloaded]")
          (str "the marker must lead, so a model reading left to right sees "
               "harness bookkeeping before it sees imitable content: " c)))))

(deftest unloaded?-recognizes-the-harness-marker
  (is (message/unloaded? "[unloaded] t69 cell → neutral"))
  (is (message/unloaded? "```tool-call\n<tool_call> [unloaded]")
      "the trailing form too — a branch compacted before this change carries it")
  (is (not (message/unloaded? "a perfectly ordinary reply")))
  (is (not (message/unloaded? nil))))

;; --- the imitation gets its own answer, and its own prefill treatment -------

(defn- no-call
  "Drive no-call-step over a stubbed journal, returning the branch."
  [said]
  (with-redefs [journal/record-turn! (fn [& _] nil)]
    (aloop/no-call-step {} (state/new-branch {:id "S0" :problem "p"}) 5
                        {:parsed nil :signals {} :said said :response {}})))

(deftest an-imitated-digest-gets-a-specific-complaint
  (let [b (no-call "```tool-call\n<tool_call> [unloaded]")
        msg (str (:content (last (:messages b))))]
    (is (str/includes? msg "bookkeeping")
        "named for what it is — told merely to emit a tool call, the model
         emits another digest")
    (is (not (str/includes? msg "No ```tool-call block"))
        "not the generic complaint")))

(deftest an-imitation-turn-is-not-prefilled
  ;; The prefill is half the trap: opening inside a fence, with a context of
  ;; digest lines, the likeliest continuation is another digest.
  (is (nil? (:prefill (no-call "[unloaded] t70 cell → neutral")))
      "a clean slate to reason in")
  (is (= "```tool-call\n" (:prefill (no-call "I think I should probably...")))
      "an ordinary no-call still has prose withheld — that is what works"))

(deftest a-truncated-reply-is-still-a-truncation
  ;; Truncation outranks imitation: a reply cut off mid-digest wants more
  ;; tokens, not a lecture about bookkeeping.
  (with-redefs [journal/record-turn! (fn [& _] nil)]
    (let [b (aloop/no-call-step {} (state/new-branch {:id "S0" :problem "p"}) 5
                                {:parsed nil :signals {:truncated true}
                                 :said "[unloaded] t1 grep" :response {}})
          msg (str (:content (last (:messages b))))]
      (is (str/includes? msg "token limit")))))

;; --- the streak rung: something bites wherever the arbiter runs -------------

(deftest the-mechanics-streak-gate-forces-an-honest-give-up
  (let [g (first (filter #(= :mechanics-streak (:gate %)) (gates/gates)))
        th (gates/threshold :mechanics-streak-threshold)
        fires? (fn [n] (boolean ((:when g) {:branch (assoc (state/new-branch
                                                            {:id "S0" :problem "p"})
                                                           :consecutive-mechanics-failures n)})))]
    (is (some? g) "the gate exists in gates.edn")
    (is (= "give_up" (:tool g)))
    (is (some? (gates/threshold (:budget g))))
    (is (fires? th))
    (is (not (fires? (dec th))))
    (testing "it outranks storm — a branch making no calls at all is further
              gone than one repeating a call, and the storm window cannot see
              it because nothing reaches dispatch"
      (let [storm (first (filter #(= :storm (:gate %)) (gates/gates)))]
        (is (< (:priority g) (:priority storm)))))))

(deftest the-streak-counter-clears-on-any-well-formed-call
  ;; The rung must not fire on a branch that recovered: record-outcome clears
  ;; the mechanics tally on any category that reached a tool.
  (let [b (-> (state/new-branch {:id "S0" :problem "p"})
              (state/record-outcome {:category :mechanics :progress? false})
              (state/record-outcome {:category :mechanics :progress? false})
              (state/record-outcome {:category :neutral :progress? false}))]
    (is (zero? (:consecutive-mechanics-failures b)))))

;; --- karamazov-3y5 -----------------------------------------------------------
;; Context pressure was measured only by hitting the wall: the overflow came
;; back from the provider, and only THEN was the budget squeezed — after the
;; failed request and its backoff were already spent.

(deftest context-pressure-is-graded-against-the-operating-ceiling
  (let [policy {:operating-ceiling 1000 :advisory 0.75 :urgent 0.90}]
    (is (nil? (state/context-pressure 500 policy)) "well under: nothing")
    (is (nil? (state/context-pressure 749 policy)) "just under advisory")
    (is (= :advisory (state/context-pressure 750 policy)))
    (is (= :advisory (state/context-pressure 899 policy)))
    (is (= :urgent (state/context-pressure 900 policy)))
    (is (= :urgent (state/context-pressure 1000 policy)) "at the ceiling, not over")
    (is (= :over (state/context-pressure 1001 policy)))
    (testing "no measurement is not pressure"
      (is (nil? (state/context-pressure nil policy)))
      (is (nil? (state/context-pressure 0 policy))))
    (testing "no ceiling configured means the signal is off, not always-on"
      (is (nil? (state/context-pressure 999999 {:operating-ceiling 0
                                                :advisory 0.75 :urgent 0.9}))))))

;; --- the no-edits nudge ------------------------------------------------------
;; Run c99c6fd6 spent turns 45-50 in the REPL — six evals around one arity bug
;; in a function it had never written down — while write_file sat at 3 from
;; turn 25. The studying gate could not see it: `eval` is work, the branch was
;; not idle, and studying's threshold is 10. This is the narrower question,
;; asked sooner: you are busy, and none of it is landing on disk.

(defn- turns-of [& tools] (mapv (fn [t] {:tool t}) tools))

(deftest no-edits-fires-only-once-the-branch-has-written-something
  (let [vocab (gates/tool-vocab :file-write)
        n (gates/threshold :no-edit-turns)
        fires? (fn [turns] (supervisor/over-studying? vocab turns n))]
    (testing "opening exploration is never nagged — nothing written yet"
      (is (not (fires? (apply turns-of (repeat 12 "eval")))))
      (is (not (fires? (apply turns-of (repeat 12 "read_file"))))))
    (testing "fires once it HAS written and then stopped for the threshold"
      (is (fires? (apply turns-of "write_file" (repeat n "eval")))))
    (testing "quiet while it is still writing"
      (is (not (fires? (turns-of "write_file" "eval" "write_file" "eval")))))
    (testing "under the threshold is not yet a stall"
      (is (not (fires? (apply turns-of "write_file" (repeat (dec n) "eval"))))))
    (testing "patch counts as writing — it is a file tool like the others"
      (is (not (fires? (apply turns-of "write_file" "eval" "patch"
                              (repeat (dec n) "eval")))))
      (is (fires? (apply turns-of "patch" (repeat n "eval")))))))

(deftest the-no-edits-gate-is-declared-and-settleable
  (let [g (first (filter #(= :no-edits (:gate %)) (gates/gates)))]
    (is (some? g) "the gate exists in gates.edn")
    (is (= :max-no-edit-nudges (:budget g)) "and is bounded, so it cannot nag forever")
    (is (some? (:prediction g)) "it declares a prediction, like every gate")
    (is (= #{"write_file" "edit_file" "patch"}
           (get (gates/tool-vocab :settle-called) :no-edits))
        "settled by the thing it asks for: a file actually written")))

;; --- gate health, per branch -------------------------------------------------
;; Run ace34d83: :no-edits fired 6 times and was ignored 3 of them — every one
;; of those by T0, which had stalled. S0 got the same nudge and complied. Rolled
;; up per GATE across the run, met > 0, so the gate read as working and the
;; watch never raised :gate-ignored. The branch that was actually stuck was
;; invisible, and the supervisor's digest carried no gate health at all.

(deftest gate-health-is-reported-per-branch
  (let [rows [{:gate "no-edits" :branch_id "T0" :outcome "unmet"}
              {:gate "no-edits" :branch_id "T0" :outcome "unmet"}
              {:gate "no-edits" :branch_id "T0" :outcome "unmet"}
              {:gate "no-edits" :branch_id "S0" :outcome "met"}
              {:gate "studying" :branch_id "T0" :outcome "unmet"}
              {:gate "milestone" :branch_id "S0" :outcome "met-late"}]
        h (telemetry/gate-health rows)]
    (testing "a gate ignored by one branch is not hidden by another obeying it"
      (is (= {:fired 3 :met 0 :unmet 3} (select-keys (get h ["T0" "no-edits"])
                                                     [:fired :met :unmet])))
      (is (= {:fired 1 :met 1 :unmet 0} (select-keys (get h ["S0" "no-edits"])
                                                     [:fired :met :unmet]))))
    (testing "met-late counts as met — the advice worked, the window was wrong"
      (is (= 1 (:met (get h ["S0" "milestone"])))))))

(deftest the-digest-names-a-gate-a-branch-is-ignoring
  (let [rows (concat (repeat 3 {:gate "no-edits" :branch_id "T0" :outcome "unmet"})
                     [{:gate "no-edits" :branch_id "S0" :outcome "met"}])
        line (telemetry/gate-lines (telemetry/gate-health rows) 3)]
    (is (string? line))
    (is (re-find #"T0" line) "the branch that is ignoring it is named")
    (is (re-find #"no-edits" line))
    (is (not (re-find #"S0" line))
        "a branch that obeyed the same gate is not reported as ignoring it"))
  (testing "nothing to say when every gate is being obeyed"
    (is (nil? (telemetry/gate-lines
               (telemetry/gate-health [{:gate "g" :branch_id "B" :outcome "met"}])
               3))))
  (testing "under the firing floor is not yet evidence"
    (is (nil? (telemetry/gate-lines
               (telemetry/gate-health [{:gate "g" :branch_id "B" :outcome "unmet"}])
               3)))))

;; --- karamazov-gez: a budget bounds the EPISODE, not the run -----------------
;; Run bd56a286: :no-edits fired 3 times (its whole budget), was obeyed once,
;; and then the branch went 143 turns without writing a file with the gate
;; permanently silent. The bound exists so a nudge cannot become the thing the
;; model answers instead of the work — that reasoning is about one episode of
;; nagging, and it was implemented as a bound on the run.

(deftest a-gate-budget-re-arms-when-its-prediction-is-met
  (let [g {:gate :no-edits :budget :max-no-edit-nudges}
        cap (gates/threshold :max-no-edit-nudges)
        hist (fn [& es] {:gate-history (vec es)})
        fire (fn [t] {:gate :no-edits :turn t})
        met  (fn [t] {:gate :no-edits :turn t :settled :met})]
    (testing "under the cap it may still fire"
      (is (not (gates/budget-exceeded? g (apply hist (map fire (range 1 cap)))))))
    (testing "at the cap it is spent"
      (is (gates/budget-exceeded? g (apply hist (map fire (range 1 (inc cap)))))))
    (testing "a MET settlement ends the episode and re-arms the budget"
      (is (not (gates/budget-exceeded?
                g (apply hist (concat (map fire (range 1 (inc cap)))
                                      [(met (+ cap 1))]))))
          "the stall ended; a later stall gets its own budget"))
    (testing "and the new episode is bounded again"
      (is (gates/budget-exceeded?
           g (apply hist (concat (map fire (range 1 (inc cap)))
                                 [(met 100)]
                                 (map fire (range 101 (+ 101 cap))))))
          "re-arming must not mean unlimited"))
    (testing "another gate's settlement does not re-arm this one"
      (is (gates/budget-exceeded?
           g (apply hist (concat (map fire (range 1 (inc cap)))
                                 [{:gate :studying :turn 50 :settled :met}])))))))

;; --- the REPL session contract ----------------------------------------------
;; Run bd56a286 is the case this exists for. T0 spent 238 turns in the REPL
;; hunting a bug that was in its own tests; T1 read for 316 turns and wrote
;; nothing. Both were reachable only by nudges, and nudges are 0-for-6
;; (karamazov-qic). The answer is not a louder nudge, it is a CONTRACT: say
;; which files you are going to change before you start exploring, and land
;; them before you finish. Declaring a file is a HYPOTHESIS about where the
;; problem is, which is exactly what T0 never formed (karamazov-70b).

(deftest a-repl-session-declares-before-it-explores
  (testing "a branch with no plan has not entered a session"
    (is (nil? (state/plan {})))
    (is (not (state/planned? {}))))
  (testing "declaring records the files and the tests"
    (let [b (state/declare-plan {} {:files ["src/fps/level.clj"]
                                    :tests ["test/fps/level_test.clj"]
                                    :goal "fix wall collision"})]
      (is (state/planned? b))
      (is (= "fix wall collision" (:goal (state/plan b))))
      ;; Tests are part of what must LAND. A declared test nobody writes is
      ;; exactly the gap this contract closes, so :files is everything owed
      ;; and :tests is the subset that are tests.
      (is (= ["src/fps/level.clj" "test/fps/level_test.clj"]
             (:files (state/plan b))))
      (is (= ["test/fps/level_test.clj"] (:tests (state/plan b))))
      (is (= 2 (count (state/unwritten b))))))
  (testing "a plan with no files is not a plan — the point is committing to a file"
    (is (not (state/planned? (state/declare-plan {} {:files [] :goal "poke about"}))))
    (is (not (state/planned? (state/declare-plan {} {:goal "poke about"}))))))

(deftest a-session-is-not-done-until-the-declared-files-are-written
  (let [b (state/declare-plan {} {:files ["src/a.clj" "test/a_test.clj"]})]
    (testing "nothing written yet: both outstanding"
      (is (= #{"src/a.clj" "test/a_test.clj"} (set (state/unwritten b)))))
    (testing "writing one leaves the other"
      (let [b (state/note-write b "src/a.clj")]
        (is (= ["test/a_test.clj"] (state/unwritten b)))))
    (testing "writing both closes the session"
      (let [b (-> b (state/note-write "src/a.clj") (state/note-write "test/a_test.clj"))]
        (is (empty? (state/unwritten b)))))
    (testing "a write nobody declared does not discharge a declared file"
      (let [b (state/note-write b "src/unrelated.clj")]
        (is (= 2 (count (state/unwritten b))))))
    (testing "paths are compared leniently — ./src/a.clj is src/a.clj"
      (let [b (state/note-write b "./src/a.clj")]
        (is (= ["test/a_test.clj"] (state/unwritten b)))))))

(deftest re-declaring-replaces-the-plan-rather-than-accreting
  ;; A model that learns the bug is elsewhere must be able to say so. The
  ;; contract is "commit to a hypothesis", not "never change your mind".
  (let [b (-> (state/declare-plan {} {:files ["src/a.clj"]})
              (state/note-write "src/a.clj")
              (state/declare-plan {:files ["test/a_test.clj"]}))]
    (is (= ["test/a_test.clj"] (:files (state/plan b))))
    (is (= ["test/a_test.clj"] (state/unwritten b))
        "the new plan's files are outstanding again")))

(deftest a-re-plan-does-not-un-write-what-the-branch-already-wrote
  ;; Run a3566c73, live. The branch fixed three files, had its `done` refused
  ;; for an unrelated reason, re-planned to describe what it had actually
  ;; done — and was then told it had never written the files it had just
  ;; fixed, because declare-plan reset the ledger. It spent turns rewriting
  ;; correct files to satisfy a counter, and `done` was refused twice.
  ;;
  ;; :repl-written is a fact about this branch's history, not about the
  ;; current hypothesis. Whether the diff is real is the SHIP gate's question
  ;; and it asks git; this only answers whether the branch went where it said.
  (let [b (-> (state/declare-plan {} {:files ["src/a.clj"]})
              (state/note-write "src/a.clj")
              (state/declare-plan {:files ["src/a.clj" "test/a_test.clj"]}))]
    (is (= ["test/a_test.clj"] (state/unwritten b))
        "src/a.clj was written in this run and stays discharged across the re-plan")))

;; --- the plan is a better arming signal than the write history --------------
;; :no-edits armed on "this branch has written before", which is a heuristic
;; groping for "is it supposed to be writing by now". A branch that has never
;; written was unreachable — T1 of run bd56a286 read for 316 turns and T0 of
;; run 90674c19 declared a plan and then explored 32 turns, both with the gate
;; unable to say anything (karamazov-gez). A DECLARED, UNLANDED PLAN answers
;; the question exactly: it is supposed to be writing, and it is not.

(deftest a-stale-plan-is-the-arming-signal
  (let [turns (fn [& ts] (mapv (fn [t] {:tool t}) ts))
        b (fn [plan-files & ts]
            (assoc (state/declare-plan {} {:files plan-files})
                   :turns (vec (apply turns ts))))
        vocab (gates/tool-vocab :file-write)
        n 4]
    (testing "no plan, no arming — a branch orienting is not stalled"
      (is (not (state/plan-stale? {:turns (apply turns (repeat 10 "eval"))} vocab n))))
    (testing "a plan whose files are all written is not stale"
      (let [x (-> (state/declare-plan {} {:files ["a.clj"]})
                  (state/note-write "a.clj")
                  (assoc :turns (apply turns (repeat 10 "eval"))))]
        (is (not (state/plan-stale? x vocab n)))))
    (testing "declared and unwritten, with no file-write in the window: stale"
      (is (state/plan-stale? (apply b ["a.clj"] (repeat n "eval")) vocab n)))
    (testing "under the window it is not yet stale"
      (is (not (state/plan-stale? (apply b ["a.clj"] (repeat (dec n) "eval")) vocab n))))
    (testing "a recent write clears it even with files still owed"
      (is (not (state/plan-stale? (apply b ["a.clj" "b.clj"]
                                         (concat (repeat (dec n) "eval") ["write_file"]))
                                  vocab n))))
    (testing "it arms on a branch that has NEVER written — the hole this closes"
      (let [never (apply b ["a.clj"] (repeat 20 "read_file"))]
        (is (empty? (filter #{"write_file"} (map :tool (:turns never)))))
        (is (state/plan-stale? never vocab n))))))

(deftest the-no-edits-gate-arms-both-ways
  ;; Two ways to be not-writing, and the gate must reach both. It used to
  ;; reach only the second, which is why a branch that had never written was
  ;; invisible to it (karamazov-gez).
  (let [gate (first (filter #(= :no-edits (:gate %)) (gates/gates)))
        n (gates/threshold :no-edit-turns)
        turns (fn [t k] (vec (repeat k {:tool t})))
        fires? (fn [b] (boolean ((:when gate) {:branch b})))]
    (testing "a DECLARED, unlanded plan arms it even with no write in the branch's life"
      (let [b (assoc (state/declare-plan {:status :active} {:files ["src/a.clj"]})
                     :turns (turns "read_file" (inc n)))]
        (is (empty? (filter #{"write_file"} (map :tool (:turns b))))
            "precondition: this branch has never written")
        (is (fires? b))
        (is (str/includes? ((:message gate) {:branch b}) "src/a.clj")
            "and the nudge names the file it owes, so it is answerable")))
    (testing "landing the plan disarms it"
      (let [b (-> (state/declare-plan {:status :active} {:files ["src/a.clj"]})
                  (state/note-write "src/a.clj")
                  (assoc :turns (turns "eval" (inc n))))]
        (is (not (fires? b)))))
    (testing "the history rule still covers a branch working without a plan"
      (let [b {:status :active
               :turns (into [{:tool "write_file"}] (turns "eval" n))}]
        (is (fires? b))))
    (testing "opening exploration with neither plan nor history is still not nagged"
      (is (not (fires? {:status :active :turns (turns "read_file" 20)}))))))

;; --- re-anchoring: a nudge quotes what the branch said it was doing ---------
;; 18 of 19 gates fired without naming the branch's own stated goal, so every
;; steer arrived as "you are doing badly" with no "at WHAT". A model cannot
;; compare an outcome against an intention it has to remember. A live team
;; worker went off-task on a superficially-similar recalled fix and nothing
;; re-anchored it (dogfood insight, run a3c9fd3d).

(deftest a-branch-states-its-goal-most-specific-first
  (testing "the repl plan's goal is the most immediate thing it committed to"
    (is (= "fix the wall collision"
           (state/stated-goal {:repl-plan {:goal "fix the wall collision"}
                               :task {:title "T"} :thesis {:goal "G"}
                               :problem "P"}))))
  (testing "then the claimed task"
    (is (= "T" (state/stated-goal {:task {:title "T"} :thesis {:goal "G"} :problem "P"}))))
  (testing "then the thesis"
    (is (= "G" (state/stated-goal {:thesis {:goal "G"} :problem "P"}))))
  (testing "and the run's problem is the floor, never nothing"
    (is (= "P" (state/stated-goal {:problem "P"}))))
  (testing "a branch that has stated nothing at all says so by returning nil"
    (is (nil? (state/stated-goal {})))
    (is (nil? (state/stated-goal {:repl-plan {:goal "  "} :problem ""})))))

(deftest the-steering-gates-re-anchor-to-that-goal
  (let [b {:id "b" :status :active
           :task {:id "t1" :title "MAKE-THE-WIDGET-SPIN"}
           :problem "P" :turns (vec (repeat 12 {:tool "read_file"}))
           :any-progress? true :turns-since-progress 12
           :consecutive-failures 5}
        msg (fn [g] (str ((:message (first (filter #(= g (:gate %)) (gates/gates))))
                          {:branch b :turn 5 :max-turns 50})))]
    (doseq [g [:progress-stalled :studying :stuck]]
      (is (str/includes? (msg g) "MAKE-THE-WIDGET-SPIN")
          (str g " steers without saying what the branch is supposed to be doing")))))

;; --- the other half: a failure steer names what failed ----------------------
;; Only :stuck named the failure it fired on. Every other gate said "something
;; is going wrong" and left the model to work out what — the same shape as
;; steering without a goal, one step further on. A model cannot repair a
;; failure it has to remember.

(deftest a-branch-can-name-its-most-recent-failure
  (testing "the newest failing turn, with its tool and its own words"
    (let [b {:turns [{:turn 1 :tool "eval" :category :failure :error "OLD"}
                     {:turn 2 :tool "shell" :category :success}
                     {:turn 3 :tool "edit_file" :category :failure :error "NEWEST"}]}]
      (is (= {:turn 3 :tool "edit_file" :error "NEWEST"} (state/last-failure b)))))
  (testing "mechanics count as failures to repair — a call made wrong is a thing to fix"
    (let [b {:turns [{:turn 1 :tool "eval" :category :mechanics :error "MALFORMED"}]}]
      (is (= "MALFORMED" (:error (state/last-failure b))))))
  (testing "nothing to report when nothing failed"
    (is (nil? (state/last-failure {:turns [{:turn 1 :tool "eval" :category :success}]})))
    (is (nil? (state/last-failure {})))))

(deftest failure-driven-gates-name-the-failure-and-how-to-look
  (let [b {:id "b" :status :active :task {:title "GOAL-X"} :problem "P"
           :consecutive-mechanics-failures 6
           :turns (vec (repeat 8 {:turn 9 :tool "eval" :category :failure
                                  :error "THE-ERROR-TEXT"}))}
        msg (fn [g] (str ((:message (first (filter #(= g (:gate %)) (gates/gates))))
                          {:branch b :turn 9 :max-turns 50})))]
    (testing "mechanics-streak names the failing call rather than the count alone"
      (let [m (msg :mechanics-streak)]
        (is (str/includes? m "THE-ERROR-TEXT"))
        (is (str/includes? m "fetch_turn")
            "and says how to look at it — information plus the tool to investigate")))))

;; --- a branch id says which task it is working -----------------------------
;; Branch ids were T0, T1… by OWNER INDEX, so one id covered every task that
;; owner ever worked. Run 8710067f ran turns 1-153 under "T0" across two
;; different tasks with two different fresh contexts — the journal could not
;; tell "T0 on the green-suite task" from "T0 on the render task". Every
;; per-branch metric aggregated across a boundary that genuinely exists, which
;; is the same class of mistake as reading per-branch turn counters in
;; aggregate.

(deftest a-branch-id-names-the-task-it-is-working
  (testing "the owner index and a slug of the task"
    (is (= "T0-get-pure-layer-suite-green-9"
           (state/branch-id-for 0 0 "Get pure-layer suite green (9 failing assertions)"))
        "readable at a glance, and says which task this branch is working"))
  (testing "a revise round is distinct again"
    ;; The slug is cut at a word boundary, so the exact tail depends on the
    ;; title — assert the PROPERTIES that matter rather than a guessed string.
    (let [id (state/branch-id-for 0 2 "Build raylib window layer (render/input/main)")]
      (is (str/starts-with? id "T0-build-raylib-window-layer"))
      (is (str/ends-with? id "v2"))
      (is (not (str/includes? id "/")) "no path separators in an id")
      (is (not= id (state/branch-id-for 0 1 "Build raylib window layer (render/input/main)"))
          "a different round is a different branch")))
  (testing "two tasks of one owner are DIFFERENT ids — the whole point"
    (is (not= (state/branch-id-for 0 0 "Get the suite green")
              (state/branch-id-for 0 0 "Build the window layer"))))
  (testing "punctuation and case do not leak into an id"
    (is (= "T1-fix-the-a-b-bug" (state/branch-id-for 1 0 "Fix the A/B bug!!"))))
  (testing "a missing or blank title still yields a usable id"
    (is (= "T3" (state/branch-id-for 3 0 nil)))
    (is (= "T3" (state/branch-id-for 3 0 "   ")))
    (is (= "T3v1" (state/branch-id-for 3 1 ""))))
  (testing "a long title is bounded — an id is a label, not a description"
    (is (>= 48 (count (state/branch-id-for 0 0 (apply str (repeat 200 "x"))))))))

;; --- a new task starts a new REPL session ----------------------------------
;; A landed plan kept `planned?` true forever, so the entry condition was
;; satisfied for the REST OF THE RUN. Run 8710067f landed its green-suite plan
;; at turn 35 and then spent 118 turns building the window layer — new work —
;; under that stale declaration, with nothing asking it to say what it was
;; changing now.

(deftest landing-a-plan-closes-the-session
  (let [b (-> (state/declare-plan {} {:files ["a.clj"]})
              (state/note-write "a.clj"))]
    (is (empty? (state/unwritten b)) "the plan is landed")
    (is (not (state/planned? b))
        "and the session is CLOSED — the next piece of work needs its own plan")))

(deftest an-open-plan-still-opens-the-repl
  (let [b (state/declare-plan {} {:files ["a.clj" "b.clj"]})]
    (is (state/planned? b))
    (is (state/planned? (state/note-write b "a.clj"))
        "partly landed is still an open session — do not close it mid-way")))

;; --- karamazov-b3z: a child sees the surface it sits in ---------------------
;; A task owner got its own contract and nothing about the whole: which sibling
;; parts exist, what the overarching goal is, what another owner is already
;; building. Metan measured the plain conditioning string passed between layers
;; at ~72% of what recursion buys — the cheapest channel there is, and this had
;; none of it. A live worker went off-task onto a superficially-similar
;; recalled fix precisely because nothing said what its part was FOR.

(deftest the-surface-names-the-goal-and-the-siblings
  (let [goal "Build a small FPS level"
        siblings [{:id "t2" :title "Build the raylib window layer" :status "open"}
                  {:id "t3" :title "Enemy AI and waves" :status "in_progress"}]
        s (board/surface-block {:goal goal :siblings siblings :mine "t1"})]
    (is (str/includes? s goal) "the whole this part serves")
    (is (str/includes? s "Build the raylib window layer"))
    (is (str/includes? s "Enemy AI and waves"))
    (is (str/includes? s "in_progress")
        "and whether somebody is already on it — that is the do-not-duplicate signal"))
  (testing "the owner's OWN task is not listed back as a sibling"
    (let [s (board/surface-block
             {:goal "G" :mine "t1"
              :siblings [{:id "t1" :title "MINE" :status "in_progress"}
                         {:id "t2" :title "OTHER" :status "open"}]})]
      (is (not (str/includes? s "MINE")))
      (is (str/includes? s "OTHER"))))
  (testing "no siblings means no surface section rather than an empty heading"
    (is (nil? (board/surface-block {:goal "G" :siblings [] :mine "t1"})))
    (is (nil? (board/surface-block {:goal "G" :siblings nil :mine "t1"}))))
  (testing "a surface with siblings but no goal still says what else exists"
    (let [s (board/surface-block {:siblings [{:id "t2" :title "OTHER" :status "open"}]
                                  :mine "t1"})]
      (is (str/includes? s "OTHER")))))

;; --- reading is not a way around the contract -------------------------------
;; Three branches across three runs took the same route: never call `eval`, so
;; the entry refusal cannot fire; never declare, so plan-stale? has nothing to
;; measure; never write, so over-studying? cannot arm. bd56a286's T1 read for
;; 316 turns, c377260b's revise branch for 148, d304f539's T0 for 87 — none of
;; them reachable by any gate, because every precondition needs the branch to
;; have entered the contract first. The contract bound the path THROUGH the
;; REPL and not the branch that never entered it.

(deftest orientation-is-free-but-not-unbounded
  (let [n (gates/threshold :orient-turns)
        vocab (gates/tool-vocab :file-write)
        reading (fn [k] (vec (repeat k {:tool "shell"})))]
    (testing "a branch orienting is not asked for a plan — reading is how you
              decide what to declare"
      (is (not (state/orienting-too-long? {:turns (reading (dec n))} vocab n))))
    (testing "past the floor with nothing declared, it is"
      (is (state/orienting-too-long? {:turns (reading n)} vocab n)))
    (testing "a declared plan ends it, whatever the reading since"
      (is (not (state/orienting-too-long?
                (assoc (state/declare-plan {} {:files ["a.clj"]})
                       :turns (reading (* 3 n)))
                vocab n))))
    (testing "and so does having written — a branch already working is not
              orienting, it is between sessions"
      (is (not (state/orienting-too-long?
                {:turns (conj (reading (* 3 n)) {:tool "write_file"})} vocab n))))))

(deftest the-orient-floor-is-well-clear-of-real-orientation
  ;; Measured: the run that DID declare did so at turn 40, having read from
  ;; turn 1. A floor below that would have interrupted the one branch that got
  ;; this right, which is the failure the arm-after-first-write guard exists to
  ;; prevent. Set above it, with room.
  (is (> (gates/threshold :orient-turns) 40)
      "the floor must not interrupt orientation that was going to succeed"))

;; --- snake_case args reach kebab-case lookups -------------------------------

(deftest an-arg-documented-with-an-underscore-reaches-a-hyphenated-lookup
  ;; A tool call arrives as JSON and is keywordized verbatim, so `timeout_ms`
  ;; becomes :timeout_ms. eval read :timeout-ms and got nil, so every eval ran
  ;; on the 10s default no matter what the model asked for — silently, since a
  ;; missing optional arg is indistinguishable from one that was never sent.
  ;;
  ;; Found by the SUPERVISOR, not by a test: it read a worker's turn that
  ;; passed 60000 and timed out at 10000, and opened a plan to find out why
  ;; (run 498450e1).
  (testing "the underscore spelling the prompt documents"
    (is (= 60000 (base/arg {:args {:timeout_ms 60000}} :timeout-ms))))
  (testing "the hyphen spelling, for a caller that already normalised"
    (is (= 60000 (base/arg {:args {:timeout-ms 60000}} :timeout-ms))))
  (testing "a plain string key, which is how some callers build args"
    (is (= 60000 (base/arg {:args {"timeout_ms" 60000}} :timeout-ms))))
  (testing "absent stays absent — an unsent optional arg must not resolve"
    (is (nil? (base/arg {:args {}} :timeout-ms))))
  (testing "an ordinary single-word arg is unaffected"
    (is (= "x" (base/arg {:args {:code "x"}} :code)))))

;; --- karamazov-70b: looking in the wrong place ------------------------------
;;
;; Every other gate measures whether the branch is DOING something. Run
;; bd56a286 was doing plenty: 238 turns of read-implementation, run-tests,
;; read-implementation, while the ten failures it chased were in its own tests.
;; :no-edits said write a file — which file? the code is correct. :studying
;; said commit and test — it tested thirteen times. Both were answerable.

(defn- verify-turn [n err]
  {:turn n :tool "shell" :category :failure :error err})

(def ^:private writes #{"write_file" "edit_file" "patch"})
(def ^:private verifiers #{"eval" "shell"})

(deftest the-same-failure-against-unchanged-code-is-the-signal
  (let [same (mapv #(verify-turn % "FAIL circle-free? expected false got true") (range 1 5))]
    (is (supervisor/repeating-one-failure? writes verifiers same 4)
        "four runs, same failure, no edit between them")
    (is (not (supervisor/repeating-one-failure? writes verifiers (butlast same) 4))
        "three is under the floor — a branch converging still fails a few times")))

(deftest a-branch-working-through-different-failures-is-making-progress
  ;; The distinction the whole gate rests on. Telling a model to doubt a
  ;; correct test would be strictly worse than silence, so repeated failure is
  ;; not enough — it has to be the SAME failure.
  (let [varied [(verify-turn 1 "FAIL arity") (verify-turn 2 "FAIL nil pointer")
                (verify-turn 3 "FAIL wrong key") (verify-turn 4 "FAIL off by one")]]
    (is (not (supervisor/repeating-one-failure? writes verifiers varied 4)))))

(deftest an-edit-ends-the-window
  ;; The implementation changed, so the next failure is evidence about new
  ;; code. Whatever the branch was staring at before is a different question.
  (let [turns (concat (mapv #(verify-turn % "FAIL same") (range 1 5))
                      [{:turn 5 :tool "edit_file" :category :success}]
                      [(verify-turn 6 "FAIL same")])]
    (is (not (supervisor/repeating-one-failure? writes verifiers turns 4))
        "one write resets the window to a single failure")))

(deftest reading-and-grepping-do-not-count-as-running-the-code
  ;; The branch must have RUN the thing. A branch that reads four times has a
  ;; different problem and :studying already speaks to it.
  (let [reads (mapv (fn [n] {:turn n :tool "read_file" :category :failure
                             :error "FAIL same"})
                    (range 1 5))]
    (is (not (supervisor/repeating-one-failure? writes verifiers reads 4)))))

(deftest the-steer-quotes-what-the-branch-is-staring-at
  ;; A gate that describes a failure the branch can already see says less than
  ;; the branch knows. Quoting it points at a line.
  (let [turns (mapv #(verify-turn % "level_test.clj:14\n  expected: (false? ...)\n  actual: true")
                    (range 1 5))
        q (supervisor/unchanged-failure writes verifiers turns 400)]
    (is (str/includes? q "level_test.clj:14"))
    (is (not (str/includes? q "\n")) "whitespace collapsed so it renders inline")
    (is (= 10 (count (supervisor/unchanged-failure writes verifiers turns 10)))
        "and clipped to the budget"))
  (is (nil? (supervisor/unchanged-failure writes verifiers [] 400))
      "no failure to quote is nil, not a throw"))

(deftest the-gate-outranks-the-three-that-would-otherwise-answer
  ;; :progress-stalled, :no-edits and :studying all hold on this branch, the
  ;; boundary allows one steer, and all three of them are answerable — "write
  ;; a file" invites "which file? the code is correct". LOWER priority wins,
  ;; and the ladder's own principle (see :no-edits' doc) is that the more
  ;; specific gate takes the slot when both are true. This is the narrowest
  ;; precondition in the band.
  (let [by (fn [n] (first (filter #(= n (:gate %)) (gates/gates))))
        mine (:priority (by :suspect-the-test))]
    (doseq [g [:progress-stalled :no-edits :studying]]
      (is (< mine (:priority (by g)))
          (str "suspect-the-test must outrank " g)))))

;; --- a call the model put inside its reasoning ------------------------------

(deftest a-call-emitted-inside-the-reasoning-is-recovered
  ;; strip-think drops reasoning before parsing, and correctly: after a
  ;; prefilled opener the thinking lands INSIDE the fence and its stray quotes
  ;; corrupt the JSON. But a reasoning model sometimes puts the CALL in there
  ;; and emits nothing outside, and stripping then destroys the only call the
  ;; turn produced.
  ;;
  ;; Measured across runs a3566c73 and f2014821 before the fix: 10 no-call
  ;; turns whose text contained a fence, parse_error empty and lengths well
  ;; under the token cap — so neither a parse failure nor a truncation. Run
  ;; f2014821 turn 2 is this shape exactly.
  (let [in-think (str "```tool-call\n<think>I need to emit exactly one call.\n"
                      "```tool-call\n{\"name\":\"read_file\",\"args\":{\"path\":\"a.clj\"}}\n```\n"
                      "</think>\nThe flagged failure is turn 53's eval.")
        got (fence/parse-tool-call in-think)]
    (is (= "read_file" (:name got)))
    (is (:scavenged? got) "marked, so a run full of these is visible")))

(deftest a-reply-cut-off-mid-thought-still-yields-its-call
  ;; The reply most likely to have spent its budget reasoning is the one whose
  ;; </think> never arrived.
  (let [got (fence/parse-tool-call
             "<think>I should read it\n```tool-call\n{\"name\":\"grep\",\"args\":{\"pattern\":\"x\"}}\n```")]
    (is (= "grep" (:name got)))
    (is (:scavenged? got))))

(deftest scavenging-is-a-fallback-and-changes-nothing-that-worked
  ;; It runs ONLY where the normal parse found nothing, so every input it sees
  ;; is one the harness was about to refuse with "No ```tool-call block".
  (testing "an ordinary call is parsed by the normal path, unmarked"
    (let [got (fence/parse-tool-call
               "<think>reasoning</think>\n```tool-call\n{\"name\":\"grep\",\"args\":{\"pattern\":\"x\"}}\n```")]
      (is (= "grep" (:name got)))
      (is (not (:scavenged? got)))))
  (testing "a reply that reasoned and called nothing is still a no-call"
    (is (nil? (fence/parse-tool-call "<think>just thinking</think>\nI am done."))))
  (testing "and the signal names it, so a run full of these points at the
            PROMPT rather than at loosening the parser further"
    (let [got (fence/parse-tool-call
               "<think>```tool-call\n{\"name\":\"done\",\"args\":{\"answer\":\"x\"}}\n```</think>")]
      (is (:scavenged (fence/signals {:finish-reason "stop"} got))))))

(deftest a-mistyped-tool-name-gets-the-nearest-one
  ;; The list of every tool is the fallback answer, not the useful one: 36
  ;; names the model has already been shown, so reading it back is a turn
  ;; spent re-reading its own prompt.
  (let [refusal (fn [t] (str (:result (tools/run-tool {:branch {:id "B1"}
                                                       :tool-name t :args {}}))))]
    (is (str/includes? (refusal "read_fil") "Did you mean `read_file`?"))
    (is (str/includes? (refusal "wrtie_file") "Did you mean `write_file`?")
        "a transposition is the commonest slip and must cost one edit, not two")
    (is (not (str/includes? (refusal "totally_made_up") "Did you mean"))
        "a name that is not a typo of anything gets no guess")
    (is (str/includes? (refusal "totally_made_up") "Available:")
        "and still gets the full list to choose from")))
