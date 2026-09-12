;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.battery-test
  "The held-out battery's assertions (karamazov-ylte.4).

  A case's expected effects are DATA over a closed vocabulary, evaluated
  against the journal a replay produced. Data and not code for the reason
  gates.edn's guards are: the running agent may add a case from an observed
  failure, and a case that could carry an arbitrary form would be an eval seam
  in the one place that must not have one — the gate deciding whether the
  agent's own edit may go live."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.battery :as battery]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn- a-run
  "A finished run with one gate fired and met, one turn, one artifact."
  [conn]
  (let [rid (runs/start-run! conn {:problem "p"})]
    (runs/open-branch! conn rid {:branch-id "T0"})
    (journal/record-turn! conn rid {:branch-id "T0" :turn 1 :tool-name "done"
                                    :args "{}" :result "ok" :category "success"})
    (let [g (journal/record-gate! conn rid {:branch-id "T0" :turn 1 :gate :milestone
                                            :prediction "ships" :window 2})]
      (journal/settle-gate! conn g :met 2))
    (runs/finish-run! conn rid :completed nil)
    rid))

(deftest each-target-is-named-and-passes-or-fails-on-its-own
  ;; The decision behind this: a scalar cannot say WHICH behaviour a candidate
  ;; broke (karamazov-7mo.3). Every expectation carries a name and its own
  ;; verdict, so a rejection is actionable rather than merely negative.
  (let [c (db/open! ":memory:")
        rid (a-run c)
        r (battery/check c rid [{:name "run completes" :assert [:status :completed]}
                                {:name "milestone fires" :assert [:gate-fired :milestone]}
                                {:name "milestone is met" :assert [:gate-met :milestone]}
                                {:name "calls done" :assert [:tool-called "done"]}])]
    (is (true? (:ok? r)))
    (is (= 4 (count (:targets r))))
    (is (every? :ok? (:targets r)))
    (is (= ["run completes" "milestone fires" "milestone is met" "calls done"]
           (mapv :name (:targets r))))))

(deftest a-failing-target-names-itself-and-what-it-saw
  (let [c (db/open! ":memory:")
        rid (a-run c)
        r (battery/check c rid [{:name "run completes" :assert [:status :completed]}
                                {:name "ships an artifact" :assert [:artifacts-at-least 1]}])]
    (is (false? (:ok? r)) "one failed target fails the case")
    (let [bad (first (remove :ok? (:targets r)))]
      (is (= "ships an artifact" (:name bad)))
      (is (some? (:actual bad)) "and reports what it actually saw, not only that it differed"))))

(deftest an-unknown-assertion-is-refused-at-check-time-not-ignored
  ;; The failure mode this prevents: a case whose assertion nobody implements
  ;; silently passes forever, and the battery grows a hole that looks like
  ;; coverage. Same argument as gates.clj compiling its :when forms at load.
  (let [c (db/open! ":memory:")
        rid (a-run c)]
    (is (thrown-with-msg? Exception #"unknown assertion"
                          (battery/check c rid [{:name "?" :assert [:invented-verb 1]}])))))

(deftest a-case-cannot-smuggle-a-form
  ;; The battery gates the agent's own edits, so a case that could carry
  ;; executable code would be an eval seam in exactly the wrong place. The
  ;; vocabulary is closed, like symbolic's guard registry.
  (let [c (db/open! ":memory:")
        rid (a-run c)]
    (is (thrown-with-msg? Exception #"unknown assertion"
                          (battery/check c rid [{:name "x" :assert ['(System/exit 0)]}])))))

(deftest accept-on-tie
  ;; Algorithm 1 line 17 of 2609.09153v1 accepts when the candidate MATCHES the
  ;; retained score, deliberately: it lets the graph drift laterally through
  ;; neutral edits into a better basin. A gate that demanded improvement would
  ;; refuse every edit that fixed something the battery does not measure.
  (is (true? (battery/accept? {:passed 3 :total 4} {:passed 3 :total 4}))
      "equal is accepted")
  (is (true? (battery/accept? {:passed 3 :total 4} {:passed 4 :total 4}))
      "better is accepted")
  (is (false? (battery/accept? {:passed 3 :total 4} {:passed 2 :total 4}))
      "worse is refused"))

(deftest a-regression-names-the-targets-that-broke
  ;; What the gate hands back on a refusal. "3/4 vs 4/4" tells the supervisor
  ;; nothing it can act on; the names of the two targets that flipped do.
  (let [before {:targets [{:name "a" :ok? true} {:name "b" :ok? true}]}
        after  {:targets [{:name "a" :ok? true} {:name "b" :ok? false}]}]
    (is (= ["b"] (battery/regressions before after)))
    (is (empty? (battery/regressions before before)))
    (testing "a target that was already failing is not a regression — the edit
              did not break it and refusing on it would block every edit until
              someone fixed something unrelated"
      (is (empty? (battery/regressions after after))))))

;; --- cases on disk ----------------------------------------------------------

(deftest a-case-file-pairs-a-recording-with-its-expectations
  (let [dir (str (System/getProperty "java.io.tmpdir") "/battery-test-" (System/nanoTime))
        sub (str dir "/endless-flight")]
    (.mkdirs (java.io.File. sub))
    (spit (str sub "/ships.edn")
          (pr-str {:id "ships"
                   :replay {:problem "p" :replies {"T0" ["a" "b"]}}
                   :expect [{:name "run completes" :assert [:status :completed]}]}))
    (let [cs (battery/load-cases dir)]
      (is (= 1 (count cs)))
      (is (= "ships" (:id (first cs))))
      (is (= "endless-flight" (:subject (first cs)))
          "the subject is the directory — an edit must hold on BOTH subjects,
           and a result that cannot say which subject it came from cannot
           enforce that"))))

(deftest both-subjects-are-required-when-both-are-present
  ;; The decision behind this: a battery of game runs only would pass an edit
  ;; that breaks samizdat working on samizdat, and self-modification is the
  ;; project's reason for existing.
  (is (true? (:ok? (battery/summarise
                    [{:subject "endless-flight" :result {:ok? true :passed 1 :total 1}}
                     {:subject "samizdat-self" :result {:ok? true :passed 1 :total 1}}]))))
  (is (false? (:ok? (battery/summarise
                     [{:subject "endless-flight" :result {:ok? true :passed 1 :total 1}}
                      {:subject "samizdat-self" :result {:ok? false :passed 0 :total 1
                                                         :targets [{:name "x" :ok? false}]}}]))))
  (testing "and the summary totals across subjects, so accept? compares like
            with like"
    (let [s (battery/summarise [{:subject "a" :result {:ok? true :passed 2 :total 2}}
                                {:subject "b" :result {:ok? false :passed 1 :total 3}}])]
      (is (= 3 (:passed s)))
      (is (= 5 (:total s))))))

(deftest an-empty-battery-is-not-a-pass
  ;; The failure this prevents: a misconfigured directory yields no cases, the
  ;; gate reports ok?, and every edit sails through while looking validated.
  ;; Absence of evidence has to read as absence, not as approval.
  (let [s (battery/summarise [])]
    (is (false? (:ok? s)))
    (is (= :battery/empty (:reason s)))))

;; --- drafting a case from a run ---------------------------------------------

(deftest a-draft-case-pins-what-the-run-actually-did
  ;; The bridge between "a run happened" and "the battery has a case".
  ;; karamazov-7mo.4's rule is that the running agent may ADD a case from an
  ;; observed failure — which needs a way to turn a run into one without
  ;; hand-writing expectations and getting them subtly wrong.
  ;;
  ;; A DRAFT, not a case. What the run did is the starting point, not the
  ;; specification: some of it is incidental and a person or a supervisor has
  ;; to say which parts are the behaviour worth pinning.
  (let [c (db/open! ":memory:")
        rid (a-run c)
        d (battery/draft-case c rid)]
    (is (= rid (get-in d [:replay :run-id])))
    (let [named (into {} (map (juxt :name identity)) (:expect d))]
      (is (contains? named "run reaches :completed"))
      (is (= [:status :completed] (:assert (named "run reaches :completed"))))
      (is (contains? named "calls done"))
      (is (contains? named "gate :milestone fires"))
      (is (contains? named "gate :milestone is met")))
    (testing "every drafted assertion uses the closed vocabulary, so a draft is
              runnable the moment it is written rather than after someone
              discovers a verb does not exist"
      (is (every? (battery/vocabulary) (map (comp first :assert) (:expect d)))))
    (testing "and it round-trips: a draft checked against its own run passes"
      (is (true? (:ok? (battery/check c rid (:expect d))))))))

(deftest a-draft-does-not-pin-a-gate-that-was-never-met
  ;; Pinning "gate X is unmet" would freeze a defect as a requirement: the
  ;; whole point of an edit might be to make that gate land. Fired is a fact
  ;; about the run and safe to pin; met is only pinned where it was met.
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})]
    (runs/open-branch! c rid {:branch-id "T0"})
    (journal/record-turn! c rid {:branch-id "T0" :turn 1 :tool-name "done"
                                 :args "{}" :result "ok"})
    (let [g (journal/record-gate! c rid {:branch-id "T0" :turn 1 :gate :ignored
                                         :prediction "p" :window 2})]
      (journal/settle-gate! c g :unmet 2))
    (runs/finish-run! c rid :completed nil)
    (let [names (set (map :name (:expect (battery/draft-case c rid))))]
      (is (contains? names "gate :ignored fires"))
      (is (not (contains? names "gate :ignored is met"))
          "an unmet gate is a defect, not a requirement to preserve"))))

(deftest a-case-cannot-be-drafted-from-a-run-that-is-still-going
  ;; Found by drafting from a live sweep run: it pinned
  ;; [:status :running] as an expectation. A case built that way asserts a
  ;; nonsense requirement — that the loop must still be executing when the
  ;; check reads it — and every candidate would fail it or pass it for
  ;; unrelated reasons. Worse, the recording is a PREFIX of a conversation, so
  ;; every replay would exhaust partway through work the run had not finished.
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})]
    (runs/open-branch! c rid {:branch-id "T0"})
    (journal/record-turn! c rid {:branch-id "T0" :turn 1 :tool-name "grep"
                                 :args "{}" :result "ok"})
    (is (thrown-with-msg? Exception #"not finished"
                          (battery/draft-case c rid)))
    (testing "and it drafts once the run ends"
      (runs/finish-run! c rid :completed nil)
      (is (some? (battery/draft-case c rid))))))

(deftest a-draft-does-not-pin-an-ending-the-loop-did-not-choose
  ;; Found seeding the first real case. A run killed by the rig's timeout is
  ;; reconciled to :interrupted, and the draft pinned [:status :interrupted] —
  ;; "an edit must keep this run getting interrupted", which is nonsense in
  ;; exactly the way [:status :running] was.
  ;;
  ;; The line is whether the LOOP chose the ending. :completed, :exhausted and
  ;; :abandoned are the loop's own outcomes and worth pinning — a branch that
  ;; ran out of budget or gave up is behaviour. :interrupted, :aborted and
  ;; :failed are things that happened TO it: a process died, an operator
  ;; stopped it, a stage threw. Pinning those freezes an accident as a
  ;; requirement, the same error as pinning an unmet gate.
  (let [c (db/open! ":memory:")
        mk (fn [status]
             (let [rid (runs/start-run! c {:problem "p"})]
               (runs/open-branch! c rid {:branch-id "T0"})
               (journal/record-turn! c rid {:branch-id "T0" :turn 1 :tool-name "done"
                                            :args "{}" :result "ok"})
               (runs/finish-run! c rid status nil)
               rid))]
    (doseq [s [:completed :exhausted :abandoned]]
      (is (some #(= [:status s] (:assert %)) (:expect (battery/draft-case c (mk s))))
          (str s " is an ending the loop chose and is worth pinning")))
    (doseq [s [:interrupted :aborted :failed]]
      (is (not-any? #(= :status (first (:assert %)))
                    (:expect (battery/draft-case c (mk s))))
          (str s " happened TO the run and must not become a requirement")))))

(deftest results-over-different-expectation-sets-are-not-comparable
  ;; Found running the gate end to end on a real run. accept? compared raw
  ;; :passed counts, so a baseline of 14/14 and a candidate of 14/15 read as
  ;; 14 >= 14 and COMMITTED — a candidate that fails a target was accepted
  ;; because it had been checked against one more expectation.
  ;;
  ;; Same-set comparison is the only meaningful one: the battery runs the same
  ;; cases before and after, so unequal totals mean the caller compared two
  ;; different things, and the honest answer is to refuse rather than to pick
  ;; a winner between them.
  (is (false? (battery/accept? {:passed 14 :total 14} {:passed 14 :total 15}))
      "more expectations, same passes — not an improvement, not comparable")
  (is (false? (battery/accept? {:passed 3 :total 4} {:passed 3 :total 3}))
      "fewer expectations is the same error inverted: dropping a target the
       baseline was held to must not read as holding steady")
  (is (true? (battery/accept? {:passed 3 :total 4} {:passed 3 :total 4})))
  (is (true? (battery/accept? {:passed 3 :total 4} {:passed 4 :total 4}))))
