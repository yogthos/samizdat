(ns samizdat.knowledge-test
  (:require [samizdat.agent.gitdiff :as gitdiff]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [samizdat.memory :as memory]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            ;; The tool namespaces register their defmethods on load, and this
            ;; test dispatches `forget` through the multimethod. Requiring only
            ;; `tools.base` left that registration to whichever other test
            ;; namespace happened to load first, so the test passed in the
            ;; suite and failed alone.
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.base :as base]
            [samizdat.store.knowledge :as knowledge]))

(def conn (atom nil))

(use-fixtures :each (fn [f] (reset! conn (db/open! ":memory:")) (f)))

(deftest remember-returns-an-id
  (let [id (knowledge/remember! @conn {:content "fred likes fish"})]
    (is (string? id))
    (is (pos? (count id)))))

(deftest recall-finds-what-matches-and-nothing-else
  ;; The ORDER is deliberately not asserted here, and that is the point of
  ;; this test being split from the two below it. recall ranks by bm25 where
  ;; FTS5 is available and by recency where it is not, so a test that pinned
  ;; one order would pass or fail on which libsqlite3 happens to be loaded —
  ;; which is a property of the machine, not of the code.
  (knowledge/remember! @conn {:content "fred likes fish"})
  (java.lang.Thread/sleep 1100)
  (knowledge/remember! @conn {:content "fred hates dogs" :kind "fact"})
  (knowledge/remember! @conn {:content "barney likes birds"})
  (is (= #{"fred hates dogs" "fred likes fish"}
         (set (mapv :content (knowledge/recall @conn "fred")))))
  (is (empty? (knowledge/recall @conn "wilma"))))

(deftest recall-ranks-by-relevance-when-fts-is-available
  ;; Why the ordering changed at all: a substring scan has no ranking to
  ;; offer, so "newest first" was the only ordering available, not a chosen
  ;; one. bm25 is what the index exists for, and it is what failures/similar
  ;; and the shared-artifact pool already order by.
  (when (db/fts5-available? @conn)
    (knowledge/remember! @conn {:content "the deploy script needs sudo"})
    (java.lang.Thread/sleep 1100)
    (knowledge/remember! @conn {:content "unrelated note about sudo policy and the deploy of other things"})
    (let [hits (mapv :content (knowledge/recall @conn "deploy script"))]
      (is (= 2 (count hits)))
      (is (= "the deploy script needs sudo" (first hits))
          "the row matching both terms outranks the one matching one, even
           though it is older — which recency ordering could not express"))))

(deftest recall-falls-back-to-a-scan-rather-than-failing
  ;; recall is on the path of a tool the model calls to orient itself, so an
  ;; exception there costs the turn. A query the tokenizer rejects must cost a
  ;; worse ranking, never an error.
  (knowledge/remember! @conn {:content "fred likes fish"})
  (with-redefs [db/fts5-available? (fn [_] false)]
    (is (= ["fred likes fish"]
           (mapv :content (knowledge/recall @conn "fred")))))
  ;; And a thrown MATCH, not only an absent extension.
  (let [real db/fetch]
    (with-redefs [db/fetch (fn [c q & opts]
                             (if (clojure.string/includes? (str (first q)) "knowledge_fts")
                               (throw (ex-info "fts5: syntax error" {}))
                               (apply real c q opts)))]
      (is (= ["fred likes fish"]
             (mapv :content (knowledge/recall @conn "fred")))))))

(deftest a-recall-records-what-it-was-asked-and-what-came-back
  ;; karamazov-ei6t.2. Recall was called exactly TWICE in each of three live
  ;; runs of 167, 203 and 219 turns, while 30-35 memories were written per run.
  ;; Nothing recorded what it returned, so "recall is unused because nobody
  ;; knows about it" and "recall is unused because it never returns anything
  ;; useful" left identical traces — and they need opposite fixes.
  (let [c @conn
        rid (runs/start-run! c {:problem "p" :provider "x" :model "m"
                                :max-turns 5 :beam-width 1})]
    (knowledge/remember! c {:content "jolt -M:test is the test command here"
                            :kind "semantic"})
    (testing "a hit records the ids, so the NEXT turn can be read against them"
      (tools/run-tool {:tool-name "recall" :conn c :run-id rid :turn 7
                       :branch {:id "B1"} :args {:query "test command"}})
      (let [n (journal/last-note c rid :recall)]
        (is (= "test command" (:query n)))
        (is (= 1 (:returned n)))
        (is (= 1 (count (:ids n))) "the ids are the half a count cannot answer")
        (is (= 7 (:turn n)))))
    (testing "and a MISS records itself too — an empty store and a bad query
              call for opposite actions, and both look like silence otherwise"
      (tools/run-tool {:tool-name "recall" :conn c :run-id rid :turn 8
                       :branch {:id "B1"} :args {:query "zzzz-no-such-thing"}})
      (let [n (journal/last-note c rid :recall)]
        (is (= 0 (:returned n)))
        (is (pos? (:live n)) "the store was not empty, so this was a missed query")))))

(deftest the-update-policy-has-a-fourth-arm
  ;; karamazov-ei6t.8, ported from lemmalog's AgentMemory. The value is not the
  ;; four outcomes — samizdat's distill! already did three of them informally —
  ;; it is that they are ONE named deterministic policy, and that the fourth
  ;; exists at all. There was no way to say "these two cannot both be true".
  (testing "the three arms samizdat already had, now named"
    (is (= :add (:action (memory/update-policy {:content "anything"} nil))))
    (is (= :noop (:action (memory/update-policy {:content "the same words"}
                                                {:content "the same words"}))))
    (is (= :update (:action (memory/update-policy
                             {:content "jolt -M:test runs the suite and is green"}
                             {:content "jolt -M:test runs the suite"})))))
  (testing "and the one it did not: a claim against its own negation"
    ;; karamazov-ko5b's shape. The supervisor wrote that requires FAIL while
    ;; its own turns showed them loading.
    (let [d (memory/update-policy
             {:content "require of flight.main fails on the source roots"}
             {:id "k-1" :content "require of flight.main loaded on the source roots"})]
      (is (= :escalate (:action d)))
      (is (= :contradicts (:reason d)))
      (is (str/includes? (:held d) "loaded")
          "the escalation carries what is already believed, or nobody can judge it")))
  (testing "narrow on purpose — a wide detector would refuse every refinement,
            which is most of what a store learns"
    (is (not= :escalate (:action (memory/update-policy
                                  {:content "the render namespace is pure"}
                                  {:content "the terrain namespace is pure"})))
        "different subjects are not a contradiction")
    (is (not= :escalate (:action (memory/update-policy
                                  {:content "python3 is not on the allow list"}
                                  {:content "python3 is not on the allow list, use eval"})))
        "agreeing negatives are not a contradiction")))

(deftest an-escalated-finding-is-recorded-rather-than-landing
  (let [c @conn
        rid (runs/start-run! c {:problem "p" :provider "x" :model "m"
                                :max-turns 5 :beam-width 1})]
    (knowledge/distill! c [{:kind :parse-error :severity :bad
                            :detail "require of flight.main loaded fine"
                            :evidence {}}]
                        {:run-id rid})
    (let [out (knowledge/distill! c [{:kind :parse-error :severity :bad
                                      :detail "require of flight.main fails always"
                                      :evidence {}}]
                                  {:run-id rid})]
      (is (true? (:escalated? (first out))))
      (let [n (journal/last-note c rid :memory-escalation)]
        (is (some? n) "oversight reads notes; an escalation nothing records is a silence")
        (is (str/includes? (str (:proposed n)) "fails always"))
        (is (str/includes? (str (:held n)) "loaded fine"))))))

(deftest a-memory-can-say-which-runs-support-it
  ;; karamazov-ei6t.5. corroborations says a memory was seen three times and
  ;; cannot say BY WHAT — which is the question a confidently false standing
  ;; claim needs asked of it.
  (let [c @conn
        r1 (runs/start-run! c {:problem "p" :provider "x" :model "m" :max-turns 5 :beam-width 1})
        r2 (runs/start-run! c {:problem "q" :provider "x" :model "m" :max-turns 5 :beam-width 1})
        id (knowledge/remember! c {:content "jolt -M:test is the test command" :run-id r1})]
    (testing "a fresh memory records no support yet, which is not the same as none"
      (is (false? (:recorded? (knowledge/support c id)))))
    (knowledge/corroborate! c id r2)
    (let [sup (knowledge/support c id)]
      (is (true? (:recorded? sup)))
      (is (= [r2] (:runs sup)) "the run that confirmed it, by name")
      (is (= 2 (:corroborations sup))))
    (testing "the same run twice does not inflate the set"
      (knowledge/corroborate! c id r2)
      (is (= [r2] (:runs (knowledge/support c id)))))))

(deftest a-run-can-open-with-what-the-last-one-learned
  ;; karamazov-ei6t.9, lemmalog's change-log. recall answers a question the
  ;; model thought to ask; this answers the one it does not know to ask.
  (let [c @conn]
    (knowledge/remember! c {:content "jolt -M:test is the test command here"
                            :kind "semantic"})
    (Thread/sleep 5)
    (let [cut (db/now)]
      (Thread/sleep 5)
      (knowledge/remember! c {:content "python3 is not on the allow list"
                              :kind "semantic"})
      (let [new (knowledge/learned-since c cut)]
        (is (= 1 (count new)) "only what is new since the cut")
        (is (str/includes? (:content (first new)) "python3"))))
    (testing "no cut means nothing to report, not everything"
      (is (empty? (knowledge/learned-since c nil))))))

(deftest the-opening-block-cuts-at-the-previous-run-START-not-its-end
  ;; THE BUG THAT COST THE BLOCK ITS FIRST LIVE RUN (karamazov-ei6t.9 follow-up).
  ;; distil-project!/distil-session! write during a run's TEARDOWN, and
  ;; finish-run! stamps ended_at after that — so a memory's created_at is a
  ;; hair BEFORE its own run's ended_at, and cutting on ended_at excludes
  ;; every memory the run wrote. Measured on sweep6: learned-since returned 0
  ;; while the store held the run's learnings, so the block never rendered on
  ;; the one run in the sweep that could have shown it.
  (let [c @conn
        r1 (runs/start-run! c {:problem "a" :provider "x" :model "m"
                               :max-turns 5 :beam-width 1})]
    ;; the memory is written DURING run 1, then run 1 ends — the real order
    (knowledge/remember! c {:content "the test command is jolt -M:test"
                            :kind "semantic" :run-id r1})
    (runs/finish-run! c r1 :completed "d")
    (let [r2 (runs/start-run! c {:problem "b" :provider "x" :model "m"
                                 :max-turns 5 :beam-width 1})
          prev (knowledge/last-run-before c r2)]
      (is (some? (:started_at prev))
          "last-run-before returns started_at, which is what the caller cuts on")
      (is (= 1 (count (knowledge/learned-since c (:started_at prev))))
          "the previous run's OWN memory is included — the whole point")
      (is (empty? (knowledge/learned-since c (:ended_at prev)))
          "and the ended_at cut is what silently dropped it, pinned so a
           refactor cannot quietly reintroduce the bug"))))

(deftest recent-limits
  (dotimes [_ 3] (knowledge/remember! @conn {:content "row"}))
  (is (= 2 (count (knowledge/recent @conn 2)))))

(deftest forget-deletes
  (let [id (knowledge/remember! @conn {:content "unique needle here"})]
    (knowledge/forget! @conn id)
    (is (empty? (knowledge/recall @conn "needle")))))

(deftest remember-rethrows-non-collision-failures-instead-of-retrying
  ;; provenance R2-15: same as messages/send! — only a UNIQUE collision is an
  ;; id-allocation problem worth retrying; anything else must propagate.
  (let [real-execute db/execute!
        inserts (atom 0)]
    (with-redefs [db/execute!
                  (fn [conn q & opts]
                    (when (str/includes? (str (first q)) "INSERT INTO knowledge")
                      (swap! inserts inc)
                      (throw (ex-info "disk I/O error" {:errno 5})))
                    (apply real-execute conn q opts))]
      (is (thrown-with-msg? Exception #"disk I/O error"
                            (knowledge/remember! @conn {:content "nope"}))))
    (is (= 1 @inserts) "a non-collision failure is not retried")))

(deftest kind-defaults-to-note
  (knowledge/remember! @conn {:content "kindless"})
  (is (= ["note"] (distinct (mapv :kind (knowledge/recall @conn "kindless"))))))

(deftest get-by-id-returns-row-and-nil-for-miss
  (let [id (knowledge/remember! @conn {:content "exact row payload"})
        row (knowledge/get-by-id @conn id)]
    (is (map? row))
    (is (= id (:id row)))
    (is (= "exact row payload" (:content row))))
  (is (nil? (knowledge/get-by-id @conn "k-nope"))))

(deftest breadcrumb-index-bounded-and-has-ids
  (let [long (str "HEAD " (apply str (repeat 200 "x")) " TAIL-END-MARKER")
        id (knowledge/remember! @conn {:content long :kind "note"})
        idx (knowledge/breadcrumb-index @conn "")]
    (is (string? idx))
    (is (str/includes? idx id))
    (is (<= (count idx) 700))
    (is (str/includes? idx "HEAD"))
    (is (not (str/includes? idx "TAIL-END-MARKER")))))

(deftest breadcrumb-index-nil-on-empty-db
  (is (nil? (knowledge/breadcrumb-index @conn ""))))

(deftest breadcrumb-index-relevance-ranked
  (knowledge/remember! @conn {:content "beta unrelated"})
  (knowledge/remember! @conn {:content "alpha needle here"})
  (let [idx (knowledge/breadcrumb-index @conn "needle")]
    (is (string? idx))
    (is (str/includes? idx "alpha"))))

(deftest forget-tool-deletes-and-reports
  ;; review4: the store fn existed but no surface reached it — recall could
  ;; surface a wrong fact with no way to drop it.
  (let [id (knowledge/remember! @conn {:content "the earth is flat"})]
    (let [r (base/run-tool {:branch {:id "B1"} :conn @conn
                            :tool-name "forget" :args {:id id}})]
      (is (= :neutral (:category r)) "forgetting is bookkeeping, like remember")
      (is (str/includes? (:result r) "Forgot")))
    (is (nil? (knowledge/get-by-id @conn id)) "the memory is gone"))
  (testing "an unknown id fails honestly"
    (let [r (base/run-tool {:branch {:id "B1"} :conn @conn
                            :tool-name "forget" :args {:id "k-none"}})]
      (is (= :failure (:category r)))
      (is (str/includes? (:result r) "No memory"))))
  (testing "a missing id argument is malformed, not a crash"
    (let [r (base/run-tool {:branch {:id "B1"} :conn @conn
                            :tool-name "forget" :args {}})]
      (is (str/includes? (:result r) "Missing")))))

;; --- salience: memory that learns from being used ---------------------------

(deftest a-memory-kind-sets-its-starting-standing
  ;; The ordering is the claim: who we are outranks what is true, which
  ;; outranks how to do things, which outranks what happened once, which
  ;; outranks what we are doing right now.
  (is (< (memory/base-salience :working)
         (memory/base-salience :episodic)
         (memory/base-salience :procedural)
         (memory/base-salience :semantic)
         (memory/base-salience :identity)
         (memory/base-salience :overview)))
  (is (= (memory/base-salience :note) (memory/base-salience :wat))
      "an unclassified kind gets the note default, not zero — burying a memory
       nobody thought to categorise is worse than mis-tiering it"))

(deftest effectiveness-is-log-damped-signed-and-bounded
  ;; The axis that makes memory a loop rather than a list, and the damping is
  ;; what stops a memory being voted to the top by repetition.
  (is (zero? (memory/effectiveness 0 0)) "no record is not a bad record")
  (is (zero? (memory/effectiveness 7 7)) "an even record says nothing")
  (is (< 0.04 (memory/effectiveness 1 0) 0.05) "the first confirmation buys most of it")
  (is (< 0.14 (memory/effectiveness 9 0) 0.16))
  (is (= (- (memory/effectiveness 3 0)) (memory/effectiveness 0 3)) "symmetric")
  (is (<= (memory/effectiveness 100000 0) (:effectiveness-cap (memory/policy)))
      "a hot playbook cannot outrank a durable identity fact on its record alone"))

(deftest confidence-is-a-tiebreak-not-a-tier-jump
  (is (zero? (memory/confidence-bonus 0.6)) "the default is neutral")
  (let [swing (- (memory/confidence-bonus 1.0) (memory/confidence-bonus 0.0))
        tier-gap (- (memory/base-salience :semantic) (memory/base-salience :procedural))]
    (is (< swing (* 3 tier-gap))
        "salience is importance and confidence is truth-likelihood; a contested
         claim must not jump above a durable one on confidence alone")))

(deftest recall-reinforces-what-it-returns
  ;; Being looked up IS the relevance signal — the cheapest honest one, since
  ;; it needs nobody to judge anything.
  (let [id (knowledge/remember! @conn {:content "the deploy needs sudo" :kind "procedural"})
        before (:salience (knowledge/get-by-id @conn id))]
    (knowledge/recall @conn "deploy sudo")
    (let [after (knowledge/get-by-id @conn id)]
      (is (> (:salience after) before))
      (is (= 1 (:use_count after)))
      (is (some? (:last_used_at after))))))

(deftest an-outcome-moves-how-a-memory-ranks
  (let [good (knowledge/remember! @conn {:content "alpha rule about widgets" :kind "procedural"})
        bad (knowledge/remember! @conn {:content "beta rule about widgets" :kind "procedural"})]
    (dotimes [_ 3] (knowledge/record-outcome! @conn good true))
    (dotimes [_ 3] (knowledge/record-outcome! @conn bad false))
    (let [rows (knowledge/recall @conn "rule widgets")]
      (is (= good (:id (first rows)))
          "the one that worked outranks the one that did not, at equal kind"))))

(deftest standing-shows-what-the-project-learned-without-reinforcing-it
  ;; The supervisor's block. Being shown by default is not evidence a memory
  ;; was useful, and counting it would inflate exactly the entries already at
  ;; the top.
  (knowledge/remember! @conn {:content "this is a todo library" :kind "overview"})
  (knowledge/remember! @conn {:content "a passing thought" :kind "working"})
  (let [rows (knowledge/standing @conn)
        overview-id (:id (first rows))]
    (is (= "overview" (:kind (first rows)))
        "a reader who does not know what the project IS cannot judge the rest")
    (is (zero? (:use_count (knowledge/get-by-id @conn overview-id)))
        "standing does not reinforce")))

(deftest corroboration-counts-distinct-runs-only
  ;; backpass VISION.md: "a new instruction needs corroboration from at least
  ;; two distinct sessions, and one session never counts twice however often it
  ;; is re-analyzed". DISTINCT is the load-bearing word — without it a long run
  ;; corroborates its own findings by repetition, which is the overfitting the
  ;; count exists to prevent.
  (let [id (knowledge/remember! @conn {:content "a measured pattern" :kind "episodic"
                                       :run-id "r1"})]
    (is (= 1 (:corroborations (knowledge/get-by-id @conn id))))
    (is (= 1 (knowledge/corroborate! @conn id "r1"))
        "the same run again is the same evidence, not more of it")
    (is (= 2 (knowledge/corroborate! @conn id "r2")))
    (is (= 2 (knowledge/corroborate! @conn id "r2")))
    (is (= 3 (knowledge/corroborate! @conn id "r3")))))

(deftest one-run-is-an-observation-and-two-is-a-pattern
  ;; Not a claim that two runs prove anything — it is the difference between a
  ;; pattern and an afternoon, and the lowest bar that is still a bar.
  (let [id (knowledge/remember! @conn {:content "x" :kind "episodic" :run-id "r1"})]
    (is (not (knowledge/corroborated? (knowledge/get-by-id @conn id))))
    (knowledge/corroborate! @conn id "r2")
    (is (knowledge/corroborated? (knowledge/get-by-id @conn id)))))

(deftest a-pattern-is-identified-by-a-column-not-by-its-text
  ;; backpass has to match memory text by bigram similarity at a tuned
  ;; threshold, with a side-car ledger keyed by hashed phrasings, because its
  ;; memory is a markdown file and an instruction has no id. We have rows.
  ;;
  ;; Reproducing text identity on top of a table with a primary key inherits a
  ;; constraint we do not have, and it is fragile in the way text matching
  ;; always is — which is the case this pins.
  (let [id (knowledge/remember! @conn {:content "[lever] beam width 5 -> 2 — worse"
                                       :kind "procedural" :run-id "r1"
                                       :pattern-key "lever:beam-width-5-2"})]
    (is (= id (:id (knowledge/by-pattern @conn "lever:beam-width-5-2"))))
    (is (nil? (knowledge/by-pattern @conn "lever:something-else")))

    (testing "and the content may be rewritten without changing identity —
              the evidence differs every run, the pattern is what recurs"
      (knowledge/remember! @conn {:content "an unrelated note" :kind "procedural"})
      (is (= id (:id (knowledge/by-pattern @conn "lever:beam-width-5-2")))))))

(deftest one-lever-worded-two-ways-is-one-record
  (let [c @conn
        write (fn [change run]
                (knowledge/distill-verdicts!
                 c [{:name "e" :change change :hypothesis "h" :verdict :worse
                     :before 1.0 :after -1.0}]
                 {:run-id run}))]
    (write "beam width 5 -> 2" "r1")
    (write "beam-width  5→2" "r2")
    (let [rows (filter #(= "procedural" (:kind %)) (knowledge/recent c 20))]
      (is (= 1 (count rows))
          "a model writes the change description fresh each time; two
           spellings of one lever must not each look like a first attempt")
      (is (= 2 (:failure_count (first rows)))
          "and the record accumulates, so a lever that keeps failing sinks"))))

(deftest an-ordinary-memory-has-no-pattern-key
  ;; Sparse by design: a fact somebody typed has no pattern, and the index
  ;; costs nothing on the common row.
  (let [id (knowledge/remember! @conn {:content "the deploy needs sudo" :kind "semantic"})]
    (is (nil? (:pattern_key (knowledge/get-by-id @conn id))))))

(deftest a-run-leaves-behind-what-it-learned-about-the-project
  ;; Everything else distilled here is the harness watching ITSELF — patterns
  ;; in how the loop ran, verdicts on the supervisor's changes. None of it is
  ;; about the codebase being worked on, so an implementor started every
  ;; session knowing nothing and spent its first turns rediscovering where the
  ;; source lives and which commands the policy refuses. Measured across the
  ;; live runs: 46 turns, zero remember calls, zero memories.
  (let [c @conn
        rid (runs/start-run! c {:problem "p"})]
    (runs/open-branch! c rid {:branch-id "B1"})
    (journal/record-turn! c rid {:branch-id "B1" :turn 1 :tool-name "shell"
                                 :args {:command "cat deps.edn"} :result "{:paths}"
                                 :category "success"})
    (journal/record-turn! c rid {:branch-id "B1" :turn 2 :tool-name "shell"
                                 :args {:command "find . | head -5"}
                                 :result "Command needs approval: not on the allow list"
                                 :category "neutral"})
    (let [facts (knowledge/distil-project! c {:run-id rid})]
      (is (= 2 (count facts)))
      (let [contents (map (comp :content #(knowledge/get-by-id c %) :id) facts)]
        (is (some #(str/includes? % "`cat deps.edn` works") contents))
        (is (some #(str/includes? % "refused by the shell policy") contents)
            "a refusal is a fact from the other side, and saves the next run
             the turn it would spend learning the same refusal")))

    (testing "these are durable facts, not episodes — the test command does not
              stop being the test command because this run ended"
      (is (every? #(= "semantic" (:kind %))
                  (filter :pattern_key (knowledge/recent c 20)))))

    (testing "and a second run confirms rather than duplicating"
      (let [rid2 (runs/start-run! c {:problem "p2"})]
        (runs/open-branch! c rid2 {:branch-id "B1"})
        (journal/record-turn! c rid2 {:branch-id "B1" :turn 1 :tool-name "shell"
                                      :args {:command "cat deps.edn"} :result "x"
                                      :category "success"})
        (let [again (knowledge/distil-project! c {:run-id rid2})]
          (is (every? :repeat? again))
          (is (= 2 (:corroborations (knowledge/by-pattern c "cmd-works:cat deps.edn")))))))))

(deftest a-recurring-finding-is-corroborated-not-penalised
  ;; karamazov-blt.25: the outcome axis means "did ACTING on this memory
  ;; work" (RFC-010), and nothing on the distillation path measures that. The
  ;; old record-outcome! call turned every re-observation of a persistent
  ;; problem into a failure_count, sinking exactly the memories that matter
  ;; most below trivia. And the raw content UPDATE skipped the FTS mirror, so
  ;; the memory answered only to its FIRST wording.
  (let [c @conn]
    (let [rid1 (runs/start-run! c {:problem "p1"})
          rid2 (runs/start-run! c {:problem "p2"})]
      (knowledge/distill! c [{:kind :tool-failing :severity :bad
                              :detail "eval keeps failing" :evidence {:rate 0.5}}]
                          {:run-id rid1})
      (knowledge/distill! c [{:kind :tool-failing :severity :bad
                              :detail "shell keeps failing" :evidence {:rate 0.6}}]
                          {:run-id rid2})
      (let [row (knowledge/by-pattern c "finding:tool-failing")]
        (is (= 2 (:corroborations row)) "recurrence is corroboration")
        (is (zero? (or (:failure_count row) 0))
            "a re-observation is not a failed outcome")
        (is (str/includes? (str (:content row)) "shell keeps failing")
            "the content follows the newest evidence")
        (is (some #(= (:id row) (:id %)) (knowledge/recall c "shell"))
            "and the memory answers to its NEW wording, not just its first")))))

;; --- the store must not tell a new run its work is already done -------------

(deftest a-completion-claim-needs-a-diff-to-back-it
  ;; karamazov-mjb, run cc88a760. The shared store held two memories from
  ;; earlier FAILED runs — "messages.clj store is settled and verified
  ;; end-to-end at the REPL" and one like it — written by workers who had
  ;; eval-prototyped and never written the files. A later worker recalled one
  ;; at turn 3, concluded its part was done, and called done at turn 14
  ;; literally saying "I did not complete the change".
  ;;
  ;; A completion claim is the ONE memory the harness can check, and the one
  ;; that does real damage when wrong, because memories outlive their run.
  (let [conn (db/open! ":memory:")
        call (fn [content changed]
               (with-redefs [gitdiff/changed-files (constantly changed)]
                 (:result (tools/run-tool {:branch {:id "W1"} :tool-name "remember"
                                           :conn conn :root "/x" :git-baseline "b"
                                           :args {:content content}}))))]
    (testing "a claim that the work is finished, off an empty diff, is refused"
      (is (str/includes? (call "the messages store is settled and verified end-to-end" [])
                         "Nothing was stored")))
    (testing "but a FINDING is not gated — a supervisor writing down what it
              diagnosed has no diff and should not need one"
      (is (str/includes? (call "jolt has no Java interop; a JVM diagnosis is wrong" [])
                         "Remembered")))
    (testing "and the same claim WITH a real diff is stored"
      (is (str/includes? (call "the messages store is settled and verified" ["src/messages.clj"])
                         "Remembered")))
    (testing "it fails OPEN when git cannot answer — nil is cannot-tell, not
              nothing-changed, and refusing on unknown would block honest
              memories on any checkout without git"
      (is (str/includes? (call "the store is settled and verified" nil) "Remembered")))))

(deftest a-completion-claim-from-another-run-is-not-authority-over-this-one
  (let [conn (db/open! ":memory:")]
    (knowledge/remember! conn {:content "the messages store is settled and verified"
                               :run-id "OLD-RUN"})
    (knowledge/remember! conn {:content "jolt has no Java interop, so a JVM diagnosis is wrong"
                               :run-id "OLD-RUN"})
    (let [out (:result (tools/run-tool {:branch {:id "W1"} :tool-name "recall" :conn conn
                                        :run-id "NEW-RUN"
                                        :args {:query "messages store jolt interop"}}))]
      (is (str/includes? out "recorded by an EARLIER run")
          "the completion claim is flagged")
      (is (= 1 (count (re-seq #"recorded by an EARLIER run" out)))
          "and only it — a finding from another run is ordinary knowledge")
      (is (str/includes? out "settled and verified")
          "the claim still SHOWS: that the work has a history is often the
           useful hint. What it no longer does is settle the question"))
    (testing "and a claim from THIS run carries no annotation"
      (knowledge/remember! conn {:content "the parser is complete and verified"
                                 :run-id "NEW-RUN"})
      (let [out (:result (tools/run-tool {:branch {:id "W1"} :tool-name "recall" :conn conn
                                          :run-id "NEW-RUN" :args {:query "parser complete"}}))]
        (is (not (str/includes? out "recorded by an EARLIER run")))))))

;; --- the text stage's opinion, kept on the line (karamazov-sb3j) -------------

(deftest recall-carries-the-text-rank-through-the-salience-sort
  ;; recall ranks in two stages: bm25 picks and orders the candidates, then
  ;; effective salience re-sorts them. The second stage can lift a distant
  ;; text match over a close one on its record, and the line the model read
  ;; showed only the standing — so a memory pulled up by what it had done
  ;; looked the same as one that fit the words. The text stage's rank rides
  ;; on the row so the line can show both, and the model can discount a weak
  ;; fit itself rather than the harness dropping it (karamazov-3bf: a ranking
  ;; is not retuned on one run's evidence; a hidden ordering is not a ranking
  ;; the model can judge).
  (when (db/fts5-available? @conn)
    (let [close (knowledge/remember! @conn {:content "the deploy script needs sudo"
                                            :kind "procedural"})
          far (knowledge/remember! @conn {:content "unrelated note about sudo policy and the deploy of other things"
                                          :kind "procedural"})]
      (dotimes [_ 3] (knowledge/record-outcome! @conn far true))
      (let [rows (knowledge/recall @conn "deploy script")]
        (is (= [far close] (mapv :id rows))
            "the record outranks the fit, at equal kind")
        (is (= [2 1] (mapv :text-rank rows))
            "and the text rank still says which one matched the words")))))

(deftest a-scan-has-no-text-rank-to-show
  ;; The LIKE path has no ranking to report: every row matched the substring
  ;; equally and newest-first is the only order it has. A rank there would be
  ;; a number that means nothing, and no number beats a false one.
  (knowledge/remember! @conn {:content "fred likes fish"})
  (with-redefs [db/fts5-available? (fn [_] false)]
    (let [rows (knowledge/recall @conn "fred")]
      (is (seq rows))
      (is (every? #(nil? (:text-rank %)) rows)))))

(deftest the-recall-line-shows-the-match-rank
  (when (db/fts5-available? @conn)
    (knowledge/remember! @conn {:content "the deploy script needs sudo" :kind "procedural"})
    (let [r (base/run-tool {:branch {:id "B1"} :conn @conn
                            :tool-name "recall" :args {:query "deploy script"}})]
      (is (str/includes? (:result r) " m1 ")
          "m1 is the closest match to the words asked")))
  (testing "a memory fetched by id had no query to be ranked against"
    (let [id (knowledge/remember! @conn {:content "a solo fact"})
          r (base/run-tool {:branch {:id "B1"} :conn @conn
                            :tool-name "recall" :args {:id id}})]
      (is (str/includes? (:result r) "a solo fact"))
      (is (not (re-find #" m\d+ " (:result r)))))))

;; --- age in runs, not days (karamazov-h27r) ---------------------------------

(deftest a-memory-written-this-run-does-not-decay-when-the-run-ends
  ;; Measured across the campaign dbs (karamazov-4ay9): every row was younger
  ;; than the decay window, and 1073 of 1117 sat within 0.2 of the floor.
  ;; curate! treated a null last_used_at as stale, so a memory lost 0.05 at
  ;; the end of the very run that wrote it and at every run end after — the
  ;; most-corroborated findings in the store ranked below `pwd works`. A
  ;; memory has to have HAD a run to go unused in before it can be said to
  ;; have gone unused.
  (let [c @conn
        r1 (runs/start-run! c {:problem "p"})
        _ (Thread/sleep 5)
        id (knowledge/remember! c {:content "a durable fact" :kind "semantic" :run-id r1})
        s0 (:salience (knowledge/get-by-id c id))]
    (knowledge/distil-session! c {:run-id r1})
    (let [row (knowledge/get-by-id c id)]
      (is (= 0 (:idle_runs row)) "the run that wrote it is not a run it went unused in")
      (is (= s0 (:salience row))))))

(deftest age-is-counted-in-runs-a-memory-could-have-been-used-in
  ;; cellularflow ages a slot per forward pass — per opportunity to be read —
  ;; not per second on the wall clock. A harness idle for a month should not
  ;; forget what it learned; a harness that ran ten times without needing a
  ;; memory has evidence about it.
  (let [c @conn
        r1 (runs/start-run! c {:problem "p"})
        _ (Thread/sleep 5)
        id (knowledge/remember! c {:content "a durable fact" :kind "semantic" :run-id r1})]
    (knowledge/distil-session! c {:run-id r1})
    (Thread/sleep 5)
    (let [r2 (runs/start-run! c {:problem "p"})]
      (knowledge/distil-session! c {:run-id r2})
      (is (= 1 (:idle_runs (knowledge/get-by-id c id))) "one run went by without it"))
    (Thread/sleep 5)
    (let [r3 (runs/start-run! c {:problem "p"})]
      (Thread/sleep 5)
      (knowledge/recall c "durable fact")
      (knowledge/distil-session! c {:run-id r3})
      (is (= 0 (:idle_runs (knowledge/get-by-id c id))) "recalled during the run: not idle"))
    (testing "a pinned memory does not age"
      (let [pinned (knowledge/remember! c {:content "pinned" :kind "semantic" :pinned true})]
        (Thread/sleep 5)
        (let [r4 (runs/start-run! c {:problem "p"})]
          (knowledge/distil-session! c {:run-id r4})
          (is (= 0 (:idle_runs (knowledge/get-by-id c pinned))))
          (is (= 1 (:idle_runs (knowledge/get-by-id c id)))))))))

(deftest decay-starts-only-past-the-window-in-runs
  (with-redefs [memory/policy (let [p (memory/policy)]
                                (fn [] (assoc p :recent-use-window-runs 2 :disuse-decay 0.05)))]
    (let [c @conn
          r1 (runs/start-run! c {:problem "p"})
          _ (Thread/sleep 5)
          id (knowledge/remember! c {:content "a durable fact" :kind "semantic" :run-id r1})
          s0 (:salience (knowledge/get-by-id c id))
          run-end! (fn [] (Thread/sleep 5)
                     (knowledge/distil-session! c {:run-id (runs/start-run! c {:problem "p"})}))]
      (knowledge/distil-session! c {:run-id r1})
      (run-end!) (run-end!)
      (is (= s0 (:salience (knowledge/get-by-id c id)))
          "two idle runs is inside the window: nothing has been shown yet")
      (run-end!)
      (is (< (Math/abs (- (- s0 0.05) (:salience (knowledge/get-by-id c id)))) 1e-9)
          "the third is past it, and the memory begins to fall"))))

(deftest the-recent-use-bonus-is-by-runs
  ;; Pure: a row used minutes ago but idle for more runs than the window is
  ;; not recently used, and a row used long ago by the clock but in the last
  ;; run is.
  (let [p (memory/policy)
        base {:kind "semantic" :salience 0.6 :last_used_at "2020-01-01T00:00:00Z"}]
    (is (> (memory/effective-salience (assoc base :idle_runs 0) p)
           (memory/effective-salience (assoc base :idle_runs (inc (:recent-use-window-runs p))) p)))
    (is (= (memory/effective-salience (assoc base :idle_runs 0) p)
           (+ 0.6 (:recent-use-bonus p))))
    (is (= (memory/effective-salience (dissoc base :last_used_at) p) 0.6)
        "never used earns no bonus however fresh it is")))

;; --- corroboration ranks (karamazov-h27r) ------------------------------------

(deftest a-pattern-seen-in-more-runs-outranks-one-seen-once
  ;; The store already counted distinct-run sightings and the count entered
  ;; the ranking nowhere: a finding confirmed by seven runs sat at the floor
  ;; under a command that worked once. cellularflow's importance is the
  ;; accumulated attention a slot receives; ours is the accumulated
  ;; independent sightings a pattern receives, damped and capped exactly like
  ;; the outcome record so repetition cannot buy the top.
  (let [c @conn
        once (knowledge/remember! c {:content "widget pattern alpha" :kind "episodic" :run-id "r1"})
        often (knowledge/remember! c {:content "widget pattern beta" :kind "episodic" :run-id "r1"})]
    (doseq [r ["r2" "r3" "r4" "r5"]] (knowledge/corroborate! c often r))
    (is (= [often once] (mapv :id (knowledge/recall c "widget pattern"))))))

(deftest corroboration-is-log-damped-and-capped
  (let [p (memory/policy)]
    (is (zero? (memory/corroboration-bonus 1 p)) "one sighting is the baseline, not evidence")
    (is (zero? (memory/corroboration-bonus nil p)))
    (is (< 0 (memory/corroboration-bonus 2 p) (memory/corroboration-bonus 7 p)))
    (is (> (- (memory/corroboration-bonus 2 p) (memory/corroboration-bonus 1 p))
           (- (memory/corroboration-bonus 3 p) (memory/corroboration-bonus 2 p)))
        "each further sighting buys less than the one before")
    (is (= (:corroboration-cap p) (memory/corroboration-bonus 1000000 p)))))

;; --- one command, one memory (karamazov-h27r) --------------------------------

(deftest trivially-different-pipelines-of-one-command-are-one-memory
  ;; harness.sqlite3 after ten runs: `jolt -M:test 2>&1 | tail -20`,
  ;; `| tail -40`, `| tail -25`, `; echo EXIT=$?` were five semantic memories
  ;; each saying the test command works — 47 rows keyed on that one command.
  ;; The pipe tail and the exit echo are how the model READS the output, not
  ;; part of what works here.
  (let [c @conn
        rid (runs/start-run! c {:problem "p"})]
    (runs/open-branch! c rid {:branch-id "B1"})
    (doseq [[t cmd] [[1 "jolt -M:test"]
                     [2 "jolt -M:test 2>&1 | tail -20"]
                     [3 "jolt -M:test 2>&1 | tail -40; echo EXIT=$?"]
                     [4 "jolt -M:test | head -n 5"]
                     [5 "jolt -M:test 2>&1 | tail -12; echo \"EXIT:$?\""]
                     ;; NOT noise: a cd changes what the command means.
                     [6 "cd sub && jolt -M:test"]]]
      (journal/record-turn! c rid {:branch-id "B1" :turn t :tool-name "shell"
                                   :args {:command cmd} :result "ok" :category "success"}))
    (let [facts (knowledge/distil-project! c {:run-id rid})]
      (is (= 2 (count facts)))
      (is (= "In this project `jolt -M:test` works."
             (:content (knowledge/by-pattern c "cmd-works:jolt -M:test"))))
      (is (some? (knowledge/by-pattern c "cmd-works:cd sub && jolt -M:test"))))))

;; --- a bounded working set (karamazov-h27r) ----------------------------------

(deftest over-the-cap-the-lowest-standing-memories-are-retired-not-deleted
  ;; cellularflow's episodic buffer has max_slots and evicts the least
  ;; important slot; the store's working set is `current = 1`, and eviction
  ;; is retire! — the row stays readable by id and in the lineage history
  ;; with the reason, it just stops being recalled. Demotion, not deletion.
  (with-redefs [memory/policy (let [p (memory/policy)]
                                (fn [] (assoc p :max-current-per-kind {:semantic 3})))]
    (let [c @conn
          ids (mapv #(knowledge/remember! c {:content (str "fact number " %) :kind "semantic"})
                    (range 5))
          pinned (knowledge/remember! c {:content "pinned fact" :kind "semantic" :pinned true})
          keep (first ids)]
      (dotimes [_ 3] (knowledge/record-outcome! c keep true))
      (let [evicted (knowledge/evict! c)]
        (is (= 2 (count evicted)) "five unpinned over a cap of three")
        (is (not (contains? (set evicted) keep)) "the one with a record stays")
        (is (not (contains? (set evicted) pinned)) "pinned is never evicted and never counted")
        (doseq [id evicted]
          (let [row (knowledge/get-by-id c id)]
            (is (some? row) "still readable by id")
            (is (= 0 (:current row)))
            (is (str/starts-with? (str (:retired_reason row)) "evicted"))))
        (is (= 4 (knowledge/live-count c)) "three semantic plus the pinned one")
        (is (empty? (knowledge/evict! c)) "idempotent at the cap")))))

;; --- graduation candidates (karamazov-h27r) ----------------------------------

(deftest patterns-confirmed-across-runs-are-surfaced-as-candidates-for-a-rule
  ;; cellularflow consolidates its most-used episodic slots into permanent
  ;; memory each epoch. distill! says promoting an episode to a rule is a
  ;; judgement and the supervisor's, so the store does not promote — it hands
  ;; the supervisor the episodes that have earned the question.
  (let [c @conn
        ep (knowledge/remember! c {:content "[tool-failing] done fails more often than it works"
                                   :kind "episodic" :run-id "r1" :pattern-key "finding:tool-failing"})
        once (knowledge/remember! c {:content "[x] seen once" :kind "episodic" :run-id "r1"})
        rule (knowledge/remember! c {:content "a rule" :kind "procedural" :run-id "r1"})
        refuted (knowledge/remember! c {:content "[y] seen and disproven" :kind "episodic" :run-id "r1"})]
    (doseq [r ["r2" "r3"]]
      (knowledge/corroborate! c ep r)
      (knowledge/corroborate! c rule r)
      (knowledge/corroborate! c refuted r))
    (knowledge/record-outcome! c refuted false)
    (is (= [ep] (mapv :id (knowledge/graduation-candidates c)))
        "corroborated episodes with a record that is not against them: a rule
         is already a rule, one sighting is an afternoon, and a refuted
         episode is not a rule in waiting")
    (is (= 3 (:corroborations (first (knowledge/graduation-candidates c)))))))
