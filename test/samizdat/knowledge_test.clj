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
