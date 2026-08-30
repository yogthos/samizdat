;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.oversight-test
  "The supervisor as a PARALLEL STREAM.

  The mechanism under test is deliberately ignorant of supervision: it runs
  some pass function on a cadence, against a budget, in a thread that cannot
  hurt the run it watches. What that pass DOES is a cell, because the harness's
  own policy about when to think and what to think about has to be something
  the agent can rewrite at runtime."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.gates :as gates]
            [samizdat.cells :as cells]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.store.db :as db]
            [samizdat.store.runs :as runs]
            [samizdat.agent.oversight :as ov]))

;; --- when a pass is due -----------------------------------------------------

(deftest a-pass-is-due-on-cadence-and-on-a-signal
  (testing "not due before the cadence has elapsed"
    (is (not (ov/due? {:last-at 100 :passes 0} {:now 150 :every-ms 100}))))
  (testing "due once it has"
    (is (ov/due? {:last-at 100 :passes 0} {:now 200 :every-ms 100})))
  (testing "the FIRST pass is due immediately — a supervisor that waits out a
            full cadence before its first look is blind through exactly the
            opening stretch where a run picks its approach"
    (is (ov/due? {:last-at nil :passes 0} {:now 0 :every-ms 100})))
  (testing "a signal makes a pass due early: the stream exists to notice
            trouble while it is forming, not on the next tick"
    (is (ov/due? {:last-at 100 :passes 0} {:now 110 :every-ms 100 :signal? true}))))

(deftest the-stream-is-bounded
  ;; Every pass is a model call. A supervisor that reasons on every tick of a
  ;; 300-turn run costs more than the run it is supervising.
  (testing "under budget, passes continue"
    (is (ov/due? {:last-at 0 :passes 3} {:now 999 :every-ms 1 :budget 5})))
  (testing "at budget, nothing is due again — including on a signal, or the
            bound would be advisory"
    (is (not (ov/due? {:last-at 0 :passes 5} {:now 999 :every-ms 1 :budget 5})))
    (is (not (ov/due? {:last-at 0 :passes 5}
                      {:now 999 :every-ms 1 :budget 5 :signal? true})))))

;; --- the stream cannot hurt the run ----------------------------------------

(deftest a-throwing-pass-neither-stops-the-stream-nor-escapes-it
  ;; The whole point of an observer is that its failure costs the run nothing.
  ;; watch.clj learned this already; a reasoning stream fails in more ways.
  (let [calls (atom 0)
        pass (fn [_] (swap! calls inc) (throw (ex-info "boom" {})))
        st (atom {:passes 0})]
    (is (nil? (ov/pass! {} st pass)))
    (is (nil? (ov/pass! {} st pass)))
    (is (= 2 @calls) "it kept going after the first throw")
    (is (= 2 (:passes @st)) "a throwing pass still spends its budget — an
                             observer that fails for free retries forever")))

(deftest the-stream-carries-one-context-across-passes
  ;; ITS OWN MEMORY STREAM. run-role mints a fresh branch per call, so the
  ;; supervisor in feature.edn re-reads the run cold every revision and cannot
  ;; refer to what it concluded last time. A stream that cannot remember its
  ;; own last conclusion cannot tell a change it made from a change it only
  ;; considered.
  (let [seen (atom [])
        pass (fn [{:keys [carry]}] (swap! seen conj carry) (inc (or carry 0)))
        st (atom {:passes 0})]
    (ov/pass! {} st pass)
    (ov/pass! {} st pass)
    (ov/pass! {} st pass)
    (is (= [nil 1 2] @seen)
        "each pass sees what the previous one returned")))

(deftest stopping-is-idempotent-and-ends-the-thread
  (let [stop (ov/start! {:enabled? false} (fn [_] nil))]
    (is (fn? stop) "a disabled stream still returns a stop function, so the
                    caller's teardown never has to check")
    (is (nil? (stop)))
    (is (nil? (stop)) "called twice from a crash path and a finally")))

;; --- the behaviour layer ----------------------------------------------------
;; The mechanism above is domain-blind. These cover the cells, which decide
;; what a pass looks at and whether it is worth a model call at all.

(defn- worth-a-look? [& args]
  (cells/load-cells!)
  (apply @(ns-resolve 'cells.oversight 'worth-a-look?) args))

(deftest a-healthy-run-costs-nothing
  ;; The cheap path has to be the DEFAULT, or the stream costs more than the
  ;; run it watches. A run that is shipping has nothing to tune, and saying so
  ;; would spend a model call to say nothing.
  (let [floors {:unmet-floor 2 :idle-floor 25}]
    (is (not (worth-a-look? {:unmet-gates 0 :idle-turns 3 :errors nil} floors)))
    (is (not (worth-a-look? {:unmet-gates 1 :idle-turns 24 :errors nil} floors))
        "just under both floors is still quiet — one unmet gate is noise")))

(deftest the-three-signals-that-buy-a-model-call
  (let [floors {:unmet-floor 2 :idle-floor 25}]
    (testing "steering that is being ignored — the harness's own words failing,
              which is the supervisor's actual subject"
      (is (worth-a-look? {:unmet-gates 2 :idle-turns 0 :errors nil} floors)))
    (testing "a run producing nothing"
      (is (worth-a-look? {:unmet-gates 0 :idle-turns 25 :errors nil} floors)))
    (testing "a stage crashed — a harness bug the loop survived, which recurs
              on the next run if nobody looks"
      (is (worth-a-look? {:unmet-gates 0 :idle-turns 0 :errors [{:x 1}]} floors)))))

(deftest the-stalls-this-project-actually-had-would-all-have-woken-it
  ;; Regression against the record rather than against a number I chose. Every
  ;; run in this campaign that went quiet did so with a long idle stretch; if a
  ;; threshold change stops waking on these, it has gone wrong.
  (let [floors {:unmet-floor (gates/threshold :oversight-unmet-floor)
                :idle-floor (gates/threshold :oversight-idle-floor)}]
    (doseq [[run idle] [["bd56a286 T1" 316] ["c377260b revise" 148]
                        ["d304f539 T0" 87] ["986f33d8 T0 after its one write" 47]]]
      (is (worth-a-look? {:idle-turns idle} floors)
          (str run " stalled for " idle " turns and nothing looked at it")))))

(deftest clipping-a-note-survives-whitespace-collapse
  ;; The bug this replaces could only fire once a pass SUCCEEDED: the note was
  ;; indexed with the original length after the whitespace had been collapsed,
  ;; so any multi-line answer overran the shortened string. It would have
  ;; thrown on the first real supervisor conclusion and been swallowed whole by
  ;; the (then silent) stage guard.
  (let [clip (do (cells/load-cells!) @(ns-resolve 'cells.oversight 'clip))]
    (is (= "a b c" (clip "a\n\n\nb\t\tc" 400))
        "collapsing must not leave the index past the end")
    (is (= "abc" (clip "abc" 400)) "shorter than the limit is returned whole")
    (is (= "ab" (clip "abcdef" 2)) "longer than the limit is cut to it")
    (is (= "" (clip nil 400)) "a pass with no answer clips to empty, not a throw")))

(deftest the-carry-continues-the-conversation-without-freezing-it
  ;; Run b2ffb2ad: S0 stuck at 23 turns across FIVE passes. The supervisor
  ;; called `done` on its first pass, and because the carry hands the whole
  ;; branch to the next pass, every later pass resumed an already-finished
  ;; branch and returned immediately. The supervisor spoke once and was
  ;; silent for the rest of the run — the exact failure the stream exists to
  ;; prevent, reintroduced by the mechanism meant to give it memory.
  ;;
  ;; The carry must preserve what it LEARNED and not that it had STOPPED.
  (let [resume (do (cells/load-cells!) @(ns-resolve 'cells.oversight 'resume-branch))
        finished {:id "S0" :messages [{:role "user" :content "hello"}
                                      {:role "assistant" :content "a conclusion"}]
                  :final-answer "done for now" :verdict :done :advisory? true}
        next-pass (resume finished)]
    (is (= 2 (count (:messages next-pass)))
        "the conversation so far is kept — that is the whole point of a stream")
    (is (nil? (:final-answer next-pass)) "not already answered")
    (is (nil? (:verdict next-pass)) "not already finished")
    (is (:advisory? next-pass) "still an advisory branch, not shippable work")))

(defn- event-count [conn run-id kind]
  (:n (first (db/fetch conn ["SELECT count(*) AS n FROM events
                               WHERE run_id = ? AND kind = ?" run-id kind]))))

(deftest every-oversight-stage-actually-runs
  ;; CELLS ARE LOAD-STRINGED, so nothing type- or arity-checks them until the
  ;; moment they run — inside a guard that catches and logs rather than throws.
  ;; A stage could therefore be broken for a whole run and the only trace was
  ;; one WARN nobody was reading. That is exactly what happened: renaming
  ;; `safely` to take a stage label missed one nested call site, so EVERY
  ;; reasoning pass of run 5a2605b1 died with "Wrong number of args (2)" and
  ;; the stream looked merely quiet.
  ;;
  ;; So: run the stages for real and assert on their OUTPUT, which a swallowed
  ;; exception cannot fake.
  (cells/load-cells!)
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        ctx {:conn conn :run-id rid :config {}}]
    (testing "gather reaches its verdict rather than the guard's fallback"
      (let [out ((:handler (cell/get-cell! :oversight/gather)) ctx {})]
        ;; The fallback also sets worth-a-look? false, so assert on a key only
        ;; the real path produces.
        (is (contains? out :oversight/idle)
            "gather fell into its exception guard — check the log for
             'oversight :gather failed'")
        (is (contains? out :oversight/unmet))))
    (testing "quiet writes its heartbeat"
      (let [out ((:handler (cell/get-cell! :oversight/quiet)) ctx {:oversight/idle 1 :oversight/unmet 0})]
        (is (some? out))
        (is (pos? (event-count conn rid "oversight-quiet"))
            "no heartbeat row — a quiet stream is indistinguishable from a dead one")))
    (testing "apply records the pass"
      ((:handler (cell/get-cell! :oversight/apply)) ctx {:oversight/idle 9 :oversight/unmet 2
                                              :oversight/answer "a\n\nconclusion"})
      (is (pos? (event-count conn rid "oversight"))))))

(deftest the-reasoning-stage-runs-all-the-way-through
  ;; THE ONE THAT MATTERS. The arity bug lived in :oversight/reason, which the
  ;; stage test above cannot reach because reason takes a model turn. So stub
  ;; the turn and assert reason still carries its answer out — a swallowed
  ;; exception anywhere in its body (the catalog call, the digest, the prompt
  ;; render) leaves the answer nil, which is exactly how run 5a2605b1 looked
  ;; from outside: passes recorded, nothing learned.
  (cells/load-cells!)
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        ctx {:conn conn :run-id rid :config {}}]
    (with-redefs [myc/run-compiled (fn [_ _ data]
                                     {:branch (assoc (:branch data)
                                                     :final-answer "nothing is wrong")})]
      (let [out ((:handler (cell/get-cell! :oversight/reason))
                 ctx {:oversight/idle 30 :oversight/unmet 2
                      :oversight/turns [] :oversight/firings []})]
        (is (= "nothing is wrong" (:oversight/answer out))
            "reason fell into its guard — check the log for 'oversight :reason failed'")
        (is (some? (:oversight/branch out))
            "the branch must come back out, or the stream has no memory")))))

(deftest a-blank-pass-says-why-it-was-blank
  ;; karamazov-r5a. Run b2ffb2ad journalled four passes, every one with
  ;; notes:null, and the record could not say whether the supervisor had
  ;; concluded nothing, run out of turns, or crashed. Three very different
  ;; things, one blank field. The verdict is what separates them.
  (cells/load-cells!)
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        ctx {:conn conn :run-id rid :config {}}
        note (fn [] (json/read-str
                     (str (:data (last (db/fetch conn
                                                 ["SELECT data FROM events
                                                    WHERE run_id = ? AND kind = 'oversight'
                                                    ORDER BY id" rid]))))
                     :key-fn keyword))]
    (testing "a pass that concluded records both its verdict and its words"
      ((:handler (cell/get-cell! :oversight/apply))
       ctx {:oversight/idle 9 :oversight/unmet 2 :oversight/verdict :done
            :oversight/answer "the classpath is wrong"})
      (is (= "done" (:verdict (note))))
      (is (str/includes? (str (:notes (note))) "classpath")))
    (testing "a pass that ran out of turns says so instead of going blank"
      ((:handler (cell/get-cell! :oversight/apply))
       ctx {:oversight/idle 21 :oversight/unmet 5 :oversight/verdict :exhausted})
      (is (= "exhausted" (:verdict (note))))
      (is (nil? (:notes (note)))))))

(deftest a-reasoning-pass-that-throws-lands-in-the-record
  ;; The guard keeps the stream alive; it must not also keep the failure
  ;; secret. A silent guard is how an arity bug survived a whole run looking
  ;; like a merely quiet supervisor.
  (cells/load-cells!)
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        ctx {:conn conn :run-id rid :config {}}]
    (with-redefs [myc/run-compiled (fn [& _] (throw (ex-info "provider exploded" {})))]
      (let [out ((:handler (cell/get-cell! :oversight/reason))
                 ctx {:oversight/idle 30 :oversight/unmet 2
                      :oversight/turns [] :oversight/firings []})]
        (is (= :error (:oversight/verdict out)))
        (is (str/includes? (str (:oversight/answer out)) "provider exploded"))))))

(deftest the-stream-and-the-stage-do-not-share-a-branch-id
  ;; Run 498450e1: branch S0 held 26 turn rows numbered up to 14. The stream
  ;; opened S0 and :feature/supervise opens S<revision>, which is S0 on the
  ;; first round — two supervisors with separate contexts writing one branch,
  ;; overwriting each other's turn numbers. A record that cannot say which
  ;; supervisor said what is a record of neither (karamazov-poe).
  (cells/load-cells!)
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        ctx {:conn conn :run-id rid :config {}}]
    (with-redefs [myc/run-compiled (fn [_ _ data]
                                     {:branch (assoc (:branch data) :final-answer "ok")})]
      (let [out ((:handler (cell/get-cell! :oversight/reason))
                 ctx {:oversight/idle 30 :oversight/unmet 2
                      :oversight/turns [] :oversight/firings []})]
        (is (= "SUP" (get-in out [:oversight/branch :id])))
        (is (not= "S0" (get-in out [:oversight/branch :id])))))))

;; --- what the budget actually counts (karamazov-808) ------------------------

(deftest a-quiet-pass-is-free-and-a-reasoning-one-is-not
  ;; Run a3566c73, live. The stream went silent exactly when the run needed
  ;; it: twelve quiet heartbeats through a healthy first half exhausted a
  ;; budget of twelve, and when the branch later livelocked with five unmet
  ;; gates — against a floor of two — nothing was left to spend.
  ;;
  ;; The budget bounds MODEL CALLS. worth-a-look? is cheap and pure precisely
  ;; so most passes make none, so counting those passes turned the mechanism
  ;; that makes the stream affordable into the one that silences it.
  (let [st (atom {:passes 0})]
    (dotimes [_ 5] (ov/pass! {} st (fn [_] {:carry :b :spent? false})))
    (is (= 0 (:passes @st)) "five quiet looks cost nothing")
    (is (= 5 (:looks @st)) "but they are still counted, so a watching stream
                            can be told apart from a dead one")
    (ov/pass! {} st (fn [_] {:carry :b :spent? true}))
    (is (= 1 (:passes @st)) "the pass that called a model spends")))

(deftest an-unreported-or-throwing-pass-is-assumed-expensive
  (testing "a bare return value spends, as it always did — guessing the other
            way is how a bound stops binding"
    (let [st (atom {:passes 0})]
      (ov/pass! {} st (fn [_] :some-carry))
      (is (= 1 (:passes @st)))
      (is (= :some-carry (:carry @st)))))
  (testing "and a throw still spends: an observer that fails for free retries
            a broken pass until the run ends"
    (let [st (atom {:passes 0})]
      (ov/pass! {} st (fn [_] (throw (ex-info "boom" {}))))
      (ov/pass! {} st (fn [_] (throw (ex-info "boom" {}))))
      (is (= 2 (:passes @st))))))

(deftest a-quiet-pass-does-not-erase-the-streams-memory
  ;; The other half of the same wiring bug. The beam's pass fn returned
  ;; (:oversight/branch out) as the carry, and a quiet pass produces no
  ;; branch — so every healthy look wiped the supervisor's accumulated context
  ;; and the next reasoning pass read the run cold. Carrying context across
  ;; passes is the entire reason this is a stream and not a node.
  (let [st (atom {:passes 0})
        remembered (fn [pass-ctx] {:carry (or nil (:carry pass-ctx)) :spent? false})]
    (ov/pass! {} st (fn [_] {:carry :the-branch :spent? true}))
    (is (= :the-branch (:carry @st)))
    (dotimes [_ 3] (ov/pass! {} st remembered))
    (is (= :the-branch (:carry @st))
        "three quiet passes later the supervisor still knows what it concluded")))

(deftest the-budget-gates-on-spent-passes-only
  (is (ov/due? {:last-at 0 :passes 0 :looks 99} {:now 999 :every-ms 1 :budget 3})
      "ninety-nine free looks do not exhaust a budget of three")
  (is (not (ov/due? {:last-at 0 :passes 3 :looks 3} {:now 999 :every-ms 1 :budget 3}))
      "three spent passes do"))
