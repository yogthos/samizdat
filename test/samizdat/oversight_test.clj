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
            [samizdat.events :as events]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.session :as session]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.agent.oversight :as ov]))

;; --- when a pass is due -----------------------------------------------------

(deftest a-pass-is-due-on-a-turn-boundary-and-no-sooner-than-the-spacing
  ;; RFC-012 F2. Phase 2 EVALUATES A TURN, so it runs when a turn has ended —
  ;; not on a wall clock that fired every two minutes whether or not the
  ;; implementer had done anything, which is what it used to do. The clock is
  ;; now a spacing: a beam of five branches must not buy a pass per turn.
  (testing "the FIRST pass is due immediately — a supervisor that waits out a
            full cadence before its first look is blind through exactly the
            opening stretch where a run picks its approach"
    (is (ov/due? {:last-at nil :passes 0} {:now 0 :every-ms 100})))
  (testing "time passing earns nothing on its own: with no turn ended since
            the last pass there is nothing to evaluate"
    (is (not (ov/due? {:last-at 100 :passes 0} {:now 1000 :every-ms 100})))
    (is (not (ov/due? {:last-at 100 :passes 0} {:now 1000 :every-ms 100 :boundary? false}))))
  (testing "a boundary inside the spacing waits"
    (is (not (ov/due? {:last-at 100 :passes 0} {:now 150 :every-ms 100 :boundary? true}))))
  (testing "a boundary after the spacing is due"
    (is (ov/due? {:last-at 100 :passes 0} {:now 200 :every-ms 100 :boundary? true}))))

(deftest the-reasoning-pass-fires-when-a-turn-ends-not-when-the-clock-ticks
  ;; The same claim, driven through the thread: the stream reads the bus, and
  ;; a :turn journal event of ITS run is the boundary.
  (let [ch (events/subscribe)
        passes (atom 0)
        stop (ov/start! {:enabled? true :poll-ms 5 :every-ms 0 :budget 100
                         :run-id "R-f2" :event-ch ch}
                        (fn [_] (swap! passes inc) {:carry nil :spent? true}))]
    (try
      (Thread/sleep 80)
      (is (= 1 @passes) "the first look is immediate; after that the clock alone buys nothing")
      (events/publish! {:kind :turn :run-id "R-f2" :branch-id "B1" :turn 1})
      (Thread/sleep 80)
      (is (= 2 @passes) "a turn of this run ending is what evaluation runs on")
      (events/publish! {:kind :turn :run-id "someone-else" :branch-id "B1" :turn 1})
      (events/publish! {:kind :step :run-id "R-f2" :branch-id "B1" :turn 2 :node :infer})
      (Thread/sleep 80)
      (is (= 2 @passes) "another run's turn is not this run's boundary, and a step mid-turn is not a boundary")
      (finally (stop) (events/unsubscribe! ch)))))

(deftest the-stream-is-bounded
  ;; Every pass is a model call. A supervisor that reasons on every tick of a
  ;; 300-turn run costs more than the run it is supervising.
  (testing "under budget, passes continue"
    (is (ov/due? {:last-at 0 :passes 3} {:now 999 :every-ms 1 :budget 5 :boundary? true})))
  (testing "at budget, nothing is due again — including on a boundary, or the
            bound would be advisory"
    (is (not (ov/due? {:last-at 0 :passes 5}
                      {:now 999 :every-ms 1 :budget 5 :boundary? true})))))

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
      (is (worth-a-look? {:unmet-gates 0 :idle-turns 0 :errors [{:x 1}]} floors)))
    (testing "the outer loop reached its soft cap — a decision the loop hands
              to the supervisor, and now there is exactly one to hand it to"
      (is (worth-a-look? {:unmet-gates 0 :idle-turns 0 :errors nil :at-cap? true} floors)))))

;; --- what the stage used to see, the stream now sees (RFC-012 F1/F4) --------

(defn- reasoning-over
  "Run :oversight/gather then :oversight/reason on `rid` with the model turn
  stubbed, returning {:gather :prob} — what gather decided and the brief the
  supervisor would have read."
  [conn rid]
  (cells/load-cells!)
  (let [ctx {:conn conn :run-id rid :config {}}
        g ((:handler (cell/get-cell! :oversight/gather)) ctx {})
        prob (atom nil)]
    (with-redefs [myc/run-compiled (fn [_ _ data]
                                     (reset! prob (get-in data [:branch :problem]))
                                     {:branch (assoc (:branch data) :final-answer "ok")})]
      ((:handler (cell/get-cell! :oversight/reason)) ctx g))
    {:gather g :prob @prob}))

(deftest a-crashed-stage-reaches-the-one-supervisor
  ;; The feature loop's stages fail soft and note :stage-error; the stage
  ;; that used to show those to a supervisor of its own is gone. The stream's
  ;; gather reads them off the journal, they buy a model call, and the brief
  ;; names the crash in its own words.
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})]
    (journal/note! conn rid :stage-error
                   {:data {:stage "critique" :node "judge" :error "boom in the judge"}})
    (let [{:keys [gather prob]} (reasoning-over conn rid)]
      (is (true? (:oversight/worth-a-look? gather)) "a crash is worth a look")
      (is (= 1 (count (:oversight/crashes gather))))
      (is (str/includes? (str prob) "STAGE CRASHED"))
      (is (str/includes? (str prob) "boom in the judge")))))

(deftest the-loops-soft-cap-is-the-supervisors-to-decide
  ;; The feature loop's route note carries the revision and the soft cap. At
  ;; the cap the loop keeps solving on its own ladder, and the supervisor is
  ;; the one who may switch, re-budget or stop it — so reaching it is worth a
  ;; model call, and the brief says so with the round's own facts.
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})]
    (journal/note! conn rid :route
                   {:data {:decision "revise" :revision 3 :soft-cap 3 :strategy "board"
                           :hollow false :tests-passed false}})
    (let [{:keys [gather prob]} (reasoning-over conn rid)]
      (is (true? (:oversight/worth-a-look? gather)))
      (is (= 3 (get-in gather [:oversight/round :revision])))
      (is (str/includes? (str prob) "REVISION CAP REACHED"))
      (is (str/includes? (str prob) "revision 3")))))

(deftest a-round-under-the-cap-with-nothing-else-wrong-is-still-quiet
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})]
    (journal/note! conn rid :route
                   {:data {:decision "revise" :revision 1 :soft-cap 6 :strategy "board"}})
    (cells/load-cells!)
    (let [g ((:handler (cell/get-cell! :oversight/gather)) {:conn conn :run-id rid :config {}} {})]
      (is (false? (:oversight/worth-a-look? g))))))

(deftest the-reasoning-pass-reads-the-counters-since-its-last-look
  ;; RFC-012 protocol rule 4: measure what you changed. The stage used to stamp
  ;; a mark before each of its looks and show the delta; the stream inherits
  ;; that, so a pass can tell whether the change it made last pass moved
  ;; anything.
  (session/reset!)
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "p"})
        mark (str "supervisor:" rid)]
    (dotimes [_ 5] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
    (is (nil? (session/since mark)) "no mark before the first pass")
    (let [{:keys [prob]} (reasoning-over conn rid)]
      (is (str/includes? (str prob) "This session so far")
          "the session block is in the brief")
      (dotimes [_ 2] (session/observe-turn! {:tool "eval" :category :success :signals {}}))
      (is (= 2 (:turns (session/since mark)))
          "and the pass stamped its mark, so the next look measures from here"))
    (session/reset!)))

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
  (is (ov/due? {:last-at 0 :passes 0 :looks 99} {:now 999 :every-ms 1 :budget 3 :boundary? true})
      "ninety-nine free looks do not exhaust a budget of three")
  (is (not (ov/due? {:last-at 0 :passes 3 :looks 3} {:now 999 :every-ms 1 :budget 3 :boundary? true}))
      "three spent passes do"))
