;; samizdat - a self-hosting agentic harness
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

(ns samizdat.control-test
  "Steering a running agent from the REPL. A human submits a directive against
  the run's db; the loop drains it at the next boundary and the arbiter injects
  it into the branch's next turn at priority zero, above every machine gate.
  The specification test drives a real run and asserts a REPL steer lands in
  the model's context."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.roles :as roles]
            [samizdat.lexicon :as lexicon]
            [clojure.set]
            [samizdat.agent.tools.base :as base]
            [samizdat.control :as control]
            [samizdat.system :as system]
            [samizdat.agent.loop :as aloop]
            [samizdat.workflow :as wf]
            [samizdat.agent.state :as state]
            [samizdat.agent.resume :as resume]
            [samizdat.api.control :as api-control]
            [samizdat.api.openai :as openai]
            [samizdat.api.runs :as api-runs]
            [samizdat.cells :as cells]
            [samizdat.store.tasks :as tasks]
            [mycelium.cell :as cell]
            [samizdat.llm.client :as llm]
            [samizdat.security.policy :as policy]
            [samizdat.store.db :as db]
            [samizdat.store.grants :as grants]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defmacro with-db [[binding] & body]
  `(let [~binding (db/open! ":memory:")]
     (try ~@body (finally (db/close ~binding)))))

(deftest steer-queues-a-message-directive
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (control/steer! c rid "wire truncate-middle into the shell tool")
      (let [[d] (interventions/pending c rid)]
        (is (= "message" (:kind d)))
        (is (str/includes? (str (:payload d)) "truncate-middle"))
        (is (= "pending" (:status d)))))))

(deftest list-and-run-scoped-viewers
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (control/steer! c rid "do the thing")
      (control/steer! c rid "then the other thing" {:branch-id "B1"})
      (is (= 2 (count (control/pending c rid))))
      (is (= ["do the thing" "then the other thing"]
             (mapv :payload (control/pending c rid)))))))

(deftest a-grant-intervention-is-applied-immediately
  ;; a#2 (docs/provenance.md): grants/grant! had no production caller, so
  ;; every deliberate :ask blocked a run forever — no endpoint, no tool, no
  ;; intervention kind wrote a grant. The human intervention surface is the
  ;; write path, and it applies on arrival rather than queueing for a
  ;; boundary, because the policy consults the grants table per command.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (testing "before the grant, the interpreter asks"
        (is (= :ask (:effect (policy/decide (grants/for-run c rid) "python3 x.py")))))
      (testing "a grant intervention writes the grant now"
        (let [r (api-control/intervene! c rid {:kind "grant"
                                               :payload {:pattern "python3 *"}})]
          (is (= "granted" (:status (:body r))))
          (is (= :allow (:effect (policy/decide (grants/for-run c rid) "python3 x.py"))))))
      (testing "a grant without a pattern is refused, not queued"
        (let [r (api-control/intervene! c rid {:kind "grant"})]
          (is (= 400 (:status r)))
          (is (str/includes? (str (get-in r [:body :error :message])) "pattern"))))
      (testing "the queued kinds still queue"
        (let [r (api-control/intervene! c rid {:kind "message" :payload "hi"})]
          (is (= "pending" (:status (:body r))))
          (is (= 1 (count (interventions/pending c rid)))))))))

;; --- the drain at the boundary ----------------------------------------------

(deftest a-pending-directive-is-drained-and-injected
  ;; A directive submitted before a turn boundary is drained by the loop, the
  ;; arbiter fires the human-directive gate at priority zero, and the payload
  ;; lands in the branch's next-turn message. Then it is resolved as applied.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          _ (runs/open-branch! c rid {:branch-id "B1"})
          ctx {:conn c :run-id rid :max-turns 10
               :llm-adapter :a :llm-config {:max-tokens 16384}}
          b (state/new-branch {:id "B1" :problem "p"})]
      (control/steer! c rid "STEER: add a docstring to truncate-middle")
      (with-redefs [llm/chat (fn [& _]
                               {:content "```tool-call\n{\"name\": \"task\", \"args\": {\"action\": \"list\"}}\n```"
                                :finish-reason "stop"})]
        (let [after (wf/run-turn ctx b 1)
              last-msg (last (:messages after))]
          (testing "the directive text is injected into the next-turn message"
            (is (str/includes? (:content last-msg) "human has intervened"))
            (is (str/includes? (:content last-msg) "add a docstring to truncate-middle")))
          (testing "the directive is resolved as applied, not left pending"
            (is (empty? (interventions/pending c rid)))
            (is (= "applied" (:status (first (interventions/history c rid)))))))))))

(deftest a-run-that-finishes-in-the-start-window-leaves-no-active-entry
  ;; provenance CR1-3: the run future's completion dissoc'd `active`
  ;; before the request thread had assoc'd it, stranding an entry that let
  ;; abort! rewrite a finished run's status to :aborted. Registration must
  ;; happen inside the run's own thread (on-start), so it can never land
  ;; after the completion dissoc.
  (with-db [c]
    (with-redefs [beam/run! (fn [{:keys [on-start]}]
                              (let [rid (str (random-uuid))]
                                (on-start rid)
                                {:run-id rid :status :completed}))]
      (let [r (api-control/start-run! {:conn c :config {:llm {:provider :local}}}
                                      {:problem "p"})
            rid (:run_id (:body r))]
        (is (= "running" (:status (:body r))))
        (let [gone? (loop [n 0]
                      (cond (nil? (get @api-control/active rid)) true
                            (< n 100) (do (Thread/sleep 10) (recur (inc n)))
                            :else false))]
          (is gone? "no stranded active entry after an instant run"))
        (is (= 409 (:status (api-control/abort! c rid)))
            "abort on a finished run refuses rather than rewriting status")))))

(deftest abort-refuses-when-the-run-won-the-finish-race
  ;; provenance R2-4: the transient window provenance A-3 could not close — the run's own
  ;; :completed lands between abort!'s registry read and its finish-run!.
  ;; The store guard refuses the rewrite; abort! must answer 409 rather
  ;; than claim an abort that did not land.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/finish-run! c rid :completed "done")
      ;; simulate the in-window registry entry the run's thread has not
      ;; dissoc'd yet
      (swap! api-control/active assoc rid {:abort (atom false)})
      (try
        (is (= 409 (:status (api-control/abort! c rid))))
        (is (= "completed" (:status (runs/get-run c rid))))
        (finally (swap! api-control/active dissoc rid))))))

(deftest a-directive-against-an-ENDED-run-is-refused-whatever-ended-it
  ;; The rule this enforces is already written at the call site: a directive
  ;; against a run that has ended would sit `pending` forever, showing in the
  ;; UI as an intervention that will never resolve (blt.38). The set that
  ;; decided "has ended" listed completed/aborted/failed and MISSED the other
  ;; two terminal statuses.
  ;;
  ;; `interrupted` is written by startup reconciliation — every row still
  ;; saying `running` when the process comes up is a leftover, since the beam
  ;; only ever runs in this process. Those runs are as over as an aborted one.
  ;;
  ;; `exhausted` is new here: the beam's exhaust path used to record :failed,
  ;; which is also what a THROWN error records, so "the harness broke" and
  ;; "the work honestly ran out of budget" were the same row (karamazov-emw).
  (with-db [c]
    (doseq [st ["completed" "aborted" "failed" "interrupted" "exhausted"]]
      (let [rid (runs/start-run! c {:problem "p"})]
        (runs/finish-run! c rid (keyword st) nil)
        (let [r (api-control/intervene! c rid {:kind "message"
                                           :payload {:text "do a thing"}})]
          (is (= 409 (:status r))
              (str "a directive against a " st " run is refused, not queued"))
          (is (empty? (interventions/pending c rid))
              (str "and nothing is left pending on a " st " run")))))))

(deftest an-unknown-intervention-kind-is-a-400-not-a-500
  ;; provenance R3-12: submit! throws on an unknown kind, and intervene! let it
  ;; fly through to the server's catch-all 500. A bad request is the
  ;; client's to fix; the API should say 400 and name the known kinds.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (let [r (api-control/intervene! c rid {:kind "explode" :payload "x"})]
        (is (= 400 (:status r)) "refused with a client error")
        (is (str/includes? (str (get-in r [:body :error :message])) "kind")
            "the message says what was wrong")
        (is (empty? (interventions/pending c rid))
            "nothing was queued")))))

(deftest a-negative-limit-is-not-a-disguised-no-limit
  ;; provenance R3-12: a negative limit went straight into SQL LIMIT, and SQLite
  ;; reads LIMIT -1 as no limit at all — so ?limit=-1 answered with the whole
  ;; table while looking like a tighter ask. The API edge clamps it to zero.
  (with-db [c]
    (dotimes [_ 3] (runs/start-run! c {:problem "p"}))
    (testing "a negative limit returns nothing, not everything"
      (is (= 0 (count (:runs (api-runs/list-runs c -1))))))
    (testing "a positive limit still bounds"
      (is (= 2 (count (:runs (api-runs/list-runs c 2))))))))

(deftest watch-follows-the-run-not-a-hardcoded-branch
  ;; provenance R3-13: watch read branch-turns for "B1" and only "B1", so on a
  ;; beam run — 5 branches by default — the supervisor's window showed one
  ;; arm of the run and the other four were invisible, including whatever
  ;; branch was actually doing the work. Default to the run's last active
  ;; branch; an explicit id still narrows.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (runs/open-branch! c rid {:branch-id "B2"})
      (journal/record-turn! c rid {:branch-id "B1" :turn 1
                                   :tool-name "shell" :result "b1-work"
                                   :category :success})
      (journal/record-turn! c rid {:branch-id "B2" :turn 1
                                   :tool-name "shell" :result "b2-work"
                                   :category :success})
      (testing "default follows the last active branch"
        (is (= ["b2-work"] (mapv :result (control/watch c rid)))))
      (testing "an explicit branch still narrows"
        (is (= ["b1-work"] (mapv :result (control/watch c rid 8 "B1"))))))))

(deftest start-reloads-gate-thresholds
  ;; review4: gates.edn is cached in an atom that survives restart!, so an
  ;; edited threshold never took effect without a process restart. start!
  ;; reloads it now — this test only pins the call.
  (let [called (atom 0)]
    (with-redefs [gates/reload-config! (fn [] (swap! called inc))]
      (system/start! (fn [_] {:status 200 :headers {} :body "ok"})
                     {:db {:path ":memory:"} :http {:port 0}})
      (system/stop!))
    (is (= 1 @called) "start! refreshed the cached gate thresholds")))

;; --- the four directives that used to be advertised and rejected ------------
;;
;; RFC-006 recorded that `drain-directives!` recognised pause, resume, extend
;; and fork and rejected them explicitly as unwired, and that rejecting beat
;; accepting silently — but that they were advertised by the control API. Half
;; of `interventions/kinds` was a promise the scheduler would not keep.

(defn- drain [c rid branches]
  (beam/drain-directives! {:conn c :run-id rid} branches
                          (interventions/pending c rid) 1))

(defn- branch [id]
  {:id id :status :active :messages [] :turn 1})

(deftest extend-raises-the-turn-cap
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (interventions/submit! c rid {:kind "extend" :payload {:turns 20}})
      (let [r (drain c rid [(branch "B1")])]
        (is (= 20 (:max-turns r)))
        (is (= "applied" (:status (first (interventions/history c rid))))))
      (testing "the round's cap is the run's plus the extension"
        (is (= 60 (beam/round-max-turns {:max-turns 40} {:max-turns 60})))
        (is (= 40 (beam/round-max-turns {:max-turns 40} {}))
            "no extension leaves the run's own cap alone")))))

(deftest extend-without-a-turn-count-is-refused-and-says-so
  ;; A directive is never silently dropped — the same discipline `cull` on the
  ;; last branch already followed.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (interventions/submit! c rid {:kind "extend" :payload {}})
      (let [r (drain c rid [(branch "B1")])
            [d] (interventions/history c rid)]
        (is (nil? (:max-turns r)))
        (is (= "rejected" (:status d)))
        (is (str/includes? (str (:disposition d)) "turns"))))))

(deftest fork-becomes-a-pending-thesis-the-spawn-cell-already-honours
  ;; No scheduler machinery of its own: :beam/spawn turns a branch's
  ;; :pending-branch-theses into siblings under the total cap, and runs after
  ;; the cull in the same round. A human's fork is the same object a branch's
  ;; own branch_theses call produces.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (interventions/submit! c rid {:kind "fork"
                                    :payload {:thesis "try the greedy bound"}})
      (let [{:keys [branches]} (drain c rid [(branch "B1") (branch "B2")])
            theses (mapcat :pending-branch-theses branches)]
        (is (= 1 (count theses)) "exactly one parent takes the fork")
        (is (= "try the greedy bound" (:goal (first theses))))
        (is (true? (:from-human? (first theses))))))))

(deftest fork-without-a-thesis-is-refused
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (interventions/submit! c rid {:kind "fork" :payload {}})
      (let [{:keys [branches]} (drain c rid [(branch "B1")])
            [d] (interventions/history c rid)]
        (is (empty? (mapcat :pending-branch-theses branches)))
        (is (= "rejected" (:status d)))
        (is (str/includes? (str (:disposition d)) "thesis"))))))

(deftest pause-and-resume-are-derived-from-the-record
  ;; Derived rather than stored, so there is one answer and it survives a
  ;; process restart: a runs column would be a second copy that a crash
  ;; between the directive and the column write could leave disagreeing with
  ;; the record the run is judged by.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (is (false? (interventions/paused? c rid)) "a fresh run is not paused")

      (interventions/submit! c rid {:kind "pause"})
      (is (false? (interventions/paused? c rid))
          "a PENDING pause has not reached a boundary yet")

      (is (true? (:paused? (drain c rid [(branch "B1")]))))
      (is (true? (interventions/paused? c rid)))

      (interventions/submit! c rid {:kind "resume"})
      (drain c rid [(branch "B1")])
      (is (false? (interventions/paused? c rid))
          "the most recently applied of the two wins"))))

(deftest a-paused-run-stays-stoppable
  ;; A pause that could not be aborted out of would be a wedge with a friendly
  ;; name. await-resume! reads the abort flag on the same pass as the pause.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (interventions/submit! c rid {:kind "pause"})
      (drain c rid [(branch "B1")])
      (is (true? (interventions/paused? c rid)))
      (let [aborted (atom true)
            waited (beam/await-resume! {:conn c :run-id rid :abort aborted})]
        (is (= 0 waited) "an aborted run does not wait for a resume")))))

(deftest await-resume-returns-immediately-when-nothing-is-paused
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (is (= 0 (beam/await-resume! {:conn c :run-id rid :abort (atom false)}))))))

(deftest every-advertised-directive-kind-does-something
  ;; The gap, asserted directly: interventions/kinds is what a human is shown,
  ;; and half of it was a promise the scheduler would not keep. A kind that is
  ;; advertised and rejected as unwired must not exist.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          payloads {"message" {:text "t"}
                    "review" {}
                    "cull" {}
                    "fork" {:thesis "t"}
                    "retract" {:artifact_id 1}
                    "extend" {:turns 5}
                    "pause" {}
                    "resume" {}
                    "switch" {:strategy "decompose"}
                    "budget" {:turns 30}
                    "stop" {:text "a dead end"}}]
      (cells/load-cells!)
      (doseq [k interventions/kinds]
        (interventions/submit! c rid {:kind k :payload (payloads k)})
        (if (contains? interventions/workflow-kinds k)
          ;; The workflow's own boundary — the feature loop's directives
          ;; stage — is where these land, not the scheduler's.
          ((:handler (cell/get-cell! :feature/supervise)) {:conn c :run-id rid} {})
          (drain c rid [(branch "B1") (branch "B2")]))
        (let [d (first (filter #(= k (:kind %)) (interventions/history c rid)))]
          (is (not= "pending" (:status d))
              (str k " reached a boundary and was left pending"))
          (is (not (str/includes? (str (:disposition d)) "not wired"))
              (str k " is advertised to a human and rejected as unwired")))))))

(deftest every-kind-has-words-and-every-word-a-kind
  ;; The names are mechanism and live in the store; what each does is prose
  ;; the model reads and lives in wordlists.edn. Two lists drift unless
  ;; something holds them together.
  (is (= interventions/kinds (set (keys (lexicon/wordlist :directive-kinds))))
      "a kind with no description, or a description of no kind")
  (is (= interventions/kinds
         (clojure.set/union interventions/branch-kinds interventions/scheduler-kinds
                            interventions/workflow-kinds))
      "every kind has exactly one boundary that owns it")
  (is (empty? (clojure.set/intersection interventions/scheduler-kinds interventions/workflow-kinds))))

(deftest a-pending-resume-ends-the-pause-wait
  ;; blt.9: paused? counts APPLIED rows, and the only code applying a resume
  ;; was the :beam/directives cell — downstream of the round-open node that
  ;; blocks while paused. The resume stayed pending forever; pause was a
  ;; one-way door to abort. await-resume! now applies the two run-level kinds
  ;; itself, every pass, at the same boundary the cell applies them.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (interventions/submit! c rid {:kind "pause"})
      (drain c rid [(branch "B1")])
      (is (true? (interventions/paused? c rid)))
      ;; the resume a human submits from ANOTHER process while the beam parks
      (interventions/submit! c rid {:kind "resume"})
      (is (number? (beam/await-resume! {:conn c :run-id rid}))
          "the wait returns rather than polling forever")
      (is (false? (interventions/paused? c rid))
          "the pending resume was applied by the wait loop itself"))))

(deftest the-per-turn-drain-leaves-the-beams-directives-alone
  ;; blt.10: the per-branch drain runs at every steer boundary and used to
  ;; resolve cull/fork/pause/resume/retract as :rejected — on a beam run,
  ;; where a round's wall-clock lives inside :beam/advance, so a human's
  ;; pause almost always landed mid-round and was eaten by the first branch
  ;; to finish its turn, before :beam/directives ever saw it.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (doseq [[k p] [["pause" {}] ["cull" {}] ["fork" {:thesis "t"}]
                     ["retract" {:artifact_id 1}] ["extend" {:turns 3}]]]
        (interventions/submit! c rid {:kind k :payload p}))
      (interventions/submit! c rid {:kind "message" :payload {:text "all hands"}})
      (interventions/submit! c rid {:kind "message" :branch-id "B1"
                                    :payload {:text "just you"}})
      (let [b (#'aloop/drain-directives! {:conn c :run-id rid :beam? true
                                          :max-turns 40}
                                         (branch "B1") 2)
            by-kind (group-by :kind (interventions/history c rid))]
        (is (some? (:pending-directive b))
            "the branch-scoped message lands here, sooner than the round top")
        (doseq [k ["pause" "cull" "fork" "retract" "extend"]]
          (is (= "pending" (:status (first (by-kind k))))
              (str k " is left for the beam drain, not eaten")))
        (let [msgs (by-kind "message")
              run-wide (first (filter #(nil? (:branch_id %)) msgs))
              scoped (first (filter #(= "B1" (:branch_id %)) msgs))]
          (is (= "pending" (:status run-wide))
              "the run-wide message is the beam's to broadcast to every branch")
          (is (= "applied" (:status scoped))))))))

(deftest both-drains-leave-the-workflows-directives-for-the-workflows-boundary
  ;; RFC-012 F5. `switch`, `budget` and `stop` are about the OUTER loop — the
  ;; feature loop's next round — and neither the per-turn drain nor the
  ;; scheduler's has anywhere to put them. They used to be rejected as
  ;; unknown by whichever boundary came first, which on a feature run was a
  ;; worker's, long before the round reached the stage that applies them.
  ;; Symmetric with blt.10: a drain leaves alone what it does not own.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (doseq [[k p] [["switch" {:strategy "decompose"}] ["budget" {:turns 30}]
                     ["stop" {:text "dead end"}]]]
        (interventions/submit! c rid {:kind k :payload p :issued-by "supervisor"}))
      (testing "the per-turn drain, on a single-branch run — where it eats
                everything else it does not hand to the beam"
        (#'aloop/drain-directives! {:conn c :run-id rid :beam? false :max-turns 40}
                                   (branch "B1") 2)
        (doseq [[_ [d]] (group-by :kind (interventions/history c rid))]
          (is (= "pending" (:status d)) (str (:kind d) " was eaten by a worker's boundary"))))
      (testing "and the scheduler's drain at the round top"
        (drain c rid [(branch "B1") (branch "B2")])
        (doseq [[_ [d]] (group-by :kind (interventions/history c rid))]
          (is (= "pending" (:status d)) (str (:kind d) " was eaten by the round top")))))))

(deftest the-supervisors-extend-reaches-the-scheduler
  ;; The intervene tool sends every structural kind as {"text": ...}, and the
  ;; drains read the turn count from :turns — so `intervene {kind: "extend",
  ;; text: "40"}` was refused every time it was tried, with a reason that
  ;; named a payload shape the tool cannot produce. One parser, both shapes.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (base/run-tool {:conn c :run-id rid :tool-name "intervene"
                      :branch {:id "SUP"} :args {:kind "extend" :text "40"}})
      (let [{:keys [max-turns]} (drain c rid [(branch "B1")])
            [d] (interventions/history c rid)]
        (is (= 40 max-turns) "the cap moved by what the supervisor asked")
        (is (= "applied" (:status d)))))
    (testing "the parser itself, over every shape a directive arrives in"
      (is (= 5 (interventions/turns-asked {:payload "{\"turns\": 5}"})))
      (is (= 7 (interventions/turns-asked {:payload "{\"text\": \"7\"}"})))
      (is (= 9 (interventions/turns-asked {:payload "{\"by\": 9}"})))
      (is (nil? (interventions/turns-asked {:payload "{\"text\": \"soon\"}"}))
          "words are not a turn count")
      (is (nil? (interventions/turns-asked {:payload "{\"turns\": -3}"}))
          "nor is a negative one"))))

(deftest extend-lands-on-a-single-branch-run
  ;; blt.12: the old arm returned the branch untouched, assuming
  ;; control/extend! (REPL-only) had raised the runs row — the HTTP path only
  ;; enqueues, so the directive sat pending forever and the cap never moved.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :max-turns 10})]
      (interventions/submit! c rid {:kind "extend" :payload {:turns 5}})
      (let [b (#'aloop/drain-directives! {:conn c :run-id rid :beam? false
                                          :max-turns 10}
                                         (branch "B1") 2)
            [d] (interventions/history c rid)]
        (is (= 5 (:extended-turns b)) "the cap travels on the branch")
        (is (= "applied" (:status d)))
        (is (= 15 (:max_turns (runs/get-run c rid)))
            "and persists on the row a crash-resume reads its budget from")))))

(deftest an-extended-branch-routes-past-the-original-cap
  ;; blt.12: ctx is fixed for the life of the run, so past the original cap
  ;; every turn routed :exhausted -> :memory/distil — one LLM reflection per
  ;; branch per extended round. The route reads the branch's extension now.
  (cells/load-cells!)
  (let [route (:handler (cell/get-cell :loop/route))
        b (branch "B1")]
    (is (= :exhausted (:verdict (route {:max-turns 5} {:branch b :turn 5}))))
    (is (= :continue (:verdict (route {:max-turns 5}
                                      {:branch (assoc b :extended-turns 3)
                                       :turn 5})))
        "an extension grants real turns, not a reflection loop")))

(deftest a-human-cull-closes-the-row-and-keeps-the-branch-in-the-record
  ;; blt.11: the drain marked :status :culled in the data map but nothing
  ;; closed the row or carried the branch through settle/tick — it vanished
  ;; from the run and its branches row stayed open forever, the exact zombie
  ;; record-inactive!'s docstring warns about.
  (cells/load-cells!)
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (runs/open-branch! c rid {:branch-id "B2"})
      (interventions/submit! c rid {:kind "cull" :branch-id "B2"})
      (let [cellfn (:handler (cell/get-cell :beam/directives))
            data (cellfn {:conn c :run-id rid}
                         {:active [(branch "B1") (branch "B2")]
                          :branches [(branch "B1") (branch "B2")]
                          :turn 3})]
        (is (= ["B1"] (mapv :id (:active data))))
        (is (some #(and (= "B2" (:id %)) (= :culled (:status %)))
                  (:branches data))
            "the verdict is visible to settle's bookkeeping, so tick keeps it")
        (let [row (first (filter #(= "B2" (:id %)) (runs/branches c rid)))]
          (is (= "culled" (:status row)) "the row is closed, not a zombie"))))))

(deftest a-resumed-branch-still-holds-its-task
  ;; karamazov-blt.21: the claim survives the crash on its ROW, but the
  ;; rebuilt branch came back with no :task — told "No task claimed", free to
  ;; claim a SECOND task, with the old row in_progress and attributed to it
  ;; forever: RFC-008's named worst state for a shared board.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :max-turns 10 :beam-width 1})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (let [t-id (tasks/create! c {:title "the part" :body "do it" :run-id rid})]
        (is (some? (tasks/claim! c t-id rid "B1")))
        (with-redefs [beam/run-rounds (fn [_ branches _] {:branches branches})]
          (let [r (resume/resume! {:conn c :config {} :llm-adapter :a
                                   :llm-config {} :run-id rid})
                b (first (:branches r))]
            (is (= t-id (get-in b [:task :id]))
                "the branch knows what it holds again")
            (is (some :pinned? (:messages b))
                "and the pinned task statement is back in its context")))))))

(deftest a-resumed-worker-keeps-its-own-problem
  ;; karamazov-blt.23: sub-workflow branches (a decompose unit, a team worker)
  ;; open on their own contract, stored on the branches row since v18 — and
  ;; the rebuild must actually READ it. The first cut bound it and then kept
  ;; using the run's problem anyway; this is the test that catches the
  ;; half-wire.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "the whole feature"
                                  :max-turns 10 :beam-width 1})]
      (runs/open-branch! c rid {:branch-id "W0" :problem "part A only"})
      (with-redefs [beam/run-rounds (fn [_ branches _] {:branches branches})]
        (let [b (first (:branches (resume/resume! {:conn c :config {}
                                                   :llm-adapter :a :llm-config {}
                                                   :run-id rid})))]
          (is (= "part A only" (:problem b))
              "the branch's own contract, not the run-level feature text")
          (is (some #(str/includes? (str (:content %)) "part A only")
                    (:messages b))
              "and its opening messages are rebuilt from it"))))))

(deftest replay-applies-the-live-loops-call-discipline
  ;; karamazov-blt.22: replay pushed EVERY journalled row through add-turn +
  ;; record-outcome, but the live loop applies neither to a provider-error row
  ;; and only record-outcome to a no-call row — so each provider error
  ;; DECREMENTED consecutive-failures on replay and the resumed branch's
  ;; counters diverged from the ones the run actually had.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :max-turns 10 :beam-width 1})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (journal/record-turn! c rid {:branch-id "B1" :turn 1 :tool-name "eval"
                                   :args {} :result "boom" :category "failure"})
      (journal/record-turn! c rid {:branch-id "B1" :turn 2
                                   :tool-name "__provider_error__"
                                   :result "timeout" :category "neutral"})
      (journal/record-turn! c rid {:branch-id "B1" :turn 3
                                   :tool-name "__no_call__"
                                   :result "no fence" :category "mechanics"})
      (with-redefs [beam/run-rounds (fn [_ branches _] {:branches branches})]
        (let [r (resume/resume! {:conn c :config {} :llm-adapter :a
                                 :llm-config {} :run-id rid})
              b (first (:branches r))]
          (is (= 1 (:consecutive-failures b))
              "the provider error neither decrements nor resets the counter")
          (is (= 1 (count (:turns b)))
              "and only the real tool turn entered the branch's own log"))))))

(deftest the-last-active-branch-survives-beside-inactive-siblings
  ;; karamazov-blt.17: the cull cell seeded its survivor count with (count
  ;; advanced), which includes branches that went done/abandoned during the
  ;; round's advance — so the LAST active branch saw phantom survivors and was
  ;; cullable, emptying the beam. It also ran the cascade over already-inactive
  ;; branches, rewriting their endings.
  (cells/load-cells!)
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          cullfn (:handler (cell/get-cell :beam/cull))
          done-b {:id "B1" :status :done :final-answer "a" :messages [] :turns []}
          failing (assoc (state/new-branch {:id "B2" :problem "p"})
                         :consecutive-failures 99
                         :turns (vec (repeat 9 {})))
          data (cullfn {:conn c :run-id rid :turn 9}
                       {:advanced [done-b failing] :turn 9})
          [b1' b2'] (:culled data)]
      (is (= :done (:status b1'))
          "an already-inactive sibling's ending is not re-judged or rewritten")
      (is (state/active? b2')
          "the last ACTIVE branch is never culled — a done sibling is not a survivor"))))

(deftest exhaust-ships-a-banked-answer-instead-of-discarding-it
  ;; karamazov-blt.20: with :stop-on-first-done? false, a branch that shipped
  ;; at round 10 while a sibling explored to the cap ended in finish-run!
  ;; :failed nil — the banked answer never reached the run row.
  (cells/load-cells!)
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (runs/open-branch! c rid {:branch-id "B2"})
      (let [done-b {:id "B1" :status :done :final-answer "the answer"
                    :messages [] :turns []}
            active-b (state/new-branch {:id "B2" :problem "p"})
            exhaust (:handler (cell/get-cell :beam/exhaust))
            data (exhaust {:conn c :run-id rid :max-turns 5}
                          {:branches [done-b active-b] :active [active-b]})]
        (is (= :completed (:status data)))
        (is (= "the answer" (get-in data [:result :answer])))
        (is (= "completed" (:status (runs/get-run c rid)))
            "the run row records the completion, not a failure")))))

(deftest a-forfeited-turns-thread-is-never-run-beside
  ;; karamazov-blt.18: a turn that blew its deadline kept executing — it
  ;; journals under the branch's id and shares its eval session — while the
  ;; beam advanced the SAME branch again next round, interleaving two turns of
  ;; one branch and making the journal diverge from the live state. The
  ;; dangling future is remembered now, and the branch forfeits until it
  ;; completes.
  (let [calls (atom 0)]
    (with-redefs [beam/advance-branch (fn [_ b _]
                                        (if (= 1 (swap! calls inc))
                                          (do (Thread/sleep 400) (assoc b :slow true))
                                          (assoc b :fast true)))]
      (let [in-flight (atom {})
            ctx {:iterating-loop? true :turn-deadline-ms 50 :in-flight in-flight}
            b (state/new-branch {:id "B1" :problem "p"})
            [r1] (beam/advance-all ctx [b] 1)]
        (is (= 1 (:timeouts r1)) "the slow turn forfeits")
        (is (contains? @in-flight "B1") "and its dangling future is remembered")
        (let [[r2] (beam/advance-all ctx [b] 2)]
          (is (= 1 (:timeouts r2))
              "the next round forfeits again rather than running beside it")
          (is (= 1 @calls) "crucially, NO second turn ran while one dangled"))
        (Thread/sleep 500)
        (let [[r3] (beam/advance-all ctx [b] 3)]
          (is (true? (:fast r3)) "once the dangling turn completes, the branch advances")
          (is (not (contains? @in-flight "B1")) "and the memory is released"))))))

(deftest a-chat-completion-run-is-registered-and-abortable
  ;; blt.13: beam/run! was called with no :abort atom and no control/active
  ;; registration, so POST /v1/runs/:id/abort answered 409 "no active run"
  ;; for a genuinely running run, for its whole (potentially hours-long) life.
  (with-db [c]
    (let [seen (atom nil)]
      (with-redefs [beam/run!
                    (fn [{:keys [abort on-start]}]
                      (is (some? abort) "an abort atom reaches the run")
                      (on-start "r-oai")
                      (reset! seen (contains? @api-control/active "r-oai"))
                      {:status :completed :run-id "r-oai" :answer "a"})]
        (openai/chat-completion {:conn c :config {:llm {:provider :local :model "m"}}}
                                {:messages [{:role "user" :content "q"}]})
        (is (true? @seen) "the run was visible to the abort endpoint while live")
        (is (not (contains? @api-control/active "r-oai"))
            "and deregistered when it finished")))))

;; --- the supervisor's hands -------------------------------------------------
;; store/interventions/submit! has always taken :issued-by, and until now no
;; tool could call it. The supervisor had the full tool surface and could
;; rewrite prompts for the NEXT run while being unable to touch the one in
;; front of it — eyes and no hands. Runs fps5 and fps6 both stalled with a
;; supervisor that could see the stall and say nothing.

(deftest intervene-puts-a-directive-on-the-live-run
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          call (fn [args]
                 (base/run-tool {:conn c :run-id rid :tool-name "intervene"
                                 :branch {:id "S0"} :args args}))]
      (testing "a well-formed steer lands as a pending directive"
        (call {:kind "message" :branch "T0" :text "write the file you named"})
        (let [[d] (interventions/pending c rid)]
          (is (= "message" (:kind d)))
          (is (= "T0" (:branch_id d)))
          (is (str/includes? (str (:payload d)) "write the file"))))
      (testing "issued_by distinguishes it from a human's directive — the
                journal must not attribute the harness's own steering to the
                operator"
        (is (= "supervisor" (:issued_by (first (interventions/history c rid))))))
      (testing "an unknown kind is a malformed call, not a crash"
        (let [r (call {:kind "obliterate" :branch "T0"})]
          (is (not (:progress? r)))
          (is (str/includes? (str (:result r)) "obliterate"))))
      (testing "the refusal lists what IS available, so the next call can be right"
        (let [r (call {:kind "obliterate" :branch "T0"})]
          (is (every? #(str/includes? (str (:result r)) %)
                      ["message" "review" "cull" "extend"])))))))

(deftest intervene-is-not-on-the-implementor-surface
  ;; A branch that could cull its siblings or raise its own turn cap is not an
  ;; implementor any more. Steering is the supervisor's job precisely because
  ;; it is the one role whose context is ABOUT the run rather than in it.
  (is (not (roles/may-use? :implementor "intervene")))
  (is (roles/may-use? :supervisor "intervene")))
