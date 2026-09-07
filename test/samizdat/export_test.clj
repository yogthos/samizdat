;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.export-test
  "Runs as training data (karamazov-5ge3).

  What is under test is fidelity and selection: a trajectory is the tape the
  model saw, role-tagged, with the steer composed into the user message the
  way the loop composes it; only runs that shipped and verified qualify; and
  no secret crosses into the file."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [samizdat.export :as export]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(def conn (atom nil))

(use-fixtures :each (fn [f]
                      (reset! conn (db/open! ":memory:"))
                      (try (f) (finally (db/close @conn)))))

(defn- turn! [rid bid n m]
  (journal/record-turn! @conn rid (merge {:branch-id bid :turn n :category :success} m)))

(defn- run!
  "A four-turn run on one branch: a shell call, a no-call turn the harness
  complained about, a write with a steer appended to its result, and done."
  [{:keys [problem verified? status] :or {verified? true status :completed}}]
  (let [rid (runs/start-run! @conn {:problem problem :provider "glm" :model "glm-5.3"})]
    (runs/open-branch! @conn rid {:branch-id "B1"})
    (turn! rid "B1" 1 {:tool-name "shell" :args {:command "ls"}
                       :assistant-text "```tool\n{\"name\":\"shell\",\"args\":{\"command\":\"ls\"}}\n```"
                       :result "src test"})
    (turn! rid "B1" 2 {:tool-name "__no_call__" :category :mechanics :parse-error "no fence"
                       :assistant-text "I will now look at the files."
                       :result "No tool call in that reply."})
    (journal/record-gate! @conn rid {:branch-id "B1" :turn 3 :gate :progress-stalled
                                     :message "You have not written a file in 3 turns."
                                     :prediction "write" :window 3})
    (turn! rid "B1" 3 {:tool-name "write_file" :args {:path "a.clj"}
                       :assistant-text "```tool\n{\"name\":\"write_file\",\"args\":{\"path\":\"a.clj\"}}\n```"
                       :result "wrote a.clj (TOKEN=hunter2)"})
    (turn! rid "B1" 4 {:tool-name "done" :args {:answer "did it"}
                       :assistant-text "```tool\n{\"name\":\"done\",\"args\":{\"answer\":\"did it\"}}\n```"
                       :result "Shipped."})
    (when verified?
      (journal/note! @conn rid :ship-verify {:branch-id "B1" :turn 4
                                             :data {:ran true :green true :timeout false}}))
    (runs/finish-run! @conn rid status "did it")
    rid))

(deftest only-shipped-and-verified-runs-qualify
  ;; The selection is the half that makes this training data rather than a
  ;; dump: a run that exhausted its turns is what NOT to imitate, and a
  ;; completed run whose ship gate never ran has no evidence it was right.
  (let [good (run! {:problem "build it"})
        _ (run! {:problem "build it too" :verified? false})
        _ (run! {:problem "never finished" :status :exhausted})
        rows (export/trajectories @conn {:known-values #{"hunter2"}})]
    (is (= [good] (mapv :run-id rows)))
    (is (= ["B1"] (mapv :branch-id rows)))))

(deftest a-trajectory-is-the-tape-the-model-saw
  (run! {:problem "build it"})
  (let [[{:keys [messages model provider]}] (export/trajectories @conn {:known-values #{"hunter2"}})
        roles (mapv :role messages)]
    (is (= ["glm" "glm-5.3"] [provider model]))
    (is (= ["system" "user"] (subvec roles 0 2)) "over the system prompt and the problem")
    (is (str/includes? (:content (second messages)) "build it"))
    (is (= ["assistant" "user" "assistant" "user" "assistant" "user"] (subvec roles 2))
        "three usable turns: the no-call turn and the complaint it drew are
         mechanics, not something to imitate")
    (is (not-any? #(str/includes? (:content %) "I will now look") messages))
    (testing "a steer rides in the user message of the turn it was appended to,
              after the rule the loop composes it behind"
      (let [after-write (nth messages 5)]
        (is (= "user" (:role after-write)))
        (is (str/includes? (:content after-write) "wrote a.clj"))
        (is (str/includes? (:content after-write)
                           "\n\n---\n\nYou have not written a file in 3 turns."))))
    (testing "the model's own fenced calls are the assistant turns, verbatim"
      (is (str/includes? (:content (nth messages 2)) "\"name\":\"shell\"")))))

(deftest a-secret-never-reaches-the-export
  (run! {:problem "build it"})
  (let [[t] (export/trajectories @conn {:known-values #{"hunter2"}})]
    (is (not-any? #(str/includes? (:content %) "hunter2") (:messages t)))
    (is (some #(str/includes? (:content %) "wrote a.clj") (:messages t))
        "redacted, not dropped")))

(deftest every-branch-when-asked-for-all
  (let [rid (run! {:problem "build it"})]
    (runs/open-branch! @conn rid {:branch-id "B2"})
    (turn! rid "B2" 1 {:tool-name "shell" :args {:command "pwd"}
                       :assistant-text "```tool\n{}\n```" :result "/x"})
    (is (= ["B1"] (mapv :branch-id (export/trajectories @conn {:known-values #{}})))
        "by default only the branch that shipped")
    (is (= ["B1" "B2"] (mapv :branch-id (export/trajectories @conn {:known-values #{} :branches :all}))))))

(deftest a-branch-exports-as-the-role-it-ran-as
  ;; The role picks the system prompt and the tool surface, and it lived only
  ;; on the in-memory branch: an exported supervisor tape opened with "You are
  ;; a Clojure developer" above a supervisor's first thought. Now on the row.
  (let [rid (runs/start-run! @conn {:problem "p" :provider "glm" :model "glm-5.3"})]
    (runs/open-branch! @conn rid {:branch-id "SUP" :role :supervisor :problem "watch the run"})
    (runs/open-branch! @conn rid {:branch-id "T0"})
    (doseq [b ["SUP" "T0"]]
      (turn! rid b 1 {:tool-name "done" :args {:answer "ok"}
                      :assistant-text "```tool\n{\"name\":\"done\"}\n```" :result "Shipped."}))
    (runs/finish-run! @conn rid :completed "ok")
    (let [rows (export/trajectories @conn {:known-values #{} :branches :all
                                           :require-verified? false})
          by-branch (into {} (map (juxt :branch-id identity)) rows)]
      (is (= "supervisor" (:role (by-branch "SUP"))))
      (is (nil? (:role (by-branch "T0"))) "the unscoped default has no role, as before the column")
      (is (not= (:content (first (:messages (by-branch "SUP"))))
                (:content (first (:messages (by-branch "T0")))))
          "a different system prompt: the role's own tool surface")
      (is (str/includes? (:content (second (:messages (by-branch "SUP")))) "watch the run")
          "and the branch's own problem"))))

(deftest a-trajectory-opens-on-the-suffix-the-branch-saw
  ;; The suffix a cell hands initial-messages — the owner prompt, a unit's
  ;; attempt framing, the supervisor's role text — is on the row (v24), so
  ;; the exported system message is the one the model opened on. Before the
  ;; column every tape opened on the bare role prompt: a supervisor's without
  ;; the supervisor role text (karamazov-kgvg).
  (let [rid (runs/start-run! @conn {:problem "p" :provider "glm" :model "glm-5.3"})]
    (runs/open-branch! @conn rid {:branch-id "SUP" :role :supervisor
                                  :prompt-suffix "YOU WATCH THE RUN"})
    (runs/open-branch! @conn rid {:branch-id "T0"})
    (doseq [b ["SUP" "T0"]]
      (turn! rid b 1 {:tool-name "done" :args {:answer "ok"}
                      :assistant-text "```tool\n{\"name\":\"done\"}\n```" :result "Shipped."}))
    (runs/finish-run! @conn rid :completed "ok")
    (let [rows (export/trajectories @conn {:known-values #{} :branches :all
                                           :require-verified? false})
          system (fn [id] (->> rows (filter #(= id (:branch-id %))) first :messages first :content))]
      (is (str/ends-with? (system "SUP") "YOU WATCH THE RUN")
          "appended to the role's system prompt, as it was live")
      (is (not (str/includes? (system "T0") "YOU WATCH THE RUN"))
          "a branch that opened on none exports on none"))))

(deftest an-empty-tool-result-is-still-a-turn-the-model-saw
  ;; Skipping empty results, the way resume does, left two assistant messages
  ;; back to back wherever a grep found nothing — 3 of 23 real trajectories.
  ;; A chat template needs the roles to alternate, and "no output" is exactly
  ;; what the model has to learn an empty result means.
  (let [rid (runs/start-run! @conn {:problem "p"})]
    (runs/open-branch! @conn rid {:branch-id "B1"})
    (turn! rid "B1" 1 {:tool-name "grep" :args {:pattern "MARKER"}
                       :assistant-text "```tool\n{\"name\":\"grep\"}\n```" :result ""})
    (turn! rid "B1" 2 {:tool-name "__provider_error__" :category :failure
                       :assistant-text nil :result "provider: empty reply"})
    (turn! rid "B1" 3 {:tool-name "done" :args {:answer "ok"}
                       :assistant-text "```tool\n{\"name\":\"done\"}\n```" :result "Shipped."})
    (runs/finish-run! @conn rid :completed "ok")
    (let [[{:keys [messages]}] (export/trajectories @conn {:known-values #{}
                                                            :require-verified? false})
          roles (mapv :role messages)]
      (is (= ["system" "user" "assistant" "user" "assistant" "user"] roles)
          "the empty result is a user turn; the turn the model never answered is not a turn")
      (is (= "" (:content (nth messages 3)))))))

(deftest jsonl-is-one-conversation-per-line
  (run! {:problem "build it"})
  (let [path (str (export/scratch-path "samizdat-export-test") ".jsonl")
        n (export/write-jsonl! path (export/trajectories @conn {:known-values #{}}))
        lines (str/split-lines (slurp path))]
    (is (= 1 n (count lines)))
    (let [row (json/read-str (first lines) :key-fn keyword)]
      (is (= "glm-5.3" (:model row)))
      (is (vector? (:messages row)))
      (is (every? #(and (string? (:role %)) (string? (:content %))) (:messages row))))))

;; --- verdicts: every judgement, labelled by what happened next --------------
;;
;; karamazov-3htz. A corpus of the harness's verdicts is only worth anything
;; with the OUTCOME beside each one: a gate's prediction settled or not, the
;; branch a critic scored shipped or culled, the round a review passed green
;; or red on its tests. Trained on its own verdicts alone, a judge learns to
;; imitate its guesses.

(defn- judged-run!
  "A run whose journal carries one of every judgement the harness makes — a
  gate firing, a beam critic score and the reprieve it earned, a finalization
  critic verdict, a board review, a round critique with its verify — on a
  branch that then shipped green, or with `:fate :culled` was culled on a red
  tree."
  [{:keys [fate] :or {fate :shipped}}]
  (let [shipped? (= fate :shipped)
        rid (runs/start-run! @conn {:problem "build it" :provider "glm" :model "glm-5.3"})]
    (runs/open-branch! @conn rid {:branch-id "B1"})
    (turn! rid "B1" 1 {:tool-name "shell" :args {:command "ls"}
                       :assistant-text "x" :result "src"})
    (let [fid (journal/record-gate! @conn rid {:branch-id "B1" :turn 2 :gate :progress-stalled
                                               :message "No progress in 4 turns (TOKEN=hunter2)."
                                               :prediction "a write" :window 3})]
      (journal/settle-gate! @conn fid (if shipped? :met :unmet) 4))
    (journal/note! @conn rid :critic-score
                   {:branch-id "B1" :turn 3
                    :data {:scores {:progress 4 :momentum 3 :distinctness 5 :viability 4}
                           :summary "BRANCH B1\nThesis: build it (TOKEN=hunter2)"
                           :reply (str "Steady.\nSCORE progress: 4\nSCORE momentum: 3\n"
                                       "SCORE distinctness: 5\nSCORE viability: 4")}})
    (journal/note! @conn rid :cull-spared
                   {:branch-id "B1" :turn 3
                    :data {:scores {:progress 4 :momentum 3 :distinctness 5 :viability 4}
                           :failures 3 :fitness 0.4}})
    (journal/note! @conn rid :critic
                   {:branch-id "B1" :turn 4
                    :data {:branch-id "B1" :turn 4 :attempt 1 :verdict :incomplete :blocked true
                           :findings "- [high] no test covers the new path (TOKEN=hunter2)"}})
    (journal/note! @conn rid :board-review
                   {:data {:task "t1" :attempt 1 :verdict :complete :decision :pass :landed true}})
    (journal/note! @conn rid :critique {:data {:decision :ship :deterministic false}})
    (journal/note! @conn rid :verify {:data {:passed shipped? :exit (if shipped? 0 1) :timeout false}})
    ;; The shipping branch finishes with a successful done; the culled one
    ;; with a failed verification — a culled branch never got its done.
    (if shipped?
      (turn! rid "B1" 4 {:tool-name "done" :args {:answer "did it"}
                         :assistant-text "x" :result "Shipped."})
      (turn! rid "B1" 4 {:tool-name "shell" :args {:command "jolt test"} :category :failure
                         :assistant-text "x" :result "FAIL in (a-test)"}))
    (journal/note! @conn rid :ship-verify {:branch-id "B1" :turn 4
                                           :data {:ran true :green shipped? :timeout false}})
    (if shipped?
      (do (runs/close-branch! @conn rid "B1" :done "shipped")
          (runs/finish-run! @conn rid :completed "did it"))
      (do (runs/close-branch! @conn rid "B1" :culled "culled after 3 consecutive failures")
          (runs/finish-run! @conn rid :exhausted "")))
    rid))

(deftest every-judgement-is-labelled-by-what-happened-next
  (let [good (judged-run! {:fate :shipped})
        bad (judged-run! {:fate :culled})
        rows (export/verdicts @conn {:known-values #{"hunter2"}})
        of (fn [rid kind] (first (filter #(and (= rid (:run-id %)) (= kind (:kind %))) rows)))]
    (testing "a failed run is in the corpus — failure is the label, not noise"
      (is (= #{good bad} (set (map :run-id rows)))))
    (testing "a gate firing carries what it said, what it predicted, and how that settled"
      (let [g (of good "gate")]
        (is (= "B1" (:branch-id g)))
        (is (= 2 (:turn g)))
        (is (= "progress-stalled" (get-in g [:situation :gate])))
        (is (str/includes? (get-in g [:situation :message]) "No progress"))
        (is (= "a write" (get-in g [:verdict :prediction])))
        (is (= "met" (get-in g [:outcome :settled])))
        (is (= "unmet" (get-in (of bad "gate") [:outcome :settled])))))
    (testing "a critic score carries the summary it judged from, its reply, and the branch's fate"
      (let [c (of good "critic-score")]
        (is (str/includes? (get-in c [:situation :summary]) "BRANCH B1"))
        (is (= 4 (get-in c [:verdict :scores :progress])))
        (is (str/includes? (get-in c [:verdict :reply]) "SCORE progress"))
        (is (= "done" (get-in c [:outcome :branch-status])))
        (is (true? (get-in c [:outcome :shipped?])))
        (let [c' (of bad "critic-score")]
          (is (= "culled" (get-in c' [:outcome :branch-status])))
          (is (false? (get-in c' [:outcome :shipped?])))
          (is (str/includes? (get-in c' [:outcome :inactive-reason]) "consecutive failures")))))
    (testing "a cull reprieve is judged by whether the spared branch went on to ship"
      (is (= 3 (get-in (of good "cull-spared") [:situation :failures])))
      (is (true? (get-in (of good "cull-spared") [:outcome :shipped?])))
      (is (false? (get-in (of bad "cull-spared") [:outcome :shipped?]))))
    (testing "a finalization verdict carries its findings and is judged by the ship verification"
      (let [j (of good "critic")]
        (is (= "B1" (:branch-id j)))
        (is (= "incomplete" (get-in j [:verdict :verdict])))
        (is (true? (get-in j [:verdict :blocked?])))
        (is (str/includes? (get-in j [:verdict :findings]) "no test covers"))
        (is (true? (get-in j [:outcome :ship-verify-green?])))
        (is (false? (get-in (of bad "critic") [:outcome :ship-verify-green?])))))
    (testing "a board review and a round critique are judged by the test run that gated the round"
      (is (= "pass" (get-in (of good "board-review") [:verdict :decision])))
      (is (true? (get-in (of good "board-review") [:outcome :verify-passed?])))
      (is (false? (get-in (of bad "board-review") [:outcome :verify-passed?])))
      (is (= "ship" (get-in (of good "critique") [:verdict :decision])))
      (is (true? (get-in (of good "critique") [:outcome :verify-passed?])))
      (is (= "completed" (get-in (of good "critique") [:outcome :run-status])))
      (is (= "exhausted" (get-in (of bad "critique") [:outcome :run-status]))))))

(deftest a-secret-never-reaches-a-verdict
  (judged-run! {:fate :shipped})
  (let [rows (export/verdicts @conn {:known-values #{"hunter2"}})
        text (pr-str rows)]
    (is (not (str/includes? text "hunter2")))
    (is (str/includes? text "No progress") "redacted, not dropped")
    (is (str/includes? text "no test covers") "in every kind of judgement")))

(deftest a-critic-score-recorded-before-the-situation-was-kept-still-exports
  ;; The note's :data used to be the bare score map. An older journal must
  ;; still project: scores under :scores, and no situation to show.
  (let [rid (runs/start-run! @conn {:problem "p" :provider "glm" :model "m"})]
    (runs/open-branch! @conn rid {:branch-id "B1"})
    (journal/note! @conn rid :critic-score
                   {:branch-id "B1" :turn 3
                    :data {:progress 2 :momentum 2 :distinctness 2 :viability 2}})
    (runs/finish-run! @conn rid :exhausted "")
    (let [[c] (export/verdicts @conn {:known-values #{}})]
      (is (= "critic-score" (:kind c)))
      (is (= 2 (get-in c [:verdict :scores :progress])))
      (is (nil? (get-in c [:situation :summary])))
      (is (= "active" (get-in c [:outcome :branch-status]))
          "a branch nobody closed reports as it is, not as a guess"))))

(deftest verdicts-jsonl-is-one-judgement-per-line
  (judged-run! {:fate :shipped})
  (let [path (str (export/scratch-path "samizdat-verdicts-test") ".jsonl")
        n (export/write-verdicts! path (export/verdicts @conn {:known-values #{}}))
        lines (str/split-lines (slurp path))]
    (is (pos? n))
    (is (= n (count lines)))
    (let [row (json/read-str (first lines) :key-fn keyword)]
      (is (string? (:run_id row)))
      (is (string? (:kind row)))
      (is (map? (:situation row)))
      (is (map? (:verdict row)))
      (is (map? (:outcome row))))))
