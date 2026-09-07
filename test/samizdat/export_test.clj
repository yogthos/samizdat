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
