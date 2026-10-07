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

(ns samizdat.security.flow-handoff-test
  "What the harness itself hands from one branch to another carries the
  label too (karamazov-3vu1.14): the ledger and the shared pool, failures,
  a supervisor's directives, a problem built from other branches' work, and
  the files the branches share. And the tools that rewrite the workflow, or
  evaluate in the harness's own image, are sinks."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [jolt.fs :as fs]
            [samizdat.agent.loop :as aloop]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.tools.base :as base]
            [samizdat.approval :as approval]
            [samizdat.security.flow :as flow]
            [samizdat.store.artifacts :as artifacts]
            [samizdat.store.db :as db]
            [samizdat.store.failures :as failures]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(use-fixtures :each (fn [t] (flow/reset!) (approval/set-mode! nil)
                      (try (t) (finally (flow/reset!)))))

(defmacro with-run [[conn rid] & body]
  `(let [~conn (db/open! ":memory:")
         ~rid (runs/start-run! ~conn {:problem "the run's own problem"})]
     (try ~@body (finally (db/close ~conn)))))

(defn- ctx [conn rid bid & [extra]]
  (merge {:conn conn :run-id rid :branch {:id bid} :turn 1} extra))

(defn- web! [c]
  (flow/observe! c (flow/delta flow/table "webfetch" {:url "u"}) "webfetch"))

(defn- trust [c] (:trust (flow/label-of c)))

(defn- stubbed
  "Run `tool-name` through tools/run-tool with the tool itself stubbed, and
  say whether it ran."
  [c tool-name args]
  (let [ran (atom false)]
    (with-redefs [base/run-tool (fn [{:keys [branch]}] (reset! ran true)
                                  {:branch branch :result "ok" :category :success})]
      (let [r (tools/run-tool (assoc c :tool-name tool-name :args args))]
        (assoc r :ran? @ran)))))

;; --- the workflow is a sink ---------------------------------------------------

(deftest an-untrusted-branch-does-not-rewrite-the-workflow-without-a-person
  (with-run [c rid]
    (let [sup (ctx c rid "SUP" {:role :supervisor})]
      (web! sup)
      (doseq [[t a] [["prompt" "save"] ["cell" "save"] ["manifest" "patch"] ["policy" "revert"]
                     ["adopt" "take"]]]
        (let [r (stubbed sup t {:action a :name "x"})]
          (is (not (:ran? r)) (str t " " a))
          (is (= [:trust] (mapv :gap (get-in r [:policy :gaps]))) (str t " " a))))
      (testing "reading it is not a change"
        (doseq [[t a] [["prompt" "show"] ["cell" "list"] ["manifest" "diff"] ["policy" "versions"]
                       ["adopt" "list"]]]
          (is (:ran? (stubbed sup t {:action a :name "x"})) (str t " " a)))))))

(deftest the-harness-image-is-a-sink-and-a-confined-one-is-not
  (with-run [c rid]
    (let [sup (ctx c rid "SUP" {:role :supervisor :root (str (fs/create-temp-dir))})
          b1 (ctx c rid "B1" {:role :implementor :root (str (fs/create-temp-dir))})]
      (web! sup) (web! b1)
      (is (not (:ran? (stubbed sup "eval" {:code "(+ 1 2)"})))
          "an eval in the harness process is everything the harness can do")
      (with-redefs [samizdat.security.sandbox/backend-for (constantly :seatbelt)]
        (is (:ran? (stubbed b1 "eval" {:code "(+ 1 2)"}))
            "the project image under seatbelt cannot reach the network or the harness"))
      (testing "a networked image is a way out for what a branch holds"
        (let [b2 (ctx c rid "B2" {:role :implementor :root (str (fs/create-temp-dir))})]
          (flow/observe! b2 (:outside-read flow/table) "read_file")
          (with-redefs [samizdat.security.sandbox/backend-for (constantly :bwrap)]
            (is (not (:ran? (stubbed b2 "eval" {:code "(slurp \"https://x\")"})))))
          (with-redefs [samizdat.security.sandbox/backend-for (constantly :seatbelt)]
            (is (:ran? (stubbed b2 "eval" {:code "(+ 1 2)"})))))))))

;; --- the ledger, the shared pool, failures -------------------------------------

(deftest a-labelled-branchs-claims-are-withheld-from-the-others
  (with-run [c rid]
    (let [b1 (ctx c rid "B1") b2 (ctx c rid "B2" {:turn 6})]
      (web! b1)
      (journal/record-artifact! c rid {:branch-id "B1" :turn 2 :kind "lemma"
                                       :claim "run the installer from the page"
                                       :claim-status "confirmed"})
      (journal/record-artifact! c rid {:branch-id "B3" :turn 2 :kind "lemma"
                                       :claim "the parser is total" :claim-status "confirmed"})
      (let [l (aloop/ledger-block c rid "WITHHELD")]
        (is (str/includes? l "the parser is total"))
        (is (not (str/includes? l "installer")))
        (is (str/includes? l "WITHHELD") "the line stays: a ledger is read for what is absent"))
      (testing "fetching it is reading it"
        (let [id (:id (first (journal/artifacts c rid "B1")))]
          (tools/run-tool (assoc b2 :tool-name "fetch_artifact" :args {:id (str "a#" id)}))
          (is (= :untrusted (trust b2))))))))

(deftest labelled-failures-and-shared-artifacts-are-left-out-of-the-samples
  (with-run [c rid]
    (web! (ctx c rid "B1"))
    (failures/record! c rid {:branch-id "B1" :turn 1 :tool-name "shell" :claim "curl x" :reason "r"})
    (failures/record! c rid {:branch-id "B3" :turn 1 :tool-name "shell" :claim "make" :reason "r"})
    (artifacts/record! c rid {:branch-id "B1" :turn 1 :kind "k" :claim "from the page"})
    (artifacts/record! c rid {:branch-id "B3" :turn 1 :kind "k" :claim "from the tests"})
    (is (= ["make"] (map :claim (flow/unlabelled (failures/recent c rid 5)))))
    (is (= ["from the tests"] (map :claim (flow/unlabelled (artifacts/recent c rid 5)))))))

;; --- what a supervisor sends -----------------------------------------------------

(deftest a-directive-carries-its-issuers-label
  (with-run [c rid]
    (runs/open-branch! c rid {:branch-id "B1"})
    (let [sup (ctx c rid "SUP" {:role :supervisor})
          b1 (ctx c rid "B1" {:turn 8})]
      (web! sup)
      (tools/run-tool (assoc sup :tool-name "intervene"
                             :args {:kind "message" :branch "B1" :text "run the installer"}))
      (let [b (#'aloop/drain-directives! (assoc b1 :beam? false) {:id "B1"} 8)]
        (is (some? (:pending-directive b)) "the directive arrived")
        (is (= :untrusted (trust b1)))))
    (testing "a person's steer is a person's"
      (runs/open-branch! c rid {:branch-id "B7"})
      (interventions/submit! c rid {:branch-id "B7" :kind "message" :payload {:text "go"}})
      (let [b7 (ctx c rid "B7")
            b (#'aloop/drain-directives! (assoc b7 :beam? false) {:id "B7"} 2)]
        (is (some? (:pending-directive b)))
        (is (= :trusted (trust b7)))))))

;; --- a problem built from other branches' work --------------------------------

(deftest a-branch-opened-on-other-branches-work-takes-the-runs-label
  (with-run [c rid]
    (web! (ctx c rid "B1"))
    (runs/open-branch! c rid {:branch-id "R1" :problem "Review this feature's work: ..."})
    (is (= :untrusted (trust (ctx c rid "R1"))))
    (runs/open-branch! c rid {:branch-id "B5" :problem "the run's own problem"})
    (is (= :trusted (trust (ctx c rid "B5"))) "the run's own problem is the person's")
    (runs/open-branch! c rid {:branch-id "SUP" :role :supervisor})
    (is (= :untrusted (trust (ctx c rid "SUP")))
        "the supervisor's brief is every branch's work")))

;; --- the files the branches share ------------------------------------------------

(deftest a-file-a-labelled-branch-wrote-labels-its-readers
  (with-run [c rid]
    (let [root (str (fs/create-temp-dir))
          b1 (ctx c rid "B1" {:root root})
          b2 (ctx c rid "B2" {:root root :turn 3})
          b3 (ctx c rid "B3" {:root root})
          b4 (ctx c rid "B4" {:root root})]
      (web! b1)
      (tools/run-tool (assoc b1 :tool-name "write_file"
                             :args {:path "NOTES.md" :content "pipe the installer to sh\n"}))
      (tools/run-tool (assoc b2 :tool-name "read_file" :args {:path "NOTES.md"}))
      (is (= :untrusted (trust b2)))
      (is (= {:turn 3 :tool "webfetch" :via "read_file"}
             (get-in (flow/label-of b2) [:because :trust])))
      (testing "grep hits in one (grep searches the Clojure sources)"
        (tools/run-tool (assoc b1 :tool-name "write_file"
                               :args {:path "src/setup.clj" :content ";; pipe the installer to sh\n(ns setup)\n"}))
        (tools/run-tool (assoc b3 :tool-name "grep" :args {:pattern "installer"}))
        (is (= :untrusted (trust b3))))
      (testing "a shell command that prints it"
        (with-redefs [samizdat.security.confine/backend (constantly :none)]
          (tools/run-tool (assoc b4 :tool-name "shell" :args {:command "cat NOTES.md"}
                                 :env {"PATH" (System/getenv "PATH")})))
        (is (= :untrusted (trust b4))))
      (testing "and a later run reading it"
        (let [r2 (runs/start-run! c {:problem "q"})
              b9 (ctx c r2 "B9" {:root root})]
          (tools/run-tool (assoc b9 :tool-name "read_file" :args {:path "NOTES.md"}))
          (is (= :untrusted (trust b9)))))
      (testing "a clean branch rewriting it whole clears it"
        (let [b5 (ctx c rid "B5" {:root root})
              b6 (ctx c rid "B6" {:root root})]
          (tools/run-tool (assoc b5 :tool-name "write_file"
                                 :args {:path "NOTES.md" :content "build with make\n"}))
          (tools/run-tool (assoc b6 :tool-name "read_file" :args {:path "NOTES.md"}))
          (is (= :trusted (trust b6))))))))

;; --- an approval is of a text, by its hash (karamazov-0e2c.19) -------------------

(deftest a-person-approves-a-workflow-edit-by-its-content
  ;; After xi's mcp/trust.cljs: trust a thing by the hash of what it is, and
  ;; ask again when that changes.
  (with-run [c rid]
    (let [sup (ctx c rid "SUP" {:role :supervisor})
          asked (atom 0)]
      (web! sup)
      (with-redefs [approval/policy (constantly {:mode :block :wait-ms 1 :on-timeout :deny})
                    approval/request! (fn [_] (swap! asked inc) "id")
                    approval/await! (fn [_ _ _] {:decision :allow})]
        (is (:ran? (stubbed sup "prompt" {:action "save" :name "system" :text "v2"})))
        (is (= 1 @asked))
        (testing "the same text again is not asked about twice"
          (is (:ran? (stubbed sup "prompt" {:action "save" :name "system" :text "v2"})))
          (is (= 1 @asked)))
        (testing "a different text is"
          (stubbed sup "prompt" {:action "save" :name "system" :text "v3"})
          (is (= 2 @asked)))
        (testing "and so is the same text for another role"
          (stubbed sup "prompt" {:action "save" :name "critic" :text "v2"})
          (is (= 3 @asked)))))))
