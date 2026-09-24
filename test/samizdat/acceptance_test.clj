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
(ns samizdat.acceptance-test
  "The acceptance checker (karamazov-a6mj.2): criteria the OPERATOR writes
  before the run, checked over the tree as it stands, which the run never
  edits. Thinkingbox's model — a test case is a task plus a checker over the
  post-run world state, testing the outcome and never the path — brought to a
  harness that runs real tools on a real tree.

  Everything here is pure: the shell and the judge are injected seams, so the
  whole decision is testable without spawning a process or paying a model."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [samizdat.agent.acceptance :as acceptance]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.judge :as judge]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.verify :as verify]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.system :as system]))

;; --- the spec ----------------------------------------------------------------

(deftest a-spec-is-a-vector-of-named-checks-and-questions
  (let [c (acceptance/normalize [{:name "suite green" :check "jolt -M:test"}
                                 {:name "fade visible"
                                  :judge "Does the answer say what the screenshot showed?"}])]
    (is (= [{:name "suite green" :kind :check :text "jolt -M:test"}
            {:name "fade visible" :kind :judge
             :text "Does the answer say what the screenshot showed?"}]
           c))))

(deftest a-malformed-criterion-is-refused-not-skipped
  ;; A criterion the checker silently dropped is a requirement nobody checks,
  ;; which is the defect the whole thing exists to prevent.
  (doseq [bad [[{:check "jolt -M:test"}]                       ; no name
               [{:name "x"}]                                    ; neither
               [{:name "x" :check "a" :judge "b"}]              ; both
               [{:name "" :check "a"}]                          ; blank name
               "jolt -M:test"]]                                 ; not a vector
    (is (thrown? Exception (acceptance/normalize bad)) (pr-str bad))))

(deftest an-absent-spec-is-no-criteria
  (is (= [] (acceptance/normalize nil)))
  (is (= [] (acceptance/normalize []))))

;; --- running it --------------------------------------------------------------

(def ^:private criteria
  (acceptance/normalize [{:name "suite green" :check "jolt -M:test"}
                         {:name "no ring in a hill" :check "jolt -M:test :only rings"}
                         {:name "says what it saw" :judge "Does the answer say what it saw?"}]))

(deftest each-criterion-gets-its-own-verdict
  ;; Per criterion, never one exit code: a failure has to name what failed,
  ;; in the failure's own words, or the branch is told "you are doing badly"
  ;; and left to guess at what.
  (let [ran (atom [])
        results (acceptance/check criteria
                                  {:run-check (fn [cmd]
                                                (swap! ran conj cmd)
                                                (if (str/includes? cmd "rings")
                                                  {:green? false :output "FAIL in (ring-zero) expected 22.0"}
                                                  {:green? true :output "63 tests, 0 failures"}))
                                   :judge (fn [_q] {:yes? true :reply "YES — it names the screenshot"})})]
    (is (= ["jolt -M:test" "jolt -M:test :only rings"] @ran))
    (is (= [true false true] (mapv :passed? results)))
    (is (= ["suite green" "no ring in a hill" "says what it saw"] (mapv :name results)))
    (testing "the failing criterion carries the failure's own words"
      (is (str/includes? (:output (second results)) "ring-zero")))
    (is (not (acceptance/all-passed? results)))
    (is (= ["no ring in a hill"] (mapv :name (acceptance/failed results))))))

(deftest only-the-kinds-asked-for-are-run
  ;; `done` is model-free by design (ship.clj: every rung runs with no model in
  ;; the path), so at the ship gate only the :check criteria run; the :judge
  ;; ones wait for the critic role at verify. A criterion not run is reported
  ;; as unknown, not as passed and not as failed.
  (let [judged (atom 0)
        results (acceptance/check criteria
                                  {:kinds #{:check}
                                   :run-check (fn [_] {:green? true :output "ok"})
                                   :judge (fn [_] (swap! judged inc) {:yes? false :reply "NO"})})]
    (is (zero? @judged))
    (is (= [true true nil] (mapv :passed? results)))
    (is (acceptance/all-passed? results) "an unknown does not block; only a false does")
    (is (= ["says what it saw"] (mapv :name (acceptance/unknown results))))))

(deftest a-check-that-times-out-fails-and-says-so
  (let [[r] (acceptance/check (acceptance/normalize [{:name "slow" :check "sleep 999"}])
                              {:run-check (fn [_] {:green? false :timeout? true :output ""})})]
    (is (false? (:passed? r)))
    (is (re-find #"(?i)timed out" (:output r)))))

(deftest a-judge-with-no-verdict-is-unknown-not-a-failure
  ;; Fail-open like the critic: a judge that answers in prose, or a provider
  ;; that errors, must not refuse the ship on its own. It is recorded as
  ;; unknown so a reader can see the gate was not decided.
  (let [[r] (acceptance/check (acceptance/normalize [{:name "q" :judge "Is it?"}])
                              {:judge (fn [_] {:yes? nil :reply "well, it depends"})})]
    (is (nil? (:passed? r)))
    (is (str/includes? (:output r) "depends"))))

(deftest a-judge-that-throws-is-unknown-not-a-failure
  (let [[r] (acceptance/check (acceptance/normalize [{:name "q" :judge "Is it?"}])
                              {:judge (fn [_] (throw (ex-info "provider down" {})))})]
    (is (nil? (:passed? r)))
    (is (str/includes? (:output r) "provider down"))))

;; --- what the branch reads -----------------------------------------------------

(deftest the-refusal-names-each-failing-criterion-and-its-output
  (let [results [{:name "suite green" :kind :check :passed? true :output "63 tests"}
                 {:name "no ring in a hill" :kind :check :passed? false
                  :output "FAIL in (ring-zero) expected 22.0"}
                 {:name "says what it saw" :kind :judge :passed? nil :output "not run"}]
        msg (acceptance/refusal results)]
    (is (str/includes? msg "no ring in a hill"))
    (is (str/includes? msg "ring-zero"))
    (is (str/includes? msg "suite green") "what was met is shown too, so the branch does not re-do it")
    (is (not (str/includes? msg "says what it saw"))
        "a criterion that did not run is not reported as unmet")))

(deftest the-table-is-one-line-per-criterion
  (let [results [{:name "a" :kind :check :passed? true :output "ok"}
                 {:name "b" :kind :check :passed? false :output "boom"}
                 {:name "c" :kind :judge :passed? nil :output "not run"}]
        t (acceptance/table results)]
    (is (= 3 (count (str/split-lines t))))
    (is (re-find #"(?m)^PASS +a" t))
    (is (re-find #"(?m)^FAIL +b" t))
    (is (re-find #"(?m)^\?    +c|(?m)^UNKNOWN +c" t))))

;; --- the yes/no judge ----------------------------------------------------------

(deftest the-yesno-parse-reads-the-first-word-of-the-reply
  (is (true? (judge/parse-yesno "YES — the answer names the screenshot.")))
  (is (true? (judge/parse-yesno "  yes.\nBecause...")))
  (is (false? (judge/parse-yesno "NO. It never mentions rendering.")))
  (is (false? (judge/parse-yesno "No")))
  (is (nil? (judge/parse-yesno "It depends on what you mean.")))
  (is (nil? (judge/parse-yesno "")))
  (is (nil? (judge/parse-yesno nil)))
  (testing "a reply that reasons first and answers last is still read"
    (is (true? (judge/parse-yesno "The diff touches draw.clj and the answer says so.\n\nYES")))))

(deftest the-yesno-prompt-carries-the-question-and-the-evidence
  (let [p (judge/yesno-prompt {:question "Does the answer say what it saw?"
                               :answer "I saw the horizon fade."
                               :diff "+ (defn fade ...)"
                               :evidence "files written: draw.clj"})]
    (is (str/includes? p "Does the answer say what it saw?"))
    (is (str/includes? p "I saw the horizon fade."))
    (is (str/includes? p "(defn fade"))
    (is (str/includes? p "draw.clj"))
    (is (re-find #"(?i)YES or NO" p) "the reply shape the parser reads is what the prompt asks for")))

;; --- the ship gate -------------------------------------------------------------

(deftest done-is-refused-by-a-failing-acceptance-check-naming-the-criterion
  ;; The shape karamazov-dsfx measured: an answer that never mentions a
  ;; requirement ships, and only the critic catches it, a round later. With
  ;; the requirement written as a criterion the operator holds, `done` is
  ;; refused at the ship gate and told which criterion, in its own words.
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "fade the horizon"})
        ship (fn [cfg]
               (tools/run-tool {:branch (state/new-branch {:id "B1" :problem "fade the horizon"})
                                :tool-name "done" :turn 3 :conn c :run-id rid
                                :root "/tmp" :git-baseline "HEAD"
                                :config {:run cfg}
                                :args {:answer "faded the horizon so the ground dissolves into sky"}}))
        spec [{:name "suite green" :check "jolt -M:test"}
              {:name "far trees stand on ground" :check "jolt -M:test :only flight.draw-test"}
              {:name "says what it saw" :judge "Does the answer say what the screenshot showed?"}]]
    (try
      (with-redefs [gitdiff/changed-files (fn [_ _] ["src/x.clj" "test/x_test.clj"])
                    verify/run-verify (fn [_ cmd _]
                                        (if (str/includes? cmd "draw-test")
                                          {:green? false :output "FAIL in (trees-stand-on-ground)\nexpected: (<= y ground)"}
                                          {:green? true :output "63 tests, 0 failures"}))]
        (testing "a failing criterion refuses the ship and names itself"
          (let [r (ship {:verify-cmd "jolt -M:test" :acceptance spec})]
            (is (not (:done? r)))
            (is (str/includes? (:result r) "far trees stand on ground") (:result r))
            (is (str/includes? (:result r) "trees-stand-on-ground") "the failure's own words")
            (is (str/includes? (:result r) "suite green") "what was met is shown too")
            (is (not (str/includes? (:result r) "says what it saw"))
                "a judge criterion is not run at the model-free ship gate and is not reported unmet")))
        (testing "the verdicts are on the journal, per criterion"
          (let [[note] (journal/notes c rid :acceptance)]
            (is (some? note))
            (is (= "done" (:at note)))
            (is (= [true false nil] (mapv :passed? (:results note))))
            (is (false? (:passed? note)))))
        (testing "with every check green the ship goes through"
          (with-redefs [verify/run-verify (fn [_ _ _] {:green? true :output "ok"})]
            (let [r (ship {:verify-cmd "jolt -M:test" :acceptance spec})]
              (is (:done? r) (:result r)))))
        (testing "no spec, no rung — exactly as before"
          (let [called (atom [])]
            (with-redefs [verify/run-verify (fn [_ cmd _] (swap! called conj cmd) {:green? true :output "ok"})]
              (let [r (ship {:verify-cmd "jolt -M:test"})]
                (is (:done? r))
                (is (= ["jolt -M:test"] @called) "only the verify command ran"))))))
      (finally (db/close c)))))

(deftest a-check-is-not-paid-for-when-an-earlier-rung-already-refused
  ;; Same economy as ship-verify: a refused answer is going back anyway, and a
  ;; process spawn per criterion is real time.
  (let [called (atom 0)]
    (with-redefs [gitdiff/changed-files (fn [_ _] ["src/x.clj" "test/x_test.clj"])
                  verify/run-verify (fn [_ _ _] (swap! called inc) {:green? true :output "ok"})]
      (let [r (tools/run-tool {:branch (state/new-branch {:id "B1" :problem "p"})
                               :tool-name "done" :turn 3 :root "/tmp" :git-baseline "HEAD"
                               :config {:run {:acceptance [{:name "a" :check "true"}]}}
                               :args {:answer ""}})]
        (is (not (:done? r)) "an empty answer is refused first")
        (is (zero? @called))))))

(deftest an-advisory-branch-is-not-held-to-the-acceptance-criteria
  ;; A reviewer or supervisor delivers a VERDICT through done, not the work
  ;; (karamazov-t86); the criteria are about the work.
  (let [called (atom 0)]
    (with-redefs [verify/run-verify (fn [_ _ _] (swap! called inc) {:green? false :output "no"})]
      (let [r (tools/run-tool {:branch (assoc (state/new-branch {:id "S0" :problem "review the round"})
                                              :advisory? true)
                               :tool-name "done" :turn 3 :root "/tmp" :git-baseline "HEAD"
                               :config {:run {:acceptance [{:name "a" :check "false"}]}}
                               :args {:answer "PASS: the round's changes implement the feature"}})]
        (is (:done? r) (:result r))
        (is (zero? @called))))))

;; --- the spec is refused at start, not at ship ---------------------------------

(deftest a-malformed-spec-refuses-to-start-the-system
  ;; Loud at config time. A spec that failed at `done` would wedge every
  ;; branch of the run on a message about the operator's file.
  (is (thrown-with-msg?
       Exception #"acceptance"
       (system/start! (fn [_] {:status 200})
                      {:db {:path ":memory:"} :http {:port 0}
                       :roles {:default :local}
                       :run {:acceptance [{:check "no name"}]}})))
  (is (not (system/started?))))
