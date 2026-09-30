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

(ns samizdat.checklist-test
  "The ship checklist (karamazov-dsfx). A requirement could ship by being
  left out of the answer: the lexical rungs catch a confession, not a
  silence. The checklist is data that exists before the work — the
  operator's acceptance criteria, the held task's tests, what the branch
  declared with `plan`, the problem's own list items — and `done` must
  account for every item by id. Omission becomes a missing key."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [samizdat.agent.checklist :as checklist]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.verify :as verify]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]
            [samizdat.store.tasks :as tasks]))

(def ^:private problem
  "Make the world stop visibly ending at the edge of the terrain.

- distant ground and trees fade toward the sky colour
- no tree is drawn past the ground it stands on
1. run the game and say what the screenshot showed

```
- not a requirement, a line inside a code block
```")

(deftest the-problem's-list-items-are-its-requirements
  (is (= ["distant ground and trees fade toward the sky colour"
          "no tree is drawn past the ground it stands on"
          "run the game and say what the screenshot showed"]
         (checklist/problem-items problem)))
  (is (= [] (checklist/problem-items "Fix the off-by-one in paging.")) "prose alone has no items"))

(deftest items-take-an-id-per-source-in-a-stable-order
  (let [its (checklist/items {:acceptance [{:name "suite green" :kind :check :text "jolt -M:test"}
                                           {:name "says what it saw" :kind :judge :text "Does it say?"}]
                              :task-tests "the page shows one item left per user"
                              :declared ["blend toward sky" "cull at the terrain edge"]
                              :problem problem})]
    (is (= ["a1" "a2" "t1" "c1" "c2" "p1" "p2" "p3"] (mapv :id its)))
    (is (= "suite green" (:text (first its))))
    (is (= "Does it say?" (:text (second its))) "a judge criterion is its question")
    (is (= #{:acceptance :task :declared :problem} (set (map :source its))))))

(deftest a-test-path-is-not-a-checklist-item
  ;; A task whose :tests is a file is judged by running it (verify), not by
  ;; the answer saying so.
  (is (empty? (checklist/items {:task-tests "test/app/core_test.clj"}))))

(deftest the-answer's-entries-are-read-leniently-and-judged-strictly
  (testing "a vector of entries, or a map by id"
    (is (= {"a1" {:status :met :evidence "ran it"}}
           (checklist/entries [{"item" "a1" "status" "met" "evidence" "ran it"}])))
    (is (= {"c1" {:status :not-met :evidence "no display"}}
           (checklist/entries {"C1" {"status" "not met" "evidence" "no display"}})))
    (is (= {"p2" {:status :n-a :evidence "menus are out of scope"}}
           (checklist/entries [{:id "p2" :status "N/A" :note "menus are out of scope"}]))))
  (testing "a JSON string of the same"
    (is (= :met (get-in (checklist/entries "[{\"item\":\"a1\",\"status\":\"met\",\"evidence\":\"x\"}]")
                        ["a1" :status]))))
  (testing "garbage is no entries, not an error"
    (is (= {} (checklist/entries "all done")))
    (is (= {} (checklist/entries nil)))))

(deftest an-item-is-accounted-for-only-with-a-known-status-and-a-reason
  (let [its (checklist/items {:declared ["one" "two" "three" "four"]})
        es (checklist/entries [{"item" "c1" "status" "met" "evidence" "test x passes"}
                               {"item" "c2" "status" "probably" "evidence" "yes"}
                               {"item" "c3" "status" "not_met" "evidence" ""}])]
    (is (= ["c2" "c3" "c4"] (mapv :id (checklist/unaccounted its es)))
        "an unknown status, a blank reason and a missing entry all count as silence")))

(deftest the-accounted-checklist-rides-on-the-answer
  (let [its (checklist/items {:declared ["blend toward sky" "cull at the edge"]})
        es (checklist/entries [{"item" "c1" "status" "met" "evidence" "fade-test passes"}
                               {"item" "c2" "status" "not_met" "evidence" "no display here"}])
        out (checklist/render its es)]
    (is (str/includes? out "blend toward sky"))
    (is (str/includes? out "fade-test passes"))
    (is (str/includes? out "no display here"))
    (is (re-find #"(?i)not met" out))))

;; --- done -----------------------------------------------------------------------

(defn- ship
  ([args] (ship args {}))
  ([args {:keys [branch cfg conn run-id]}]
   (with-redefs [gitdiff/changed-files (fn [_ _] ["src/x.clj" "test/x_test.clj"])
                 verify/run-verify (fn [_ _ _] {:green? true :output "Ran 3 tests. 0 failures, 0 errors"})]
     (tools/run-tool {:branch (or branch (state/new-branch {:id "B1" :problem problem}))
                      :tool-name "done" :turn 3 :root "/tmp" :git-baseline "HEAD"
                      :conn conn :run-id run-id
                      :config {:run (merge {:verify-cmd "jolt -M:test"} cfg)}
                      :args args}))))

(def ^:private silent-answer
  "Distant ground and trees now blend toward the sky colour by distance, and
  trees are only drawn inside the terrain. The suite is green.")

(deftest done-refuses-an-answer-that-is-silent-on-an-item
  ;; dsfx's case D: the answer that simply never mentions the requirement.
  (let [r (ship {:answer silent-answer})]
    (is (not (:done? r)))
    (is (str/includes? (:result r) "p3") (:result r))
    (is (str/includes? (:result r) "run the game and say what the screenshot showed")
        "the refusal names the item in its own words")
    (is (str/includes? (:result r) "checklist") "and says how to account for it")))

(deftest an-honest-not-met-ships
  ;; ylte.1: an honest limit is a result. What is refused is silence.
  (let [r (ship {:answer silent-answer
                 :checklist [{"item" "p1" "status" "met" "evidence" "fade test"}
                             {"item" "p2" "status" "met" "evidence" "cull test"}
                             {"item" "p3" "status" "not_met" "evidence" "no window server on this host"}]})]
    (is (:done? r) (:result r))
    (is (str/includes? (:answer r) "no window server on this host")
        "the shipped answer carries the checklist, so the critic reads the claims")))

(deftest acceptance-criteria-are-items-for-a-run-level-answer
  (let [spec [{:name "suite green" :check "jolt -M:test"}
              {:name "says what it saw" :judge "Does the answer say what the screenshot showed?"}]
        branch (state/new-branch {:id "B1" :problem "fade distant ground and trees toward the sky colour"})]
    (let [r (ship {:answer silent-answer} {:branch branch :cfg {:acceptance spec}})]
      (is (not (:done? r)))
      (is (str/includes? (:result r) "a2"))
      (is (str/includes? (:result r) "Does the answer say what the screenshot showed?")))
    (let [r (ship {:answer silent-answer
                   :checklist {"a1" {"status" "met" "evidence" "jolt -M:test green"}
                               "a2" {"status" "met" "evidence" "the screenshot shows the fade"}}}
                  {:branch branch :cfg {:acceptance spec}})]
      (is (:done? r) (:result r)))))

(deftest a-task-branch-answers-for-its-task-not-the-run
  ;; A board piece is judged on its own contract; the run's criteria and the
  ;; problem's list are the run-level answer's to account for.
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem problem})]
    (try
      (let [t (tasks/create! c {:title "register the test ns" :run-id rid
                                :contract "Register the namespace:\n- in the :require vector\n- in the run-tests list"
                                :tests "the runner lists flight.horizon-test in both places\n\nWrite the test first."})
            _ (tasks/claim! c t rid "B1")
            r (ship {:answer "registered it in the require vector and the run list"}
                    {:conn c :run-id rid :cfg {:acceptance [{:name "suite green" :check "jolt -M:test"}]}})]
        (is (not (:done? r)))
        (is (str/includes? (:result r) "t1"))
        (is (str/includes? (:result r) "in the run-tests list") "its task's contract is its problem")
        (is (not (str/includes? (:result r) "Write the test first")) "the tests' first paragraph only")
        (is (not (str/includes? (:result r) "distant ground")) "the run's problem is not this branch's")
        (is (not (str/includes? (:result r) "a1")) "nor are the run's criteria"))
      (finally (db/close c)))))

(deftest what-plan-declared-is-owed-and-a-re-plan-cannot-drop-it
  (let [b (-> (state/new-branch {:id "B1" :problem "fix the pager"})
              (state/declare-checklist ["pages are 1-based" "the last page is not empty"])
              (state/declare-checklist ["pages are 1-based" "a page size of 0 is refused"]))]
    (is (= ["pages are 1-based" "the last page is not empty" "a page size of 0 is refused"]
           (state/checklist b)))
    (let [r (ship {:answer "fixed the pager"} {:branch b})]
      (is (not (:done? r)))
      (is (str/includes? (:result r) "the last page is not empty")))))

(deftest the-plan-tool-takes-a-checklist
  (let [r (tools/run-tool {:branch (state/new-branch {:id "B1" :problem "fix the pager"})
                           :tool-name "plan" :turn 1
                           :args {:files ["src/pager.clj"] :checklist ["pages are 1-based"]}})]
    (is (= ["pages are 1-based"] (state/checklist (:branch r))))))

(deftest an-advisory-branch-owes-no-checklist
  (let [r (ship {:answer "PASS: the round implements the feature"}
                {:branch (assoc (state/new-branch {:id "S0" :problem problem}) :advisory? true)})]
    (is (:done? r) (:result r))))

(deftest the-checklist-is-journalled-on-a-ship
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem problem})]
    (try
      (ship {:answer silent-answer
             :checklist [{"item" "p1" "status" "met" "evidence" "fade test"}
                         {"item" "p2" "status" "met" "evidence" "cull test"}
                         {"item" "p3" "status" "n/a" "evidence" "the operator runs it"}]}
            {:conn c :run-id rid})
      (let [[note] (journal/notes c rid :checklist)]
        (is (= ["p1" "p2" "p3"] (mapv :id (:items note))))
        (is (= "n-a" (:status (some #(when (= "p3" (:id %)) %) (:entries note))))))
      (finally (db/close c)))))
