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

(ns samizdat.exam-ratchet-test
  "The exam ratchet at `done` (karamazov-fgsb, owner's decision 2026-09-29):
  a run may change or delete a test that existed when it started, and must
  say why. Every pre-existing test the branch's tree no longer holds
  unchanged is named at ship time, and an unexplained one refuses `done`.

  The fixtures are the real edits the bead measured and the 2026-09-29
  corpus of 50 edit_file hunks: a per-assertion-line diff missed a deleted
  table row, a changed `let` input and a continuation line, which comparing
  whole test forms catches."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [samizdat.agent.exam :as exam]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.verify :as verify]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(def ^:private db-test-before
  "(ns app.db-test (:require [clojure.test :refer [deftest is testing]] [app.db :as db]))

(deftest active-count
  (testing \"active-count counts done=0 only\"
    (is (= 1 (db/active-count *conn*)))))

(deftest blank-titles
  (testing \"add-todo! with a blank title returns nil and stores nothing\"
    (is (nil? (db/add-todo! *conn* \"   \")))
    (is (= 0 (count (db/list-todos *conn*))))))
")

(defn- touched [files]
  (mapv #(select-keys % [:path :test :kind]) (exam/touched files)))

(deftest a-deleted-deftest-is-touched
  ;; a3ba69bb t28: blank-titles went as collateral in an edit repairing
  ;; another test.
  (let [after (str/replace db-test-before #"(?s)\(deftest blank-titles.*" "")]
    (is (= [{:path "test/app/db_test.clj" :test "blank-titles" :kind :deleted}]
           (touched [{:path "test/app/db_test.clj" :before db-test-before :after after}])))))

(deftest a-changed-input-or-table-row-is-touched
  (testing "a changed let input (8710067f t25)"
    (is (= [{:path "test/e_test.clj" :test "grunt-hits" :kind :changed}]
           (touched [{:path "test/e_test.clj"
                      :before "(deftest grunt-hits (let [e {:x 1.0}] (is (hit? e))))"
                      :after "(deftest grunt-hits (let [e {:x 0.9}] (is (hit? e))))"}]))))
  (testing "a row gone from a data table (4e785664 t22)"
    (is (= [{:path "test/d_test.clj" :test "cases" :kind :changed}]
           (touched [{:path "test/d_test.clj"
                      :before "(def cases [[{:a 1} nil] [nil nil]])"
                      :after "(def cases [[{:a 1} nil]])"}])))))

(deftest what-keeps-a-test's-meaning-is-not-touched
  (testing "whitespace and comments"
    (is (empty? (touched [{:path "test/t_test.clj"
                           :before "(deftest t\n  (is (= 1 (f))) ; one\n)"
                           :after "(deftest t (is (= 1 (f))))"}]))))
  (testing "a fn literal reads with fresh gensyms each time and is still the same test"
    (is (empty? (touched [{:path "test/t_test.clj"
                           :before "(deftest t (is (every? #(pos? %) (xs))))"
                           :after "(deftest t (is (every? #(pos? %) (xs))))"}]))))
  (testing "a new test, and a helper that changed"
    (is (empty? (touched [{:path "test/t_test.clj"
                           :before "(defn- helper [] 1)\n(deftest t (is (= 1 (helper))))"
                           :after "(defn- helper [] (inc 0))\n(deftest t (is (= 1 (helper))))\n(deftest u (is true))"}])))))

(deftest a-test-moved-to-another-file-is-not-touched
  ;; e1b765e7: the largest false alarm of the first detector was a refactor.
  (let [t "(deftest seam-flies (is (= 3 (count (fly)))))"]
    (is (empty? (touched [{:path "test/a_test.clj" :before (str "(ns a)\n" t) :after "(ns a)"}
                          {:path "test/b_test.clj" :before nil :after (str "(ns b)\n" t)}])))))

(deftest a-file-that-does-not-read-falls-back-to-its-assertion-lines
  ;; 170f4ec9 (Python): four contract assertions replaced by a bare call.
  (is (= [{:path "test_calc.py" :test nil :kind :changed}]
         (touched [{:path "test_calc.py"
                    :before "def test_mul():\n    assert mul(2, 3) == 6\n    assert mul(0, 5) == 0\n"
                    :after "def test_mul():\n    mul(1, 1)\n"}])))
  (is (empty? (touched [{:path "test_calc.py"
                         :before "def test_mul():\n    assert mul(2, 3) == 6\n"
                         :after "def test_mul():\n    assert mul(2, 3) == 6\n    assert mul(0, 5) == 0\n"}]))
      "an added assertion changes nothing that was there"))

(deftest an-explanation-names-the-test-and-gives-a-reason
  (let [ts [{:path "test/app/db_test.clj" :test "blank-titles" :kind :deleted}
            {:path "test_calc.py" :test nil :kind :changed}]]
    (is (= ts (exam/unexplained ts {})))
    (is (= [(second ts)]
           (exam/unexplained ts (exam/explanations [{"test" "blank-titles" "reason" "moved to validation_test"}]))))
    (is (= [(first ts)]
           (exam/unexplained ts (exam/explanations {"test_calc.py" "the contract changed: mul is gone"})))
        "a file stands for the assertions of a file that does not read")
    (is (= ts (exam/unexplained ts (exam/explanations [{"test" "blank-titles" "reason" " "}])))
        "a blank reason explains nothing")
    ;; Run 582980ef: the model named the test as ns/name and as name (file).
    (is (= [(second ts)]
           (exam/unexplained ts (exam/explanations [{"test" "app.db-test/blank-titles" "reason" "moved"}]))))
    (is (= [(second ts)]
           (exam/unexplained ts (exam/explanations [{"test" "Blank-Titles (test/app/db_test.clj)" "reason" "moved"}]))))
    (is (= ts (exam/unexplained ts (exam/explanations [{"test" "blank-titles-2" "reason" "moved"}])))
        "a different name is not the test")))

;; --- done -----------------------------------------------------------------------

(defn- tmp-root []
  (let [d (io/file (System/getProperty "java.io.tmpdir") (str "exam-" (System/nanoTime)))]
    (.mkdirs (io/file d "test" "app"))
    (.getPath d)))

(defn- ship [root args {:keys [conn run-id branch]}]
  (with-redefs [gitdiff/changed-files (fn [_ _] ["src/app/db.clj" "test/app/db_test.clj"])
                gitdiff/file-at (fn [_ _ path] (when (= path "test/app/db_test.clj") db-test-before))
                verify/run-verify (fn [_ _ _] {:green? true :output "Ran 2 tests. 0 failures, 0 errors"})]
    (tools/run-tool {:branch (or branch (state/new-branch {:id "B1" :problem "make blank titles an error"}))
                     :tool-name "done" :turn 3 :root root :git-baseline "HEAD"
                     :conn conn :run-id run-id
                     :config {:run {:verify-cmd "jolt -M:test"}}
                     :args args})))

(deftest done-refuses-an-unexplained-deleted-test
  (let [root (tmp-root)]
    (spit (io/file root "test/app/db_test.clj") (str/replace db-test-before #"(?s)\(deftest blank-titles.*" ""))
    (let [r (ship root {:answer "blank titles are now an error; the suite is green"} {})]
      (is (not (:done? r)))
      (is (str/includes? (:result r) "blank-titles") (:result r))
      (is (str/includes? (:result r) "changed_tests") "and says how to explain it"))
    (let [r (ship root {:answer "blank titles are now an error; the suite is green"
                        :changed_tests [{"test" "blank-titles" "reason" "blank titles now throw; pinned in blank-title-throws"}]}
                  {})]
      (is (:done? r) (:result r))
      (is (str/includes? (:answer r) "blank titles now throw") "the reason rides on the answer"))))

(deftest a-tree-that-kept-its-tests-owes-nothing
  (let [root (tmp-root)]
    (spit (io/file root "test/app/db_test.clj")
          (str db-test-before "\n(deftest blank-title-throws (is (thrown? Exception (db/add-todo! *conn* \"\"))))\n"))
    (is (:done? (ship root {:answer "blank titles are now an error; the suite is green"} {})))))

(deftest an-explanation-from-earlier-in-the-run-still-counts
  ;; A board run: the branch that changed the test explained it when it
  ;; shipped; a sibling shipping later over the same tree is not asked again.
  (let [root (tmp-root)
        c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "make blank titles an error"})]
    (try
      (spit (io/file root "test/app/db_test.clj") (str/replace db-test-before #"(?s)\(deftest blank-titles.*" ""))
      (is (:done? (ship root {:answer "blank titles are now an error; the suite is green"
                              :changed_tests [{"test" "blank-titles" "reason" "replaced by blank-title-throws"}]}
                        {:conn c :run-id rid})))
      (is (seq (journal/notes c rid :tests-explained)))
      (is (:done? (ship root {:answer "blank titles are now an error; the suite is green"}
                        {:conn c :run-id rid :branch (state/new-branch {:id "B2" :problem "make blank titles an error"})}))
          "the sibling ships over the explained change")
      (finally (db/close c)))))

(deftest an-advisory-branch-owes-no-explanation
  (let [root (tmp-root)]
    (spit (io/file root "test/app/db_test.clj") "")
    (is (:done? (ship root {:answer "REVISE: blank-titles was deleted without a word"}
                      {:branch (assoc (state/new-branch {:id "S0" :problem "review"}) :advisory? true)})))))
