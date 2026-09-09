;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.split-test
  "The split tool: a parent hands work down by writing the stubs the pieces
  must fill, and the harness verifies mechanically that it did — a split whose
  stubs are not in the tree is declined the way a manifest that does not
  compile is declined."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.tools.split]
            [samizdat.store.db :as db]
            [samizdat.store.runs :as runs]
            [samizdat.store.tasks :as tasks]))

(def ^:private stubbed
  "What a parent writes before it delegates: the signatures, hollow."
  "(ns example.core)

(defn parse-line
  \"Parse one line into {:key :value}.\"
  [line]
  (throw (ex-info \"not implemented\" {})))

(defn render-report
  \"Render parsed lines as a report string.\"
  [rows]
  (throw (ex-info \"not implemented\" {})))

(defn run
  \"The composition: parse every line, then render.\"
  [lines]
  (render-report (map parse-line lines)))
")

(def ^:private sketched-tests
  "(ns example.core-test
  (:require [clojure.test :refer [deftest is]]))

(deftest parse-line-splits-on-the-first-colon
  (is (= {:key \"a\" :value \"b\"} (example.core/parse-line \"a:b\"))))

(deftest render-report-joins-rows
  (is (string? (example.core/render-report []))))
")

(defn- with-project
  "A throwaway tree with the parent's stubs already written, plus a live run."
  [f]
  (let [root (str (fs/create-temp-dir {:prefix "split-test"}))
        conn (db/open! ":memory:")]
    (try
      (fs/create-dirs (fs/path root "src" "example"))
      (fs/create-dirs (fs/path root "test" "example"))
      (spit (io/file root "src/example/core.clj") stubbed)
      (spit (io/file root "test/example/core_test.clj") sketched-tests)
      (let [run-id (runs/start-run! conn {:problem "build the report"
                                          :max-turns 10 :beam-width 1})]
        (runs/open-branch! conn run-id {:branch-id "B1"})
        (f {:conn conn :run-id run-id :root root}))
      (finally (fs/delete-tree root)))))

(defn- call [{:keys [conn run-id root]} args]
  (base/run-tool {:branch {:id "B1"} :conn conn :run-id run-id :root root
                  :tool-name "split" :args args}))

(def ^:private good-parts
  [{"name" "parse-line" "description" "Parse one line into a map."
    "file" "src/example/core.clj" "stubs" ["parse-line"]
    "tests" "test/example/core_test.clj"}
   {"name" "render-report" "description" "Render the rows as a report."
    "file" "src/example/core.clj" "stubs" ["render-report"]
    "tests" "test/example/core_test.clj"}])

(deftest a-verified-split-creates-a-child-row-per-piece
  (with-project
    (fn [{:keys [conn run-id] :as p}]
      (let [r (call p {:reason "two responsibilities" :parts good-parts})]
        (is (= :neutral (:category r)) (str "accepted: " (:result r)))
        (let [rows (filter :parent_id (tasks/board conn {:run-id run-id}))]
          (is (= 2 (count rows)) "one child task per piece")
          (is (apply = (map :parent_id rows))
              "both hang off the same parent task, so the tree is walkable")
          (testing "the contract is the stub SOURCE, not a restatement of the ask"
            (is (some #(str/includes? (:contract %) "Parse one line into") rows)))
          (testing "and tests is the PATH, so the harness can run exactly this
                    piece against a tree that is red by design"
            (is (every? #(= "test/example/core_test.clj" (:tests %)) rows)))
          (testing "the delegation is also recorded addressably, so the ship
                    gate can ask whether the piece was actually built"
            (is (every? #(= "src/example/core.clj" (:stub_file %)) rows))
            (is (= #{"parse-line" "render-report"} (set (map :stubs rows))))))))))

(deftest a-split-naming-a-stub-that-is-not-there-is-declined
  (with-project
    (fn [p]
      (let [r (call p {:reason "two things"
                       :parts [(assoc (first good-parts) "stubs" ["parse-line" "absent-fn"])
                               (second good-parts)]})]
        (is (= :mechanics (:category r)) "declined, and not charged as a failure")
        (is (:edit-rejected? r)
            "a rejected edit, the same shape as a manifest that does not compile")
        (is (str/includes? (:result r) "absent-fn") "it names what is missing")
        (is (not (str/includes? (:result r) "render-report"))
            "and does not complain about the piece that was fine")))))

(deftest a-split-handing-over-work-already-done-is-declined
  ;; `run` is implemented — it is the parent's own composition. Delegating it
  ;; would hand a child a contract that is already met, and the child would
  ;; ship without doing anything.
  (with-project
    (fn [p]
      (let [r (call p {:reason "three things"
                       :parts (conj good-parts
                                    {"name" "run" "description" "compose them"
                                     "file" "src/example/core.clj" "stubs" ["run"]
                                     "tests" "test/example/core_test.clj"})})]
        (is (= :mechanics (:category r)))
        (is (str/includes? (:result r) "run"))))))

(deftest two-pieces-may-not-own-the-same-stub
  (with-project
    (fn [p]
      (let [r (call p {:reason "overlap"
                       :parts [(first good-parts)
                               (assoc (second good-parts) "stubs" ["parse-line"])]})]
        (is (= :mechanics (:category r)))
        (is (str/includes? (:result r) "parse-line"))))))

(deftest a-split-into-one-piece-is-not-a-split
  (with-project
    (fn [p]
      (let [r (call p {:reason "just the one" :parts [(first good-parts)]})]
        (is (= :mechanics (:category r)))))))

(deftest a-piece-must-name-tests-that-exist
  (with-project
    (fn [p]
      (let [r (call p {:reason "two things"
                       :parts [(assoc (first good-parts) "tests" "test/example/nope_test.clj")
                               (second good-parts)]})]
        (is (= :mechanics (:category r)))
        (is (str/includes? (:result r) "nope_test.clj"))))))

(deftest nothing-is-written-when-any-piece-fails-verification
  ;; All or nothing: a half-created split leaves rows nobody owns and a parent
  ;; that thinks it delegated less than it did.
  (with-project
    (fn [{:keys [conn run-id] :as p}]
      (call p {:reason "two things"
               :parts [(first good-parts)
                       (assoc (second good-parts) "stubs" ["absent-fn"])]})
      (is (empty? (tasks/board conn {:run-id run-id}))
          "no parent row and no child rows"))))

(deftest the-pieces-hang-off-the-task-the-agent-holds
  ;; An agent splits the task it is working, so that task is the pieces'
  ;; parent. Without this the split is reported out of band and the tree is
  ;; not walkable from the work the agent was actually given.
  (with-project
    (fn [{:keys [conn run-id root]}]
      (let [held (tasks/create! conn {:title "build the report" :run-id run-id})]
        (is (some? (tasks/claim! conn held run-id "B1")))
        (base/run-tool {:branch {:id "B1"} :conn conn :run-id run-id :root root
                        :tool-name "split"
                        :args {:reason "two things" :parts good-parts}})
        (let [kids (tasks/children-of conn held)]
          (is (= 2 (count kids)) "the pieces are children of the held task")
          (is (= #{"parse-line" "render-report"} (set (map :title kids)))))))))

(deftest a-split-parks-the-branch-and-blocks-the-row-it-was-working
  ;; karamazov-ioo.15.4. The split IS the branch's last act. It had nothing
  ;; left to do until its pieces came back, and the worst thing it could do
  ;; with the turns it kept was implement the very stubs it had handed down —
  ;; after which each child's ship gate found its contract already met and
  ;; shipped having done nothing. `verify` refuses a part whose stub is
  ;; already filled for that reason; the same state was one turn away and
  ;; unchecked.
  (with-project
    (fn [{:keys [conn run-id root]}]
      (let [held (tasks/create! conn {:title "build the report" :run-id run-id})
            _ (tasks/claim! conn held run-id "B1")
            r (base/run-tool {:branch {:id "B1" :status :active}
                              :conn conn :run-id run-id :root root
                              :tool-name "split"
                              :args {:reason "two things" :parts good-parts}})
            row (tasks/get-task conn held)]
        (is (= :neutral (:category r)) (str "accepted: " (:result r)))
        (testing "the branch parks"
          (is (= :parked (get-in r [:branch :status])))
          (is (= 2 (count (get-in r [:branch :delegated])))
              "carrying the ids it is waiting on"))
        (testing "and the row it holds is blocked rather than released"
          (is (= "blocked" (:status row)))
          (is (= "B1" (:branch_id row))
              "still named, so nothing hands the task to somebody else while
               the work it asked for is being built"))))))

(deftest a-refused-split-parks-nothing
  (with-project
    (fn [{:keys [conn run-id root]}]
      (let [held (tasks/create! conn {:title "build the report" :run-id run-id})
            _ (tasks/claim! conn held run-id "B1")
            r (base/run-tool {:branch {:id "B1" :status :active}
                              :conn conn :run-id run-id :root root
                              :tool-name "split"
                              :args {:reason "one thing"
                                     :parts [(first good-parts)]}})]
        (is (= :active (get-in r [:branch :status]))
            "a branch whose split was declined is still working")
        (is (= "in_progress" (:status (tasks/get-task conn held))))))))
