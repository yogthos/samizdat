;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.collab-test
  "Team workers share one working tree, on purpose: the parts of a feature
  belong in the same files. What they had no way to see was each other.

  Live, on a three-worker run: src/kit/core.clj was written fifteen times by
  three branches, full-file `write_file` overwrites interleaved with surgical
  `edit_file`s, two of them landing on the same turn. The tree came out
  coherent because the last writer happened to hold a complete picture. The
  only coordination that existed was the mailbox — which carries what a branch
  chose to ANNOUNCE, not what it did."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.files :as files]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn- turn! [conn run-id branch turn tool path & [category]]
  (journal/record-turn! conn run-id
                        {:branch-id branch :turn turn :tool-name tool
                         :args {:path path} :result "ok"
                         :category (or category :success)}))

(defn- fixture []
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "build the thing"})]
    ;; W1 reads core.clj, then two siblings change it under her.
    (turn! conn rid "W1" 1 "read_file" "src/core.clj" :neutral)
    (turn! conn rid "W2" 2 "write_file" "src/core.clj")
    (turn! conn rid "W3" 3 "edit_file" "src/core.clj")
    (turn! conn rid "W2" 4 "write_file" "test/core_test.clj")
    ;; A call that failed changes nothing and must not be reported as work.
    (turn! conn rid "W3" 5 "edit_file" "src/core.clj" :mechanics)
    [conn rid]))

(deftest a-worker-sees-which-files-its-siblings-are-in
  (let [[conn rid] (fixture)
        seen (journal/sibling-writes conn rid "W1" 8)]
    (testing "one entry per path, not one per write"
      (is (= 2 (count seen)))
      (is (= #{"src/core.clj" "test/core_test.clj"} (set (map :path seen)))))
    (testing "every collaborator is named, not just the last one"
      ;; Reporting the latest writer alone answers a different question from
      ;; the one a worker has, which is `who else is in this file`.
      (let [core (first (filter #(= "src/core.clj" (:path %)) seen))]
        (is (= #{"W2" "W3"} (set (:branches core))))
        (is (= 3 (:turn core)) "the turn it was last actually changed")))
    (testing "a branch never sees itself"
      (is (not-any? #(some #{"W1"} (:branches %)) seen)))
    (testing "a failed call changed nothing and is not reported as work"
      ;; W3's t5 edit was :mechanics. If it counted, core.clj's last-changed
      ;; turn would read 5.
      (is (= 3 (:turn (first (filter #(= "src/core.clj" (:path %)) seen))))))
    (testing "and a solo branch has nothing to be told"
      (let [solo (db/open! ":memory:")
            srid (runs/start-run! solo {:problem "alone"})]
        (turn! solo srid "B1" 1 "write_file" "src/core.clj")
        (is (empty? (journal/sibling-writes solo srid "B1" 8)))))))

(deftest a-sibling-that-patched-a-file-is-reported-like-any-other-writer
  ;; `patch` is a file-writing tool — it is in gates.edn's :file-write
  ;; vocabulary beside write_file and edit_file — but sibling-writes named
  ;; only the other two, so anchored edits were invisible to the one
  ;; mechanism that tells workers they share a tree. Two workers in one file,
  ;; one of them patching, and neither was told.
  ;;
  ;; The vocabulary is userspace and tunable, so the query reads it rather
  ;; than restating it: a project that adds a writing tool gets this for free,
  ;; which is the failure mode that produced the gap in the first place.
  (let [conn (db/open! ":memory:")
        rid (runs/start-run! conn {:problem "anchored"})]
    (turn! conn rid "W1" 1 "read_file" "src/core.clj" :neutral)
    (turn! conn rid "W2" 2 "patch" "src/core.clj")
    (let [seen (journal/sibling-writes conn rid "W1" 8)]
      (is (= ["src/core.clj"] (mapv :path seen)))
      (is (= ["W2"] (:branches (first seen)))))
    (testing "and the staleness notice sees it too"
      (is (= "W2" (:branch (journal/changed-since-read conn rid "W1" "src/core.clj")))))))

(deftest writing-a-file-a-sibling-changed-under-you-says-so
  (let [[conn rid] (fixture)
        ctx {:conn conn :run-id rid :branch {:id "W1"} :args {:path "src/core.clj"}}]
    (testing "the most recent sibling change since this branch last read it"
      (let [c (journal/changed-since-read conn rid "W1" "src/core.clj")]
        (is (= "W3" (:branch c)))
        (is (= 3 (:turn c)))))
    (testing "the notice names who, what and when"
      (let [n (files/stale-note ctx)]
        (is (some? n))
        (is (str/includes? n "W3"))
        (is (str/includes? n "src/core.clj"))))
    (testing "a path nobody else touched is quiet"
      (is (nil? (files/stale-note (assoc ctx :args {:path "src/other.clj"})))))
    (testing "a file this branch has never read says nothing"
      ;; A branch writing a file it has not looked at is a different problem,
      ;; and this one has nothing to say about it.
      (is (nil? (journal/changed-since-read conn rid "W1" "test/core_test.clj"))))
    (testing "and once the branch has caught up, the notice stops"
      (turn! conn rid "W1" 6 "read_file" "src/core.clj" :neutral)
      (is (nil? (files/stale-note ctx))))))

(deftest the-notice-rides-the-result-and-never-blocks-the-write
  ;; Workers sharing a tree are collaborating. Which version should win is
  ;; exactly the judgement the harness does not have, so it reports and gets
  ;; out of the way.
  (let [[conn rid] (fixture)
        ctx {:conn conn :run-id rid :branch {:id "W1"} :args {:path "src/core.clj"}}
        result {:result "Wrote 100 chars to src/core.clj." :category :success}
        out (files/with-stale result ctx)]
    (is (= :success (:category out)) "still a success")
    (is (str/includes? (:result out) "Wrote 100 chars"))
    (is (str/includes? (:result out) "W3"))
    (testing "a failed write gets no notice — it changed nothing"
      (is (= "no" (:result (files/with-stale {:result "no" :category :mechanics} ctx)))))
    (testing "and a broken journal never fails a write that succeeded"
      (is (= result (files/with-stale result {:branch {:id "W1"} :args {:path "x"}}))))))
