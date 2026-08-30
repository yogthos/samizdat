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

(ns samizdat.retire-test
  "A memory that turned out to be wrong is EVIDENCE, not litter.

  `restate!` used to `UPDATE knowledge SET content = ?` and delete the old FTS
  row — the thing karamazov-1sy's own design forbids. You cannot retract what
  you overwrote: once the previous wording is gone there is no way to ask what
  we believed, what made us believe it, or what was concluded from it.

  That is the failure mode the lemmalog write-up is about, and samizdat has it
  twice on record. The 238-turn run re-read a correct implementation hunting a
  defect that was in its own tests — every re-read CONFIRMED the code was fine,
  which is exactly why it read again. karamazov-60c spent ten turns reading a
  file because the run could not tell a stale session from a broken one.

  A LINEAGE WITH A CURRENT FLAG, not userspace's MAX(version): max-version
  cannot express `retracted with no replacement`, and that is what a retraction
  IS. And every row carries its CAUSE, because the model can only act on what
  it knows about — `this is false` is a dead end, `this is false and here is
  what made us believe it` is a lead."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.store.db :as db]
            [samizdat.store.knowledge :as k]))

(defn- store []
  (let [conn (db/connect ":memory:")]
    (db/migrate! conn)
    conn))

;; --- the cause ---------------------------------------------------------------

(deftest a-memory-records-what-made-us-believe-it
  (let [conn (store)
        id (k/remember! conn {:content "the adapter drops chunked bodies"
                              :cause "run f2014821 turn 12: a chunked POST arrived empty"})]
    (is (= "run f2014821 turn 12: a chunked POST arrived empty"
           (:cause (k/get-by-id conn id))))))

(deftest a-memory-with-no-stated-cause-is-still-a-memory
  ;; Most writes have nothing interesting to say about their origin, and
  ;; inventing text would make the history lie — userspace/rationale's rule.
  (let [conn (store)
        id (k/remember! conn {:content "prefer edit_file over write_file"})]
    (is (some? (k/get-by-id conn id)))
    (is (nil? (:cause (k/get-by-id conn id))))))

;; --- restating keeps the old version -----------------------------------------

(deftest restating-retires-the-old-row-rather-than-overwriting-it
  (let [conn (store)
        id (k/remember! conn {:content "the store is sqlite"
                              :cause "read deps.edn"})
        new-id (k/restate! conn id "the store is dolt" {:reason "migrated in karamazov-ioo.17"})]
    (testing "a NEW row carries the new wording"
      (is (some? new-id))
      (is (not= id new-id))
      (is (= "the store is dolt" (:content (k/get-by-id conn new-id)))))
    (testing "and the old one is still readable, with its original wording"
      (let [old (k/get-by-id conn id)]
        (is (some? old) "the old row was destroyed — nothing can be reconsidered")
        (is (= "the store is sqlite" (:content old)))
        (is (= "read deps.edn" (:cause old))
            "the reason we believed the old thing went with it")))
    (testing "exactly one of them is current"
      (is (= 0 (:current (k/get-by-id conn id))))
      (is (= 1 (:current (k/get-by-id conn new-id)))))
    (testing "and the new one says what it replaced, and why"
      (is (= id (:supersedes (k/get-by-id conn new-id))))
      (is (str/includes? (str (:retired_reason (k/get-by-id conn id)))
                         "karamazov-ioo.17")))))

(deftest a-lineage-reads-newest-first-and-holds-every-version
  (let [conn (store)
        a (k/remember! conn {:content "v1"})
        b (k/restate! conn a "v2" {})
        c (k/restate! conn b "v3" {})
        h (k/history conn c)]
    (is (= ["v3" "v2" "v1"] (mapv :content h)))
    (is (= 1 (count (filter #(= 1 (:current %)) h)))
        "a lineage has exactly one current version")))

;; --- retracting with no replacement ------------------------------------------

(deftest a-retracted-memory-leaves-a-lineage-with-no-current-version
  ;; THE CASE MAX(version) CANNOT EXPRESS. "This was wrong and nothing replaces
  ;; it" is a different fact from "this was replaced", and it is the one a
  ;; disproven premise produces.
  (let [conn (store)
        id (k/remember! conn {:content "the bug is in src/fps"
                              :cause "three reads of the same file"})]
    (k/retire! conn id {:reason "the defect was in the tests, not the implementation"})
    (let [row (k/get-by-id conn id)]
      (is (some? row) "a retracted memory is evidence, not litter")
      (is (= 0 (:current row)))
      (is (some? (:retired_at row)))
      (is (str/includes? (str (:retired_reason row)) "tests")))
    (testing "and nothing in the lineage is current"
      (is (empty? (filter #(= 1 (:current %)) (k/history conn id)))))))

(deftest retiring-twice-is-not-an-error
  (let [conn (store)
        id (k/remember! conn {:content "x"})]
    (k/retire! conn id {:reason "first"})
    (is (nil? (k/retire! conn id {:reason "second"})))
    (is (str/includes? (str (:retired_reason (k/get-by-id conn id))) "first")
        "the second retirement overwrote the first reason")))

;; --- retired rows leave the working set --------------------------------------

(deftest recall-does-not-return-retired-memories
  ;; The whole point: a disproven belief must stop being handed back as
  ;; something to act on, while staying available to anything asking what we
  ;; used to think.
  (let [conn (store)
        id (k/remember! conn {:content "zzunique the bug is in the parser"})]
    (is (seq (k/recall conn "zzunique")) "sanity: it is findable while current")
    (k/retire! conn id {:reason "disproven"})
    (is (empty? (filter #(= id (:id %)) (k/recall conn "zzunique")))
        "a retracted memory was still being recalled as live knowledge")))

(deftest a-restated-memory-is-recalled-by-its-new-wording-only
  (let [conn (store)
        id (k/remember! conn {:content "zzalpha the store is sqlite"})
        new-id (k/restate! conn id "zzalpha the store is dolt" {})]
    (let [hits (map :id (k/recall conn "zzalpha"))]
      (is (some #{new-id} hits))
      (is (not (some #{id} hits))
          "the superseded wording is still searchable as if it were live"))))

;; --- recall says which kind of nothing ---------------------------------------

(deftest recall-distinguishes-an-empty-store-from-a-miss
  ;; Lemmalog's measured failure: extraction gaps cause SILENCE, and silence is
  ;; indistinguishable from absence. The two call for different actions — write
  ;; it down, or search again — so they must not read the same.
  (let [conn (store)]
    (is (= :empty (k/recall-status conn "anything"))
        "an empty store should say so rather than look like a bad query"))
  (let [conn (store)]
    (k/remember! conn {:content "something unrelated"})
    (is (= :no-match (k/recall-status conn "zzznothingmatchesthis")))
    (is (= :hits (k/recall-status conn "unrelated")))))

(deftest a-store-holding-only-retired-memories-is-empty-for-recall
  ;; It is not a miss: there is nothing live to match. Telling the model to
  ;; refine its query would be a lie.
  (let [conn (store)
        id (k/remember! conn {:content "only thing here"})]
    (k/retire! conn id {:reason "wrong"})
    (is (= :empty (k/recall-status conn "only thing here")))))

;; --- a key names a subject, not a row ----------------------------------------

(deftest by-pattern-follows-the-lineage-to-the-current-version
  ;; A pattern key names a SUBJECT, and a subject now has versions. `by-pattern`
  ;; was `LIMIT 1` with no filter, which was fine while restating rewrote the
  ;; row and became a coin flip the moment it started retiring the old one.
  ;; Landing on the retired row would send every later corroboration to a
  ;; memory nobody can recall, and freeze the live one silently.
  (let [conn (store)
        id (k/remember! conn {:content "the project is a harness"
                              :pattern-key "project:overview"})
        new-id (k/restate! conn id "the project is a harness and a sandbox" {})]
    (is (= new-id (:id (k/by-pattern conn "project:overview"))))
    (is (= 1 (:current (k/by-pattern conn "project:overview"))))))

(deftest a-fully-retired-subject-has-no-current-row-to-find
  (let [conn (store)
        id (k/remember! conn {:content "x" :pattern-key "lever:beam-width"})]
    (k/retire! conn id {:reason "the lever was removed"})
    (is (nil? (k/by-pattern conn "lever:beam-width"))
        "a retired subject was still answering as if it were live")))
