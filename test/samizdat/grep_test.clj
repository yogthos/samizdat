(ns samizdat.grep-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [samizdat.agent.files :as files]))

(def ^:private fixture-root
  (doto (io/file (System/getProperty "java.io.tmpdir")
                 (str "grep-test-" (System/nanoTime)))
    (.mkdirs)))

(.mkdirs (io/file fixture-root "sub"))

(spit (io/file fixture-root "a.clj")
      "(ns demo)\n\n(defn hello []\n  :hello)\n\n(defn bye []\n  :bye)\n")

(spit (io/file fixture-root "notes.txt")
      "(defn hidden [] :nope)\n")

(spit (io/file fixture-root "sub" "b.clj")
      "(defn nested []\n  :nested)\n")

(deftest finds-matching-lines-with-line-numbers
  (let [hits (files/grep-project (str fixture-root) "\\(defn")]
    (is (= 3 (count hits)))
    (is (= #{{:path "a.clj" :line 3 :text "(defn hello []"}
            {:path "a.clj" :line 6 :text "(defn bye []"}
            {:path "sub/b.clj" :line 1 :text "(defn nested []"}}
           (set hits)))))

(deftest ignores-non-clojure-extensions
  (is (empty? (files/grep-project (str fixture-root) "hidden"))))

(deftest no-match-is-empty
  (is (empty? (files/grep-project (str fixture-root) "zzz-no-such-thing"))))

(deftest paths-are-relative-to-root
  (let [hits (files/grep-project (str fixture-root) "defn nested")]
    (is (= ["sub/b.clj"] (mapv :path hits)))
    (is (= ["(defn nested []"] (mapv :text hits)))))

;; --- karamazov-2py -----------------------------------------------------------
;; grep took the first 200 hits and said nothing about the rest, with no offset
;; argument to continue from — so a 201-hit search was unresumable by
;; construction, and 200-truncated was indistinguishable from 200-total. This
;; is the rule read_file already learned: "A truncation marker has to end with
;; the call that continues it, or it is a dead end the model can only walk into
;; again."

(deftest a-scope-narrows-the-search
  (testing "one path prefix"
    (is (= ["sub/b.clj"]
           (distinct (mapv :path (files/grep-project (str fixture-root) "\\(defn"
                                                     {:paths "sub"}))))))
  (testing "several path prefixes"
    (is (= #{"a.clj" "sub/b.clj"}
           (set (mapv :path (files/grep-project (str fixture-root) "\\(defn"
                                                {:paths ["a.clj" "sub"]}))))))
  (testing "a scope matching nothing finds nothing, and does not throw"
    (is (empty? (files/grep-project (str fixture-root) "\\(defn" {:paths "nope"}))))
  (testing "no scope is every file, as before"
    (is (= 3 (count (files/grep-project (str fixture-root) "\\(defn" {}))))))

(deftest a-truncated-search-says-so-and-says-how-to-continue
  (let [hits (mapv (fn [i] {:path "a.clj" :line i :text (str "hit " i)}) (range 1 11))]
    (testing "within the cap: everything, no continuation"
      (let [p (files/grep-page hits 0 25)]
        (is (= 10 (count (:hits p))))
        (is (= 10 (:total p)))
        (is (nil? (:next p)))))
    (testing "over the cap: the total is reported and `next` continues"
      (let [p (files/grep-page hits 0 4)]
        (is (= 4 (count (:hits p))))
        (is (= 10 (:total p)) "the count of ALL hits, not of this page")
        (is (= 4 (:next p)))))
    (testing "the continuation resumes exactly where the page stopped"
      (let [p (files/grep-page hits 4 4)]
        (is (= [5 6 7 8] (mapv :line (:hits p))))
        (is (= 8 (:next p)))))
    (testing "the last page has no continuation"
      (let [p (files/grep-page hits 8 4)]
        (is (= [9 10] (mapv :line (:hits p))))
        (is (nil? (:next p)))))))
