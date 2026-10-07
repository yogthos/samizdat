;; Ported from sdiff, https://github.com/semantic-namespace/diff (commit
;; 70e436e), MIT License, Copyright (c) 2026 @tangrammer.
;;
;; SPDX-License-Identifier: MIT

(ns samizdat.sdiff.core-test
  "Fixtures are the changed Clojure files of three merged semantic-namespace/atlas
  pull requests, taken at the PR's merge base (old) and head (new); see
  test/fixtures/atlas.refs. Each test states what a reviewer should learn.
  Behaviours no atlas PR exercises yet (extraction modulo renamed locals, the
  signature-only tag) are covered by small inline sources."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [rewrite-clj.node :as n]
            [samizdat.sdiff.core :as c]
            [samizdat.sdiff.edn :as e]))

(defn- fixture-dir [pr] (str "test/fixtures/sdiff/atlas-" pr "/"))

(defn fixture [pr path]
  (let [d (fixture-dir pr)
        side (fn [s] (let [f (io/file (str d s "/" path))] (if (.exists f) (slurp f) "")))]
    (c/file-report path (side "old") (side "new"))))

(defn pr-fixture
  "Every file of a fixture PR, rolled up the way `samizdat.sdiff.git/report` does."
  [pr]
  (let [root (io/file (fixture-dir pr) "new")
        paths (for [f (file-seq root) :when (.isFile f)]
                (str/replace (subs (.getPath f) (inc (count (.getPath root)))) "\\" "/"))]
    (c/rollup-renames (mapv (partial fixture pr) (sort paths)))))

(defn form [report id] (some #(when (= id (:id %)) %) (:forms report)))
(defn file [{:keys [files]} path] (some #(when (= path (:path %)) %) files))
(defn ops [f] (mapv (juxt :op :path) (:changes f)))
(defn src [node] (some-> node n/string))

(deftest atlas-7-extraction-with-drift
  (let [r (fixture "7" "core/src/atlas/registry.cljc")
        caller (form r ["defn" "compile!"])
        site (first (filter :extracted (:changes caller)))
        ex (:extraction (form r ["defn" "dev-id-conflicts"]))]
    (testing "the caller now calls the new function in the same binding"
      (is (= [["arity" 1] ["let 2"] ["binding" "dev-id-conflicts"]] (:path site)))
      (is (= "dev-id-conflicts" (:extracted site)))
      (is (= "(dev-id-conflicts entries)" (src (:new site)))))
    (testing "the new function is reported as extracted from that binding"
      (is (= ["defn" "compile!"] (:from ex)))
      (is (= [["arity" 1] ["let 2"] ["binding" "dev-id-conflicts"]] (:from-path ex)))
      (is (empty? (:renamed ex))))
    (testing "drift: the three behaviour changes hidden inside the move"
      (let [drift (into {} (for [d (:drift ex)] [(last (:path d)) [(:op d) (src (:old d)) (src (:new d))]]))]
        (is (= 3 (count (:drift ex))))
        (is (= [:replaced "(next cids)"] (take 2 (drift ["test"]))) "the conflict test changed")
        (is (= [:replaced "(last cids)"] (take 2 (drift ["key" ":winner"]))) "the winner changed")
        (is (= [:added nil "(sort-by str)"] (drift ["sort-by 4"])) "results are now sorted")))
    (is (some? (form r ["defn-" "comparable-identities?"])) "the helper is a new form")))

(deftest atlas-7-other-files
  (let [inv (fixture "7" "core/src/atlas/invariant.cljc")
        [chg] (:changes (form inv ["def" "core-invariants"]))]
    (is (= :added (:op chg)))
    (is (= "invariant-dev-id-is-unique" (src (:new chg))))
    (is (= [[:added-form nil]] (ops (form inv ["defn" "invariant-dev-id-is-unique"])))))
  (let [t (fixture "7" "test/atlas/registry_test.clj")
        [chg] (:changes (form t ["ns" "atlas.registry-test"]))]
    (is (= "[atlas.invariant :as inv]" (src (:new chg))))))

(deftest atlas-4-move-into-a-binding
  (let [r (fixture "4" "core/src/atlas/tooling/lsp_helpers.cljc")
        f (form r ["defn" "find-definition"])
        by-path (into {} (for [c (:changes f)] [(last (:path c)) c]))
        out (by-path ["body"])
        in (by-path ["binding" "[neg-score _ best]"])]
    (is (= :comments (:op (by-path ["docstring"]))) "docstring change is not semantic")
    (is (= [["binding" "[neg-score _ best]"]] (take-last 1 (:moved-to out))))
    (is (= [["body"]] (take-last 1 (:moved-from in))))
    (is (= ["(map-indexed (fn [i c] [(- (score c)) i c]))"] (mapv (comp src first) (:kept in)))
        "the ranking pipeline is what moved")))

(deftest atlas-4-reader-conditional-dropped
  (let [r (fixture "4" "core/src/atlas/tooling/lsp_helpers.cljc")
        f (form r ["defn-" "registration-line?"])
        by-op (group-by :op (:changes f))]
    (is (= :reader-macro (n/tag (:old (first (:wrapper by-op))))) "the #? wrapper is gone")
    (is (= "or" (c/head (:old (first (:reshaped by-op))))))
    (is (= "let" (c/head (:new (first (:reshaped by-op))))))
    (is (= ["(= content keyword-str)"] (mapv (comp src first) (:kept (first (:reshaped by-op))))))))

(deftest atlas-2-renames-roll-up-across-files
  (let [{:keys [renames] :as r} (pr-fixture "2")]
    (is (= #{{:from ":atlas/schema" :to ":atlas/data-schema" :count 5}
             {:from "rt/all-with-aspect" :to "dev-ids-of-type" :count 3}}
           (set renames)))
    (testing "a file whose only changes are renames gets its own verdict"
      (is (= :rename-only (:verdict (file r "core/src/atlas/ide.cljc"))))
      (is (= :semantic (:verdict (file r "core/src/atlas/docs.cljc")))))
    (testing "an operator renamed at a call site is a head change"
      (let [f (form (file r "core/src/atlas/docs.cljc") ["defn" "system-overview"])
            heads (filter #(= ["head"] (last (:path %))) (:changes f))]
        (is (= 3 (count heads)))
        (is (every? #(= ["rt/all-with-aspect" "dev-ids-of-type"] (:rename %)) heads))))
    (testing "a site where operator and argument both changed is not a rename"
      (let [f (form (file r "core/src/atlas/docs.cljc") ["defn" "system-overview"])
            c (some #(when (= ["binding" "schemas"] (last (:path %))) %) (:changes f))]
        (is (= :replaced (:op c)))
        (is (nil? (:rename c)))))))

(deftest extraction-modulo-renamed-locals
  (let [old "(defn f [xs]\n  (let [n (count xs)]\n    (if (< 1 n) :many (let [x (first xs)] (cond (string? x) :string (keyword? x) :keyword :else :any)))))\n"
        new "(defn kind [v] (cond (string? v) :string (keyword? v) :keyword :else :any))\n(defn f [xs]\n  (let [n (count xs)]\n    (if (< 1 n) :many (kind (first xs)))))\n"
        r (c/file-report "x.clj" old new)
        ex (:extraction (form r ["defn" "kind"]))]
    (is (= "kind" (:fn ex)))
    (is (= {"x" "v"} (:renamed ex)))
    (is (= [:dropped] (mapv :op (:drift ex))) "the let around the cond was not carried over")))

(deftest signature-only-tag
  (let [r (c/file-report "s.clj" "(defmethod accept :and [_ _ _ _] :any)" "(defmethod accept :and [_ _ children _] :any)")
        f (form r ["defmethod" "accept" ":and"])]
    (is (= [[:replaced [["args"] ["#3"]]]] (ops f)))
    (is (= "signature only: parameters changed, body did not" (:note f)))
    (is (empty? (:renames (c/rollup-renames [r r]))) "`_ → children` is a parameter coming into use, not a rename")))

(deftest verdicts-for-non-semantic-changes
  (is (= :whitespace-only (:verdict (c/file-report "w.clj" "(defn a [x]\n  (inc x))" "(defn a [x] (inc x))"))))
  (is (= :comments-only (:verdict (c/file-report "c.clj" "(defn a [x] (inc x))" "(defn a \"adds one\" [x] (inc x))"))))
  (is (= :semantic (:verdict (c/file-report "u.clj" "(defn a [x] (inc x))" "(defn a [x] #_(inc x))"))) "uneval is semantic"))

(deftest canon-handles-a-repeated-local
  (let [[st order] (c/canon (first (c/forms "(+ a a b)")) #{"a" "b"})]
    (is (= [:list "+" [:local 0] [:local 0] [:local 1]] st))
    (is (= ["a" "b"] order))))

(deftest edn-report-is-plain-data-with-positions
  (let [r (fixture "7" "core/src/atlas/registry.cljc")
        f (e/form->edn r (form r ["defn" "compile!"]))
        rt (edn/read-string (pr-str f))]
    (is (= (:id f) (:id rt)) "round-trips through the reader")
    (is (str/starts-with? (get-in f [:new :src]) "(defn compile!"))
    (is (every? pos-int? (map #(get-in f [:new %]) [:row :end-row])) "the head-side form carries its line range")
    (is (some #(= "(dev-id-conflicts entries)" (get-in % [:new :src])) (:changes f)))))

(deftest pairing-a-form-made-public
  (let [r (c/file-report "p.clj" "(defn- f [x] (inc x))" "(defn f [x] (inc x))")
        [f & more] (:forms r)]
    (is (nil? more) "one entry, not a removal and an addition")
    (is (= ["defn" "f"] (:id f)))
    (is (= ["defn-" "f"] (:was f)))
    (is (= [{:op :visibility :path [] :from "private" :to "public"}] (:changes f)))))

(deftest pairing-a-form-wrapped-by-a-new-macro
  (let [old "(data/def :a/key {:type :user :pseudo (fn [x] (encrypt x))})"
        new "(bind :data.a/key #{:domain/a} (data/def :a/key {:type :user :pseudo (fn [x] (encrypt x))}))"
        [f] (:forms (c/file-report "w.clj" old new))]
    (is (= ["bind" ":data.a/key"] (:id f)))
    (is (= ["data/def" ":a/key"] (:was f)))
    (is (= [:wrapped] (mapv :op (:changes f))))))

(deftest pairing-a-form-renamed-and-rewritten
  (let [old "(register! :fn/old {:context [:a :b] :response [:c] :impl (fn [{:keys [a b]}] (+ a b))})"
        new "(register! :fn/new {:context [:a :b] :response [:c :d] :impl (fn [{:keys [a b]}] (+ a b))})"
        [f & more] (:forms (c/file-report "r.clj" old new))]
    (is (nil? more))
    (is (= ["register!" ":fn/old"] (:was f)))
    (is (some #(= [["name"]] (:path %)) (:changes f)) "the name change is its own row")))

(deftest unrelated-forms-stay-apart
  (let [r (c/file-report "u.clj" "(defn a [x] (str \"a\" x))" "(defn b [m] (reduce-kv assoc {} m))")]
    (is (= #{[:removed-form] [:added-form]} (set (map #(mapv :op (:changes %)) (:forms r)))))))

(deftest a-single-arity-gaining-an-arity
  (let [old "(defn f [{:keys [a b]}] (concat (map str a) (map str b)))"
        new "(defn f ([m] (f nil m)) ([cap {:keys [a b]}] (let [b (if cap (take cap b) b)] (concat (map str a) (map str b)))))"
        f (first (:forms (c/file-report "a.clj" old new)))
        paths (set (map :path (:changes f)))]
    (is (contains? paths [["arity" 1]]) "the delegating arity is the new piece")
    (is (some #(= ["arity" 2] (first %)) paths) "the old body is diffed inside the 2-arity")
    (is (not-any? #(= [["body"]] %) paths) "no stray removal of the old body")))

(deftest a-moved-form-is-a-semantic-change
  (let [r (c/file-report "o.clj" "(defn a [] 1)\n(defn b [] (a))\n(defn c [] 2)\n" "(defn b [] (a))\n(defn a [] 1)\n(defn c [] 2)\n")]
    (is (= :semantic (:verdict r)))
    (is (= [["defn" "a"]] (:moved r)) "the smallest set of forms whose position changed")
    (is (empty? (:forms r)) "no form's code changed"))
  (is (empty? (:moved (c/file-report "s.clj" "(defn a [] 1)\n(defn b [] 2)\n" "(defn a [] 1)\n(defn b [] 3)\n")))))

(deftest an-edited-map-in-a-vector-is-a-change-inside-it
  (let [old "(def cases [{:label \"a\" :from [\"x\"]} {:label \"b\" :from [\"y\"]}])"
        new "(def cases [{:label \"a\" :from [\"emailtok\"]} {:label \"b\" :from [\"y\"]}])"
        [f] (:forms (c/file-report "v.clj" old new))
        [chg] (:changes f)]
    (is (= 1 (count (:changes f))))
    (is (= :replaced (:op chg)))
    (is (= ["key" ":from"] (last (:path chg))) "the change is named by its place inside the element")))
