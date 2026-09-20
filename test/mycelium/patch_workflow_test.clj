(ns mycelium.patch-workflow-test
  "patch ops on the WORKFLOW dialect — the manifest form samizdat writes.

  mycelium has two manifest dialects. The self-describing one carries
  {:id :doc :schema :on-error} per cell and validates through
  mycelium.manifest/validate-manifest; patch_test.clj covers it as upstream
  does. The workflow one names cells by bare handler keyword (or {:id
  :params}) and takes its schema from the registry, compiling through
  mycelium.workflow — every samizdat manifest is written that way, and its
  validator is samizdat.manifests/compile-loop, not mycelium's. The ops are
  data edits either way; what differs is the validator seam, the shape an
  added cell takes, and the sections that reference cells. Those three are
  what this namespace pins."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [mycelium.patch :as patch]))

(def workflow
  "A workflow-dialect manifest with the samizdat sections that name cells:
   bare-keyword cells and :invariants alongside :constraints."
  {:description "t"
   :input-schema [:map]
   :cells {:start   :t/parse
           :process :t/process
           :journal :t/journal
           :err     {:id :t/err :params {:retries 2}}}
   :edges {:start   {:success :process, :failure :err}
           :process :journal
           :journal :end
           :err     :end}
   :dispatches {:start [[:success {:y true}]
                        [:failure '_]]}
   :constraints [{:type :must-follow :if :process :then :journal}]
   :invariants [{:type :must-follow :if :process :then :journal :enforced true
                 :protects "a processed turn is recorded"}
                {:type :must-precede :cell :start :before :journal :enforced true
                 :protects "nothing is recorded that was not parsed"}
                {:type :never-together :cells [:journal :err]
                 :unenforced-because "no checker"}]})

(defn- accept
  "A validator that records what it saw and accepts everything."
  [seen]
  (fn [m] (swap! seen conj m) m))

;; ===== the validator seam =====

(deftest a-caller-supplied-validator-replaces-myceliums
  ;; mycelium.manifest/validate-manifest rejects a bare-keyword cell ("missing
  ;; :id"), so without the seam no samizdat manifest could be patched at all.
  (let [seen   (atom [])
        result (patch/apply-ops workflow {:ops [{:op "rename-cell" :from :process :to :transform}]
                                          :validator (accept seen)})]
    (is (contains? (:cells result) :transform))
    (testing "the input is checked before any edit and the result after all of them"
      (is (= 2 (count @seen)))
      (is (= workflow (first @seen)))
      (is (= result (second @seen)))))
  (testing "a refusal from the validator is the refusal the caller sees"
    (is (thrown-with-msg? Exception #"no such handler"
          (patch/apply-ops workflow {:ops [{:op "rename-cell" :from :process :to :transform}]
                                     :validator (fn [m]
                                                  (if (contains? (:cells m) :transform)
                                                    (throw (ex-info "no such handler" {}))
                                                    m))})))))

;; ===== :invariants are reference sites =====

(deftest rename-rewrites-invariants-like-constraints
  ;; samizdat derives :constraints from the :enforced invariants at compile
  ;; time (manifests/enforced-constraints), so an invariant left naming the
  ;; old cell would fail the compile the rename is meant to survive.
  (let [result (patch/apply-ops workflow {:ops [{:op "rename-cell" :from :journal :to :record}]
                                          :validator identity})]
    (is (= [{:type :must-follow :if :process :then :record}] (:constraints result)))
    (is (= :record (get-in result [:invariants 0 :then])))
    (is (= :record (get-in result [:invariants 1 :before])))
    (is (= [:record :err] (get-in result [:invariants 2 :cells])))
    (testing "the prose on each invariant is untouched"
      (is (= "a processed turn is recorded" (get-in result [:invariants 0 :protects])))
      (is (= "no checker" (get-in result [:invariants 2 :unenforced-because]))))))

(deftest cell-refs-lists-invariants
  (let [refs (patch/cell-refs workflow :journal)
        roles (frequencies (map :role refs))]
    (is (= 1 (:constraint roles)))
    (is (= 3 (:invariant roles)))
    (is (= #{[:invariants 0 :then] [:invariants 1 :before] [:invariants 2 :cells]}
           (set (map :path (filter #(= :invariant (:role %)) refs)))))))

(deftest remove-cell-is-blocked-by-an-invariant
  ;; Like a constraint: an invariant is a claim about the graph, and nothing
  ;; can guess what the claim means once its cell is gone.
  (let [e (try (patch/apply-ops workflow {:ops [{:op "remove-cell" :name :journal :rewire :end}]
                                          :validator identity})
               (catch Exception e e))]
    (is (instance? Exception e))
    (is (re-find #"still referenced" (ex-message e)))
    (is (re-find #"invariant :invariants 0 :then" (ex-message e)))))

;; ===== bare-keyword cells =====

(deftest add-cell-with-only-a-handler-is-a-bare-keyword
  ;; The registry holds the doc and schema; a workflow manifest names the
  ;; handler and nothing else, and an added cell should read like the ones
  ;; around it.
  (let [result (patch/apply-ops workflow {:ops [{:op "add-cell" :name :audit :id :t/audit :after :journal}]
                                          :validator identity})]
    (is (= :t/audit (get-in result [:cells :audit])))
    (is (= :audit (get-in result [:edges :journal])))
    (is (= :end (get-in result [:edges :audit])))))

(deftest add-cell-with-params-is-an-id-map
  (let [result (patch/apply-ops workflow {:ops [{:op "add-cell" :name :audit :id :t/audit
                                                 :params {:depth 3} :edges :end}]
                                          :validator identity})]
    (is (= {:id :t/audit :params {:depth 3}} (get-in result [:cells :audit])))))

(deftest add-cell-with-a-doc-is-the-self-describing-form
  ;; The other dialect is still reachable from the same op: asking for a doc
  ;; or a schema is asking for an inline definition.
  (let [result (patch/apply-ops workflow {:ops [{:op "add-cell" :name :audit :id :t/audit :doc "audit"
                                                 :edges :end}]
                                          :validator identity})]
    (is (= {:id :t/audit :doc "audit" :schema {:input [:map] :output [:map]} :on-error nil}
           (get-in result [:cells :audit])))))

(deftest set-cell-field-on-a-bare-keyword
  (testing ":id swaps the handler and keeps the bare form"
    (let [result (patch/apply-ops workflow {:ops [{:op "set-cell-field" :name :process :field :id
                                                   :value :t/process2 :expect :t/process}]
                                            :validator identity})]
      (is (= :t/process2 (get-in result [:cells :process])))))
  (testing "any other field promotes the keyword to an :id map"
    (let [result (patch/apply-ops workflow {:ops [{:op "set-cell-field" :name :process :field :params
                                                   :value {:retries 1}}]
                                            :validator identity})]
      (is (= {:id :t/process :params {:retries 1}} (get-in result [:cells :process])))))
  (testing ":expect reads the bare keyword as its :id"
    (let [e (try (patch/apply-ops workflow {:ops [{:op "set-cell-field" :name :process :field :id
                                                   :value :t/x :expect :t/stale}]
                                            :validator identity})
                 (catch Exception e e))]
      (is (re-find #"expected :t/stale but found :t/process" (ex-message e)))))
  (testing "an :id map edits in place"
    (let [result (patch/apply-ops workflow {:ops [{:op "set-cell-field" :name :err :field :params
                                                   :value {:retries 5} :expect {:retries 2}}]
                                            :validator identity})]
      (is (= {:id :t/err :params {:retries 5}} (get-in result [:cells :err]))))))

;; ===== render on a samizdat manifest =====

(deftest render-keeps-a-shipped-manifests-prose
  ;; loop.edn is the reason render is worth vendoring: forty comment runs
  ;; explaining why each edge is where it is. A rename that reprinted the
  ;; file would erase every one of them from the stored body.
  (let [text (slurp "resources/manifests/loop.edn")
        old  (edn/read-string text)
        new  (patch/apply-ops old {:ops [{:op "rename-cell" :from :journal :to :record}]
                                   :validator identity})
        out  (patch/render text old new)]
    (is (= new (edn/read-string out)))
    (is (= (count (re-seq #";;" text)) (count (re-seq #";;" out))))
    (testing "only the lines that name the cell change"
      ;; Two of the five are inside the :invariants VECTOR. A render that
      ;; swapped the whole vector for a pretty-print would round-trip but
      ;; reflow every :protects paragraph in the file.
      (let [changed (remove (set (str/split-lines text)) (str/split-lines out))
            named   (filter #(re-find #":journal\b" %) (str/split-lines text))]
        (is (= 5 (count named)))
        (is (= (count named) (count changed)) (pr-str changed))
        (is (every? #(str/includes? % ":record") changed))))))

;; ===== diff on bare-keyword cells =====

(deftest diff-reads-a-bare-keyword-cell-as-its-id
  ;; Upstream diffs cells field by field, which calls `keys` on each side; a
  ;; bare keyword is its own :id, and a promotion to an {:id ..} map is a
  ;; change to the OTHER fields, not to the identity.
  (let [swapped  (patch/apply-ops workflow {:ops [{:op "set-cell-field" :name :process :field :id :value :t/p2}]
                                            :validator identity})
        promoted (patch/apply-ops workflow {:ops [{:op "set-cell-field" :name :process :field :params :value {:n 1}}]
                                            :validator identity})]
    (is (= {:id [:t/process :t/p2]}
           (get-in (patch/diff-manifests workflow swapped) [:cells :changed :process])))
    (is (= {:params [nil {:n 1}]}
           (get-in (patch/diff-manifests workflow promoted) [:cells :changed :process])))
    (is (true? (:same? (patch/diff-manifests workflow workflow))))))
