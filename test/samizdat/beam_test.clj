;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.beam-test
  "The beam's ROUND as a manifest.

  The inner turn has been a manifest since karamazov-ioo.20; the round was a
  loop/recur in compiled code, which made the harness's own scheduling policy
  the one thing a project could not change about itself. These tests pin the
  three things that had to survive the move: the round's ORDER, its three
  endings, and the driver's ownership of the crash record and teardown — the
  two things a manifest cannot own."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [mycelium.cell :as cell]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.state :as state]
            [samizdat.cells :as cells]
            [samizdat.llm.client :as llm]
            [samizdat.store.db :as db]
            [samizdat.agent.resume :as resume]
            [samizdat.store.runs :as runs]
            [samizdat.workflow :as workflow]))

(use-fixtures :once (fn [f] (cells/load-cells!) (f)))

(defn- manifest []
  (workflow/read-definition
   (slurp (clojure.java.io/resource "manifests/beam.edn"))))

(defn- branch [id & {:as extra}]
  (merge (state/new-branch {:id id :problem "p"
                            :messages [{:role "system" :content "s"}
                                       {:role "user" :content "p"}]})
         extra))

(defn- ctx-for [conn run-id & {:as extra}]
  (merge {:conn conn :run-id run-id :max-turns 3 :config {}
          :beam-width 2 :iterating-loop? true}
         extra))

;; --- the manifest ------------------------------------------------------------

(deftest the-round-manifest-compiles
  (is (some? (workflow/compile-loop (manifest)))))

(deftest the-round-manifest-is-in-the-catalogue
  ;; A scheduler the supervisor cannot see is one it can never adapt.
  (is (contains? (set (map :name (workflow/catalog nil))) "beam")))

(deftest every-round-node-is-a-registered-cell
  (doseq [[node cell-id] (:cells (manifest))]
    (is (some? (cell/get-cell cell-id))
        (str node " -> " cell-id " is not registered"))))

(deftest the-round-is-not-mistaken-for-a-turn
  ;; workflow/iterating? decides whether the beam schedules a manifest per
  ;; branch. The SCHEDULER must never be scheduled that way — five concurrent
  ;; copies of the thing that runs the beam is not a wider beam.
  (is (not (workflow/iterating? (manifest)))
      "the round has no :llm/infer of its own, so it is not a turn"))

(deftest the-order-constraints-are-compile-time-errors
  ;; The reasons are in cells/beam.clj. What matters here is that an edit which
  ;; breaks them is REFUSED rather than quietly producing a beam that decides
  ;; a branch's fate on last round's evidence.
  (testing "scoring must precede the retention pass"
    (is (thrown? Exception
                 (workflow/compile-loop
                  (assoc-in (manifest) [:edges :score] :settle)))))
  (testing "a branch must be written down before its slot is refilled"
    (is (thrown? Exception
                 (workflow/compile-loop
                  (assoc-in (manifest) [:edges :settle] :spawn))))))

;; --- the three endings, driven end to end ------------------------------------

(defn- drive
  "Run the real scheduler manifest with the model call stubbed out. `advance`
  is (fn [branches turn] -> branches), standing in for a round of real turns."
  [conn run-id advance & {:as ctx-extra}]
  (let [branches (:branches ctx-extra [(branch "B1") (branch "B2")])
        ctx (ctx-for conn run-id (dissoc ctx-extra :branches))]
    (doseq [b branches] (runs/open-branch! conn run-id {:branch-id (:id b)}))
    (with-redefs [beam/advance-all (fn [_ctx bs turn] (advance bs turn))
                  ;; The critic is a sub-LLM call; retention treats absent
                  ;; scores as "no opinion", which is the path under test.
                  beam/ensure-scored (fn [_ctx bs _turn] bs)]
      (beam/run-rounds ctx branches 1))))

(deftest a-shipped-branch-completes-the-run
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})]
    (try
      (let [r (drive c rid (fn [bs _turn]
                             (mapv #(if (= "B1" (:id %))
                                      (assoc % :status :done :final-answer "shipped it")
                                      %)
                                   bs)))]
        (is (= :completed (:status r)))
        (is (= "shipped it" (:answer r)))
        (testing "and the run row says so"
          (is (= "completed" (:status (runs/get-run c rid))))))
      (finally (db/close c)))))

(deftest the-turn-cap-exhausts-the-run-and-reports-residuals
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})]
    (try
      ;; Never ships: the cap is what ends this.
      (let [r (drive c rid (fn [bs _turn] bs))]
        (is (= :exhausted (:status r)))
        (is (some? (:report r)) "the residual report is what a resume reads")
        (is (string? (:report-text r)))
        (testing "and the ROW says so too, rather than :failed"
          ;; A thrown error also records :failed (beam/run!'s catch), so
          ;; recording an honest end-of-budget the same way made "the harness
          ;; broke" and "the work did not finish in the turns it had" the same
          ;; row. The in-memory result said :exhausted all along; only the
          ;; durable record lied (karamazov-emw).
          (is (= "exhausted" (:status (runs/get-run c rid)))))
        (testing "it is over, so a directive against it is refused"
          (is (runs/terminal? (runs/get-run c rid))))
        (testing "and still resumable — running out of budget is the ordinary
                  reason to continue a run, not a reason to refuse to"
          (is (resume/resumable? c rid))))
      (finally (db/close c)))))

(deftest an-empty-beam-exhausts-rather-than-spinning
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})]
    (try
      (let [r (drive c rid (fn [bs _turn]
                             (mapv #(assoc % :status :culled
                                           :inactive-reason "test") bs)))]
        (is (= :exhausted (:status r))))
      (finally (db/close c)))))

(deftest the-abort-flag-stops-the-run-without-its-cooperation
  ;; Checked at the top of every round, before anything else happens: a stop
  ;; must not need the round to agree to it.
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})
        rounds (atom 0)]
    (try
      (let [abort (atom false)
            r (drive c rid
                     (fn [bs _turn] (swap! rounds inc) (reset! abort true) bs)
                     :abort abort)]
        (is (= :aborted (:status r)))
        (is (= 1 @rounds) "the round after the flag was set never ran"))
      (finally (db/close c)))))

(deftest an-abort-set-before-the-first-round-runs-nothing
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})
        rounds (atom 0)]
    (try
      (let [r (drive c rid (fn [bs _turn] (swap! rounds inc) bs)
                     :abort (atom true))]
        (is (= :aborted (:status r)))
        (is (zero? @rounds)))
      (finally (db/close c)))))

;; --- what the driver still owns ----------------------------------------------

(deftest a-crash-mid-round-is-recorded-before-it-is-rethrown
  ;; gen-11 threw here and the exception reached a tty and nowhere else; the
  ;; row stayed 'running' for the nine hours the run had been dead.
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})]
    (try
      (is (thrown-with-msg?
           Exception #"boom"
           (drive c rid (fn [_bs _turn] (throw (ex-info "boom" {}))))))
      (is (= "failed" (:status (runs/get-run c rid)))
          "a crash that leaves no trace is indistinguishable from a slow round")
      (finally (db/close c)))))

(deftest teardown-sees-the-branches-as-they-stood-when-the-round-died
  ;; A thrown manifest hands nothing back, so the driver keeps its own window.
  (let [c (db/open! ":memory:")
        rid (runs/start-run! c {:problem "p"})
        disposed (atom [])]
      (try
        (with-redefs [beam/advance-all
                      (fn [_ctx bs _turn]
                        (mapv #(assoc % :marked-by-the-round true) bs))
                      beam/ensure-scored (fn [_ctx bs _turn] bs)
                      beam/record-inactive!
                      (fn [_ctx bs] (reset! disposed bs) (throw (ex-info "die" {})))]
          (is (thrown? Exception
                       (beam/run-rounds (ctx-for c rid) [(branch "B1")] 1))))
        (is (every? :marked-by-the-round @disposed)
            "the round's own progress is visible to teardown, not the input")
        (finally (db/close c)))))

;; --- the wiring, across every shipped manifest -------------------------------

(deftest every-shipped-cell-is-reachable-from-some-manifest
  ;; RFC-002 F1. A cell no manifest references never runs, so a capability that
  ;; is "available for later" is really absent — :probe/ab-model sat like that,
  ;; with a docstring claiming it recorded comparisons it could not have made.
  ;;
  ;; :subworkflows count as references: orchestrator declares
  ;; {:loop/worker "worker"} and register-subworkflows! registers it as a
  ;; workflow-cell at compile time, so it is absent from cells/loaded and
  ;; present when the manifest compiles. A check that misses that reports a
  ;; false missing-cell (RFC-002 F2).
  (let [names (->> (file-seq (java.io.File. "resources/manifests"))
                   (filter #(.isFile %))
                   (map #(clojure.string/replace (.getName %) #"\.edn$" "")))
        defs (for [n names]
               (workflow/read-definition
                (slurp (clojure.java.io/resource (str "manifests/" n ".edn")))))
        referenced (into (set (mapcat #(vals (:cells %)) defs))
                         (mapcat #(keys (:subworkflows %)) defs))
        registered (set (keys (cells/loaded)))]
    (is (seq registered))
    (is (empty? (remove referenced registered))
        (str "these cells are registered but no manifest reaches them, so they"
             " never run: " (pr-str (vec (remove referenced registered)))))
    (is (empty? (remove registered (remove (set (mapcat #(keys (:subworkflows %)) defs))
                                           (set (mapcat #(vals (:cells %)) defs)))))
        "and no manifest references a cell that is not registered")))

(defn- shipped-manifest-names []
  (->> (file-seq (java.io.File. "resources/manifests"))
       (filter #(.isFile %))
       (map #(clojure.string/replace (.getName %) #"\.edn$" ""))))

(defn- definition-of [n]
  (workflow/read-definition (slurp (clojure.java.io/resource (str "manifests/" n ".edn")))))

(deftest a-manifest-says-which-of-its-invariants-are-enforced
  ;; RFC-002 recorded, in bold, that there was no way to tell from a manifest
  ;; which of its invariants the compiler would catch — an editor could not
  ;; know what was defended. Four ordering rules lived in cell docstrings only,
  ;; on the stated grounds that mycelium's constraint vocabulary could not
  ;; express them.
  ;;
  ;; Three of the four turned out to be expressible: `:must-precede` says "if
  ;; `before` is on this path, `cell` appears earlier on it", which is exactly
  ;; the shape of directives-before-advance, cull-before-spawn and
  ;; dispatch-before-arbiter. Only the manifests were using `:must-follow`
  ;; alone. The fourth (settle before fire) orders two steps INSIDE one cell,
  ;; where a path-based checker has nothing to look at, and it is declared
  ;; unenforced with that reason.
  (doseq [n (shipped-manifest-names)
          :let [d (definition-of n)
                declared (workflow/invariants d)]
          :when (seq declared)]
    (testing n
      (is (every? #(contains? % :protects) declared)
          "every declared invariant says what it protects")
      (doseq [i (workflow/unenforced-invariants d)]
        (is (seq (:unenforced-because i))
            (str "unenforced invariant " (pr-str (:rule i))
                 " does not say why nothing checks it — `no constraint` and"
                 " `no constraint yet` are different facts"))))))

(deftest an-enforced-invariant-reaches-the-compiler
  ;; The derivation is the point: one list, so a rule cannot be documented as
  ;; enforced while nothing checks it. That is the failure direction that
  ;; matters — it reads as a defence and is not one.
  (doseq [n (shipped-manifest-names)
          :let [d (definition-of n)]]
    (testing n
      (is (= (count (filter :enforced (workflow/invariants d)))
             (count (workflow/enforced-constraints d)))
          "every enforced invariant became a constraint, and nothing else did")
      (doseq [c (workflow/enforced-constraints d)]
        (is (contains? c :type) "a derived constraint carries its type")
        (is (not-any? #{:enforced :protects :unenforced-because} (keys c))
            (str "reader-facing keys reached the compiler: " (pr-str c)))))))

(deftest a-must-precede-invariant-is-really-enforced
  ;; The check the whole change rests on: assert the COMPILER refuses a
  ;; manifest that breaks one of the newly-enforced rules, rather than
  ;; trusting that adding the entry did something. A constraint that compiles
  ;; either way would be worse than the docstring it replaced.
  (let [d (definition-of "beam")
        ;; Route around :directives, so :advance runs without it.
        broken (assoc-in d [:edges :start :continue] :advance)]
    (is (thrown? Throwable (workflow/compile-loop broken))
        "advancing without draining directives compiles")))

(deftest the-driver-provides-every-ctx-key-it-declares
  ;; The other end of workflow/ctx-keys. A contract cells are held to and no
  ;; driver satisfies would be worse than none: every cell would compile and
  ;; every read would still be nil.
  ;;
  ;; Checked against the ctx the production driver actually builds, captured
  ;; by intercepting run-rounds rather than by re-listing the keys here — a
  ;; second list would drift from the first, which is the failure this whole
  ;; pair exists to prevent.
  (let [captured (atom nil)]
    (with-redefs [beam/run-rounds (fn [ctx _branches _turn]
                                    (reset! captured ctx)
                                    {:status :completed :branches []})
                  llm/chat (fn [& _] {:content "" :finish-reason "stop"})]
      (let [c (db/open! ":memory:")]
        (try
          (beam/run! {:conn c :config {:run {:width 1}} :llm-adapter :a
                      :llm-config {:max-tokens 100} :problem "p" :max-turns 3})
          (finally (db/close c)))))
    (is (some? @captured) "the driver ran")
    (let [;; :live-branches is set by run-rounds itself, which this test
          ;; replaced — it is the one key the interception cannot observe.
          expected (disj workflow/ctx-keys :live-branches)
          missing (remove #(contains? @captured %) expected)]
      (is (empty? missing)
          (str "workflow/ctx-keys promises cells these, and the beam driver "
               "does not set them: " (pr-str (vec missing)))))))

(deftest a-cell-declares-every-ctx-key-it-reads
  ;; :requires is only worth having if it cannot drift from the source. A
  ;; declaration that quietly stopped matching the handler would put the
  ;; conventional-ctx-keys gap straight back, with a table on top of it
  ;; asserting otherwise.
  ;;
  ;; Reads the shipped cell SOURCE, since that is what load-string registers.
  (doseq [f (->> (file-seq (java.io.File. "resources/cells"))
                 (filter #(.isFile %))
                 (filter #(clojure.string/ends-with? (.getName %) ".clj")))
          :let [src (slurp f)]
          form (read-string {:read-cond :allow} (str "[" src "]"))
          :when (and (seq? form) (= 'cell/defcell (first form)))
          :let [cell-id (second form)
                meta-map (nth form 2)
                declared (set (:requires meta-map))
                handler (last form)
                ;; What the handler reads out of ctx: the keys it destructures
                ;; from its first argument, plus any (:key ctx) in its body.
                binding (first (second handler))
                destructured (when (map? binding) (map keyword (:keys binding)))
                direct (->> (tree-seq coll? seq handler)
                            (filter #(and (seq? %) (= 2 (count %))
                                          (keyword? (first %))
                                          (= 'ctx (second %))))
                            (map first))
                read-keys (set (concat destructured direct))]]
    (testing (str cell-id)
      (is (contains? meta-map :requires)
          "every cell declares :requires, even when it is empty — an absent
           declaration and a declared-nothing look identical otherwise")
      (let [undeclared (remove declared read-keys)]
        (is (empty? undeclared)
            (str cell-id " reads ctx keys it does not declare: "
                 (pr-str (vec undeclared))))))))

(deftest every-shipped-manifest-compiles-and-is-selectable
  ;; A manifest in the catalogue that cannot compile is a trap: the supervisor
  ;; is told it can switch to it, and the switch fails at run time.
  (let [names (->> (file-seq (java.io.File. "resources/manifests"))
                   (filter #(.isFile %))
                   (map #(clojure.string/replace (.getName %) #"\.edn$" "")))
        listed (set (map :name (workflow/catalog nil)))]
    (doseq [n names]
      (is (contains? listed n) (str n " is not in the catalogue the supervisor reads"))
      (is (some? (workflow/compiled-manifest n)) (str n " does not compile")))))

(deftest the-beam-narrows-to-what-the-provider-can-serve-under-the-deadline
  ;; karamazov-41a.8. gates.edn :beam-contention says how many calls the
  ;; provider serves at once and how long a turn takes; the width is clamped
  ;; to what fits one turn deadline, before the first branch opens, instead
  ;; of the deadline abandoning the tail of every round.
  (is (= 4 (beam/contended-width 5 {:provider-concurrency 1 :expected-turn-ms 200000}
                                 900000)))
  (is (= 5 (beam/contended-width 5 {:provider-concurrency 2 :expected-turn-ms 200000}
                                 900000)))
  (testing "no policy, or a policy that names no provider, changes nothing"
    (is (= 5 (beam/contended-width 5 nil 900000)))
    (is (= 5 (beam/contended-width 5 {} 900000)))
    (is (= 5 (beam/contended-width 5 {:provider-concurrency nil :expected-turn-ms nil}
                                   900000))))
  (testing "a turn that cannot fit at all runs at width one rather than not at all"
    (is (= 1 (beam/contended-width 5 {:provider-concurrency 1 :expected-turn-ms 2000000}
                                   900000)))))
