;; samizdat - a claim-first verification harness
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

(ns samizdat.store-test
  "Phase 0 storage, kept as tests rather than only as smoke probes.

  The migration statement-count check is the one that matters. db.sqlite/query
  calls sqlite3_prepare_v2 with a null tail pointer, so a migration written as
  one multi-statement string would execute only its first statement and report
  no error. Counting objects created against statements written is how that
  failure mode stays loud."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [jdbc.core :as jdbc]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.loop :as branch-loop]
            [samizdat.agent.state :as state]
            [samizdat.api.runs :as api-runs]
            [samizdat.store.artifacts :as artifacts]
            [samizdat.store.db :as db]
            [samizdat.store.interventions :as interventions]
            [samizdat.lexicon :as lexicon]
            [samizdat.store.failures :as failures]
            [samizdat.store.journal :as journal]
            [samizdat.store.migrations :as migrations]
            [samizdat.store.runs :as runs]))

(defmacro with-db [[binding] & body]
  `(let [~binding (db/open! ":memory:")]
     (try ~@body (finally (db/close ~binding)))))

(deftest journalling-a-value-json-cannot-write-does-not-kill-the-turn
  ;; The journal is a RECORD of the work. It must not be able to destroy the
  ;; work it is recording. gen-31 lost B1.3 and B2.2 — both on the run's actual
  ;; target — to "Don't know how to write JSON of class java.lang.Character"
  ;; thrown out of the turn writer: the args held a String where a collection
  ;; was expected, it seq'd into Characters, and the exception propagated up to
  ;; the beam, which abandoned the branch. Same shape as vf-jki and as the
  ;; database-lock cull: a non-mathematical event ending mathematical work.
  ;;
  ;; The root cause is fixed in the XML parser, but the journal must degrade
  ;; rather than throw for the NEXT unserialisable thing nobody predicted.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (is (some? (journal/record-turn!
                  c rid {:branch-id "B1" :turn 1 :tool-name "thesis"
                         :args {:subClaims [\a \b]}
                         :result "ok" :category "neutral"}))
          "the turn is still recorded")
      (let [row (first (db/fetch c ["SELECT args FROM turns WHERE run_id = ? AND turn = 1" rid]))]
        (is (string? (:args row)) "and the args column holds something readable")))))

(deftest a-connection-does-not-die-when-something-else-is-reading
  ;; A branch was lost to this on 2026-08-18: an external `sqlite3` query held
  ;; a read lock, the harness's next write got SQLITE_BUSY with a zero busy
  ;; timeout, and the branch was abandoned with "database is locked" while it
  ;; was mid-proof. In-process access is serialized, so this only ever bites
  ;; when someone looks at the file — which is the first thing anyone does
  ;; with a run.
  (let [f (str "/tmp/samizdat-lock-test-" (System/nanoTime) ".sqlite3")]
    (try
      (let [c (db/connect f)]
        (is (= "wal" (clojure.string/lower-case (str (first (vals (db/fetch-one c "PRAGMA journal_mode"))))))
            "readers must not exclude the writer")
        (is (pos? (long (first (vals (db/fetch-one c "PRAGMA busy_timeout")))))
            "and a writer that does contend must wait rather than fail"))
      (finally
        (doseq [suffix ["" "-wal" "-shm"]]
          (.delete (java.io.File. (str f suffix))))))))

(deftest migrations-apply
  (with-db [c]
    (is (= (count migrations/migrations) (db/schema-version c)))
    (is (every? (set (db/table-names c))
                ["runs" "branches" "turns" "artifacts" "failures"
                 "gate_firings" "interventions" "events"
                 "shared_artifacts"]))))

(deftest migrations-are-idempotent
  (with-db [c]
    (let [v (db/schema-version c)
          tables (db/table-names c)]
      (db/migrate! c)
      (is (= v (db/schema-version c)))
      (is (= tables (db/table-names c))))))

(deftest every-migration-statement-runs
  (testing "no migration hides statements behind a multi-statement string"
    ;; Every entry must be a vector of single statements. A statement holding
    ;; a second one after a semicolon would be silently dropped by
    ;; sqlite3_prepare_v2, so reject that shape outright rather than trusting
    ;; that nobody writes it.
    (doseq [[i statements] (map-indexed vector migrations/migrations)]
      (is (vector? statements) (str "migration v" (inc i) " must be a vector"))
      (doseq [sql statements]
        (is (not (re-find #";\s*\S" (clojure.string/replace sql #"--[^\n]*" "")))
            (str "migration v" (inc i) " has a statement containing a `;` followed by"
                 " more SQL, which sqlite3_prepare_v2 would silently drop:\n" sql))))))

(deftest a-turn-records-what-it-cost
  ;; The adapter parsed usage and client/chat threaded it through, and the
  ;; agent loop dropped it on the floor: nothing outside the bench harness and
  ;; the raw passthrough API ever read :usage, and `turns` had no token
  ;; columns. So the harness could not answer what a run cost — for a system
  ;; whose whole operating rule is that a generation is hours of provider
  ;; spend, that is the one number it should never have been missing.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (journal/record-turn! c rid
                            {:branch-id "B1" :turn 1 :tool-name "verify"
                             :result "ok" :category :success
                             :usage {:prompt-tokens 100 :completion-tokens 20
                                     :total-tokens 120
                                     :cache-hit-tokens 80 :cache-miss-tokens 20}})
      (let [t (first (journal/turns c rid))]
        (is (= 100 (:prompt_tokens t)))
        (is (= 20 (:completion_tokens t)))
        (is (= 80 (:cache_hit_tokens t)))
        (is (= 20 (:cache_miss_tokens t)))))
    (testing "a turn with no usage stores nulls, not zeros"
      ;; The provider-error path has no response and therefore no usage. A
      ;; zero there would be a claim that the call was free, and summing it
      ;; would under-report the run's cost rather than admit ignorance.
      (let [rid (runs/start-run! c {:problem "p2"})]
        (journal/record-turn! c rid
                              {:branch-id "B1" :turn 1
                               :tool-name "__provider_error__"
                               :result "boom" :category :neutral})
        (let [t (first (journal/turns c rid))]
          (is (nil? (:prompt_tokens t)))
          (is (nil? (:cache_hit_tokens t))))))))

(deftest a-run-reports-the-width-it-is-actually-running-at
  ;; beam_width is the repopulation FLOOR, not a cap: repopulate only fires
  ;; below it, and branch-out grows past it up to :max-total-branches, which is
  ;; 15. A run started at width 5 was observed carrying 9 active branches — 9
  ;; concurrent provider calls per round and 9 engine processes, from a
  ;; parameter that reads like a ceiling.
  ;;
  ;; The number is knowable from the branches table, so report it rather than
  ;; leaving whoever set beam_width to discover the multiplier from `ps`.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :beam-width 2})]
      (doseq [b ["B1" "B2" "B3"]]
        (runs/open-branch! c rid {:branch-id b}))
      (runs/close-branch! c rid "B3" :culled "dominated")
      (let [d (api-runs/get-run c rid)]
        (is (= 2 (get-in d [:run :beam_width]))
            "the requested width is still reported, unchanged")
        (is (= 2 (get-in d [:run :active_branches]))
            "alongside how many branches are actually running")
        (is (= (gates/threshold :max-total-branches)
               (get-in d [:run :max_branches]))
            "and the ceiling that actually bounds it")))))

(deftest a-quarantined-claim-does-not-cross-into-the-next-run
  ;; vf-4tw. A Lean reply with no goal list was read as a closed proof, so
  ;; three artifacts were confirmed on the tactic `classical`, which closes
  ;; nothing: gen-24's two proofs of lemma (B) and gen-25's TARGET 1. The
  ;; engine bug is fixed, but those rows are still marked confirmed, and
  ;; seeding would carry them into the next run as inherited CONFIRMED lemmas.
  ;;
  ;; Prose in the problem statement cannot undo that. The ledger is generated
  ;; from this table every turn and says CONFIRMED; a paragraph saying
  ;; otherwise is a contradiction the branch has to adjudicate, and the
  ;; harness should not be handing out contradictions. So the row simply does
  ;; not cross.
  ;;
  ;; Keyed on claim text, not id: ids are per-run, and the whole point is to
  ;; stop a claim propagating across the boundary where ids change.
  (with-db [c]
    (let [g1 (runs/start-run! c {:problem "one"})
          g2 (runs/start-run! c {:problem "two"})]
      (journal/record-artifact! c g1 {:branch-id "B1" :turn 9 :kind :lean :tier :slow
                                      :claim "a finite DAG admits a rank function"
                                      :code "theorem b : True := by\n  classical"
                                      :claim-status :confirmed})
      (journal/record-artifact! c g1 {:branch-id "B2" :turn 4 :kind :smt :tier :fast
                                      :claim "the coefficient bound holds"
                                      :code "(assert true)"
                                      :claim-status :confirmed})
      (let [n (artifacts/seed-from-run!
               c g2 g1 {:quarantine ["a finite DAG admits a rank function"]})
            claims (set (map :claim (artifacts/recent c g2 20)))]
        (is (= 1 n) "the quarantined claim is not counted as seeded")
        (is (not (contains? claims "a finite DAG admits a rank function"))
            "and it is not in the pool the ledger reads")
        (is (contains? claims "the coefficient bound holds")
            "everything else still crosses"))))

  (testing "quarantine matches on normalised text, not exact bytes"
    ;; The same lemma crosses generations reworded; an exact-match quarantine
    ;; would let it back in on a change of capitalisation or punctuation.
    (with-db [c]
      (let [g1 (runs/start-run! c {:problem "one"})
            g2 (runs/start-run! c {:problem "two"})]
        (journal/record-artifact! c g1 {:branch-id "B1" :turn 9 :kind :lean :tier :slow
                                        :claim "A finite DAG admits a rank function."
                                        :code "x" :claim-status :confirmed})
        (artifacts/seed-from-run!
         c g2 g1 {:quarantine ["a finite dag admits a rank function"]})
        (is (empty? (artifacts/recent c g2 20))))))

  (testing "no quarantine behaves exactly as before"
    (with-db [c]
      (let [g1 (runs/start-run! c {:problem "one"})
            g2 (runs/start-run! c {:problem "two"})]
        (journal/record-artifact! c g1 {:branch-id "B1" :turn 1 :kind :smt :tier :fast
                                        :claim "kept" :code "x" :claim-status :confirmed})
        (is (= 1 (artifacts/seed-from-run! c g2 g1)))
        (is (= 1 (artifacts/seed-from-run! c (runs/start-run! c {:problem "three"})
                                           g1 nil)))))))

(deftest a-seeded-run-reports-that-sharing-is-on
  ;; beam/run! forces :share-artifacts? true for any run started with a
  ;; seed-run, in memory, overriding config. Nothing recorded it, so /health
  ;; reported the CONFIG value — during one run it said sharing was off while
  ;; that run had fired 91 shared-artifact-hit events. A run-level fact
  ;; reported as a global one is how a later investigation starts from a false
  ;; premise; this one cost a detour before the contradiction resolved.
  ;;
  ;; Derived from the run-seeded event rather than a new column: the journal
  ;; already records the fact, so the row does not need to.
  (with-db [c]
    (let [plain (runs/start-run! c {:problem "p"})
          seeded (runs/start-run! c {:problem "q"})]
      (journal/note! c seeded :run-seeded {:data {:source "prior" :artifacts 3}})
      (is (true? (get-in (api-runs/get-run c seeded) [:run :share_artifacts]))
          "a seeded run shares regardless of config")
      (is (false? (get-in (api-runs/get-run c plain) [:run :share_artifacts]))
          "an unseeded one does not claim to"))))

(deftest the-shared-block-carries-the-code-not-just-the-claim
  ;; Three gen-20 branches proved the same scalarization lemma — B4 as Hf/Hg,
  ;; B3 as H/K, B4.2 as alpha/beta — while being shown each other's claims five
  ;; times each. They were not failing to discover it. `render` printed the
  ;; claim text alone, and a branch cannot cite a theorem statement it has
  ;; never seen, so re-deriving was the only move available to it.
  ;;
  ;; The code was there the whole time: seed-from-run! copies it precisely so
  ;; an inherited lemma can be re-confirmed in one cheap turn, and both
  ;; `similar` and `recent` select it. Only the renderer dropped it.
  (let [block (artifacts/render
               [{:branch_id "B3" :kind "lean" :tier "slow"
                 :claim "Scalarization yields lexicographic minimizer"
                 :code "theorem scalarization_yields_lex_min : True := trivial"}])]
    (is (re-find #"Scalarization yields" block))
    (is (re-find #"scalarization_yields_lex_min" block)
        "the statement a sibling would need to build on must be in the block")
    (testing "an artifact with no code still renders"
      (is (re-find #"bare claim"
                   (artifacts/render [{:branch_id "B1" :kind "prolog"
                                       :tier "fast" :claim "bare claim"}]))))))

(deftest in-run-artifacts-outrank-seeded-ones-in-the-block
  ;; gen-20 served 91 shared artifacts, 67 of them seeded from gen-19 and only
  ;; 24 produced by the run itself. A completed run contributes a full pool at
  ;; turn 0 while the live run's starts empty, so seeds win on volume from the
  ;; start and keep winning.
  ;;
  ;; Ranked rather than dropped: a campaign that seeds without restating the
  ;; prior results in its problem statement would lose them entirely, and
  ;; seed-from-run! exists for exactly that case.
  (let [entries [{:branch_id "seed:B1" :claim "old a"} {:branch_id "seed:B2" :claim "old b"}
                 {:branch_id "seed:B3" :claim "old c"} {:branch_id "B4" :claim "new a"}
                 {:branch_id "B5" :claim "new b"}]
        ranked (artifacts/prefer-in-run entries)]
    (is (= ["B4" "B5" "seed:B1" "seed:B2" "seed:B3"] (mapv :branch_id ranked))
        "everything the run produced itself comes first")
    (is (= (set entries) (set ranked)) "and nothing is dropped")
    (testing "order within each group is preserved — relevance ranking survives"
      (is (= ["new a" "new b"] (mapv :claim (take 2 ranked)))))))

(deftest an-in-run-lemma-reaches-the-block-even-when-seeds-dominate
  ;; The half of the fix that ranking alone does not give. context-block used
  ;; to fetch exactly the number it displays, so when the top 5 by relevance
  ;; were all seeds — which is the normal case early, since a completed run
  ;; contributes its whole pool at turn 0 — reordering them just reordered
  ;; seeds. The pool has to be fetched wider than it is shown.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (doseq [i (range 10)]
        (artifacts/record! c rid {:branch-id (str "seed:B" i) :turn 1 :kind :lean
                                  :tier :slow
                                  :claim (str "scalarization lemma variant " i)
                                  :code (str "theorem seeded_" i " : True := trivial")}))
      (artifacts/record! c rid {:branch-id "B3" :turn 9 :kind :lean :tier :slow
                                :claim "scalarization lemma proved in this run"
                                :code "theorem in_run_scalarization : True := trivial"})
      (let [{:keys [block]} (#'branch-loop/context-block
                             c rid (state/new-branch {:id "B7" :problem "p"})
                             "scalarization lemma" true)]
        (is (re-find #"in_run_scalarization" block)
            "the run's own lemma must reach the block, not be buried under seeds")
        (is (re-find #"proved in this run" block))
        ;; Scoped to the shared-artifact section, which is what prefer-in-run
        ;; governs. The settled-state ledger above it legitimately lists the
        ;; inherited entries in its own section, so comparing positions across
        ;; the whole context block measures the wrong thing.
        (let [shared (subs block (.indexOf block "Confirmed by other branches"))]
          (is (< (.indexOf shared "proved in this run")
                 (.indexOf shared "variant"))
              "within the shared block, in-run entries come before seeded ones"))))))

(deftest the-ledger-carries-what-was-ruled-out-not-only-what-was-proved
  ;; 127 refuted artifacts exist across the project and not one had ever been
  ;; shared: loop/shareable? admits only :confirmed, so a branch was told what
  ;; siblings had PROVEN and never what they had DISPROVEN. A refutation is
  ;; worth more per token than a confirmation for choosing what to try — a
  ;; confirmation adds a fact, a refutation prunes a direction.
  ;;
  ;; Read from `artifacts` rather than `shared_artifacts`, which has no
  ;; claim_status column: the authoritative table already carries everything,
  ;; so this needs no migration and cannot drift from the record.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          art (fn [bid status claim]
                (journal/record-artifact!
                 c rid {:branch-id bid :turn 1 :kind :lean :tier :slow
                        :claim claim :code (str "theorem t_" claim)
                        :claim-status status}))]
      (art "B1" :confirmed "scalarization yields the lex minimum")
      (art "B2" :refuted "zero total signed weight preserves optimality")
      (art "B3" :unfaithful "sat is offered for a universal claim")
      (art "B4" :empirical "tie sizes on 60 random 3x3 grids")
      (let [led (journal/ledger c rid)
            established (mapv :claim (:established led))
            ruled-out (mapv :claim (:ruled-out led))]
        (is (= ["scalarization yields the lex minimum"] established))
        (is (= ["zero total signed weight preserves optimality"] ruled-out)
            "the disproven half, which nothing carried before")
        (testing "unfaithful stays out — the encoding never established the claim"
          ;; 110 of these exist. Sharing one spreads an assertion that nothing
          ;; verified, which is the opposite of what a ledger is for.
          (is (not-any? #(re-find #"sat is offered" %)
                        (concat established ruled-out))))
        (testing "every row carries an id, so the encoding can be fetched"
          (is (every? :id (concat (:established led) (:ruled-out led)))))))))

(deftest seeding-is-transitive-so-a-campaign-does-not-forget
  ;; seed-from-run! read only the source's OWN artifacts, so each generation
  ;; passed on what it proved and dropped everything it had inherited. The
  ;; phase-unwrapping campaign lost gen-19's 16 lemmas at the gen-20 boundary
  ;; and would have lost gen-20's 11 at the next one — a chain that forgets
  ;; faster than it learns, which is the opposite of what seeding is for.
  ;;
  ;; Provenance is preserved rather than restacked: an inherited row is
  ;; already `seed:B4`, and copying it again must not produce `seed:seed:B4`.
  (with-db [c]
    (let [g1 (runs/start-run! c {:problem "one"})
          g2 (runs/start-run! c {:problem "two"})
          g3 (runs/start-run! c {:problem "three"})]
      (journal/record-artifact! c g1 {:branch-id "B1" :turn 1 :kind :lean :tier :slow
                                      :claim "first-generation lemma"
                                      :code "theorem g1 : True := trivial"
                                      :claim-status :confirmed})
      (artifacts/seed-from-run! c g2 g1)
      (journal/record-artifact! c g2 {:branch-id "B7" :turn 1 :kind :smt :tier :fast
                                      :claim "second-generation lemma"
                                      :code "(assert true)"
                                      :claim-status :confirmed})
      (let [n (artifacts/seed-from-run! c g3 g2)
            claims (set (map :claim (artifacts/recent c g3 20)))]
        (is (= 2 n) "both generations cross, not just the most recent")
        (is (contains? claims "first-generation lemma")
            "the inherited lemma survives a second hop")
        (is (contains? claims "second-generation lemma")))
      (testing "provenance is not restacked"
        (is (not-any? #(re-find #"seed:seed:" (str (:branch_id %)))
                      (artifacts/recent c g3 20))))
      (testing "a claim already present is not duplicated"
        ;; g2 holds g1's lemma as an inherited row and its own; re-seeding
        ;; must not multiply it.
        (let [claims (map :claim (artifacts/recent c g3 20))]
          (is (= (count claims) (count (distinct claims)))))))))

(deftest the-ledger-shows-what-the-run-inherited
  ;; Seeding copies a prior run's confirmed artifacts into shared_artifacts,
  ;; not into artifacts, so a seeded run's ledger read "established 0" while
  ;; the run held eleven inherited lemmas. The whole point of seeding is to
  ;; carry verified results forward; a settled-state view that omits them is
  ;; telling the branch the opposite of the truth, and leaves the statement's
  ;; hand-written summary as the only route — which is the fragility seeding
  ;; exists to remove.
  ;;
  ;; Its own section, and its own handle prefix: seeded rows are in a
  ;; different table and therefore a different id space, so `s#7` and `a#7`
  ;; must not be confusable.
  (with-db [c]
    (let [src (runs/start-run! c {:problem "prior"})
          rid (runs/start-run! c {:problem "p"})]
      (journal/record-artifact! c src {:branch-id "B4" :turn 2 :kind :lean
                                       :tier :slow :claim "weighted sum yields lex min"
                                       :code "theorem wsl : True := trivial"
                                       :claim-status :confirmed})
      (artifacts/seed-from-run! c rid src)
      (journal/record-artifact! c rid {:branch-id "B1" :turn 1 :kind :smt
                                       :tier :fast :claim "found here"
                                       :code "(assert true)"
                                       :claim-status :confirmed})
      (let [led (journal/ledger c rid)]
        (is (= ["found here"] (mapv :claim (:established led)))
            "this run's own work stays in Established")
        (is (= ["weighted sum yields lex min"] (mapv :claim (:inherited led)))
            "and the inherited lemma is no longer invisible")
        (let [block (artifacts/render-ledger led)]
          (is (re-find #"(?i)inherited" block))
          (is (re-find #"s#" block) "seeded rows use the shared-table handle")
          (is (< (.indexOf block "found here") (.indexOf block "weighted sum"))
              "this run's own results lead"))))))

(deftest a-sketch-does-not-cross-generations
  ;; seed-from-run! selects claim_status = 'confirmed', and this pin is what
  ;; keeps a future edit from widening that to "anything engine-accepted". A
  ;; sketch IS engine-accepted in the narrow sense that it elaborates, which
  ;; is exactly why it must not cross: the next run would open with a plan
  ;; sitting in the pool its ledger calls CONFIRMED, and a branch reading
  ;; "inherited" would build on steps nobody ever proved. The quarantine
  ;; machinery exists for confirmed rows that should not have been; this is
  ;; the cheaper case — never let the row in at all.
  (with-db [c]
    (let [g1 (runs/start-run! c {:problem "one"})
          g2 (runs/start-run! c {:problem "two"})]
      (journal/record-artifact! c g1 {:branch-id "B1" :turn 3 :kind :lean :tier :fast
                                      :claim "the union bound splits the cost"
                                      :code "theorem split : t := by sorry"
                                      :claim-status :sketch})
      (journal/record-artifact! c g1 {:branch-id "B2" :turn 5 :kind :lean :tier :slow
                                      :claim "each per-edge term is bounded"
                                      :code "theorem edge : t := by rfl"
                                      :claim-status :confirmed})
      (let [n (artifacts/seed-from-run! c g2 g1)
            claims (set (map :claim (artifacts/recent c g2 20)))]
        (is (= 1 n) "only the confirmation is counted as seeded")
        (is (not (contains? claims "the union bound splits the cost"))
            "the plan stays behind with the run that planned it")
        (is (contains? claims "each per-edge term is bounded"))))))

(deftest the-ledger-renders-sketches-as-plans-not-results
  ;; The ledger's whole design rule is "sections, never one list with a status
  ;; column" — a refutation formatted like a confirmation is worse than not
  ;; sharing it. A sketch is the same hazard one step removed: it elaborates,
  ;; its citations exist, and every `sorry` in it is a step still open. So it
  ;; gets its own section under `p#`, worded so a model skimming the block
  ;; cannot mistake it for something an engine checked, and it must stay out
  ;; of both settled halves.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})
          art (fn [bid status claim]
                (journal/record-artifact!
                 c rid {:branch-id bid :turn 1 :kind :lean :tier :fast
                        :claim claim :code (str "theorem t_" claim)
                        :claim-status status}))]
      (art "B1" :confirmed "each per-edge term is bounded")
      (art "B2" :sketch "the union bound splits the cost")
      (let [led (journal/ledger c rid)]
        (is (= ["the union bound splits the cost"] (mapv :claim (:sketches led))))
        (is (= [] (mapv :claim (:ruled-out led))))
        (is (= ["each per-edge term is bounded"] (mapv :claim (:established led)))
            "the plan did not leak into the settled halves")
        (let [block (artifacts/render-ledger led)]
          (is (re-find #"(?i)unverified" block)
              "the section says so on its face")
          (is (re-find #"p#" block) "plans carry their own handle")
          (is (re-find #"(?i)still open" block))
          (is (< (.indexOf block "each per-edge term is bounded")
                  (.indexOf block "the union bound splits the cost"))
              "established leads; the plan follows, never the reverse"))))))

(deftest fetch-artifact-resolves-both-id-spaces
  ;; a#N indexes artifacts, s#N indexes shared_artifacts. One tool, two
  ;; tables, so the prefix has to disambiguate — otherwise a branch fetching
  ;; a ledger id could silently get an unrelated row.
  (with-db [c]
    (let [src (runs/start-run! c {:problem "prior"})
          rid (runs/start-run! c {:problem "p"})]
      (journal/record-artifact! c src {:branch-id "B9" :turn 1 :kind :lean
                                       :tier :slow :claim "inherited claim"
                                       :code "theorem inherited : True := trivial"
                                       :claim-status :confirmed})
      (artifacts/seed-from-run! c rid src)
      (let [sid (:id (first (artifacts/recent c rid 5)))]
        (is (re-find #"theorem inherited"
                     (str (:code (journal/shared-artifact-by-id c rid sid))))
            "a seeded row is reachable by its shared id")
        (is (nil? (journal/shared-artifact-by-id c src sid))
            "and only from the run that inherited it")))))

(deftest the-ledger-includes-the-branch-s-own-work-and-stays-small
  ;; Unlike the shared block, which excludes own-branch rows because a branch
  ;; re-reading its own lemmas mid-narrative is noise. As a LIST it is the
  ;; point: the alternative is scanning an 80-turn transcript for what you
  ;; established. gen-20's entire confirmed knowledge is 1,495 chars — under
  ;; 400 tokens — against B3's ~50k-char transcript.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (doseq [i (range 3)]
        (journal/record-artifact! c rid {:branch-id "B1" :turn i :kind :smt
                                         :tier :fast :claim (str "own fact " i)
                                         :code "(assert true)"
                                         :claim-status :confirmed}))
      (let [led (journal/ledger c rid)]
        (is (= 3 (count (:established led))) "own artifacts are in the ledger")
        (let [block (artifacts/render-ledger led)]
          (is (re-find #"own fact 0" block))
          (is (re-find #"(?i)established" block))
          (testing "an empty ledger renders nothing rather than an empty heading"
            (is (nil? (artifacts/render-ledger {:established [] :ruled-out []})))))))))

(deftest the-ledger-collapses-a-lemma-several-branches-proved
  ;; Read live off gen-22 at round 20: a#687, a#689 and a#693 were the same
  ;; scalarization coefficient inequality, proved by three branches and listed
  ;; as three established facts. A ledger exists so a branch can see the state
  ;; of the run at a glance; three spellings of one lemma is the opposite, and
  ;; it also hides the more useful signal — that the line is well covered and
  ;; not worth a fourth attempt.
  (let [rows [{:id 687 :branch_id "B3" :kind "smt" :tier "fast"
               :claim "For any integers B >= 1, E >= 1, define Qmax = E*B*B, LR = 2*B, K = LR + 1, H = K*Qmax + LR + 1. Then LR < K and K*Qmax + LR < H, so the scalarization coefficient inequalities hold."}
              {:id 689 :branch_id "B3" :kind "smt" :tier "fast"
               :claim "For any integers B >= 1, E >= 1, define Qmax = E*B*B, LR = 2*B, K = LR + 1, H = K*Qmax + LR + 1. Then LR < K and K*Qmax + LR < H, i.e. the scalarization coefficient inequalities hold."}
              {:id 700 :branch_id "B9" :kind "lean" :tier "slow"
               :claim "An entirely different result about prefix sums of a zero-sum sequence."}]
        grouped (artifacts/dedupe-claims rows)]
    (is (= 2 (count grouped)) "the two spellings collapse, the unrelated one stays")
    (let [block (artifacts/render-ledger {:established rows :ruled-out [] :inherited []})]
      (is (re-find #"(?i)2 branches|also proved by" block)
          (str "and the collapse is stated rather than silent: " (subs block 0 400)))
      (is (re-find #"prefix sums" block) "unrelated results are untouched"))))

(deftest the-ledger-keeps-the-conclusion-when-a-claim-is-long
  ;; Truncating at a fixed head cut the payload off: one live entry read
  ;; "...the flow on any edge leaving R is at …", losing the bound, which is
  ;; the only part a branch needs. Keep both ends — the hypotheses say when it
  ;; applies and the tail says what it gives you.
  (let [long-claim (str "Cut lemma for flow coordinate bound: if k is a nonnegative integer flow "
                        "with divergence b and R is a vertex set with no edge entering it, and "
                        "several further conditions hold that pad this statement out well past "
                        "any sensible display width, then the flow on any edge leaving R is at "
                        "most the total divergence of R.")
        block (artifacts/render-ledger
               {:established [{:id 1 :branch_id "B2" :kind "lean" :tier "slow"
                               :claim long-claim}]
                :ruled-out [] :inherited []})]
    (is (re-find #"Cut lemma for flow coordinate bound" block) "the hypotheses survive")
    (is (re-find #"most the total divergence" block) "and so does the conclusion")
    (is (< (count block) (+ 400 (count long-claim))) "without printing the whole thing twice")))

(deftest the-ledger-renders-refuted-so-it-cannot-be-read-as-proved
  ;; A refutation formatted like a confirmation is worse than not sharing it.
  ;; Separate sections rather than a status tag on a line, so a model skimming
  ;; the block cannot merge the two.
  (let [block (artifacts/render-ledger
               {:established [{:id 1 :branch_id "B1" :kind "lean" :tier "slow"
                               :claim "the rule is well defined"}]
                :ruled-out [{:id 2 :branch_id "B2" :kind "smt" :tier "fast"
                             :claim "the natural strengthening holds"}]})
        est-at (.indexOf block "the rule is well defined")
        ref-at (.indexOf block "the natural strengthening holds")]
    (is (pos? est-at))
    (is (pos? ref-at))
    (is (re-find #"(?i)ruled out|disproven|refuted" block)
        "the disproven section must be labelled as such")
    (is (< est-at ref-at) "established first, then what is closed")
    (testing "the id is present and usable as a handle"
      (is (re-find #"a#2" block)))))

(deftest fts5-is-available-through-the-ffi-binding
  ;; Distinct from the sqlite3 CLI having FTS5. The failure mode is a
  ;; migration that throws at startup.
  (let [c (db/connect ":memory:")]
    (try
      (is (db/fts5-available? c))
      (finally (db/close c)))))

(deftest failures-fts-round-trips
  (with-db [c]
    (jdbc/execute! c ["INSERT INTO failures_fts (claim, reason) VALUES (?, ?)"
                      "sidon set of size 24 exists" "z3 returned unsat"])
    (jdbc/execute! c ["INSERT INTO failures_fts (claim, reason) VALUES (?, ?)"
                      "the coloring is schur-good" "monochromatic triple at 3+3=6"])
    (is (= 1 (count (jdbc/fetch c ["SELECT claim FROM failures_fts WHERE failures_fts MATCH ?"
                                   "sidon"]))))
    (is (= 1 (count (jdbc/fetch c ["SELECT claim FROM failures_fts WHERE failures_fts MATCH ?"
                                   "monochromatic"]))))
    (is (empty? (jdbc/fetch c ["SELECT claim FROM failures_fts WHERE failures_fts MATCH ?"
                               "lean"])))))

(deftest a-turn-keeps-what-the-model-said
  ;; v1 stored the tool call and the result but not the prose, so a turn that
  ;; produced no tool call recorded only that fact. Nine of twenty turns in a
  ;; Lean run came back __no_call__ and the question "why" had no answer in the
  ;; data, because the one artefact that would settle it was the one thing not
  ;; kept. This asserts the no-call path in particular, since that is the path
  ;; where every other column is empty.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :beam-width 1})]
    (runs/open-branch! c rid {:branch-id "B1" :created-at-turn 0})
    (journal/record-turn! c rid
                          {:branch-id "B1" :turn 1 :tool-name "__no_call__"
                           :result "[harness] No tool-call block." :category "failure"
                           :assistant-text "I think the answer is 4. Let me explain at length."
                           :reasoning-text "the model's private reasoning"})
    (let [t (first (journal/turns c rid))]
      (is (= "__no_call__" (:tool_name t)))
      (is (= "I think the answer is 4. Let me explain at length." (:assistant_text t)))
      (is (= "the model's private reasoning" (:reasoning_text t)))))))

(deftest a-turn-without-a-response-stores-null-rather-than-the-string-null
  ;; The provider-error path has no response at all. Coercing that to "" or
  ;; "null" would make "the model said nothing" indistinguishable from "the
  ;; model was never asked".
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :beam-width 1})]
    (runs/open-branch! c rid {:branch-id "B1" :created-at-turn 0})
    (journal/record-turn! c rid
                          {:branch-id "B1" :turn 1 :tool-name "__provider_error__"
                           :result "timeout" :category "neutral"})
    (is (nil? (:assistant_text (first (journal/turns c rid))))))))

(deftest branch-turns-reads-one-branch-and-leaves-the-bulk-behind
  ;; The GUI's branch panel hung forever on gen-14. branch-detail called
  ;; journal/turns, which selects EVERY turn of the whole run with every
  ;; column and then filters for one branch in Clojure — so opening one
  ;; branch fetched 5.5MB of assistant_text that the panel never renders,
  ;; took over two minutes, and blew the client's 45s socket timeout. The
  ;; run's index on (run_id, branch_id, turn) was sitting unused.
  ;;
  ;; assistant_text and reasoning_text stay out of this projection on
  ;; purpose. They are the bulk, nothing displaying a branch wants them, and
  ;; resume — which does — keeps using journal/turns.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :beam-width 1})]
      (runs/open-branch! c rid {:branch-id "B1" :created-at-turn 0})
      (runs/open-branch! c rid {:branch-id "B2" :created-at-turn 0})
      (journal/record-turn! c rid {:branch-id "B1" :turn 1 :tool-name "verify"
                                   :result "ok" :category "success"
                                   :assistant-text "a very long reply"
                                   :reasoning-text "private reasoning"})
      (journal/record-turn! c rid {:branch-id "B2" :turn 1 :tool-name "thesis"
                                   :result "registered" :category "neutral"
                                   :assistant-text "another long reply"})
      (let [ts (journal/branch-turns c rid "B1")
            t (first ts)]
        (is (= 1 (count ts)) "only the branch asked for")
        (is (= "verify" (:tool_name t)))
        (is (= "ok" (:result t)) "the result is what gets rendered, so it stays")
        (is (not (contains? t :assistant_text))
            "the bulk column is not in the projection at all")
        (is (not (contains? t :reasoning_text))))
      (is (= 1 (count (journal/branch-turns c rid "B2"))))
      (is (empty? (journal/branch-turns c rid "nosuch")))
      ;; resume still needs the full row, so the old accessor keeps its shape.
      (is (= "a very long reply"
             (:assistant_text (first (journal/turns c rid))))))))

;; --- liveness ---------------------------------------------------------------

(deftest last-progress-tracks-the-newest-journal-entry
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :beam-width 1})]
      ;; start-run! journals :run-started, so a fresh run already has progress.
      (is (some? (runs/last-progress-at c rid)))
      (journal/note! c rid :turn {:data {:n 1}})
      (is (some? (runs/last-progress-at c rid))))
    (is (nil? (runs/last-progress-at c "no-such-run"))
        "a run with no events has no progress timestamp rather than a fake one")))

(deftest a-run-is-stalled-only-when-it-is-running-and-quiet
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :beam-width 1})
          ;; The run's only event is :run-started, which we age by choosing a
          ;; threshold shorter than it has existed rather than by editing rows.
          long-window 3600000]
      (is (false? (runs/stalled? c rid long-window))
          "a run that just emitted an event is not stalled")
      (is (true? (runs/stalled? c rid -1))
          "past the threshold with no newer event, a running run is stalled")
      (runs/finish-run! c rid :completed "answer")
      (is (false? (runs/stalled? c rid -1))
          "a finished run is quiet because it is over, not because it is stuck"))))

(deftest a-run-left-running-by-a-crash-is-reconciled-at-startup
  ;; status='running' is a claim the beam makes once and never revisits, so a
  ;; process that dies mid-run leaves the row asserting forever. gen-18 and
  ;; gen-11 both sat that way; the second was filed as a separate bug before
  ;; anyone noticed it was the same defect, which is the argument for fixing
  ;; the mechanism rather than the rows.
  ;;
  ;; Nothing in-process can distinguish "running" from "was running when we
  ;; died" — but nothing can be running at STARTUP, because the beam only ever
  ;; runs in this process. So the reconciliation is sound exactly here and
  ;; nowhere else.
  (with-db [c]
    (let [crashed (runs/start-run! c {:problem "died" :beam-width 1})
          done    (runs/start-run! c {:problem "finished" :beam-width 1})]
      (runs/finish-run! c done :completed "answer")
      (is (= 1 (runs/reconcile-orphans! c))
          "exactly the one row still claiming to run")
      (let [r (runs/get-run c crashed)]
        (is (= "interrupted" (:status r))
            "a run nobody is running is interrupted, not completed or failed —
             it neither finished nor errored, and saying either would be a lie")
        (is (some? (:ended_at r))
            "an ended run needs an end time or every duration is wrong"))
      (is (= "completed" (:status (runs/get-run c done)))
          "a finished run is left alone")
      (is (zero? (runs/reconcile-orphans! c))
          "idempotent: a second startup has nothing to reconcile"))))

(deftest resuming-a-run-puts-it-back-into-running
  ;; A resumed run is running, and the row has to say so. Before crashed runs
  ;; were reconciled the row still read 'running' from the original start, so
  ;; nothing had to set it and nothing did; reconciling to 'interrupted' made
  ;; that omission visible — gen-19 resumed, took turns, and its row still
  ;; said interrupted.
  ;;
  ;; It is not cosmetic. stalled? only reports on a run whose status is
  ;; 'running', so a resumed run that wedges is invisible to the one check
  ;; built to notice exactly that. ended_at has to go too, or the run has an
  ;; end time that precedes half its turns.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :beam-width 1})]
      (runs/reconcile-orphans! c)
      (is (= "interrupted" (:status (runs/get-run c rid))))
      (runs/mark-running! c rid)
      (let [r (runs/get-run c rid)]
        (is (= "running" (:status r)))
        (is (nil? (:ended_at r)) "a running run has not ended")))))

(deftest a-confirmed-artifact-can-be-retracted
  ;; The campaign has now banked "confirmed" artifacts that were nothing four
  ;; separate ways: three proofs whose whole body was `classical`, two vacuous
  ;; theorems, and gen-30 a#829, closed with `sorry`. Each hole got fixed, but
  ;; there was no way to undo a row already written, and a live run rereads the
  ;; ledger every turn.
  ;;
  ;; seed-from-run!'s quarantine says why prose cannot do this job: "the ledger
  ;; is generated from this table every turn and says CONFIRMED, so a paragraph
  ;; saying otherwise is a contradiction the branch has to adjudicate". That
  ;; applies within a run, not only across the seeding boundary.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p" :beam-width 1})
          _ (runs/open-branch! c rid {:branch-id "B1" :created-at-turn 0})
          _ (journal/record-artifact!
             c rid {:branch-id "B1" :turn 1 :kind :lean
                    :claim "a real lemma" :code "theorem t : True := by trivial"
                    :claim-status :confirmed :tier :slow})
          _ (journal/record-artifact!
             c rid {:branch-id "B1" :turn 2 :kind :lean
                    :claim "closed with sorry" :code "theorem u : True := by sorry"
                    :claim-status :confirmed :tier :slow})
          bad-id (:id (first (filter #(= "closed with sorry" (:claim %))
                                     (journal/artifacts c rid))))]
      (artifacts/record! c rid {:branch-id "B1" :turn 2 :kind :lean :tier :slow
                                :claim "closed with sorry"
                                :code "theorem u : True := by sorry"})

      (testing "it starts out established, which is the problem"
        (is (= 2 (count (journal/confirmed-artifacts c rid "B1")))))

      (testing "retracting takes it out of the run's settled state"
        (is (true? (artifacts/retract! c rid bad-id "closed with sorry")))
        (let [left (journal/confirmed-artifacts c rid "B1")]
          (is (= 1 (count left)))
          (is (= "a real lemma" (:claim (first left))) "and leaves the honest one alone")))

      (testing "and out of the cross-branch block siblings read"
        (is (empty? (filter #(= "closed with sorry" (:claim %))
                            (artifacts/recent c rid 50)))))

      (testing "and out of what a later generation inherits"
        (let [next-rid (runs/start-run! c {:problem "p2" :beam-width 1})]
          (artifacts/seed-from-run! c next-rid rid)
          (is (= ["a real lemma"] (mapv :claim (artifacts/recent c next-rid 50))))))

      (testing "retracting something that is not there says so rather than lying"
        (is (false? (artifacts/retract! c rid 99999 "no such row"))))

      (testing "the retraction is on the record"
        ;; A result that silently disappears is its own kind of unreliable.
        (is (some #(= "artifact-retracted" (str (:kind %)))
                  (journal/events-since c rid 0 200)))))))

(deftest the-ledger-block-does-not-call-a-plan-settled
  ;; The block's heading was "What this run has settled" and the sketch
  ;; section was added underneath it. A sketch is precisely what the run has
  ;; NOT settled, and this campaign has banked worthless-but-confirmed
  ;; artifacts five separate ways — classical, anonymous examples, vacuous
  ;; statements, sorry, and a True-concluding inspection — every one of them a
  ;; case of something unverified being read as verified. A model skimming
  ;; headings is exactly the reader that failure mode needs.
  (let [block (artifacts/render-ledger
               {:established [{:id 1 :branch_id "B1" :turn 2 :kind "lean"
                               :tier "slow" :claim "a proved lemma"}]
                :sketches [{:id 3 :branch_id "B1" :turn 4 :kind "lean"
                            :tier "fast" :claim "an approach, nothing proved"}]})]
    (testing "the plan is present and under its own prefix"
      (is (clojure.string/includes? block "p#3"))
      (is (clojure.string/includes? block "an approach, nothing proved")))
    (testing "and the block does not describe it as settled"
      (is (not (re-find #"(?i)^##\s+What this run has settled\s*$"
                        (or (first (filter #(clojure.string/starts-with? % "## ")
                                           (clojure.string/split-lines block)))
                            "")))
          "the top heading has to cover plans too, or it misdescribes them"))
     (testing "the sketch section still says plainly that nothing in it is verified"
       (is (re-find #"(?i)UNVERIFIED|not results" block)))))

(deftest a-failed-migration-rolls-back-and-keeps-the-version
  ;; provenance R2-3: migrations are non-idempotent ALTERs, but each ran as
  ;; autocommitted statements with the version bump after the last one — a
  ;; crash between the two left user_version stale and every later boot
  ;; died on `duplicate column name` forever. A migration must be
  ;; all-or-nothing with the bump inside it.
  (with-db [c]
    (db/execute! c ["PRAGMA user_version = 1"])
    (with-redefs [migrations/migrations
                  [["CREATE TABLE mg_one (x)"]
                   ["CREATE TABLE mg_two (x)"
                    "THIS IS NOT SQL"
                    "CREATE TABLE mg_three (x)"]]]
      (is (thrown? Exception (db/migrate! c))))
    (is (= 1 (db/schema-version c)) "the version bump did not survive the rollback")
    (is (empty? (filter #(str/starts-with? % "mg_") (db/table-names c)))
        "no partially-applied statements survive")
    (with-redefs [migrations/migrations
                  [["CREATE TABLE mg_one (x)"]
                   ["CREATE TABLE mg_two (x)" "CREATE TABLE mg_three (x)"]]]
      (is (= 2 (db/migrate! c))
          "the next boot with the fixed migration applies cleanly"))
    (is (= #{"mg_two" "mg_three"}
           (set (filter #(str/starts-with? % "mg_") (db/table-names c)))))))

(deftest lifecycle-writes-are-decided-by-the-row
  ;; provenance R2-4: finish-run!/mark-running!/close-branch!/resolve! were
  ;; WHERE id = ? only, so a stale caller rewrote a terminal run (abort!'s
  ;; transient window vs the run's own completion) or a closed branch's
  ;; inactive_reason. Guards follow reconcile-orphans!'s precedent.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (is (pos? (runs/finish-run! c rid :completed "answer")))
      (is (zero? (runs/finish-run! c rid :aborted nil))
          "a stale finisher loses to the terminal row")
      (let [r (runs/get-run c rid)]
        (is (= "completed" (:status r)))
        (is (= "answer" (:final_answer r))))
      (is (zero? (runs/mark-running! c rid))
          "a terminal run cannot be marked running again")
      (is (= "completed" (:status (runs/get-run c rid))))
      ;; mark-running! still serves the resume path: failed -> running
      (let [r2 (runs/start-run! c {:problem "q"})]
        (runs/finish-run! c r2 :failed nil)
        (is (pos? (runs/mark-running! c r2)))
        (is (= "running" (:status (runs/get-run c r2)))))
      (runs/close-branch! c rid "B1" :exhausted "turn cap")
      (is (zero? (runs/close-branch! c rid "B1" :abandoned "late close"))
          "a late close does not overwrite a closed branch")
      (is (= "exhausted" (:status (runs/get-branch c rid "B1"))))
      (let [i1 (interventions/submit! c rid {:kind "pause"})
            other (runs/start-run! c {:problem "r"})]
        (is (pos? (interventions/resolve! c rid i1 :applied "ok" 3)))
        (is (zero? (interventions/resolve! c rid i1 :rejected "double resolve" 4)))
        (is (zero? (interventions/resolve! c other i1 :applied "wrong run" 5)))
        (is (= "applied" (:status (some #(when (= i1 (:id %)) %)
                                        (interventions/history c rid)))))))))

(deftest old-finished-runs-get-their-events-pruned-on-the-next-start
  ;; provenance R2-11: events are a durable duplicate of every turn/artifact/
  ;; failure/gate write whose only readers are the live tail and
  ;; last-progress-at; nothing ever pruned them, so the one shared DB file
  ;; grew without bound. The sweep runs at run START, not at finish: a
  ;; tailing client still reads a just-finished run's events (that is how
  ;; it sees :run-finished), so events ride out a 24h window and the
  ;; durable tables hold the content forever.
  (with-db [c]
    (let [old (runs/start-run! c {:problem "old"})
          recent (runs/start-run! c {:problem "recent"})]
      (journal/note! c old :gate {:gate :test})
      (runs/finish-run! c old :completed "done")
      ;; backdate its end past the retention window
      (db/execute! c ["UPDATE runs SET ended_at = ? WHERE id = ?"
                      (str (.minusSeconds (java.time.Instant/now) (* 48 3600))) old])
      ;; a fresh finished run inside the window keeps its events
      (journal/note! c recent :gate {:gate :test})
      (runs/finish-run! c recent :completed "ok")
      ;; starting the next run sweeps
      (runs/start-run! c {:problem "next"})
      (is (zero? (:n (db/fetch-one c ["SELECT COUNT(*) AS n FROM events
                                       WHERE run_id = ?" old])))
          "a finished run's events are pruned once past the window")
      (is (pos? (:n (db/fetch-one c ["SELECT COUNT(*) AS n FROM events
                                      WHERE run_id = ?" recent])))
          "a just-finished run keeps its events for the tail"))))

(deftest branch-detail-surfaces-unsettled-gates
  ;; review4: unsettled-gates was a store fn no production reader called.
  ;; The branch panel is its natural consumer — advice that fired and was
  ;; never heeded belongs next to the turns it targeted.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (journal/record-gate! c rid {:branch-id "B1" :turn 2 :gate :stuck
                                   :priority 2 :message "m" :prediction "p"
                                   :window 3})
      (let [d (api-runs/branch-detail c rid "B1")]
        (is (= 1 (count (:unsettled-gates d))))
        (is (= "stuck" (:gate (first (:unsettled-gates d)))))
        (is (= #{:branch :turns :unsettled-gates :artifacts}
               (set (keys d))))))))

;; --- retention --------------------------------------------------------------

(deftest the-run-record-is-kept-forever-by-default
  ;; RFC-009's central property is that a resume rebuilds branch state by
  ;; replay and that a crashed run stays inspectable. Pruning turns ends both
  ;; for that run, so it is an operator's decision about disk and never a
  ;; default anyone inherits.
  (is (nil? (:run-record-days (lexicon/policy :retention)))
      "if this ever defaults to a number, the harness silently starts
       discarding the account every other property is built on")
  (is (pos? (:events-hours (lexicon/policy :retention)))
      "events are a tail buffer and do sweep on their own"))

(deftest pruning-the-record-leaves-the-run-and-takes-the-detail
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (journal/record-turn! c rid {:branch-id "B1" :turn 1 :tool-name "eval"
                                   :args {} :result "r" :category "success"})
      (journal/record-gate! c rid {:branch-id "B1" :turn 1 :gate "stuck"
                                   :message "m" :prediction "p" :window 3})
      (failures/record! c rid {:branch-id "B1" :turn 1 :tool-name "eval"
                               :claim "c" :reason "why"})
      (runs/finish-run! c rid "completed" "done")

      (testing "a cutoff before the run leaves everything alone"
        (is (= 0 (journal/prune-run-record! c "2000-01-01T00:00:00Z")))
        (is (= 1 (count (journal/turns c rid)))))

      (testing "a cutoff after it takes the detail"
        (is (= 1 (journal/prune-run-record! c "2999-01-01T00:00:00Z")))
        (is (empty? (journal/turns c rid)))
        (is (empty? (journal/gate-firings c rid)))
        (is (empty? (failures/recent c rid))))

      (testing "and leaves the run itself — an index of what happened,
                without the bulk, which beats a deleted row when somebody
                asks what happened six months ago"
        (let [r (runs/get-run c rid)]
          (is (some? r))
          (is (= "completed" (:status r)))
          (is (= "p" (:problem r))))))))

(deftest a-running-run-is-never-pruned
  ;; A row that says it is running is either live or a leftover
  ;; reconcile-orphans! has not seen yet, and neither is safe to strip.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (journal/record-turn! c rid {:branch-id "B1" :turn 1 :tool-name "eval"
                                   :args {} :result "r" :category "success"})
      (is (= 0 (journal/prune-run-record! c "2999-01-01T00:00:00Z")))
      (is (= 1 (count (journal/turns c rid)))))))

(deftest pruning-takes-the-fts-mirror-with-the-rows
  ;; An orphaned index entry keeps ranking against nothing, which is worse
  ;; than either keeping or deleting cleanly. The deletes are ordered so the
  ;; subquery that finds the rowids still has rows to find.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "B1"})
      (failures/record! c rid {:branch-id "B1" :turn 1 :tool-name "eval"
                               :claim "the greedy bound holds" :reason "it does not"})
      (runs/finish-run! c rid "completed" "done")
      (is (seq (failures/similar c rid "greedy bound")))
      (journal/prune-run-record! c "2999-01-01T00:00:00Z")
      (is (empty? (failures/similar c rid "greedy bound"))
          "the index went with the rows"))))

(deftest enabling-record-retention-does-not-break-run-start
  ;; start-run! logged the sweep through an alias the ns never required; jolt
  ;; resolves aliases lazily, so the ns loaded fine and the throw waited for
  ;; the first run start that actually pruned something — at which point every
  ;; subsequent start failed until the knob was reverted (karamazov-blt.31).
  ;; The one code path that exercises the advertised retention feature is the
  ;; one this drives.
  (with-db [c]
    (let [old (runs/start-run! c {:problem "old"})]
      (runs/open-branch! c old {:branch-id "B1"})
      (journal/record-turn! c old {:branch-id "B1" :turn 1 :tool-name "eval"
                                   :args {} :result "r" :category "success"})
      (runs/finish-run! c old "completed" "done")
      (db/execute! c ["UPDATE runs SET ended_at = ? WHERE id = ?"
                      (str (.minusSeconds (java.time.Instant/now) (* 40 86400)))
                      old])
      (with-redefs [lexicon/policy (fn [k] (get {:retention {:events-hours 24
                                                             :run-record-days 30}}
                                               k))]
        (let [id (runs/start-run! c {:problem "next"})]
          (is (some? id) "the sweep-and-log path starts the run")
          (is (empty? (journal/turns c old))
              "and the aged-out record was actually pruned"))))))

;; --- re-opening a branch a resume already has (karamazov-otd) ---------------

(deftest open-branch-is-idempotent-so-a-resume-does-not-crash
  ;; Branch ids are round-scoped (T0, T0v1), so a resumed run's board claims
  ;; the same task to the same id. The plain INSERT died on the (run_id, id)
  ;; primary key and took the board stage down with it.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "T0" :problem "part one"})
      (is (= "T0" (runs/open-branch! c rid {:branch-id "T0" :problem "part one"}))
          "re-opening returns the id rather than throwing")
      (is (= 1 (count (db/fetch c ["SELECT * FROM branches WHERE run_id = ? AND id = ?"
                                   rid "T0"])))
          "and leaves exactly one row"))))

(deftest re-opening-never-rewrites-how-a-branch-ended
  ;; The one thing the existing row holds that must survive: its ending.
  ;; close-branch! refuses to rewrite a closed branch's status (R2-4), and
  ;; re-opening must not do through the back door what closing refuses to do.
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "T0"})
      (runs/close-branch! c rid "T0" :done nil)
      (runs/open-branch! c rid {:branch-id "T0"})
      (let [row (first (db/fetch c ["SELECT * FROM branches WHERE run_id = ? AND id = ?"
                                    rid "T0"]))]
        (is (= "done" (:status row))
            "still done — a rejoin is not a resurrection")))))

(deftest a-rejoin-is-journalled-as-a-rejoin
  (with-db [c]
    (let [rid (runs/start-run! c {:problem "p"})]
      (runs/open-branch! c rid {:branch-id "T0"})
      (runs/open-branch! c rid {:branch-id "T0"})
      (let [kinds (set (map :kind (db/fetch c ["SELECT kind FROM events WHERE run_id = ?" rid])))]
        (is (contains? kinds "branch-opened"))
        (is (contains? kinds "branch-rejoined")
            "the record distinguishes a first open from a resume's rejoin")))))
