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

(ns samizdat.store.artifacts
  "The run-scoped shared confirmed-artifact log, the twin of the failure log.

  Branches already share what was DISPROVEN; this shares what an engine
  CONFIRMED, with provenance inline — which branch proved it, with which
  engine, at which tier — so a consumer can see where a lemma came from.
  The difference from UCLA's harness is the entry condition: there, proven
  results were self-reported and trust had to be taken on faith. Here only
  engine-confirmed claim-statuses ever enter, so a branch cannot talk its own
  results into the pool. The cost is beam diversity: branches may converge on
  shared lemmas instead of exploring. That is why the flag defaults to off and
  why sweep-widths runs the beam both ways.

  Like the failure log this is FTS5-backed, a query for the lemmas most like
  what a branch is about to try rather than a vector re-rendered whole into
  every context. The FTS table is standalone, and sync is app-managed here."
  (:require [clojure.set]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            ;; db.jdbc registers the java.sql shim clojure.jdbc compiles against and
            ;; points connection construction at the native driver; it has to load
            ;; before jdbc.core.
            [db.jdbc]
            [jdbc.core :as jdbc]
            [samizdat.lexicon :as lexicon]
            [samizdat.llm.message :as message]
            [samizdat.prompt :as prompt]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]))

(defn record!
  "A confirmed artifact into the shared log. Callers gate on claim-status and
  the config flag; this function only writes."
  [conn run-id {:keys [branch-id turn kind tier claim code]}]
  (db/with-writer
    (db/execute! conn
                   ["INSERT INTO shared_artifacts (run_id, branch_id, turn, kind, tier,
                                                   claim, code, created_at)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                    run-id branch-id turn (name kind) (name (or tier :fast))
                    (str claim) (str code) (db/now)])
    (let [id (db/last-insert-id conn)]
      (db/execute! conn
                     ["INSERT INTO shared_artifacts_fts (rowid, claim) VALUES (?, ?)"
                      id (str claim)]))))

(defn retract!
  "Undo a confirmation that turned out not to be one. Returns true if a row
  changed, false if there was nothing there.

  The campaign has banked worthless artifacts as CONFIRMED four separate ways:
  three whose entire proof body was `classical`, two vacuous theorems, and
  gen-30 a#829, closed with `sorry`. Each hole was fixed where it was found,
  but a row already written stayed written, and a live run rereads the ledger
  every turn — so a run could spend hours building on a lemma the harness had
  already learned was void.

  seed-from-run!'s quarantine argues why prose cannot do this job: the ledger
  is generated from the table every turn and says CONFIRMED, so a paragraph
  saying otherwise is a contradiction the branch has to adjudicate. That is
  just as true inside a run as across the seeding boundary.

  Both tables, because they feed different readers: `artifacts` is the run's
  own ledger and what a later generation inherits, `shared_artifacts` is the
  block siblings read. The row is marked rather than deleted — a retracted
  result is a thing that happened, and the reason belongs in the record."
  [conn run-id artifact-id reason]
  (db/with-writer
    (let [n (db/execute! conn
                         ["UPDATE artifacts SET claim_status = 'retracted'
                           WHERE run_id = ? AND id = ? AND claim_status = 'confirmed'"
                          run-id artifact-id])
          claim (:claim (first (db/fetch conn ["SELECT claim FROM artifacts
                                                WHERE run_id = ? AND id = ?"
                                               run-id artifact-id])))]
      (if (and claim (pos? (if (number? n) n 0)))
        (do
          ;; Matched on claim text: the shared row is a copy with its own id,
          ;; and the boundary where ids stop lining up is exactly this one.
          (doseq [r (db/fetch conn ["SELECT id FROM shared_artifacts
                                     WHERE run_id = ? AND claim = ?" run-id claim])]
            (db/execute! conn ["DELETE FROM shared_artifacts_fts WHERE rowid = ?" (:id r)])
            (db/execute! conn ["DELETE FROM shared_artifacts WHERE id = ?" (:id r)]))
          (journal/note! conn run-id :artifact-retracted
                         {:data {:artifact-id artifact-id :claim claim :reason reason}})
          true)
        false))))

(defn- normalize-claim
  "Spelling-level normalisation only — case and punctuation — matching
  consensus/normalize-claim. Two phrasings that group together there must
  group together here."
  [claim]
  (-> (str/lower-case (or claim ""))
      (str/replace #"[^a-z0-9]+" " ")
      str/trim))

(defn seed-from-run!
  "Copy `source-run-id`'s engine-confirmed artifacts into `run-id`'s shared
  log, for cross-run campaigns: a new run continues from a prior one's
  verified results without inheriting its dead ends.

  Claim AND code cross over — the code is what lets a branch re-confirm an
  inherited lemma in one cheap turn instead of reconstructing the encoding.
  The branch id is prefixed `seed:` so provenance is visible in the context
  block and no live branch's own-branch exclusion hides a seed. Only
  confirmed claim-statuses cross; refuted and existential stay behind. The
  done gate still requires in-run re-verification, so nothing inherited can
  ship on faith. Returns the number of artifacts seeded.

  `:quarantine` is a list of claim texts that must NOT cross, for a row marked
  confirmed that the harness has since learned was not. vf-4tw confirmed three
  artifacts on a Lean reply carrying no goal list, and those rows are still
  marked confirmed; seeding would carry them forward as inherited CONFIRMED
  lemmas. Prose in a problem statement cannot undo that — the ledger is
  generated from this table every turn and says CONFIRMED, so a paragraph
  saying otherwise is a contradiction the branch has to adjudicate, and the
  harness should not be handing out contradictions.

  Matched on normalised text rather than id, because ids are per-run and the
  boundary where they change is exactly the boundary this has to hold at."
  ([conn run-id source-run-id] (seed-from-run! conn run-id source-run-id nil))
  ([conn run-id source-run-id {:keys [quarantine]}]
  (let [blocked (into #{} (map normalize-claim) quarantine)
        own (db/fetch conn
                      ["SELECT branch_id, kind, tier, claim, code FROM artifacts
                        WHERE run_id = ? AND claim_status = 'confirmed' ORDER BY id"
                       source-run-id])
        ;; TRANSITIVE. Reading only the source's own artifacts made each
        ;; generation pass on what it proved and drop everything it had
        ;; inherited: the phase-unwrapping campaign lost gen-19's 16 lemmas at
        ;; the gen-20 boundary and would have lost gen-20's 11 at the next —
        ;; a chain that forgets faster than it learns.
        inherited (db/fetch conn
                            ["SELECT branch_id, kind, tier, claim, code
                              FROM shared_artifacts
                              WHERE run_id = ? AND branch_id LIKE 'seed:%'
                              ORDER BY id" source-run-id])
        ;; Own first, so where a run re-proved something it inherited, its own
        ;; provenance is the one that crosses.
        rows (->> (concat own inherited)
                  (remove #(contains? blocked (normalize-claim (:claim %))))
                  (reduce (fn [acc r]
                            (if (contains? (:seen acc) (:claim r))
                              acc
                              (-> acc (update :seen conj (:claim r))
                                  (update :rows conj r))))
                          {:seen #{} :rows []})
                  :rows)]
    (doseq [r rows]
      (record! conn run-id {:branch-id (let [b (str (:branch_id r))]
                                         ;; Already-seeded rows keep their
                                         ;; original provenance rather than
                                         ;; accumulating seed:seed:seed:.
                                         (if (str/starts-with? b "seed:")
                                           b
                                           (str "seed:" b)))
                            :turn 0
                            :kind (keyword (:kind r))
                            :tier (keyword (:tier r))
                            :claim (:claim r)
                            :code (:code r)}))
    (journal/note! conn run-id :run-seeded
                   {:data {:source source-run-id :artifacts (count rows)
                           :quarantined (count blocked)}})
    (count rows))))

(defn- fts-query
  "Turn free text into an FTS5 OR query.

  FTS5's query language treats several characters as operators, so raw model
  prose is not a safe query string. Words are extracted and quoted; anything
  below the lexicon's search-token floor is dropped as noise."
  [text]
  (->> (str/split (str/lower-case (or text "")) #"[^a-z0-9]+")
       (filter #(>= (count %) (lexicon/tuning :claim-matching :min-search-token-length)))
       distinct
       (take 12)
       (map #(str "\"" % "\""))
       (str/join " OR ")))

(defn similar
  "The shared artifacts most like `text`, best match first.

  Returns [] rather than throwing when the query has no usable terms, because
  an empty shared log is a normal state and a branch should not lose its turn
  to a search that found nothing."
  ([conn run-id text] (similar conn run-id text 5))
  ([conn run-id text limit]
   (let [q (fts-query text)]
     (if (str/blank? q)
       []
       (try
         (db/fetch conn
                     ["SELECT sa.id, sa.branch_id, sa.turn, sa.kind, sa.tier, sa.claim, sa.code
                       FROM shared_artifacts_fts fts
                       JOIN shared_artifacts sa ON sa.id = fts.rowid
                       WHERE shared_artifacts_fts MATCH ? AND sa.run_id = ?
                        ORDER BY bm25(shared_artifacts_fts) LIMIT ?"
                       q run-id limit])
          (catch Throwable e
            ;; Empty stays the contract, but a persistent fault must leave a
            ;; trace (provenance R2-15).
            (log/warn "artifacts/similar failed; returning empty:" (ex-message e))
            []))))))

(defn recent
  ([conn run-id] (recent conn run-id 10))
  ([conn run-id limit]
   (db/fetch conn ["SELECT id, branch_id, turn, kind, tier, claim, code FROM shared_artifacts
                      WHERE run_id = ? ORDER BY id DESC LIMIT ?" run-id limit])))

(defn- max-shared-code-chars
  "How much of a shared artifact's code the block carries.

  A theorem STATEMENT is what a sibling needs in order to build on a lemma;
  the proof body is not. Generous enough for a Lean signature with a dozen
  hypotheses, small enough that five entries cannot crowd out the branch's own
  context — the block re-renders every turn, so its size is a per-turn cost.

  A function reading `:context-budget`, not a constant: it was 700 under this
  same paragraph, which documents the number without making it reachable. How
  much the model gets to see is the most project-specific setting in the
  harness, and it belongs in one table with the rest of that question."
  []
  (lexicon/budget :shared-code-chars))

(defn- max-ledger-claim-chars
  "How much of a claim the ledger shows. `:context-budget :ledger-claim-chars`."
  []
  (lexicon/budget :ledger-claim-chars))

(defn- shingles [s]
  (let [width (lexicon/tuning :claim-matching :shingle-width)
        toks (re-seq #"[a-z0-9]+" (str/lower-case (str s)))]
    (if (< (count toks) width)
      (set toks)
      (set (map #(str/join " " %) (partition width 1 toks))))))

(defn near-duplicate?
  "Whether two claims say the same thing — shingle Jaccard at or above
  `threshold`, defaulting to the lexicon's `:near-duplicate-threshold`.

  Gates the ledger's dedupe, which collapses claims that are the same theorem
  differently worded (see dedupe-claims). The sketch diversity gate that also
  read it (vf-eaw) left with the proof harness's tool surface (provenance R3-9)."
  ([a b] (near-duplicate? a b (lexicon/tuning :claim-matching
                                               :near-duplicate-threshold)))
  ([a b threshold]
   (let [x (shingles a) y (shingles b)]
     (and (seq x) (seq y)
          (let [i (count (clojure.set/intersection x y))]
            (>= (/ (double i) (count (clojure.set/union x y))) threshold))))))

(defn dedupe-claims
  "Collapse entries whose claims say the same thing, keeping the fullest.

  Read live off gen-22: three branches proved one scalarization inequality and
  the ledger listed it three times. A ledger is for seeing the state of a run
  at a glance, and three spellings of one lemma defeats that — while hiding
  the signal that actually helps, which is that the line is well covered and
  does not want a fourth attempt.

  The survivor carries `:also-proved-by`, so the collapse is stated rather
  than silent. Fuzzy rather than exact: the live duplicates differed only in
  variable names and an `i.e.` for a `so`."
  [rows]
  (reduce (fn [acc r]
            (if-let [i (first (keep-indexed
                               (fn [i k] (when (near-duplicate? (:claim k) (:claim r)) i))
                               acc))]
              (let [k (nth acc i)
                    ;; The longer statement is the more useful one to show.
                    winner (if (> (count (str (:claim r))) (count (str (:claim k)))) r k)]
                (assoc acc i (assoc winner :also-proved-by
                                    (conj (vec (:also-proved-by k)) (:branch_id r)))))
              (conj acc r)))
          []
          rows))

(defn- elide
  "Shorten a long claim while keeping BOTH ends.

  Cutting only the head threw away the conclusion: one live entry read
  \"…the flow on any edge leaving R is at …\", losing the bound, which is the
  only part a branch needs. The hypotheses say when a lemma applies and the
  tail says what it gives you; a middle is the safe thing to drop."
  [s]
  (let [c (str/trim (str s))
        cap (max-ledger-claim-chars)]
    (if (<= (count c) cap)
      c
      ;; Two thirds head, one third tail: the hypotheses say when a lemma
      ;; applies and the tail says what it gives you.
      (let [head (quot (* 2 cap) 3)
            tail (- cap head)]
        (str (subs c 0 head) " … " (subs c (- (count c) tail)))))))

(defn- ledger-line
  "One entry. `prefix` selects the id space: `a#` indexes this run's own
  artifacts, `s#` the shared pool a seed was copied into. Two tables, one
  fetch tool, so the handles must not be confusable."
  [prefix {:keys [id branch_id kind tier claim also-proved-by]}]
  (str "- [" prefix id " " branch_id " " (name (or kind "?")) "/"
       (name (or tier "?")) "] " (elide claim)
       (when (seq also-proved-by)
         (str " (also proved by " (str/join ", " (distinct also-proved-by))
              " — " (inc (count (distinct also-proved-by)))
              " branches have this; it does not need another)"))))

(defn render-ledger
  "The run's settled state, as a block for a branch's next-turn context.

  Two sections, never one list with a status column: a refutation formatted
  like a confirmation is worse than not sharing it at all, and a model
  skimming a single list will merge them. Established first, because it is
  what a branch builds on; ruled out second, because it is what stops a branch
  repeating a closed line.

  Ids are handles, not decoration — `a#12` is what `fetch_artifact` takes, so
  the encodings stay out of the block and cost a turn only when wanted."
  [{:keys [established ruled-out sketches inherited]}]
  (when (or (seq established) (seq ruled-out) (seq sketches) (seq inherited))
    ;; The SENTINELS stay compiled and the PROSE does not. message/ledger-open
    ;; and ledger-close are what strip-stale-ledgers matches to drop every copy
    ;; but the newest, so a supervisor editing the block's wording must not be
    ;; able to break that by accident — while the wording itself is exactly the
    ;; kind of thing a project should be able to change.
    ;;
    ;; What the template says, and why it is worded the way it is, moved with
    ;; it into prompts/ledger.md: two sections rather than one list with a
    ;; status column, sketches held out of both settled halves under their own
    ;; `p#` prefix, and a top line that covers plans without endorsing them.
    (let [lines (fn [prefix rows]
                  (when (seq rows)
                    (str/join "\n" (map (partial ledger-line prefix)
                                        (dedupe-claims rows)))))]
      (str message/ledger-open "\n"
           (prompt/render "ledger"
                          {:established (lines "a#" established)
                           :ruled-out   (lines "a#" ruled-out)
                           :sketches    (lines "p#" sketches)
                           :inherited   (lines "s#" inherited)})
           message/ledger-close))))

(defn prefer-in-run
  "In-run artifacts first, seeded ones after, order preserved within each.

  A completed run contributes its whole pool at turn 0 while the live run's
  starts empty, so without this seeds win on volume from the start and keep
  winning: gen-20 served 67 seeded artifacts against 24 of its own, spending
  three quarters of the channel re-telling a prior run's results.

  Ranked rather than filtered. A campaign that seeds without restating the
  prior results in its problem statement would lose them entirely, and
  `seed-from-run!` exists for exactly that case."
  [entries]
  (let [seed? #(str/starts-with? (str (:branch_id %)) "seed:")]
    (into (vec (remove seed? entries)) (filter seed? entries))))

(defn render
  "Shared artifacts as the block that goes into a branch's next-turn context:
  engine-confirmed by other branches, provenance inline.

  Carries the CODE as well as the claim. Three gen-20 branches proved the same
  scalarization lemma while being shown each other's claims five times each —
  a branch cannot cite a theorem statement it has never seen, so re-deriving
  was the only move open to it. The code was already being selected by
  `similar` and `recent`, and `seed-from-run!` copies it precisely so an
  inherited lemma can be re-confirmed in one cheap turn; only this function
  dropped it."
  [entries]
  (when (seq entries)
    (prompt/render
     "shared-artifacts"
     {:entries
      (str/join "\n"
                (for [{:keys [branch_id kind tier claim code]} (prefer-in-run entries)]
                  (let [cap (max-shared-code-chars)]
                    (str "- [" branch_id " " (name kind) "/" (name tier) "] " claim
                         (when-not (str/blank? (str code))
                           (str "\n  ```\n  "
                                (let [c (str/trim (str code))]
                                  (str/replace (if (> (count c) cap)
                                                 (str (subs c 0 cap) "\n… [truncated]")
                                                 c)
                                               "\n" "\n  "))
                                "\n  ```"))))))})))
