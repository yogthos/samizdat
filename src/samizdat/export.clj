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

(ns samizdat.export
  "Runs as training data: the journal projected into role-tagged conversations.

  MECHANISM ONLY. Which runs are worth imitating, which branches, and which
  turns are noise is `gates.edn :export`; this namespace knows how to turn a
  run into the tape its model saw and how to write tapes out, and nothing
  about why anyone wants them.

  WHY THIS SHAPE. A supervised fine-tune (the ChatML-with-masked-loss kind)
  needs two things: the conversation exactly as the model saw it, role-tagged
  so the loss mask is a role test; and only the conversations worth imitating.
  The first is what `resume` rebuilds — `assistant_text` is what the model
  said, `result` what the harness answered — with one addition resume skips:
  the steer a gate appended to a result, composed behind the same rule
  `steer-step` composes it behind, because a model trained without the
  steers would meet them cold. The second is policy: by default a run that
  ended `completed` with a green ship verification, and only the branch that
  shipped.

  FIDELITY, stated rather than discovered. The system message is the CURRENT
  template, not the one the run saw — the journal keeps a digest of it, not
  the text — which is the prompt a trained model will meet at inference, so
  for training it is the right one. The text the cell appended to it — an
  owner prompt, a unit's attempt framing, the supervisor's role text — IS on
  the row (v24) and is replayed verbatim; a branch older than that column
  exports on the base prompt alone. The context block (shared tree,
  breadcrumbs) and compaction folds are not journalled and are not replayed:
  the tape is the verbatim one. Turns of a skipped category (mechanics — a
  no-call, a parse error) go out with the complaint they drew, since neither
  is behaviour to learn; a turn with no assistant text (a provider failure)
  goes out too, since the model said nothing to learn from. Every turn that
  stays contributes exactly an assistant message and a user message — an
  EMPTY tool result is still an answer the model saw, and a chat template
  needs the roles to alternate — so the tape alternates strictly after the
  problem. The branch's role (v23) picks the system prompt and its tool
  surface, as it did live; a branch older than that column exports as the
  unscoped default. Every string crosses `secrets/redact` on its way out
  (karamazov-5ge3).

  THE SECOND PROJECTION: VERDICTS (karamazov-3htz). Every judgement the
  harness made — a gate firing with its prediction, a beam critic's scores,
  a cull reprieve, a finalization verdict, a board review, a round critique
  — as `{:situation :verdict :outcome}`, where the OUTCOME is what the
  journal says happened next: the prediction settled met or unmet, the
  scored branch shipped or was culled, the round's tests went green or red.
  The label is the point. A judge trained on its own verdicts learns to
  imitate its guesses; trained on verdicts beside outcomes it learns which
  guesses were right. So verdicts come from EVERY run whatever its status —
  the failed runs are where the 'this verdict was wrong' labels live — and
  the selection rule is the opposite of the trajectories'. Which kinds count
  is `gates.edn :export :verdicts`."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [samizdat.agent.loop :as branch-loop]
            [samizdat.lexicon :as lexicon]
            [samizdat.security.secrets :as secrets]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

(defn policy
  "The export policy, from gates.edn."
  []
  (lexicon/policy :export))

(defn- default-known-values
  "The secrets this process knows about, for the redaction boundary. Best
  effort: a CLI with an odd environment still exports, with the regex half of
  redaction rather than none."
  []
  (try (secrets/known-values (into {} (System/getenv)))
       (catch Throwable _ #{})))

(defn- parse-json [s]
  (try (json/read-str (str s) :key-fn keyword)
       (catch Throwable _ nil)))

(defn- latest-ship-verify
  "Per branch, whether its LATEST ship verification on this run was green.
  Latest, because a branch can go red and then green, and it is the last
  word that says whether what it shipped was verified. A branch never
  verified is absent — unknown, which is not red.

  Read off the events table rather than through `journal/notes`: a note
  comes back as its data map alone, and the branch is a column beside it."
  [conn run-id]
  (->> (db/fetch conn ["SELECT branch_id, data FROM events
                         WHERE run_id = ? AND kind = 'ship-verify' ORDER BY id"
                       run-id])
       (reduce (fn [m {:keys [branch_id data]}]
                 (assoc m branch_id (boolean (:green (parse-json data)))))
               {})))

(defn- green-branches
  "Branches whose latest ship verification on this run was green."
  [conn run-id]
  (->> (latest-ship-verify conn run-id)
       (keep (fn [[branch green?]] (when green? branch)))
       set))

(defn- done-branches
  "Branches that finished with a successful `done` — the fallback identity of
  'the one that shipped' for a run whose verification was off."
  [turns]
  (->> turns
       (filter #(and (= "done" (str (:tool_name %)))
                     (= "success" (str (:category %)))))
       (map :branch_id)
       set))

(defn qualifying-runs
  "The runs the policy says are worth imitating: a terminal status in
  `:statuses`, and — when `:require-verified?` — at least one branch whose
  ship verification came back green. Oldest first."
  [conn p]
  (let [statuses (set (map name (:statuses p)))]
    (->> (db/fetch conn ["SELECT * FROM runs ORDER BY started_at, id"])
         (filter #(contains? statuses (str (:status %))))
         (filter #(or (not (:require-verified? p))
                      (seq (green-branches conn (:id %)))))
         vec)))

(defn conversation
  "One branch's tape as role-tagged messages: the role's system prompt with
  the suffix the branch opened on, the problem, then for each turn the
  model's own text and the harness's answer, with any steer a gate appended
  to that answer composed behind the same `---` rule the loop uses. Turns of
  a skipped category are left out along with the complaint they drew."
  [{:keys [problem prompt-suffix role turns firings skip-categories known-values]}]
  (let [skip (set (map name (or skip-categories [])))
        steers (reduce (fn [m f] (update m (:turn f) (fnil conj []) (str (:message f))))
                       {} firings)
        scrub #(secrets/redact (str %) known-values)]
    (reduce (fn [msgs t]
              (if (or (contains? skip (str (:category t)))
                      (empty? (:assistant_text t)))
                msgs
                (let [steer (get steers (:turn t))
                      body (cond-> (str (:result t))
                             (seq steer) (str "\n\n---\n\n" (str/join "\n\n" steer)))]
                  (-> msgs
                      (conj {:role "assistant" :content (scrub (:assistant_text t))})
                      (conj {:role "user" :content (scrub body)})))))
            (mapv #(update % :content scrub)
                  (branch-loop/initial-messages problem prompt-suffix role))
            turns)))

(defn trajectories
  "Every qualifying run's shipping branch(es) as `{:run-id :branch-id
  :provider :model :messages}`, oldest run first.

  `opts` override the policy for one call — `:branches :all` to take every
  branch, `:known-values` to name the secrets to redact (the process
  environment's by default)."
  ([conn] (trajectories conn {}))
  ([conn opts]
   (let [p (merge (policy) (dissoc opts :known-values))
         known (or (:known-values opts) (default-known-values))]
     (into []
           (mapcat
            (fn [run]
              (let [run-id (:id run)
                    turns (journal/turns conn run-id)
                    firings (journal/gate-firings conn run-id)
                    by-branch (group-by :branch_id turns)
                    chosen (if (= :all (keyword (:branches p)))
                             (set (keys by-branch))
                             (into (green-branches conn run-id) (done-branches turns)))
                    ;; The branch's OWN problem where it has one — a decompose
                    ;; unit's contract, a team worker's sub-task — else the
                    ;; run's, the same rule resume applies.
                    rows (into {} (map (juxt :id identity)) (runs/branches conn run-id))]
                (into []
                      (keep (fn [branch-id]
                              (when-let [ts (seq (get by-branch branch-id))]
                                {:run-id run-id
                                 :branch-id branch-id
                                 :role (some-> (get-in rows [branch-id :role]) not-empty)
                                 :provider (:provider run)
                                 :model (:model run)
                                 :messages
                                 (conversation
                                  {:problem (or (not-empty (str (get-in rows [branch-id :problem])))
                                                (:problem run))
                                   :role (some-> (get-in rows [branch-id :role]) not-empty keyword)
                                   ;; nil for a row older than v24: the base
                                   ;; prompt alone, as before the column.
                                   :prompt-suffix (get-in rows [branch-id :prompt_suffix])
                                   :turns (sort-by (juxt :turn :id) ts)
                                   :firings (filter #(= branch-id (:branch_id %)) firings)
                                   :skip-categories (:skip-categories p)
                                   :known-values known})})))
                      (sort chosen)))))
           (qualifying-runs conn p)))))

;; --- verdicts ---------------------------------------------------------------

(defn- redacted
  "Every string inside `x` through the redaction boundary."
  [x known]
  (walk/postwalk #(if (string? %) (secrets/redact % known) %) x))

(defn- branch-fates
  "What became of each branch: the status and reason the branches table
  holds, and whether it shipped — a green ship verification, or failing
  that a successful `done`."
  [conn run-id turns]
  (let [green (green-branches conn run-id)
        done (done-branches turns)]
    (into {}
          (map (fn [b]
                 [(:id b) {:branch-status (str (:status b))
                           :inactive-reason (some-> (:inactive_reason b) str not-empty)
                           :shipped? (boolean (or (green (:id b)) (done (:id b))))}]))
          (runs/branches conn run-id))))

(defn- run-verdicts
  "One run's judgements as rows, every kind and every branch, each with the
  outcome the journal records after it. Which kinds go out is the caller's
  filter; this knows how each kind is labelled.

  Per-branch judgements (a critic score, a reprieve, a finalization verdict)
  are labelled by the branch's fate. Round-level ones (a board review, a
  critique) are labelled by the test run that gated their round, which is
  the NEXT :verify note after them in the journal. A gate firing is labelled
  by its own settlement."
  [conn run]
  (let [run-id (:id run)
        run-status (str (:status run))
        turns (journal/turns conn run-id)
        fates (branch-fates conn run-id turns)
        verified (latest-ship-verify conn run-id)
        events (db/fetch conn ["SELECT id, branch_id, turn, kind, data FROM events
                                 WHERE run_id = ?
                                   AND kind IN ('critic-score', 'cull-spared', 'critic',
                                                'board-review', 'critique', 'verify')
                                 ORDER BY id"
                               run-id])
        verifies (into []
                       (comp (filter #(= "verify" (:kind %)))
                             (map #(assoc (or (parse-json (:data %)) {}) :id (:id %))))
                       events)
        round-outcome (fn [id]
                        (let [v (some #(when (> (:id %) id) %) verifies)]
                          {:verify-passed? (when v (boolean (:passed v)))
                           :run-status run-status}))
        branch-outcome (fn [branch-id]
                         (merge {:branch-status nil :inactive-reason nil :shipped? false}
                                (get fates branch-id)))
        base (fn [branch-id turn kind]
               {:run-id run-id :branch-id branch-id :turn turn :kind kind})
        gates (for [g (journal/gate-firings conn run-id)]
                (assoc (base (:branch_id g) (:turn g) "gate")
                       :situation {:gate (str (:gate g)) :message (str (:message g))}
                       :verdict {:prediction (str (:prediction g)) :window (:window g)}
                       :outcome {:settled (some-> (:outcome g) str)
                                 :settled-at-turn (:settled_at_turn g)}))
        noted (for [e events
                    :let [d (or (parse-json (:data e)) {})
                          kind (str (:kind e))
                          branch-id (or (not-empty (str (:branch_id e))) (:branch-id d))
                          turn (or (:turn e) (:turn d))]
                    :when (not= "verify" kind)]
                (case kind
                  "critic-score"
                  ;; :data used to be the bare score map; both shapes read.
                  (assoc (base branch-id turn kind)
                         :situation {:summary (:summary d)}
                         :verdict {:scores (or (:scores d) (dissoc d :summary :reply))
                                   :reply (:reply d)}
                         :outcome (branch-outcome branch-id))
                  "cull-spared"
                  (assoc (base branch-id turn kind)
                         :situation d
                         :verdict {:spared? true}
                         :outcome (branch-outcome branch-id))
                  "critic"
                  (assoc (base branch-id turn kind)
                         :situation {:attempt (:attempt d)}
                         :verdict {:verdict (some-> (:verdict d) str)
                                   :blocked? (boolean (:blocked d))
                                   :reason (:reason d)
                                   :findings (:findings d)}
                         :outcome (assoc (branch-outcome branch-id)
                                         :ship-verify-green? (get verified branch-id)))
                  "board-review"
                  (assoc (base branch-id turn kind)
                         :situation {:task (:task d) :attempt (:attempt d)
                                     :landed? (boolean (:landed d))}
                         :verdict {:verdict (some-> (:verdict d) str)
                                   :decision (some-> (:decision d) str)
                                   :reason (:reason d)
                                   :findings (:findings d)}
                         :outcome (round-outcome (:id e)))
                  "critique"
                  (assoc (base branch-id turn kind)
                         :situation {:deterministic? (boolean (:deterministic d))}
                         :verdict {:decision (some-> (:decision d) str)
                                   :verdict (some-> (:verdict d) str)
                                   :reason (:reason d)
                                   :findings (:findings d)}
                         :outcome (round-outcome (:id e)))))]
    (vec (sort-by (fn [r] (or (:turn r) 0)) (concat gates noted)))))

(defn verdicts
  "Every judgement the harness made, across every run whatever its status,
  as `{:run-id :branch-id :turn :kind :situation :verdict :outcome}`, oldest
  run first. The kinds are `gates.edn :export :verdicts :kinds`; `opts` may
  name `:kinds` to override and `:known-values` for the secrets to redact
  (the process environment's by default). Every string is redacted."
  ([conn] (verdicts conn {}))
  ([conn opts]
   (let [kinds (set (or (:kinds opts) (:kinds (:verdicts (policy)))))
         known (or (:known-values opts) (default-known-values))]
     (into []
           (comp (mapcat #(run-verdicts conn %))
                 (filter #(or (empty? kinds) (contains? kinds (:kind %))))
                 (map #(-> %
                           (update :situation redacted known)
                           (update :verdict redacted known)
                           (update :outcome redacted known))))
           (db/fetch conn ["SELECT * FROM runs ORDER BY started_at, id"])))))

;; --- writing -----------------------------------------------------------------

(defn scratch-path
  "A fresh path under the system temp dir, prefixed. The CLI's default output
  when none is named."
  [prefix]
  (str (System/getProperty "java.io.tmpdir") "/" prefix "-" (random-uuid)))

(defn- spit-jsonl!
  "Write `rows` to `path` one JSON object per line and return how many.
  Creates the parent directory; overwrites."
  [path rows]
  (let [rows (vec rows)]
    (some-> (java.io.File. (str path)) .getAbsoluteFile .getParentFile .mkdirs)
    (spit path (str (str/join "\n" (map json/write-str rows))
                    (when (seq rows) "\n")))
    (count rows)))

(defn write-jsonl!
  "Write trajectories to `path` as JSONL — one conversation per line, in the
  messages shape every fine-tuning pipeline reads — and return how many."
  [path rows]
  (spit-jsonl! path (map (fn [r] {:run_id (:run-id r)
                                  :branch_id (:branch-id r)
                                  :role (:role r)
                                  :provider (:provider r)
                                  :model (:model r)
                                  :messages (:messages r)})
                         rows)))

(defn write-verdicts!
  "Write verdicts to `path` as JSONL — one judgement per line — and return
  how many."
  [path rows]
  (spit-jsonl! path (map (fn [r] {:run_id (:run-id r)
                                  :branch_id (:branch-id r)
                                  :turn (:turn r)
                                  :kind (:kind r)
                                  :situation (:situation r)
                                  :verdict (:verdict r)
                                  :outcome (:outcome r)})
                         rows)))

(defn -main
  "jolt -M -m samizdat.export <harness.sqlite3> [out.jsonl] [--verdicts]

  Trajectories by default; `--verdicts` writes the judgements projection.
  Connects WITHOUT migrating — an archived campaign db is read as it is and
  left as it was — since everything read here has been in the schema since
  the first migration."
  [& args]
  (let [flag? #(str/starts-with? (str %) "--")
        flags (set (filter flag? args))
        [db-path out-path] (remove flag? args)
        verdicts? (contains? flags "--verdicts")]
    (if (or (not db-path) (seq (disj flags "--verdicts")))
      (do (println "usage: jolt -M -m samizdat.export <harness.sqlite3> [out.jsonl] [--verdicts]")
          (System/exit 2))
      (let [conn (db/connect db-path)
            out (or out-path
                    (str (scratch-path (if verdicts? "samizdat-verdicts" "samizdat-trajectories"))
                         ".jsonl"))]
        (try
          (let [n (if verdicts?
                    (write-verdicts! out (verdicts conn))
                    (write-jsonl! out (trajectories conn)))]
            (println (str n (if verdicts? " verdicts -> " " trajectories -> ") out)))
          (finally (db/close conn)))
        (System/exit 0)))))
