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
  (karamazov-5ge3)."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
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

(defn- green-branches
  "Branches whose LATEST ship verification on this run was green. Latest,
  because a branch can go red and then green, and it is the last word that
  says whether what it shipped was verified.

  Read off the events table rather than through `journal/notes`: a note
  comes back as its data map alone, and the branch is a column beside it."
  [conn run-id]
  (->> (db/fetch conn ["SELECT branch_id, data FROM events
                         WHERE run_id = ? AND kind = 'ship-verify' ORDER BY id"
                       run-id])
       (reduce (fn [m {:keys [branch_id data]}]
                 (let [d (try (json/read-str (str data) :key-fn keyword)
                              (catch Throwable _ nil))]
                   (assoc m branch_id (boolean (:green d)))))
               {})
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

(defn scratch-path
  "A fresh path under the system temp dir, prefixed. The CLI's default output
  when none is named."
  [prefix]
  (str (System/getProperty "java.io.tmpdir") "/" prefix "-" (random-uuid)))

(defn write-jsonl!
  "Write trajectories to `path` as JSONL — one conversation per line, in the
  messages shape every fine-tuning pipeline reads — and return how many.
  Creates the parent directory; overwrites."
  [path rows]
  (some-> (java.io.File. (str path)) .getAbsoluteFile .getParentFile .mkdirs)
  (spit path
        (str (str/join "\n"
                       (map (fn [r]
                              (json/write-str {:run_id (:run-id r)
                                               :branch_id (:branch-id r)
                                               :role (:role r)
                                               :provider (:provider r)
                                               :model (:model r)
                                               :messages (:messages r)}))
                            rows))
             (when (seq rows) "\n")))
  (count rows))

(defn -main
  "jolt -M -m samizdat.export <harness.sqlite3> [out.jsonl]

  Connects WITHOUT migrating — an archived campaign db is read as it is and
  left as it was — since everything read here has been in the schema since
  the first migration."
  [& [db-path out-path]]
  (if-not db-path
    (do (println "usage: jolt -M -m samizdat.export <harness.sqlite3> [out.jsonl]")
        (System/exit 2))
    (let [conn (db/connect db-path)
          out (or out-path (str (scratch-path "samizdat-trajectories") ".jsonl"))]
      (try
        (let [n (write-jsonl! out (trajectories conn))]
          (println (str n " trajectories -> " out)))
        (finally (db/close conn)))
      (System/exit 0))))
