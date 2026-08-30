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

(ns samizdat.store.knowledge
  "Long-term memory: facts a run keeps beyond its own turns.

  Turns and artifacts are the run's record of what it did; a knowledge row
  is a claim it decided was worth carrying forward. Rows are plain data keyed
  by short ids, same shape as tasks, so anything that can hold an id can point
  at a memory.

  Recall is FTS5-ranked where the loaded libsqlite3 has FTS5, and a LIKE scan
  where it does not. Knowledge is the one table that deliberately outlives
  every run, so it is the one whose row count only goes up — a substring scan
  degrades exactly as the memory becomes worth having. The LIKE path stays
  because `db/fts5-available?` is a real question about the library that
  happens to be loaded, not a formality, and a harness that cannot remember
  is worse than one that remembers slowly."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [samizdat.lexicon :as lexicon]
            [samizdat.memory :as memory]
            [samizdat.store.db :as db]))

(defn- new-id
  "Six hex chars, same scheme as tasks — readable in a transcript, cheap
  to say out loud, retried on the rare collision."
  []
  (str "k-" (subs (str/replace (str (random-uuid)) "-" "") 0 6)))

(defn- index!
  "Mirror a row into knowledge_fts. App-managed, like the other two FTS
  tables. Best effort: a memory that was stored but not indexed is worse to
  lose than to under-rank, so an index failure never fails the write."
  [conn id content]
  (try
    (db/with-writer
      (db/execute! conn
                   ["INSERT INTO knowledge_fts (rowid, content)
                     SELECT rowid, ? FROM knowledge WHERE id = ?"
                    (str content) id]))
    (catch Throwable e
      (log/warn "knowledge: indexing" id "failed:" (ex-message e)))))

(defn- fts-query
  "Free text as an FTS5 OR query. FTS5 treats several characters as
  operators, so raw model prose is not a safe query string — the same
  quoting the failure log and the shared-artifact pool do."
  [text]
  (->> (str/split (str/lower-case (or text "")) #"[^a-z0-9]+")
       (remove str/blank?)
       (filter #(>= (count %) (lexicon/tuning :claim-matching
                                              :min-search-token-length)))
       distinct
       (take 12)
       (map #(str "\"" % "\""))
       (str/join " OR ")))

(defn- recall-like
  "The substring scan. Newest first, because with no ranking the only
  ordering worth having is recency."
  [conn query limit]
  (db/fetch conn
            ["SELECT * FROM knowledge WHERE content LIKE ? AND current = 1
              ORDER BY created_at DESC, id DESC LIMIT ?"
             (str "%" query "%") (long limit)]))

(defn completion-claim?
  "Whether `content` asserts that work is FINISHED.

  The one kind of memory the harness can check against the tree, and the one
  kind that does real damage when it is wrong: memories outlive the run that
  wrote them, so a false one tells every later worker its part is already
  done. `vocab` is wordlists.edn :completion-claim, because which phrases mean
  finished is language, not mechanism."
  [content vocab]
  (let [t (str/lower-case (str content))]
    (boolean (some #(str/includes? t (str %)) vocab))))

(defn remember!
  "Insert a fact and return its id. Kind defaults to 'note' — the column
  exists so later kinds (decisions, gotchas, references) need no migration."
  [conn {:keys [content kind salience confidence run-id pinned pattern-key
                cause supersedes lineage-id]}]
  (when (str/blank? (str content))
    (throw (ex-info "a memory needs content" {})))
  (let [kind (or kind "note")
        p (memory/policy)
        ;; Kind decides the starting importance unless the caller overrides it.
        ;; A memory arrives with a standing, not at zero: what it is worth is
        ;; then moved by whether it gets used and whether acting on it works.
        salience (double (or salience (memory/base-salience kind p)))
        confidence (double (or confidence (:default-confidence p)))
        now (db/now)]
    (loop [attempt 1]
      (let [id (new-id)
            n (try
                (db/with-writer
                  (db/execute! conn
                               ;; `last_run_id` is set AT CREATION, not left for the first
                   ;; corroboration to fill in. A memory formed during run r1
                   ;; has already been seen by r1 — leaving it null made the
                   ;; first corroborate! inside that same run look like a
                   ;; second distinct sighting and count it, which is exactly
                   ;; the double-count the distinct-run rule exists to prevent.
                   ;; `lineage_id` defaults to the row's own id: a new memory
                   ;; is its own lineage, which is what every pre-migration row
                   ;; was backfilled to. `cause` is why it was written — see
                   ;; the v20 note; the model can only act on what it knows
                   ;; about, and a belief with no recorded origin cannot be
                   ;; reconsidered later, only deleted.
                   ["INSERT INTO knowledge (id, content, kind, created_at,
                                             salience, confidence, run_id, pinned,
                                             last_run_id, pattern_key,
                                             lineage_id, current, cause, supersedes)
                                 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?, ?)"
                                id (str content) kind now
                                salience confidence run-id (if pinned 1 0)
                                run-id pattern-key
                                (or lineage-id id) cause supersedes]))
                1
                (catch Exception e
                  ;; Only a UNIQUE collision is an id problem; anything else
                  ;; (disk, lock) must surface as itself (provenance R2-15).
                  (if (db/id-collision? e) 0 (throw e))))]
        (if (pos? n)
          (do (index! conn id content) id)
          (if (< attempt 5)
            (recur (inc attempt))
            (throw (ex-info "could not allocate a knowledge id" {}))))))))

(defn touch!
  "Record that these memories were USED: bump the count, stamp the time, and
  reinforce the salience.

  Being looked up IS the relevance signal — the cheapest and most honest one
  available, because it needs nobody to judge anything. Best effort: a failure
  to record a use must never fail the recall it came from, or asking what you
  know would be riskier than not asking."
  [conn ids]
  (when (seq ids)
    (try
      (let [p (memory/policy)
            now (db/now)]
        (db/with-writer
          (doseq [id ids]
            (db/execute! conn
                         ["UPDATE knowledge
                             SET use_count = use_count + 1,
                                 last_used_at = ?,
                                 salience = MIN(?, salience + ?)
                           WHERE id = ?"
                          now (:salience-cap p) (:use-reinforcement p) id]))))
      (catch Throwable e
        (log/warn "knowledge: recording use failed:" (ex-message e))))))

(defn record-outcome!
  "Report that acting on a memory worked, or did not.

  This is the axis that makes memory a loop rather than a list. Everything
  else — kind, use, recency — measures whether a memory gets READ; only this
  measures whether reading it HELPED. A memory nobody reports on is not
  penalised: an empty record contributes nothing either way, because most
  memories are never reported on and treating silence as failure would decay
  the whole store toward the few that happen to get graded."
  [conn id worked?]
  (db/with-writer
    (db/execute! conn
                 [(if worked?
                    "UPDATE knowledge SET success_count = success_count + 1 WHERE id = ?"
                    "UPDATE knowledge SET failure_count = failure_count + 1 WHERE id = ?")
                  id])))

(defn recall
  "Memories matching `query`, most worth reading first.

  TWO stages, and they answer different questions. FTS5 (or a LIKE scan) picks
  the candidates — is this memory ABOUT what was asked. Effective salience
  orders them — is this memory worth acting on: how important it is, whether
  it has been used lately, whether acting on it has worked, and how sure we
  are it is true. Text relevance alone would rank the note nobody has ever
  needed level with the one that saved three runs.

  Recall REINFORCES what it returns. That is deliberate and it is the whole
  feedback loop in one line: a memory that keeps answering questions climbs,
  and one that never surfaces decays.

  Falls back on a thrown query too, not only on a missing extension: a query
  the tokenizer rejects must cost a worse ranking, never an error — recall is
  on the path of a tool the model calls to orient itself, and an exception
  there costs the turn."
  ([conn query] (recall conn query (:recall-limit (memory/policy))))
  ([conn query limit]
   (let [q (fts-query query)
         ;; A wider candidate net than the caller asked for, because the
         ;; ranking that matters happens after: taking the top `limit` by TEXT
         ;; and then re-sorting those by standing would only re-order whatever
         ;; the text search happened to like.
         n (* 3 (long limit))
         candidates
         (or (when (and (seq q) (db/fts5-available? conn))
               (try
                 (seq (db/fetch conn
                                ["SELECT k.* FROM knowledge_fts fts
                                  JOIN knowledge k ON k.rowid = fts.rowid
                                  WHERE knowledge_fts MATCH ? AND k.current = 1
                                  ORDER BY bm25(knowledge_fts) LIMIT ?"
                                 q n]))
                 (catch Throwable e
                   (log/warn "knowledge/recall fell back to LIKE:" (ex-message e))
                   nil)))
             (recall-like conn query n))
         ranked (take limit (memory/rank candidates))]
     (touch! conn (map :id ranked))
     (vec ranked))))

(defn get-by-id
  "The single knowledge row for id, or nil when there is no such memory."
  [conn id]
  (first (db/fetch conn ["SELECT * FROM knowledge WHERE id = ? LIMIT 1" id])))

(def ^:private preamble
  "Memories you kept earlier (still active; fetch the full text with the recall tool by id):")

(defn corroborate!
  "Record that `run-id` also saw this memory's pattern, and return the new
  corroboration count.

  DISTINCT runs only. A second sighting inside the same run confirms nothing —
  it is the same evidence counted twice — and without that guard a long run
  would corroborate its own findings by repetition, which is precisely the
  overfitting the count exists to prevent."
  [conn id run-id]
  (let [row (get-by-id conn id)]
    (if (and run-id (not= run-id (:last_run_id row)))
      (do (db/with-writer
            (db/execute! conn
                         ["UPDATE knowledge
                             SET corroborations = corroborations + 1, last_run_id = ?
                           WHERE id = ?"
                          run-id id]))
          (inc (or (:corroborations row) 1)))
      (or (:corroborations row) 1))))

(defn- unindex!
  "Drop a row from the FTS mirror so it stops being recallable, leaving the row
  itself alone. Best effort, like `index!`."
  [conn id]
  (try
    (db/with-writer
      (db/execute! conn ["DELETE FROM knowledge_fts
                           WHERE rowid IN (SELECT rowid FROM knowledge WHERE id = ?)" id]))
    (catch Throwable e
      (log/warn "knowledge: unindexing" id "failed:" (ex-message e)))))

(defn retire!
  "Mark a memory no longer true, with the reason. Returns the id, or nil when
  there was nothing current to retire.

  THE CASE `restate!` CANNOT COVER, and the one a disproven premise actually
  produces: this was wrong and NOTHING replaces it. `the defect is in src/`
  had to become false with nothing taking its place — the 238-turn run did not
  need that belief edited into a different one, it needed it withdrawn.

  The row stays. A retired memory is evidence: what we thought, what made us
  think it (`cause`), and why it stopped being true. Deleting it would leave
  the next run to rediscover the same wrong idea with nothing to warn it, which
  is what `forget!` is for when a memory is merely noise rather than refuted."
  [conn id {:keys [reason]}]
  (let [row (get-by-id conn id)]
    (when (and row (= 1 (:current row)))
      (db/with-writer
        (db/execute! conn
                     ["UPDATE knowledge SET current = 0, retired_at = ?, retired_reason = ?
                        WHERE id = ? AND current = 1"
                      (db/now) reason id]))
      ;; Out of the search index, so recall stops handing it back as something
      ;; to act on. Still fetchable by id and through `history`.
      (unindex! conn id)
      id)))

(defn restate!
  "Record a NEW version of a memory and retire the old one. Returns the new
  row's id, or nil when the text did not change.

  For a memory that describes a moving target rather than a standing fact.
  The project overview is the one: it says what the codebase IS, and a run
  that adds a namespace makes the previous wording wrong without making the
  memory wrong to hold.

  THIS USED TO REWRITE THE ROW IN PLACE, which karamazov-1sy's own design
  forbids and karamazov-oov is about: you cannot retract what you overwrote.
  Once the previous wording was gone there was no way to ask what we believed
  or what made us believe it, so a premise that turned out false left no trace
  and everything downstream of it kept standing.

  The lineage is what the old id bought: `lineage_id` carries across every
  version, so `history` still answers how often this project has been
  described, and a reader citing an old id still finds the row it cited."
  ([conn id content] (restate! conn id content {}))
  ([conn id content {:keys [reason cause]}]
   (let [row (get-by-id conn id)]
     (when (and row (not= (str content) (str (:content row))))
       (let [lineage (or (:lineage_id row) id)
             new-id (remember! conn {:content content
                                     :kind (:kind row)
                                     :salience (:salience row)
                                     :confidence (:confidence row)
                                     :run-id (:run_id row)
                                     :pinned (= 1 (:pinned row))
                                     :pattern-key (:pattern_key row)
                                     :lineage-id lineage
                                     :supersedes id
                                     :cause cause})]
         (db/with-writer
           ;; THE RECORD MOVES WITH THE LINEAGE. Rewriting in place kept the
           ;; use count and corroborations for free; a new row starts at zero,
           ;; and letting it would mean a project that gets re-described every
           ;; run looks freshly observed every run — the exact double-count the
           ;; distinct-run rule exists to prevent, inverted. The subject is the
           ;; same subject; only its wording moved.
           (db/execute! conn
                        ["UPDATE knowledge
                            SET corroborations = ?, use_count = ?, last_used_at = ?,
                                success_count = ?, failure_count = ?, last_run_id = ?
                          WHERE id = ?"
                         (or (:corroborations row) 1) (or (:use_count row) 0)
                         (:last_used_at row)
                         (or (:success_count row) 0) (or (:failure_count row) 0)
                         (:last_run_id row) new-id])
           (db/execute! conn
                        ["UPDATE knowledge SET current = 0, retired_at = ?, retired_reason = ?
                           WHERE id = ?"
                         (db/now) (or reason "superseded") id]))
         (unindex! conn id)
         new-id)))))

(defn history
  "Every version in this memory's lineage, newest first — including retired
  ones, which is the point. Answers what we used to believe and why it changed."
  [conn id]
  (let [row (get-by-id conn id)
        lineage (or (:lineage_id row) id)]
    (vec (db/fetch conn
                   ["SELECT * FROM knowledge WHERE lineage_id = ?
                      ORDER BY created_at DESC, id DESC"
                    lineage]))))

(defn live-count
  "How many memories are current — the number that makes a miss actionable.
  \"No match, and 40 things are recorded\" says try other words; \"no match\"
  alone says nothing."
  [conn]
  (or (:n (first (db/fetch conn ["SELECT COUNT(*) AS n FROM knowledge WHERE current = 1"])))
      0))

(defn recall-status
  "Which KIND of nothing a recall found: `:hits`, `:no-match`, or `:empty`.

  Lemmalog's measured failure mode is that extraction gaps produce SILENCE,
  and silence is indistinguishable from absence — the agent cannot tell `this
  was never written down` from `your query missed`. The two call for opposite
  actions (write it down; search again), so they must not read the same. Same
  argument as the standing discoverability rule, applied to recall.

  A store holding only RETIRED memories is `:empty`, not `:no-match`: there is
  nothing live to have matched, and telling the model to refine its query
  would be a lie."
  [conn query]
  (if (seq (recall conn query))
    :hits
    (if (pos? (live-count conn)) :no-match :empty)))

(defn corroborated?
  "Whether a memory has been seen in enough distinct runs to act on.

  One run is an observation; the bar for calling it a pattern is
  `gates.edn :corroboration :min-runs`. Below it the memory is still worth
  READING — it may be the only warning anyone gets — but it is not evidence
  for retuning the loop."
  [row]
  (>= (or (:corroborations row) 1)
      (:min-runs (lexicon/policy :corroboration))))

(defn by-pattern
  "The memory recording `pattern-key`, or nil.

  One indexed lookup. This replaced a scan of every row of a kind followed by
  a string-prefix match on the content — text identity, which is what you do
  when your memory is a file and a claim has no id. A row has a key.

  THE CURRENT ONE. A key names a subject, and since karamazov-oov a subject
  can have several versions of itself: `LIMIT 1` with no filter was fine while
  a restate rewrote the row, and became a coin flip the moment restating
  started retiring the old one. Picking the retired row would send every later
  corroboration to a memory nobody can recall and freeze the live one — the
  project overview would stop being updated and nothing would say why."
  [conn pattern-key]
  (db/fetch-one conn ["SELECT * FROM knowledge WHERE pattern_key = ? AND current = 1
                        ORDER BY created_at DESC, id DESC LIMIT 1"
                      pattern-key]))

(defn- lever-key
  "A stable identity for a lever, from the change the supervisor described.

  Normalised, because the description is prose a model writes fresh each time:
  `beam width 5 -> 2` and `beam-width 5→2` are the same lever and must not
  accumulate two records. Lowercased, punctuation and whitespace collapsed —
  crude, and enough to survive rewording that does not change the subject."
  [change]
  (str "lever:" (-> (str change)
                    str/lower-case
                    (str/replace #"[^a-z0-9]+" "-")
                    (str/replace #"^-+|-+$" ""))))

(defn distill!
  "Turn a session's findings into long-term memories, and return what was
  written.

  THE BRIDGE FROM SHORT-TERM TO LONG-TERM. The session tally is what is
  happening now, in memory, and it dies with the process; a pattern that held
  across a whole session is a candidate for something the NEXT session should
  start out knowing. Distillation is how the second becomes the first.

  Two rules keep this from silting the store up:

  A finding is written once. The same pattern recurring is not new knowledge —
  it is the same knowledge, confirmed — so a matching memory has its record
  updated rather than a duplicate added. That is what makes recurrence show up
  as CONFIDENCE (a memory reported on repeatedly) instead of as volume.

  Findings are `episodic`, not `procedural`. What a session measured is a
  thing that happened, not a rule; turning `this run's calls kept failing to
  parse` into a standing rule is exactly the overreach that makes an automatic
  memory writer worse than none. Promoting an episode to a rule is a judgement,
  and judgement is the supervisor's — it has `remember` for that.

  Successes are distilled too. A store that only remembers what went wrong
  teaches the next session that everything is broken."
  [conn findings {:keys [run-id]}]
  (vec
   (for [{:keys [kind severity detail evidence]} findings
         :let [content (str "[" (name kind) "] " detail " " (pr-str evidence))
               ;; Identity is the finding's KIND, in a column. The evidence
               ;; differs every run and the pattern is what recurs.
               pattern (str "finding:" (name kind))
               existing (by-pattern conn pattern)]]
     (if existing
       ;; A recurring finding is a RE-OBSERVATION, and corroborate! is its
       ;; record. It is NOT an outcome: `record-outcome! (= :good severity)`
       ;; here conflated "this finding is bad news" with "acting on this
       ;; memory failed", so every re-observation of a persistent problem
       ;; added a failure_count and the most persistent problems progressively
       ;; ranked below trivia (karamazov-blt.25). Only acting on a memory
       ;; earns it an outcome — the verdict path below.
       ;;
       ;; The content refresh goes through restate! so the FTS mirror follows
       ;; the wording — the raw UPDATE left the memory recallable only by its
       ;; FIRST phrasing (same bead).
       ;;
       ;; CORROBORATE FIRST, THEN RESTATE, and the order is load-bearing since
       ;; karamazov-oov: restating retires the old row and opens a new one, so
       ;; a corroboration recorded afterwards lands on the retired version and
       ;; the live memory stays at one sighting forever. `restate!` carries the
       ;; record onto the new row, so counting first is what makes it survive.
       (let [corroborations (corroborate! conn (:id existing) run-id)
             new-id (or (restate! conn (:id existing) content) (:id existing))]
         (db/with-writer
           (db/execute! conn ["UPDATE knowledge SET run_id = ? WHERE id = ?"
                              run-id new-id]))
         {:id new-id :kind kind :repeat? true
          :corroborations corroborations})
       {:id (remember! conn {:content content :kind "episodic" :run-id run-id
                             :pattern-key pattern
                             ;; A measured pattern is better evidenced than a
                             ;; typed-in note, and worse than a rule somebody
                             ;; concluded. Started slightly above the default
                             ;; and left to earn the rest.
                             :confidence 0.7})
        :kind kind :repeat? false :corroborations 1}))))

(defn distill-verdicts!
  "Write what each experiment concluded into long-term memory, and return what
  was written.

  THE HEREDITY OF SELECTION, and without it the loop has none. An experiment
  lives in the session tally and dies with the process, so a lever that was
  tried and made things worse is forgotten by the next session, which is free
  to try it again — and will, because it looked like a good idea the first
  time too. Variation and measurement without inheritance is not evolution, it
  is thrashing with statistics.

  Written as `procedural`, unlike a session finding. This is the one case where
  a rule IS the right shape: a finding says `this run's calls kept failing to
  parse`, which is an episode, while a verdict says `changing X did not move
  the fitness`, which is a fact about a LEVER and holds beyond the run that
  discovered it.

  The verdict becomes the memory's RECORD, not just its text: `better` is a
  success and `worse` or `unchanged` a failure, so a lever tried repeatedly
  without result accumulates a negative record and sinks in the ranking on its
  own. `too-early` is not written at all — an unfinished experiment has
  concluded nothing, and recording it would teach the next session that the
  lever was tested when it was not."
  [conn experiments {:keys [run-id]}]
  (vec
   (for [{:keys [name change hypothesis verdict before after]} experiments
         :when (and change (not= :too-early verdict))
         :let [pattern (lever-key change)
               content (str "[lever] " change " — " (clojure.core/name verdict)
                            (when (and before after)
                              (format " (fitness %.2f -> %.2f)" before after))
                            ". Expected: " hypothesis)
               existing (by-pattern conn pattern)
               worked? (= :better verdict)]]
     (if existing
       ;; Here the outcome IS earned: the lever was pulled and measured.
       ;; Content refresh through restate! so the FTS mirror follows the new
       ;; wording (karamazov-blt.25).
       (do (record-outcome! conn (:id existing) worked?)
           (restate! conn (:id existing) content)
           (db/with-writer
             (db/execute! conn ["UPDATE knowledge SET run_id = ? WHERE id = ?"
                                run-id (:id existing)]))
           {:id (:id existing) :lever change :verdict verdict :repeat? true})
       (let [id (remember! conn {:content content :kind "procedural" :run-id run-id
                                 :pattern-key pattern :confidence 0.7})]
         (record-outcome! conn id worked?)
         {:id id :lever change :verdict verdict :repeat? false})))))

(defn curate!
  "Decay the salience of memories that have gone unused, and return how many
  moved.

  CURATION, which nothing did. `memory/decayed` existed from the first day of
  the salience model and was never called from anywhere — so salience only ever
  climbed, every recall reinforced, and nothing ever fell. A ranking where
  everything rises is a ranking that stops discriminating: after enough runs
  the top of the list is whatever was written earliest, not what is worth
  reading.

  Skips the pinned, and skips anything used inside the window — being looked up
  recently is exactly the signal that it should not decay. Floored rather than
  allowed to reach zero: a memory that decayed to nothing would be
  indistinguishable from one that was never important, and `this WAS worth
  writing down and has not been needed since` is a different fact worth
  keeping."
  [conn]
  (let [p (memory/policy)
        cutoff (str (.minusSeconds (java.time.Instant/now)
                                   (* 86400 (long (:recent-use-window-days p)))))
        stale (db/fetch conn
                        ["SELECT id, salience FROM knowledge
                           WHERE current = 1
                             AND pinned = 0
                             AND (last_used_at IS NULL OR last_used_at < ?)
                             AND salience > ?"
                         cutoff (:decay-floor p)])]
    (db/with-writer
      (doseq [{:keys [id salience]} stale]
        (db/execute! conn ["UPDATE knowledge SET salience = ? WHERE id = ?"
                           (memory/decayed salience p) id])))
    (count stale)))

(defn- fact
  "One derived project memory, rendered from its template in gates.edn
  :project-facts. A plain `{{cmd}}` substitution rather than a selmer render:
  the store sits below the prompt layer, and one placeholder does not justify
  reaching up through it."
  [k cmd]
  (-> (get (lexicon/policy :project-facts) k)
      (str/replace "{{cmd}}" (str cmd))))

(defn distil-project!
  "Write what this run DISCOVERED ABOUT THE PROJECT into long-term memory.

  The gap this closes: every other distillation here is about the HARNESS —
  patterns in how the loop ran, verdicts on changes the supervisor made. None
  of it is about the codebase being worked on, so an implementor started every
  session knowing nothing about the project and spent its first turns finding
  out again where the source lives, how to run the tests, and which commands
  the policy will refuse. Measured across the live runs: 46 turns, zero
  `remember` calls, zero memories.

  Derived from what the run DID, not from asking a model to summarise. A shell
  command that exited zero is a fact about this project — that command works
  here — and it needed no judgement to discover and needs none to record. A
  command the policy refused is the same kind of fact from the other side, and
  worth more than it looks: it saves the next run the turn it would spend
  learning the same refusal.

  `semantic`, because these are durable facts about the project rather than
  episodes of a run: the test command does not stop being the test command
  because this run ended. Pattern-keyed on the command, so a fact is written
  once and corroborated across runs rather than duplicated."
  [conn {:keys [run-id]}]
  (let [rows (db/fetch conn
                       ["SELECT tool_name, args, result, category FROM turns
                          WHERE run_id = ? AND tool_name = ? ORDER BY turn"
                        run-id "shell"])
        command-of (fn [r] (try (some-> (json/read-str (str (:args r)) :key-fn keyword)
                                        :command str str/trim not-empty)
                                (catch Throwable _ nil)))
        facts (->> rows
                   (keep (fn [r]
                           (when-let [cmd (command-of r)]
                             (cond
                               (= "success" (:category r))
                               {:key (str "cmd-works:" cmd) :content (fact :cmd-works cmd)}

                               (re-find (re-pattern (lexicon/wordlist :shell-refusal))
                                        (str (:result r)))
                               {:key (str "cmd-refused:" cmd) :content (fact :cmd-refused cmd)}))))
                   distinct)]
    (vec
     (for [{:keys [key content]} facts
           :let [existing (by-pattern conn key)]]
       (if existing
         (do (corroborate! conn (:id existing) run-id)
             {:id (:id existing) :fact key :repeat? true})
         {:id (remember! conn {:content content :kind "semantic" :run-id run-id
                               :pattern-key key :confidence 0.8})
          :fact key :repeat? false})))))

(defn record-workflow-outcome!
  "Note that `workflow` drove a run on this project and whether it shipped.

  ONE ROW PER WORKFLOW, pattern-keyed, with the success and failure counts
  doing the accumulating — the same axis a memory earns its salience on. After
  a few runs the store holds a fitness signal on the workflow CHOICE itself:
  the factory loop went 0 for 4 on one project while decompose went 1 for 1,
  and that is precisely the evidence a run should have before deciding how to
  drive itself.

  This is what makes decompose-on-stuck real at the granularity the harness
  operates at. The recursive solver inside the decompose manifest already
  splits a unit that will not pass its tests; what nothing did was notice that
  DIRECT ATTEMPTS ON THIS PROJECT KEEP GETTING STUCK and choose differently
  next time. A run cannot learn that within itself — it only ever sees its own
  attempt — so it has to be written down for the next one.

  Best effort: a failure to record how a run went must not change how it went."
  [conn {:keys [workflow run-id shipped?]}]
  (try
    (when-not (str/blank? (str workflow))
      (let [key (str "workflow:" workflow)
            row (by-pattern conn key)
            id (if row
                 (do (corroborate! conn (:id row) run-id) (:id row))
                 (remember! conn {:content (fact :workflow workflow)
                                  :kind "procedural" :run-id run-id
                                  :pattern-key key :confidence 0.5}))]
        (record-outcome! conn id (boolean shipped?))
        id))
    (catch Throwable e
      (log/warn "recording the workflow outcome failed:" (ex-message e))
      nil)))

(defn workflow-record
  "What this project knows about how each workflow has gone: a seq of
  {:workflow :shipped :failed :runs}, best first.

  Read by samizdat.agent.select, so a run choosing how to drive itself sees
  the evidence rather than only the problem text."
  [conn]
  (->> (db/fetch conn ["SELECT pattern_key, success_count, failure_count, corroborations
                          FROM knowledge WHERE pattern_key LIKE 'workflow:%' AND current = 1"])
       (keep (fn [r]
               (when-let [nm (second (str/split (str (:pattern_key r)) #":" 2))]
                 {:workflow nm
                  :shipped (or (:success_count r) 0)
                  :failed (or (:failure_count r) 0)
                  :runs (+ (or (:success_count r) 0) (or (:failure_count r) 0))})))
       (sort-by (juxt (comp - :shipped) :failed))
       vec))

(defn distil-session!
  "Write everything this session concluded into long-term memory: the patterns
  it measured, and the verdicts on the changes the supervisor made.

  ONE function both drivers call, because it was in only one of them. The beam
  distilled at run end and `workflow/run!` — the single-branch driver, which is
  what the factory loop uses and therefore what most runs are — did not. A live
  run completed, produced a finding, and formed no memory at all: the short-term
  half worked, the long-term half was never reached, and nothing said so. The
  bridge existing in one driver is the same as not existing, for every run that
  uses the other.

  Best effort in both halves: a failure to remember must never turn a finished
  run into a failed one."
  [conn {:keys [run-id findings experiments]}]
  (let [written (when (seq findings) (distill! conn findings {:run-id run-id}))
        verdicts (when (seq experiments)
                   (distill-verdicts! conn experiments {:run-id run-id}))
        ;; And what the run learned about the PROJECT, which is the half that
        ;; was missing entirely: everything else here is the harness watching
        ;; itself.
        project (try (distil-project! conn {:run-id run-id})
                     (catch Throwable _ nil))
        ;; And curation, so the store does not become a ranking in which
        ;; everything has risen.
        decayed (try (curate! conn) (catch Throwable _ 0))]
    {:findings (vec written) :verdicts (vec verdicts) :project (vec project)
     :decayed decayed}))

(defn standing
  "The memories with the highest standing, whatever they are about — what this
  project has LEARNED, as opposed to what it happens to be doing.

  Query-free on purpose. A supervisor deciding whether the loop needs adjusting
  does not have a search term; it has a situation, and what it needs is the
  handful of things previous runs concluded that are still worth acting on. The
  ranking is the whole answer to `which handful`.

  The `overview` memory comes first when there is one — it is the project
  orientation, and a reader who does not know what the project IS cannot judge
  anything else in the list.

  Does NOT reinforce. Being shown to a supervisor by default is not evidence
  that a memory was useful, and counting it would inflate exactly the entries
  that are already at the top."
  ([conn] (standing conn (:recall-limit (memory/policy))))
  ([conn limit]
   (let [rows (db/fetch conn ["SELECT * FROM knowledge WHERE current = 1 ORDER BY salience DESC LIMIT ?"
                              (long (* 3 limit))])
         {overviews true others false} (group-by #(= "overview" (:kind %)) rows)]
     (vec (take limit (concat (memory/rank overviews) (memory/rank others)))))))

(defn recent
  "The latest n memories regardless of content — what the run has been
  keeping lately, for orienting without a search term."
  [conn n]
  (db/fetch conn
            ["SELECT * FROM knowledge WHERE current = 1
              ORDER BY created_at DESC, id DESC LIMIT ?"
             (long n)]))

(defn forget!
  "Delete a memory by id. Memories are cheap to re-record if a fact turns
  out wrong, so deletion is total and returns the row count removed.

  The index row goes FIRST and by rowid, while the row it points at still
  exists to be found. Reversed, the delete would leave an orphan in
  knowledge_fts that recall would keep joining against nothing — a memory
  that is gone but still costs a result slot."
  [conn id]
  (db/with-writer
    (try
      (db/execute! conn
                   ["DELETE FROM knowledge_fts
                     WHERE rowid IN (SELECT rowid FROM knowledge WHERE id = ?)"
                    id])
      (catch Throwable e
        (log/warn "knowledge: unindexing" id "failed:" (ex-message e))))
    (db/execute! conn ["DELETE FROM knowledge WHERE id = ?" id])))

(defn- preview
  "Content flattened to one line and cut at n chars, ellipsized only
  when something was actually cut."
  [content n]
  (let [flat (str/trim (str/replace (str content) #"\s+" " "))]
    (if (> (count flat) n)
      (str (subs flat 0 n) "...")
      flat)))

(defn- index-line
  "id, kind in brackets, preview — everything needed to decide whether
  to dereference this id, and nothing more."
  [{:keys [id kind content]}]
  (str id " [" (or kind "note") "] " (preview content 70)))

(defn- fit-lines
  "Keep as many whole lines as fit beside the preamble under cap. A line
  that would overflow the budget is dropped entire, never cut mid-line,
  so the index is bounded by construction."
  [preamble cap lines]
  (loop [kept [] [l & more] lines]
    (let [candidate (str/join "\n" (cond-> (cons preamble kept) l (conj l)))]
      (cond
        (nil? l) kept
        (<= (count candidate) cap) (recur (conj kept l) more)
        :else kept))))

(defn breadcrumb-index
  "A bounded index of kept memories as a string, or nil when there are
  none. One line per memory — id, kind, ~70-char preview — so the index
  is cheap enough to sit in every turn's context while full text is
  fetched on demand by id (stub-and-expand). A non-blank query ranks the
  rows via recall; a blank query falls back to the most recent. The
  whole string is hard-capped so it never becomes a content dump."
  ([conn query] (breadcrumb-index conn query {}))
  ([conn query {:keys [rows cap] :or {rows 8 cap 700}}]
   (let [picked (if (str/blank? query)
                  ;; `standing`, not `recent`. With no claim to rank against,
                  ;; the question is `what does this project know that is worth
                  ;; knowing`, and recency answers a different one — it surfaces
                  ;; whatever was written last, which on a busy run is this
                  ;; run's own working notes. Everything the salience model
                  ;; knows about importance, use and outcome was being ignored
                  ;; on exactly the turns where a branch has no lead to follow
                  ;; and most needs orienting.
                  (standing conn rows)
                  (recall conn query rows))]
     (when (seq picked)
       (str/join "\n" (cons preamble (fit-lines preamble cap (map index-line picked))))))))
