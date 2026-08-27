;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.telemetry
  "A compact health digest of a run, for the supervisor to introspect on. What
  each part of the loop did and how it landed — worker outcomes, per-branch
  turn/thrash counts, the loop's mechanics (parse-error / no-call) rate, the
  review and critic decisions, the revision history — rendered as text the
  supervisor reads to diagnose what is suboptimal and decide what to tune.

  Pure over already-extracted facts + journal rows, so it is testable without a
  run. The supervisor is a general reasoning agent; this only gives it eyes."
  (:require [clojure.string :as str]
            [samizdat.agent.gates :as gates]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]))

(defn- s [x] (when x (str/lower-case (str (if (keyword? x) (name x) x)))))

(defn failure-exemplars
  "The concrete failures behind the rates, newest last, each carrying its
  journal row id so `fetch_turn` can pull the full record.

  A rate tells the supervisor something is wrong; the exemplar tells it
  WHAT. The live session that motivated this (2026-08-27) found a parser
  bug and a context-overflow retry loop by reading the actual failing rows
  — the parse error's own complaint, the provider's own words — none of
  which the digest carried. The supervisor's job is exactly that
  read-the-failure-then-fix-the-cause loop, so the digest now leads with
  the failures. Three kinds, because their fixes live in different places:
  a call that did not parse is a format/prompt problem, a provider failure
  is an endpoint problem, a tool failure is the work itself going wrong.

  Returns DATA — {:parse :provider :tool}, each {:count n :lines s} or
  absent — and the run-health template owns the words around it, so the
  section headings the supervisor reads are prompts/ prose like everything
  else it reads. Pure over the rows; caps are gates.edn :supervisor-digest
  policy."
  [rows {:keys [per-kind chars]}]
  (let [snip (fn [x] (let [t (str/replace (str x) #"\s+" " ")]
                       (if (> (count t) chars)
                         (str (subs t 0 chars) "…")
                         t)))
        line (fn [r note]
               (str "- turn " (:turn r) " (" (:branch_id r) ", row " (:id r)
                    (when-let [t (:tool_name r)] (str ", " t)) "): "
                    (snip note)))
        kind (fn [lines]
               (when (seq lines)
                 {:count (count lines)
                  :lines (str/join "\n" (take-last per-kind lines))}))
        parse (for [r rows :when (= "__parse_error__" (:tool_name r))]
                (line r (or (not-empty (str (:parse_error r))) (:result r))))
        provider (for [r rows :when (= "__provider_error__" (:tool_name r))]
                   (line r (:result r)))
        tool (for [r rows
                   :when (and (= "failure" (str (:category r)))
                              (not (str/starts-with? (str (:tool_name r)) "__")))]
               (line r (:result r)))
        m (cond-> {}
            (kind parse) (assoc :parse (kind parse))
            (kind provider) (assoc :provider (kind provider))
            (kind tool) (assoc :tool (kind tool)))]
    (not-empty m)))

(defn branch-health
  "Per-branch health from journal turn rows: turns taken, how many were
  mechanics (a no-call or parse-repair — the loop spinning without acting), and
  whether the branch ever shipped a `done`. The mechanics rate is the clearest
  thrash signal — a branch burning turns on empty/mis-parsed calls."
  [rows]
  (->> (group-by :branch_id rows)
       (map (fn [[b rs]]
              (let [n (count rs)
                    mech (count (filter #(= "mechanics" (s (:category %))) rs))]
                [b {:turns n
                    :mechanics mech
                    :mechanics-rate (if (pos? n) (/ (double mech) n) 0.0)
                    :shipped? (boolean (some #(= "done" (s (:tool_name %))) rs))}])))
       (into (sorted-map))))

(defn- health-policy []
  (lexicon/policy :run-health))

(defn- signal
  "One run-health sentence, rendered from gates.edn `:run-health :signals`."
  ([k] (signal k {}))
  ([k ctx]
   (prompt/render-str (get-in (health-policy) [:signals k]) ctx)))

(defn signals
  "The suboptimality flags the digest calls out explicitly, so the supervisor
  does not have to re-derive the obvious: a stage crashed, nothing shipped, a
  thrashing branch, the reviewer bouncing the work, a run deep into revisions.

  The CONDITIONS are here and the SENTENCES are not. Which facts about a run
  deserve the supervisor's attention is mechanism — it is the same question
  whatever the project builds — while the words that carry them, and the two
  numbers behind the thrash judgement, are `gates.edn :run-health`."
  [{:keys [results review revision errors hollow? tests-passed? verify-note
           at-cap? soft-cap]} health]
  (let [total (count results)
        shipped (count (filter #(= :done (:status %)) results))
        {:keys [thrash-min-turns thrash-mechanics-rate]} (health-policy)]
    (cond-> []
      (seq errors)
      (into (map #(signal :stage-crashed {:detail %}) errors))

      hollow?
      (conj (signal :hollow))

      (and (pos? total) (zero? shipped))
      (conj (signal :nobody-shipped))

      (and (some? tests-passed?) (not tests-passed?) (not hollow?))
      (conj (signal :tests-failing
                    {:detail (or verify-note (signal :tests-failing-fallback))}))

      at-cap?
      (conj (signal :revision-cap {:revision revision :soft-cap soft-cap}))

      (some (fn [[_ h]] (and (>= (:turns h) thrash-min-turns)
                             (>= (:mechanics-rate h) thrash-mechanics-rate)))
            health)
      (conj (signal :thrash))

      (= :revise review)
      (conj (signal :reviewer-bounced))

      (>= (or revision 0) 1)
      (conj (signal :revising {:revision revision})))))

(defn digest
  "The run-health block the supervisor reads. `facts` = {:results :review
  :critic :revision}; `rows` = the run's journal turns."
  [{:keys [results review critic revision errors] :as facts} rows]
  (let [health (branch-health rows)
        total (count results)
        shipped (count (filter #(= :done (:status %)) results))
        sigs (signals facts health)]
    (prompt/render
     "run-health"
     {:heading (signal :heading {:revision (or revision 0)})
      :shipped shipped
      :total total
      :outcomes (pr-str (frequencies (map :status results)))
      :reviewer (or (s review) "n/a")
      :critic (or (s critic) "n/a")
      :per-branch (str/join "\n"
                            (for [[b h] health]
                              (str "- " b ": " (:turns h) " turns, "
                                   (:mechanics h) " thrash, shipped=" (:shipped? h))))
      :failures (failure-exemplars rows (gates/threshold :supervisor-digest))
      :signals (when (seq sigs)
                 (str/join "\n" (map #(str "- " %) sigs)))})))
;; NOTE: run-health.md destructures :failures itself ({{failures.parse.count}}
;; etc.) — the map is the seam, the words are the template's.
