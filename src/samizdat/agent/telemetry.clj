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

  Carries a few WINS alongside them, and that is load-bearing rather than
  decorative: Metan (research/2608.24735v1, App. D) ablated exactly this
  ratio and found a failures-only diet \"strips out positive exemplars and
  produces overfit constraints\" — an improver shown only what broke writes
  rules against breakage instead of rules for working. gates.edn
  :supervisor-digest :wins sets how many.

  Returns DATA — {:parse :provider :tool :wins}, each {:count n :lines s} or
  absent — and the run-health template owns the words around it, so the
  section headings the supervisor reads are prompts/ prose like everything
  else it reads. Pure over the rows; caps are gates.edn :supervisor-digest
  policy."
  [rows {:keys [per-kind chars wins]}]
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
        ;; What WORKED, so the supervisor is not reasoning from breakage
        ;; alone. Progress-bearing successes only — a read that returned
        ;; fine is not news; a write or a green check is.
        won (for [r rows
                  :when (and (= "success" (str (:category r)))
                             (not (str/starts-with? (str (:tool_name r)) "__")))]
              (line r (:result r)))
        cap-wins (fn [lines]
                   (when (and wins (pos? wins) (seq lines))
                     {:count (count lines)
                      :lines (str/join "\n" (take-last wins lines))}))
        m (cond-> {}
            (kind parse) (assoc :parse (kind parse))
            (kind provider) (assoc :provider (kind provider))
            (kind tool) (assoc :tool (kind tool))
            (cap-wins won) (assoc :wins (cap-wins won)))]
    ;; A run with nothing but successes needs no failures section at all —
    ;; wins alone are not a report of trouble.
    (when (some m [:parse :provider :tool])
      (not-empty m))))

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

(defn layer-of
  "Which layer a failure belongs to: `:base`, `:userspace`, or nil when the
  text does not say (karamazov-i1u).

  This is the single most useful thing the harness can tell its supervisor
  about a failure, and it used to tell it nothing. A live supervisor spent
  108 of a run's 211 turns on a `board/next` crash whose cause was
  `samizdat.store.runs/open-branch!` — compiled base, unreachable from a
  role loop whose file tools are scoped to the project — and, unable to
  tell that from a cell bug, degenerated into 26 shell calls hunting a
  source tree it will never be allowed to open.

  The distinction is legible in the text, and WHICH text says which layer is
  a vocabulary — wordlists.edn `:failure-layers` — not a constant here. The
  namespace prefixes are exactly the sort of thing a project that renamed or
  re-rooted something has to be able to correct without a rebuild.
  Userspace is tested first and wins a tie: a cell frame in the trace means
  the cell is the thing that can actually be edited, whatever base code it
  went on to call."
  [s]
  (let [t (str s)
        {:keys [userspace base]} (lexicon/wordlist :failure-layers)
        any? (fn [pats] (boolean (some #(re-find (re-pattern %) t) pats)))]
    (cond
      (any? userspace) :userspace
      (any? base) :base
      :else nil)))

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
      (into (map (fn [e]
                   ;; Which layer owns it, so the supervisor knows before it
                   ;; starts whether this is its to fix (karamazov-i1u). The
                   ;; branch is passed as booleans rather than compared in
                   ;; the template: selmer's `if` tests truthiness and has no
                   ;; equality operator.
                   (let [l (layer-of e)]
                     (signal :stage-crashed
                             {:detail e
                              :layer (some-> l name)
                              :base? (= :base l)
                              :userspace? (= :userspace l)})))
                 errors))

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
