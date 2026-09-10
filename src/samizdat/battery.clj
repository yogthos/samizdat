;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.battery
  "The held-out battery's expectations. MECHANISM: a closed vocabulary of
  assertions over a finished run's journal, and the accept rule the gate uses.

  WHY PER-TARGET AND NOT A SCALAR. karamazov-7mo.3 argued it and
  2609.09153v1's own reporting shows the cost of the alternative: a validation
  score of 0.62 against 0.64 says a candidate is worse and cannot say WHAT it
  broke, so the refusal teaches nothing and the next round re-derives the same
  edit. Every expectation here carries a name and its own verdict.

  WHY THE VOCABULARY IS CLOSED. This battery gates the agent's own edits to
  the harness. A case that could carry an arbitrary form would put an eval
  seam in the one place that must not have one — the check deciding whether a
  self-modification goes live. Same argument samizdat.symbolic makes for its
  guard registry: \"rebuilding arbitrary host evaluation inside the engine
  would give back exactly what this namespace exists to remove.\" An
  unrecognised assertion THROWS rather than passing, because a case whose
  assertion nobody implemented would silently pass forever and the battery
  would grow a hole shaped like coverage.

  WHY ACCEPT ON TIE. Algorithm 1 line 17 of the paper accepts a candidate that
  matches the retained score, deliberately, so the graph can drift laterally
  through neutral edits into a better basin. A gate demanding improvement
  would refuse every edit that fixed something the battery does not measure —
  which is most edits, since the battery is small by design."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [samizdat.replay :as replay]
            [samizdat.store.db :as db]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

;;; ------------------------------------------------------------- the readings

(defn- reading
  "Everything the assertions may look at, read once per case. A closed set,
  so an assertion cannot reach past what is declared here."
  [conn run-id]
  (let [turns (journal/turns conn run-id)
        tally (journal/gate-tally conn run-id)]
    {:status (some-> (:status (runs/get-run conn run-id)) keyword)
     :tools (set (keep :tool_name turns))
     :turn-count (count turns)
     :artifacts (count (journal/artifacts conn run-id))
     :gates (into {} (for [g tally] [(keyword (str (:gate g))) g]))}))

;;; ------------------------------------------------------------- the verbs

(defn- kw
  "A case's argument as a keyword, whether it was written as one or as a
  string. (str :completed) keeps the colon, so (keyword (str x)) on a keyword
  yields ::completed and silently matches nothing — a case that looks right
  and asserts nothing is the worst thing a battery can contain."
  [x]
  (cond (keyword? x) x
        (nil? x) nil
        :else (keyword (str x))))

(defmulti ^:private verb
  "One assertion. [reading args] -> {:ok? bool :actual any}."
  (fn [v _reading _args] v))

(defmethod verb :status [_ r [expected]]
  {:ok? (= (kw expected) (:status r)) :actual (:status r)})

(defmethod verb :gate-fired [_ r [g]]
  (let [row (get (:gates r) (kw g))]
    {:ok? (boolean (some-> (:fired row) pos?)) :actual (or (:fired row) 0)}))

(defmethod verb :gate-met [_ r [g]]
  ;; met OR met-late counts. journal.clj:608's distinction is about which
  ;; REPAIR a gate needs; for "did the branch take the advice" they are the
  ;; same answer, and a battery that failed on a late settle would refuse an
  ;; edit for widening a window.
  (let [row (get (:gates r) (kw g))
        n (+ (or (:met row) 0) (or (:met_late row) 0))]
    {:ok? (pos? n) :actual n}))

(defmethod verb :gate-unmet [_ r [g]]
  (let [row (get (:gates r) (kw g))]
    {:ok? (boolean (some-> (:unmet row) pos?)) :actual (or (:unmet row) 0)}))

(defmethod verb :tool-called [_ r [t]]
  {:ok? (contains? (:tools r) (str t)) :actual (vec (sort (:tools r)))})

(defmethod verb :tool-not-called [_ r [t]]
  {:ok? (not (contains? (:tools r) (str t))) :actual (vec (sort (:tools r)))})

(defmethod verb :artifacts-at-least [_ r [n]]
  {:ok? (>= (:artifacts r) (long n)) :actual (:artifacts r)})

(defmethod verb :turns-at-most [_ r [n]]
  {:ok? (<= (:turn-count r) (long n)) :actual (:turn-count r)})

(defmethod verb :default [v _ _]
  (throw (ex-info (str "unknown assertion " (pr-str v)
                       " — the battery's vocabulary is closed; add a verb in "
                       "samizdat.battery rather than widening a case")
                  {:error :unknown-assertion :verb v})))

(defn vocabulary
  "Every assertion verb a case may use. A test walks this; a prompt can print
  it, so the agent adding a case can see what is available rather than guess."
  []
  (into (sorted-set) (remove #{:default}) (keys (methods verb))))

;;; ------------------------------------------------------------- checking

(defn check
  "Run `expectations` against the finished run `run-id`.

  Returns {:ok? bool :passed n :total n :targets [{:name :ok? :actual}]}. One
  failing target fails the case; every target is still reported, because a
  candidate that broke three things and a candidate that broke one are
  different candidates."
  [conn run-id expectations]
  (let [r (reading conn run-id)
        targets (mapv (fn [{:keys [name assert]}]
                        (let [[v & args] assert
                              {:keys [ok? actual]} (verb v r (vec args))]
                          {:name name :assert assert :ok? ok? :actual actual}))
                      expectations)]
    {:ok? (every? :ok? targets)
     :passed (count (filter :ok? targets))
     :total (count targets)
     :targets targets}))

;;; ------------------------------------------------------------- the gate rule

(defn accept?
  "Whether `after` may be committed given `before`. Ties accepted."
  [before after]
  (>= (long (:passed after 0)) (long (:passed before 0))))

(defn regressions
  "The names of targets that passed BEFORE and fail AFTER.

  Only the flips. A target already failing before the edit is not this edit's
  doing, and refusing on it would block every candidate until somebody fixed
  something unrelated — which turns a validation gate into a freeze."
  [before after]
  (let [was-ok (into #{} (comp (filter :ok?) (map :name)) (:targets before))]
    (into [] (comp (remove :ok?) (map :name) (filter was-ok)) (:targets after))))

;;; ------------------------------------------------------------- cases on disk

(defn load-cases
  "Every case under `dir`, one per .edn file, tagged with its SUBJECT — the
  directory it sits in.

  The subject matters because the gate requires both: a battery of
  endless-flight runs only would pass an edit that breaks samizdat working on
  its own repo, and self-modification is the project's reason for existing
  (AGENTS.md's standing rule). A result that cannot say which subject it came
  from cannot enforce that."
  [dir]
  (let [root (io/file (str dir))]
    (if-not (.isDirectory root)
      []
      (vec (for [f (file-seq root)
                 :when (and (.isFile f) (str/ends-with? (.getName f) ".edn"))
                 :let [c (edn/read-string (slurp f))]]
             (assoc c
                    :subject (.getName (.getParentFile f))
                    :path (.getPath f)))))))

(defn summarise
  "Fold per-case results into the one verdict the gate reads.

  AN EMPTY BATTERY IS NOT A PASS. A misconfigured directory yields no cases;
  if that read as ok? then every edit would sail through while looking
  validated, which is worse than having no gate at all because it looks like
  having one. Absence of evidence reads as absence."
  [results]
  (if (empty? results)
    {:ok? false :reason :battery/empty :passed 0 :total 0 :targets [] :subjects []}
    {:ok? (every? (comp :ok? :result) results)
     :passed (reduce + 0 (map (comp #(or % 0) :passed :result) results))
     :total (reduce + 0 (map (comp #(or % 0) :total :result) results))
     :subjects (vec (distinct (map :subject results)))
     :targets (into [] (mapcat (comp :targets :result)) results)
     :regressions (into [] (comp (mapcat (comp :targets :result))
                                 (remove :ok?) (map :name))
                        results)}))

;;; ------------------------------------------------------- drafting from a run

(defn draft-case
  "A DRAFT case from a finished run: its replay, plus expectations describing
  what the run actually did.

  A draft, not a case. What a run did is the starting point and not the
  specification — some of it is incidental, and which parts are the behaviour
  worth pinning is a judgement. This exists so karamazov-7mo.4's rule that the
  agent may ADD a case from an observed failure is practical rather than a
  matter of hand-writing expectations and getting them subtly wrong.

  WHAT IT WILL NOT PIN: a gate that fired and was never met. That a gate FIRED
  is a fact about the run and safe to hold; that it went unmet is a defect, and
  pinning it would freeze the defect as a requirement — the whole point of a
  later edit might be to make that gate finally land. Same reason
  retirement-candidates treats never-met as a reason to look, not a property
  to preserve.

  Every assertion it writes comes from the closed vocabulary, so a draft is
  runnable the moment it exists rather than after someone discovers a verb
  does not."
  [conn run-id]
  (let [row (runs/get-run conn run-id)]
    ;; REFUSE AN UNFINISHED RUN. Found by drafting from a live sweep run: it
    ;; produced [:status :running] as an expectation, which asserts that the
    ;; loop must still be executing when the check reads it. And the recording
    ;; would be a PREFIX — every replay of it exhausts partway through work
    ;; the run had not finished doing.
    (when-not (runs/terminal? row)
      (throw (ex-info (str "run " run-id " is not finished (" (:status row) ")")
                      {:error :run-not-finished :run-id run-id
                       :status (:status row)}))))
  (let [r (reading conn run-id)]
    ;; The WHOLE run id. A truncation length would be a number in src/
    ;; deciding something a project might want different, and base-test's
    ;; ratchet is right to refuse it — while nobody actually wants to tune how
    ;; many characters of a case's name they see. Removing the decision beats
    ;; moving it to gates.edn or granting it an exemption.
    {:id (str "run-" run-id)
     :replay (replay/record conn run-id)
     :expect (into []
                   cat
                   [;; ONLY AN ENDING THE LOOP CHOSE. :completed, :exhausted
                    ;; and :abandoned are the loop's own outcomes — a branch
                    ;; that shipped, ran out of budget, or gave up. :interrupted,
                    ;; :aborted and :failed happened TO the run: a process died,
                    ;; an operator stopped it, a stage threw. Pinning one of
                    ;; those freezes an accident as a requirement, which is the
                    ;; same error as pinning a gate that was never met.
                    (when (contains? #{:completed :exhausted :abandoned} (:status r))
                      [{:name (str "run reaches " (:status r))
                        :assert [:status (:status r)]}])
                    (for [t (sort (:tools r))
                          :when (contains? #{"done" "give_up"} t)]
                      {:name (str "calls " t) :assert [:tool-called t]})
                    (when (pos? (:artifacts r))
                      [{:name (str "banks at least " (:artifacts r) " artifact(s)")
                        :assert [:artifacts-at-least (:artifacts r)]}])
                    (for [[g row] (sort-by key (:gates r))
                          :when (pos? (or (:fired row) 0))]
                      {:name (str "gate " g " fires") :assert [:gate-fired g]})
                    (for [[g row] (sort-by key (:gates r))
                          :when (pos? (+ (or (:met row) 0) (or (:met_late row) 0)))]
                      {:name (str "gate " g " is met") :assert [:gate-met g]})])}))
