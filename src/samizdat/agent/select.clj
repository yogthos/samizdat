;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.select
  "Which workflow drives a run.

  `workflow/catalog` was written as \"the compiled menu the self-healing loop
  chooses from\" and nothing chose from it: every caller was in test/. A run
  read `:run :loop` once from config before it started and nothing ever wrote
  it, so `decompose` — documented as decompose-on-stuck — could only be picked
  BEFORE anything was stuck, and `team` could not be reached by a problem that
  turned out to have parts. Measured: four consecutive runs of a task with
  obvious separable parts exhausted the factory loop's turn budget with both
  alternatives sitting unreachable in the catalogue.

  So this is the menu being read. One small model call at run start, against
  the same descriptions a supervisor would read, choosing among the manifests
  policy admits.

  WHAT IS MECHANISM HERE AND WHAT IS NOT. This namespace knows how to ASK; it
  knows nothing about what the answer should be. Which manifests are
  candidates, whether selection runs at all, what happens when it fails, and
  every word the model reads are gates.edn :workflow-selection and
  prompts/workflow-select.md. The one thing that cannot live in a cell is the
  call itself: a cell runs inside a manifest, and this chooses the manifest.

  An explicit `:run :loop` always wins. Selection fills the gap where a run
  said nothing, and never overrides a caller who did."
  (:require [clojure.string :as str]
            [samizdat.lexicon :as lexicon]
            [samizdat.llm.client :as llm]
            [samizdat.llm.message :as message]
            [samizdat.prompt :as prompt]
            [samizdat.store.knowledge :as knowledge]
            [samizdat.workflow :as workflow]))

(defn policy [] (lexicon/policy :workflow-selection))

(defn candidates
  "The manifests selection may choose between: the catalogue, narrowed to the
  names policy admits and to those that actually exist.

  A whitelist rather than the whole catalogue because several manifests say in
  their own description that they are components — `worker`, `reviewer` and
  `supervisor` are pieces of the feature loop, not run-level drivers — and a
  menu that offers them invites a run to be driven by half a workflow."
  [conn]
  (let [allowed (set (:candidates (policy)))]
    ;; :turn-sliceable? as well as the whitelist. The whitelist is a claim
    ;; about which workflow suits which task; this is a claim about which
    ;; ones can drive a run at all, and it must not depend on a policy value
    ;; the agent may widen (karamazov-4sx).
    (filterv #(and (contains? allowed (:name %)) (:turn-sliceable? %))
             (workflow/catalog conn))))

(defn history-lines
  "How each workflow has gone on this project, as lines for the prompt.

  THIS IS WHAT MAKES THE CHOICE ADAPTIVE RATHER THAN A ONE-SHOT GUESS. A run
  reading only the problem text is guessing from a description; a run that can
  also see that direct attempts here have gone nought for four, while
  decompose went one for one, is choosing on evidence. It is also the only
  granularity at which the harness CAN learn that — a run sees just its own
  attempt, so the record has to be written down for the next one.

  Only workflows still on the menu, and only those with a run behind them: a
  line saying nothing has been tried is a line that costs tokens to say
  nothing."
  [conn cands]
  (let [on-menu (set (map :name cands))]
    (->> (knowledge/workflow-record conn)
         (filter #(contains? on-menu (:workflow %)))
         (filter #(pos? (:runs %)))
         (mapv (fn [{:keys [workflow shipped runs]}]
                 (str workflow " — shipped " shipped " of " runs
                      (if (= 1 runs) " run" " runs") " on this project"))))))

(defn build-prompt
  "The user message the chooser reads: the problem, the menu, and how each
  workflow has gone here before.

  Separate from the call so it can be asserted on, for the reason reflect's is:
  what a model picks is not something a test can pin, but a candidate that
  never reached the menu is a harness bug and this makes it visible."
  ([problem cands] (build-prompt problem cands nil))
  ([problem cands history]
   (prompt/render "workflow-select"
                  {:problem (str problem)
                   ;; not-empty, not vec: selmer reads an empty vector as
                   ;; truthy, so `{% if history %}` would print the heading
                   ;; over nothing on the first run a project ever makes.
                   :history (not-empty (vec history))
                   :workflows (mapv (fn [{:keys [name description]}]
                                      {:name name :description description})
                                    cands)})))

(defn parse-choice
  "The chosen name from a reply, or nil.

  THE WHOLE REPLY MUST BE A NAME. The system prompt asks for the name and
  nothing else, and this holds it to that: punctuation, quotes, backticks and
  surrounding whitespace come off, and what remains must equal a name on the
  menu.

  It used to accept a name found ANYWHERE in the reply, which is how a test
  scripting an unrelated model response had its run silently driven by a
  different workflow. `loop` in particular is an ordinary English word — a
  reply that mentions looping over something is not a vote — and the reply is
  about to choose the code path for an entire run. A loose match here is worse
  than no match, because no match falls back to the factory loop and says so.

  A reply that explains itself therefore returns nil, and the run uses the
  fallback. That is the right trade: the instruction is unambiguous, and a
  model that will not follow it is not one to hand the wheel to.

  Two things come off first, and neither loosens the rule. A <think> block,
  through the same `message/strip-think-blocks` the rest of the harness uses —
  a reasoning model's scratchpad is not its answer, and DeepSeek returned
  `<think>…</think>\ncritic` on the first live selection this ever made. Then
  everything but the LAST non-blank line, because that is where a model puts
  the answer when it has said anything at all. What remains still has to BE a
  name."
  [reply cands]
  (let [names (set (map :name cands))
        cleaned (->> (str/split-lines (message/strip-think-blocks (str reply)))
                     (remove str/blank?)
                     last
                     str
                     str/trim)
        cleaned (-> cleaned
                    (str/replace #"^[\s`'\"*_#-]+" "")
                    (str/replace #"[\s`'\"*_.!,;:]+$" "")
                    str/lower-case)]
    (first (filter #(= cleaned (str/lower-case %)) names))))

(defn pick!
  "Choose a workflow for `problem`, or nil to leave the decision alone.

  nil on every uncertainty — selection off, no candidates, the call failing,
  a reply naming nothing on the menu — because the caller's fallback is the
  factory loop, which is what the run would have used anyway. A run must never
  fail to start because the harness could not decide how to drive it."
  [{:keys [conn llm-adapter llm-config]} problem]
  (try
    (let [p (policy)
          cands (candidates conn)]
      (when (and (:enabled? p)
                 (seq cands)
                 llm-adapter
                 ;; A one-line problem is the factory loop's case by
                 ;; definition — there is nothing to split and nothing to
                 ;; decompose — so it is not worth a model call to be told so.
                 ;; The floor is policy (gates.edn :min-problem-chars); at 0 it
                 ;; is off and every run is chosen for.
                 (>= (count (str/trim (str problem)))
                     (or (:min-problem-chars p) 0)))
        ;; THE ONE PROVIDER CALL THIS HARNESS CANNOT BILL (karamazov-2rqb.1).
        ;; Every other side model records a side_calls row so the run's token
        ;; budget can see it; this one runs from beam/run! BEFORE
        ;; runs/start-run!, because the choice it makes decides which manifest
        ;; is compiled and the run row records the width that compile decides.
        ;; There is no run_id to attribute it to yet. It is one small call per
        ;; run start, and inventing a nullable-run_id row nothing sums would
        ;; buy a schema wart rather than a number.
        (let [reply (:content (llm/chat llm-adapter llm-config
                                        [{:role "system" :content (prompt/prompt "workflow-select-system")}
                                         {:role "user"
                                          :content (build-prompt problem cands
                                                                 (history-lines conn cands))}]
                                        {:temperature 0.0}))]
          (parse-choice reply cands))))
    (catch Throwable _ nil)))
