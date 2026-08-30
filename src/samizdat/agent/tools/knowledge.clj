;; samizdat - a self-hosting agentic harness
;; License: GPL-3.0-or-later

(ns samizdat.agent.tools.knowledge
  "Long-term memory tools: remember a fact, recall it by search.

  Both are :neutral on purpose — same reasoning as the task board and
  fetch_artifact. Recording a memory is bookkeeping; the fact it stores was
  established by whatever turn produced it, and that turn already got its
  credit. Charging recall would be worse: reading your own notes back is not
  progress, and a run that searches before every claim would farm the
  counter."
  (:require [clojure.string :as str]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.lexicon :as lexicon]
            [samizdat.agent.tools.base :as base]
            [samizdat.store.knowledge :as knowledge]
            [samizdat.prompt :as prompt]))

(defn- msg [ctx] (prompt/render "memory-tool" ctx))

(def ^:private usage (delay (msg {:usage true})))

(defn- memory-line
  "One memory as the model reads it, with the two numbers that say why it is
  ranked where it is. A list that hid its own ordering would be asking the
  model to trust it; showing the standing and the record lets the model judge
  a memory the way the ranking did."
  ([m] (memory-line m nil))
  ([m this-run]
   (str (:id m) " [" (:kind m) "]"
        (when-let [s (:salience m)] (format " s%.2f" (double s)))
        (let [w (or (:success_count m) 0) f (or (:failure_count m) 0)]
          (when (pos? (+ w f)) (str " " w "✓/" f "✗")))
        " " (:content m)
        ;; A COMPLETION CLAIM FROM ANOTHER RUN IS NOT AUTHORITY OVER THIS ONE.
        ;; It was true of a tree that no longer exists, or it was never true —
        ;; run cc88a760 recalled two such claims from FAILED runs and a worker
        ;; concluded its part was already done. The claim still shows, because
        ;; it is often the useful hint that the work has a history; what it no
        ;; longer does is settle the question (karamazov-mjb).
        (when (and this-run
                   (:run_id m)
                   (not= (str (:run_id m)) (str this-run))
                   (knowledge/completion-claim?
                    (:content m) (lexicon/wordlist :completion-claim)))
          (str "\n    ↳ " (prompt/prompt "memory-stale-completion"))))))

(defn- unverified-completion?
  "Whether this memory claims the work is finished while the branch has changed
  nothing on disk.

  Both halves required. A memory that merely records a FINDING is not gated —
  a supervisor writing down what it diagnosed has no diff and should not need
  one. What is gated is a claim about the tree made by a branch the tree does
  not corroborate.

  Fails OPEN: when git cannot answer, `changed-files` returns nil, which is
  \"cannot tell\" and not \"nothing changed\". Refusing on an unknown would
  block honest memories on any checkout without git."
  [{:keys [root git-baseline]} content]
  (boolean (and (knowledge/completion-claim?
                 content (lexicon/wordlist :completion-claim))
                (when (and root git-baseline)
                  (let [changed (gitdiff/changed-files root git-baseline)]
                    (and (some? changed) (empty? changed)))))))

(defmethod base/run-tool "remember" [{:keys [branch conn] :as ctx}]
  (if-let [miss (base/missing ctx :content)]
    (base/malformed branch (str miss "\n\n" @usage))
    (let [kind (or (base/arg ctx :kind) "procedural")
          content (base/arg ctx :content)]
      (if (unverified-completion? ctx content)
        ;; GROUND TRUTH, and it is the only memory the harness can check.
        ;; Prototyping in eval is not finishing, and a "settled and verified"
        ;; recorded off an empty diff is the exact poison that told run
        ;; cc88a760's worker its part was already done (karamazov-mjb).
        (base/fail branch (prompt/prompt "memory-unverified"))
        (let [id (knowledge/remember!
                  conn {:content content
                        :kind kind
                        :confidence (some-> (base/arg ctx :confidence) str parse-double)
                        :run-id (:run-id ctx)})]
          (base/ok branch (msg {:remembered true :id id :kind kind
                                :content content})))))))

(defmethod base/run-tool "outcome" [{:keys [branch conn] :as ctx}]
  ;; The axis that makes memory a loop rather than a list. Kind, use and
  ;; recency all measure whether a memory gets READ; only this measures
  ;; whether reading it HELPED.
  (if-let [miss (base/missing ctx :id)]
    (base/malformed branch (str miss "\n\n" @usage))
    (let [id (str (base/arg ctx :id))
          worked? (contains? #{"true" "yes" "1"} (str/lower-case (str (base/arg ctx :worked))))]
      (if-not (knowledge/get-by-id conn id)
        (base/fail branch (msg {:no-memory true :id id}))
        (do (knowledge/record-outcome! conn id worked?)
            (base/ok branch (msg {:outcome-recorded true :id id :worked worked?})))))))

(defmethod base/run-tool "forget" [{:keys [branch conn] :as ctx}]
  (if-let [miss (base/missing ctx :id)]
    (base/malformed branch (str miss "\n\n" @usage))
    (if (pos? (knowledge/forget! conn (base/arg ctx :id)))
      (base/ok branch (msg {:forgot true :id (base/arg ctx :id)}))
      (base/fail branch (msg {:no-memory true :id (base/arg ctx :id)})))))

(defmethod base/run-tool "recall" [{:keys [branch conn] :as ctx}]
  (if-let [id (base/arg ctx :id)]
    (if-let [row (knowledge/get-by-id conn id)]
      (base/ok branch (memory-line row (:run-id ctx)))
      (base/fail branch (msg {:no-memory true :id id})))
    (if-let [miss (base/missing ctx :query)]
      (base/malformed branch (str miss "\n\n" @usage))
      (let [rows (knowledge/recall conn (base/arg ctx :query))]
        (base/ok branch
                 (if (seq rows)
                   (str/join "\n" (map #(memory-line % (:run-id ctx)) rows))
                   (msg {:no-match true :query (base/arg ctx :query)})))))))
