;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.tools.intervene
  "The `intervene` tool: the supervisor's hands on a RUNNING run.

  Everything else on the supervisor's surface either observes the run or tunes
  the harness for the next one. `store/interventions/submit!` has always taken
  an `:issued-by`, and until this tool existed nothing but a human at a REPL
  could call it — so the supervisor could watch a branch stall, diagnose it
  exactly, rewrite a prompt for some future run, and do nothing whatever about
  the one in front of it. Runs fps5 and fps6 both stalled that way.

  A directive lands in the same queue a human's does and is drained at the same
  boundary by the same cell, so this adds a WRITER to a proven channel rather
  than a second steering path. The receiving end is known to work: the
  human-directive gate was met 1/1 in run 986f33d8.

  `:issued-by` is \"supervisor\" so the journal never attributes the harness's
  own steering to the operator. Reading a run's history afterwards, which
  directives were the machine's and which were a person's is the difference
  between the loop steering itself and someone steering it.

  The wording lives in prompts/intervene-tool.md, and what each kind does in
  wordlists.edn :directive-kinds — the store holds the names, the resources
  hold the words."
  (:require [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]
            [samizdat.store.interventions :as interventions]))

(defn- msg [ctx] (prompt/render "intervene-tool" ctx))

(defn- described
  "Kind -> what it does, for the listings a refusal renders."
  []
  (lexicon/wordlist :directive-kinds))

(defn- payload-for
  "The directive's payload. `message` and `fork` carry prose the branch reads,
  so their text is the payload itself; the rest are structural and take a map."
  [kind text]
  (if (contains? #{"message" "fork"} kind) (or text "") {:text text}))

(defmethod base/run-tool "intervene" [{:keys [branch conn run-id] :as ctx}]
  (let [kind (some-> (base/arg ctx :kind) str str/trim not-empty)
        target (some-> (base/arg ctx :branch) str str/trim not-empty)
        text (some-> (base/arg ctx :text) str not-empty)]
    (cond
      (nil? kind)
      (base/malformed branch (msg {:needs-kind true :kinds (described)}))

      ;; An unknown kind is the model guessing at the vocabulary. Name what is
      ;; available rather than only what was wrong, so the retry can be right
      ;; the first time — the eval-syntax lesson (karamazov-7d4).
      (not (contains? interventions/kinds kind))
      (base/malformed branch (msg {:unknown kind :kinds (described)}))

      :else
      (try
        (interventions/submit! conn run-id
                               {:branch-id target
                                :kind kind
                                :payload (payload-for kind text)
                                :issued-by "supervisor"})
        (base/ok branch (msg {:submitted true :kind kind :target target :text text})
                 :progress? true)
        (catch Throwable e
          ;; The store refuses some directives on its own terms — culling the
          ;; last running branch, most importantly. That is a policy refusal
          ;; answering a well-formed call, not the supervisor's mistake.
          (base/refusal branch (msg {:refused (or (ex-message e) (str e))
                                     :kind kind})))))))
