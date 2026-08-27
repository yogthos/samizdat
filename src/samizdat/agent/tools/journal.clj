;; samizdat - a self-hosting agentic harness
;; License: GPL-3.0-or-later

(ns samizdat.agent.tools.journal
  "Reading the run's own record: fetch_artifact, fetch_turn."
  (:require
            [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]
            [samizdat.llm.message :as message]))

;; --- the journal, readable --------------------------------------------------

(defmethod base/run-tool "fetch_artifact" [{:keys [branch conn run-id] :as ctx}]
  ;; The ledger lists claims with ids and leaves the encodings out, so the code
  ;; costs a turn only when a branch actually wants it rather than riding in
  ;; every context block. This is what makes an id actionable.
  ;;
  ;; Deliberately :neutral, via `ok`: a lookup establishes nothing. Reporting
  ;; :success would clear the branch's consecutive-failure count and read as
  ;; progress, which is the "well-formed but useless call" failure the
  ;; progress guards exist to catch.
  (if-let [m (base/missing ctx :id)]
    (base/malformed branch m)
    (let [raw (str/trim (str (base/arg ctx :id)))
          ;; `a#` is this run's own artifacts, `s#` the shared pool a seed was
          ;; copied into — two tables, two id spaces. A bare number means the
          ;; branch's own, which is the common case. `p#` is also this run's
          ;; own artifacts — the ledger's handle for a SKETCH, same table,
          ;; different status, so the prefix survives the round trip.
          shared? (str/starts-with? raw "s#")
          own? (or (str/starts-with? raw "a#") (str/starts-with? raw "p#"))
          sketch? (str/starts-with? raw "p#")
          id (parse-long (str/replace raw #"^[aps]#" ""))
          ;; An explicit prefix is honoured exactly. A BARE number tries this
          ;; run's own artifacts and then falls back to the shared pool:
          ;; insisting on the prefix cost six of the first eleven fetches in
          ;; an observed run.
          own (when (and conn run-id id (not shared?))
                (journal/artifact-by-id conn run-id id))
          a (or own
                (when (and conn run-id id (not own?))
                  (journal/shared-artifact-by-id conn run-id id)))
          ;; Which space it actually came from, so the echoed handle matches
          ;; what the ledger showed.
          from-shared? (and a (nil? own))]
      (if-not a
        ;; :mechanics, not :failure — a lookup that finds nothing refutes
        ;; nothing; a bad id is a call made wrong.
        (base/malformed branch (str "No artifact " raw " in this run."
                          " Ids come from the settled-state block: `a#12` for"
                          " something this run established, `s#7` for something"
                          " it inherited. A run cannot reach another run's"
                          " artifacts."))
        (base/ok branch
            (str (if from-shared? "s#" (if sketch? "p#" "a#")) (:id a)
                 " [" (:branch_id a) " " (:kind a) "/" (:tier a) "]"
                 ;; The status travels with the encoding or a refutation reads
                 ;; as an established result. Seeded rows carry no status
                 ;; column of their own; seed-from-run! copies only confirmed
                 ;; artifacts, so saying so is accurate rather than a guess.
                 " status " (if from-shared?
                              "CONFIRMED (inherited from the seed run)"
                              (str/upper-case (str (:claim_status a))))
                 (when (:verdict a) (str ", verdict " (:verdict a)))
                 "\n\nCLAIM\n" (:claim a)
                 "\n\nENCODING\n" (:code a)))))))

(defmethod base/run-tool "fetch_turn" [{:keys [branch conn run-id] :as ctx}]
  ;; The other half of compaction. Unloading a branch's early turns to one
  ;; line each is only honest if a line can be opened again; before this,
  ;; the digest pointed at a journal the branch had no tool to read.
  ;;
  ;; :neutral for the same reason as fetch_artifact — a lookup establishes
  ;; nothing, and reporting success would clear the failure count.
  (if-let [m (base/missing ctx :turn)]
    (base/malformed branch m)
    (let [raw (str/trim (str (base/arg ctx :turn)))
          n (parse-long (str/replace raw #"^t" ""))
          ;; An explicit :branch reads ANOTHER branch's turn — the diagnosis
          ;; affordance the run-health digest's failure exemplars point at:
          ;; the supervisor is handed "turn 3 (T0, ...)" and has to be able
          ;; to open it, or the digest points at records its reader cannot
          ;; reach. Deliberate and auditable (the fetch is itself a journalled
          ;; turn); the DEFAULT stays own-branch, so sibling isolation — what
          ;; crosses between peers is the settled-state block — still holds
          ;; unless a branch asks for a specific other turn by name.
          bid (or (some-> (base/arg ctx :branch) str str/trim not-empty)
                  (:id branch))
          t (when (and conn run-id n)
              (journal/branch-turn conn run-id bid n))]
      (if-not t
        ;; :mechanics for the same reason as fetch_artifact's miss. The words
        ;; are prompts/fetch-turn-miss.md — the prose ratchet's rule.
        (base/malformed branch
                        (prompt/render "fetch-turn-miss"
                                       {:turn raw
                                        :branch bid
                                        :cross (not= bid (:id branch))}))
        (base/ok branch
            (str "t" (:turn t)
                 (when (not= bid (:id branch)) (str " [" bid "]"))
                 " " (:tool_name t)
                 " → " (or (:category t) "neutral")
                 (when (seq (str (:args t))) (str "\n\nARGUMENTS\n" (:args t)))
                 ;; Reasoning is stripped: it is 96% of stored assistant text
                 ;; and is dropped from every prior turn on the way to the
                 ;; wire anyway. Reloading it here would undo that in one call.
                 (when-let [said (some-> (:assistant_text t)
                                         message/strip-think-blocks
                                         not-empty)]
                   (str "\n\nWHAT YOU SAID\n" said))
                 "\n\nRESULT\n" (:result t)))))))
