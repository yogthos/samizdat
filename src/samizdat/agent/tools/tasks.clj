;; samizdat - a self-hosting agentic harness
;; License: GPL-3.0-or-later

(ns samizdat.agent.tools.tasks
  "The task board tool: task create/list/show/update/claim/close."
  (:require
            [samizdat.agent.gates :as gates]
            [clojure.string :as str]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.state :as state]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]
            [samizdat.store.tasks :as tasks]))

;; --- the task board ----------------------------------------------------------

(defn- task-line
  "One line for a task. Shows the HOLDER when a branch has claimed it, which
  is what makes the board usable by a team: several implementors fanned out
  over one feature share a run, so a worker needs to see that W1 is on sz-a3f2
  rather than discovering it by trying to claim it."
  [t]
  (str (:id t) " [" (:status t) "/" (:priority t)
       (when-not (= "task" (:type t)) (str " " (:type t)))
       (when (:parent_id t) (str " < " (:parent_id t)))
       (when (seq (str (:branch_id t))) (str " @" (:branch_id t)))
       "] " (:title t)))

(defn- render-task [conn t]
  (str (task-line t)
       (when (seq (:body t)) (str "\n\n" (:body t)))
       (when (seq (:contract t)) (str "\n\nCONTRACT\n" (:contract t)))
       (when (seq (:tests t)) (str "\n\nTESTS\n" (:tests t)))
       (when-let [kids (seq (tasks/children-of conn (:id t)))]
         (str "\n\nCHILDREN\n" (str/join "\n" (map task-line kids))))))

;; --- the current task -------------------------------------------------------
;;
;; A branch works ONE task at a time, and the harness keeps it in front of the
;; model at both ends of the context without ever paying for it:
;;
;;   APPENDED ONCE, on claim, and pinned. An append lands at the end of the
;;   message array, which is where the prefix cache boundary already is, so it
;;   costs nothing — and from then on it IS part of the stable prefix. It is
;;   never rewritten, which is the whole trick: a block held at a fixed early
;;   position and rewritten when the task changes invalidates every cached
;;   token behind it, and one carrying anything per-turn would mean the cache
;;   never warms at all. That is the LR-4 defect (compaction appending its
;;   digest to the problem message) in a new place, and this avoids it by
;;   construction rather than by care.
;;
;;   RESTATED EVERY TURN in the context block, which is also at the end and
;;   therefore also free, and is where a model attends most.
;;
;; Pinned means compaction never unloads it: the task matters MORE the longer
;; it runs, so ageing it out is exactly backwards.

(defn task-statement
  "The pinned message a claimed task appends. Marked :pinned? so compaction
  leaves it alone, and stamped with the task id so a later turn can tell which
  statement belongs to which task."
  [branch t]
  (state/add-message
   branch "user"
   (str "[harness] " (prompt/render "task-claimed"
                       {:id (:id t) :title (:title t) :body (:body t)
                        :contract (:contract t) :tests (:tests t)
                        ;; The same number the :over-budget gate measures
                        ;; against, so the question asked at claim time and
                        ;; the answer measured in flight cannot drift apart
                        ;; (karamazov-5ot9).
                        :budget (gates/threshold :task-line-budget)}))
   {:pinned? true :task-id (:id t)}))

(defn- take-task
  "Set `t` as the branch's current task and append its statement."
  [branch t]
  (-> branch
      (assoc :task {:id (:id t) :title (:title t)})
      (task-statement t)))

(defn- holding
  "The branch's current task when it is still genuinely open, else nil.

  Checked against the ROW rather than trusting the branch: another agent on the
  run may have closed it, and a branch refusing to claim because of a task
  somebody else finished would be stuck on a ghost."
  [conn branch]
  (when-let [held (:task branch)]
    (let [row (tasks/get-task conn (:id held))]
      (when (and row (not (tasks/terminal? (:status row)))) row))))

(def ^:private task-usage
  (str "Actions: create {title, body?, type?, priority?, parentId?, contract?, tests?},"
       " list, show {id}, update {id, ...fields}, claim {id},"
       " switch {id, reason}, close {id, status?}."))

(defmethod base/run-tool "task" [{:keys [branch conn run-id] :as ctx}]
  ;; Every action is `ok` (:neutral) on purpose: working the board is
  ;; bookkeeping, and bookkeeping is not progress — the same reasoning as
  ;; fetch_artifact. Grounding work in tasks is required; credit for the work
  ;; itself comes from artifacts. Bad ids, bad statuses, and unknown actions
  ;; are :mechanics — calls made wrong, not failed lines of inquiry.
  (let [action (some-> (base/arg ctx :action) str str/trim str/lower-case not-empty)
        want (fn [k] (let [v (base/arg ctx k)]
                       (when-not (and (some? v) (not (and (string? v) (str/blank? v))))
                         (base/malformed branch (str "`task " action "` needs `" (name k) "`. "
                                                task-usage)))))]
    (try
      (case action
        nil
        (base/malformed branch (str "`task` needs an `action`. " task-usage))

        "create"
        (or (want :title)
            (let [id (tasks/create! conn {:title (base/arg ctx :title)
                                          :body (base/arg ctx :body)
                                          :type (base/arg ctx :type)
                                          :status (base/arg ctx :status)
                                          :priority (base/arg ctx :priority)
                                          :parent-id (base/arg ctx :parentId)
                                          :contract (base/arg ctx :contract)
                                          :tests (base/arg ctx :tests)
                                          :run-id (when-not (base/arg ctx :backlog) run-id)})]
              (base/ok branch (str "Created " (task-line (tasks/get-task conn id))))))

        "list"
        (let [rows (tasks/board conn {:run-id run-id})]
          (base/ok branch (if (seq rows)
                       (str/join "\n" (map task-line rows))
                       "The board is empty.")))

        "show"
        (or (want :id)
            (if-let [t (tasks/get-task conn (base/arg ctx :id))]
              (base/ok branch (render-task conn t))
              (base/malformed branch (str "No task " (base/arg ctx :id) "."))))

        "update"
        (or (want :id)
            (if-not (tasks/get-task conn (base/arg ctx :id))
              (base/malformed branch (str "No task " (base/arg ctx :id) "."))
              (let [t (tasks/update! conn (base/arg ctx :id)
                                     {:title (base/arg ctx :title)
                                      :body (base/arg ctx :body)
                                      :type (base/arg ctx :type)
                                      :status (base/arg ctx :status)
                                      :priority (base/arg ctx :priority)
                                      :parent-id (base/arg ctx :parentId)
                                      :contract (base/arg ctx :contract)
                                      :tests (base/arg ctx :tests)})]
                (base/ok branch (str "Updated " (task-line t))))))

        "claim"
        (or (want :id)
            ;; One task at a time. Refused rather than silently switched: a
            ;; branch that picks up a second task has abandoned the first
            ;; without saying so, and "until it is done" stops meaning
            ;; anything. The refusal names the way out.
            (if-let [held (holding conn branch)]
              (if (= (:id held) (base/arg ctx :id))
                (base/ok branch (str "Already working on " (task-line held)))
                (base/malformed branch (prompt/render "task-busy"
                                         {:current-id (:id held)
                                          :current-title (:title held)})))
              (if-let [t (tasks/claim! conn (base/arg ctx :id) run-id (:id branch))]
                (base/ok (take-task branch t)
                         (str "Claimed " (task-line t))
                         :progress? true)
                (base/malformed branch (str "Cannot claim " (base/arg ctx :id)
                                       ": no such task, or another run holds it.")))))

        "switch"
        (or (want :id) (want :reason)
            (let [held (holding conn branch)
                  reason (str (base/arg ctx :reason))]
              (if (and held (= (:id held) (base/arg ctx :id)))
                ;; Switching to what you already hold: a no-op, NOT a
                ;; claim-then-release — the idempotent re-claim succeeded and
                ;; the release then set the row back to open while the branch
                ;; kept working it, so a sibling could claim the same task
                ;; (karamazov-blt.34).
                (base/ok branch (str "Already working on " (task-line held)))
              (if-let [t (tasks/claim! conn (base/arg ctx :id) run-id (:id branch))]
                (do
                  ;; The task being set down goes back to the board rather than
                  ;; staying attributed to a branch that is no longer doing it.
                  (when held (tasks/release! conn (:id held) (:id branch)))
                  ;; Journalled, because setting a task down half-finished is
                  ;; exactly the decision a later reader needs explained — and
                  ;; a switch that leaves no record is indistinguishable from
                  ;; drift.
                  (when (and conn run-id)
                    (journal/note! conn run-id :task-switch
                                   {:branch-id (:id branch)
                                    :data {:from (:id held) :to (:id t)
                                           :reason reason}}))
                  (base/ok (take-task (cond-> branch
                                        ;; the set-down task's statement stops
                                        ;; being pinned (swd)
                                        held (state/unpin-task-statement (:id held)))
                                      t)
                           (str (if held
                                  (str "Set down " (:id held) " and claimed ")
                                  "Claimed ")
                                (task-line t)
                                "\nRecorded why: " reason)
                           :progress? true))
                (base/malformed branch (str "Cannot switch to " (base/arg ctx :id)
                                       ": no such task, or another run holds it."))))))

        "close"
        (or (want :id)
            (let [row (tasks/get-task conn (base/arg ctx :id))]
              (cond
                (not row)
                (base/malformed branch (str "No task " (base/arg ctx :id) "."))

                ;; Only the holder closes held work. Any branch could close a
                ;; sibling's in-progress task — "tidying the board" away from
                ;; under the branch working it (karamazov-blt.34). Unheld
                ;; rows stay closable by anyone: cancelling backlog is
                ;; bookkeeping, not theft.
                (and (some? (:branch_id row))
                     (not= (:branch_id row) (:id branch)))
                (base/malformed branch
                                (str "Task " (:id row) " is held by "
                                     (:branch_id row) "; only the holder"
                                     " closes it."))

                :else
              (let [t (tasks/close! conn (base/arg ctx :id) (or (base/arg ctx :status) "done"))
                    ;; Closing the CURRENT task clears the slot, so the next
                    ;; context block asks for the next one instead of pointing
                    ;; at finished work.
                    branch (cond-> branch
                             (= (:id t) (:id (:task branch)))
                             (-> (assoc :task nil)
                                 ;; the finished statement stops being pinned,
                                 ;; so compaction can fold it away (swd)
                                 (state/unpin-task-statement (:id t))))]
                (base/ok branch (str "Closed " (task-line t))
                         :progress? true)))))

        (base/malformed branch (str "Unknown task action `" action "`. " task-usage)))
      (catch Throwable e
        ;; Unknown statuses, missing parents: the store's validation errors are
        ;; calls made wrong, and the message already says what was wrong. When
        ;; the store names the valid values (bad status/priority), pass them on
        ;; so the retry is informed rather than another guess.
        (base/malformed branch (str "`task " action "` refused: " (ex-message e)
                               (when-let [valid (:valid (ex-data e))]
                                 (str " Valid: " (str/join ", " valid)))
                               "\n" task-usage))))))

