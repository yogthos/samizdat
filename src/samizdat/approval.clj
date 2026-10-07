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

(ns samizdat.approval
  "Asking a person, with a deadline.

  samizdat runs unattended by design. The shell policy's `:ask` refuses the
  call and teaches the model to retry; a human adds a grant afterwards and
  the run never stopped. That is the right default and it stays the default.

  This is the other bet, for when somebody IS watching: the branch parks on
  a question and a person answers it. What makes that safe rather than a
  liability is the deadline. Every wait here is bounded by a number in
  gates.edn and resolves to a STATED default when it expires, and a run that
  ends releases every waiter it had. A campaign that starts at 3am with
  nobody at the terminal comes out the far side; it does not hang.

  MECHANISM ONLY. Nothing here decides when to ask — `resolve-ask` reads
  `:approval` from gates.edn and does what the project said, and the default
  there is `:refuse`, which is exactly the behaviour the harness had before
  this namespace existed. A feature landing must not change what an
  unattended run does.

  Two shapes over one registry: a yes/no on a tool call (the permission
  gate) and a questionnaire (`ask_human`). They differ only in what the
  request carries and what the answer carries, so they share the queue, the
  deadline and the endpoints."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [samizdat.agent.gates :as gates]
            [samizdat.events :as events]))

(def modes
  "What happens when a run needs a person: :refuse (nobody is asked; a
  simulated user answers ask_human when the run has user context), :block
  (the question waits for a person, up to :wait-ms), :attended (:block while
  a front end follows the run's event stream, :refuse while none does), or
  :yolo (every ask is allowed without asking; a deny is still a deny)."
  #{:refuse :block :attended :yolo})

(def ^:private project-modes
  "The modes the project's own gates.edn may choose. Both SAFE: refusing, and
  asking a person. gates.edn is userspace the agent edits, so a mode that
  loosens anything is the operator's or a person's to set, never the file's
  (karamazov-3vu1.2)."
  #{:refuse :block :attended})

(defonce ^:private session-mode (atom nil))

(defonce ^:private operator
  ;; The operator's `:approval` block from config.edn — see `configure!`.
  (atom nil))

(defn configure!
  "Install the operator's `:approval` block (config.edn): `{:mode
  :refuse|:block|:yolo :on-timeout :deny|:allow :wait-ms n}`, or nil. Called
  at system start. config.edn is the one settings file no tool the agent
  holds can write, which is why the loosening keys are read from here and
  nowhere else."
  [m]
  (clojure.core/reset! operator (when (map? m) m))
  nil)

(declare policy)

(defn set-mode!
  "Set the session's approval mode, or nil to go back to the configured one.
  Returns the mode in force, or nil for one that is not a mode."
  [mode]
  (let [m (some-> mode name keyword)]
    (cond
      (nil? mode) (do (clojure.core/reset! session-mode nil) (:mode (policy)))
      (contains? modes m) (do (clojure.core/reset! session-mode m) m)
      :else nil)))

(defn policy
  "The approval policy: `{:mode :refuse|:block|:attended|:yolo :wait-ms n
  :on-timeout :deny|:allow}`.

  Layered by who may loosen it. The project's gates.edn may pick :refuse,
  :block or :attended and the wait; :on-timeout :allow and :yolo come only
  from the operator's config.edn or a person's session mode. A value the
  file sets outside that is ignored rather than honoured, in the direction
  of asking.

  Given a `run-id`, :attended is resolved for that run — :block while a
  front end follows it (samizdat.events/watched?), :refuse while none does —
  so a caller deciding whether to ask about THIS run never sees :attended."
  ([run-id]
   (let [p (policy)]
     (cond-> p
       (= :attended (:mode p))
       (assoc :mode (if (events/watched? run-id) :block :refuse)))))
  ([]
   (let [g (let [t (gates/threshold :approval)] (if (map? t) t {}))
         o (or @operator {})
         mode (or @session-mode
                  (get modes (:mode o))
                  (get project-modes (:mode g))
                  :refuse)]
     {:mode mode
      :wait-ms (or (:wait-ms o) (:wait-ms g))
      :on-timeout (if (= :allow (:on-timeout o)) :allow :deny)})))

(defonce ^:private requests (atom {}))

(defonce ^:private approved-content
  ;; #{[run-id key]}: workflow texts a person allowed on a run, by hash.
  (atom #{}))

;; {run-id [pattern …]} — what a person allowed ALWAYS, for this session. In
;; memory and never written anywhere: an allow that outlived the person who
;; gave it would be a standing permission nobody chose to leave on.
(defonce ^:private allowed (atom {}))

(defonce ^:private exact-allowed
  ;; {run-id #{command}}: commands a person allowed always where no pattern
  ;; could stand for them — a flow gap, a compound that hides what it runs.
  (atom {}))

(defn session-grants
  "The patterns a person allowed always for `run-id` in this session."
  [run-id]
  (get @allowed run-id []))

(defn reset!
  "Drop every request, releasing anyone waiting. For tests and teardown."
  []
  (let [old @requests]
    (clojure.core/reset! requests {})
    (clojure.core/reset! exact-allowed {})
    (doseq [[_ {:keys [promise]}] old]
      (when promise (deliver promise {:decision :deny :note "harness shutting down"}))))
  nil)

(defn- announce!
  "Say on the bus that a question was asked or settled, so a front end
  following the run sees it without polling. Only the id and the state:
  the question itself is read from GET /v1/runs/:id/approvals."
  [{:keys [id run-id branch-id status kind]}]
  (events/publish! {:kind :approval :run-id run-id :branch-id branch-id
                    :data {:id id :status status :kind (some-> kind name)}}))

(defn- new-id []
  (str (java.util.UUID/randomUUID)))

(defn request!
  "Register a question and return its id. Does not wait — `await!` does that,
  so a caller can register, publish, and then park."
  [{:keys [run-id branch-id kind input details reason questions always always-exact gaps]}]
  (let [id (new-id)]
    (swap! requests assoc id
           {:id id :run-id run-id :branch-id branch-id
            :kind (or kind :shell)
            :input input :details details :reason reason :questions questions
            ;; What the call lacks, as data (samizdat.security.flow): the
            ;; person is shown the call and its gaps, not the model's account
            ;; of why it wants to run it.
            :gaps gaps
            ;; The pattern "allow always" would allow, when there is one, so
            ;; the person sees what they would be agreeing to.
            :always always
            ;; True when `always` is the exact command rather than a
            ;; pattern: allowing it always allows that text and no other.
            :always-exact (boolean always-exact)
            :status "pending"
            :asked-at (System/currentTimeMillis)
            :promise (promise)})
    (announce! (get @requests id))
    id))

(defn pending
  "Questions still waiting, for `run-id` — or every one of them when nil.

  Only the ones still unanswered: an entry survives being decided until its
  waiter has collected the answer (see `decide!`), and showing operators a
  question that has already been settled would invite a second answer to a
  decision that has been acted on.

  Without the `:promise`, which is a parked thread and not something to
  serialise onto a wire."
  [run-id]
  (->> (vals @requests)
       (filter #(= "pending" (:status %)))
       (filter #(or (nil? run-id) (= run-id (:run-id %))))
       (sort-by :asked-at)
       (mapv #(dissoc % :promise))))

(defn decide!
  "Answer a pending question. True when this call was the one that answered
  it, false when there was nothing to answer.

  FIRST ANSWER WINS, and a second is refused rather than applied. Two
  operators with two TUIs open is the ordinary case, and the branch has
  already resumed on the first answer — a second that overwrote it would be
  changing a decision that has been acted on.

  The entry is MARKED answered, not removed. Deleting it here lost the race
  where a person answers between `request!` and `await!`: the promise was
  delivered to an entry nobody could find any more, and the branch was
  denied a command a human had just allowed. `await!` retires it once it has
  collected the answer."
  [id decision]
  (let [applied (atom false)]
    (swap! requests
           (fn [m]
             (let [r (get m id)]
               (if (and r (= "pending" (:status r)))
                 (do (clojure.core/reset! applied r)
                     (assoc-in m [id :status] "decided"))
                 (do (clojure.core/reset! applied false) m)))))
    (if-let [r @applied]
      (do (deliver (:promise r) decision)
          (announce! (assoc r :status "decided"))
          true)
      false)))

(defn await!
  "Wait for `id` to be answered, at most `wait-ms`, then `default`.

  The deadline is the point. Returns the decision map either way, so a
  caller never has to distinguish `answered deny` from `nobody was there`
  unless it wants to — `:timed-out` on the fallback says which."
  [id wait-ms default]
  (if-let [p (:promise (get @requests id))]
    (let [d (deref p wait-ms ::timeout)]
      ;; Retire it either way. A question whose asker has stopped waiting is
      ;; not one a person can still usefully answer, and an answered one has
      ;; now been collected.
      (swap! requests dissoc id)
      (if (= ::timeout d) (assoc default :timed-out true) d))
    ;; No such request — never registered, or already collected.
    (assoc default :timed-out true)))

(defn abandon!
  "Release every waiter on `run-id` — the run is over — and forget what was
  allowed always for it.

  Without this, an aborted or crashed run leaves threads parked on questions
  nobody will ever answer, and the process never gets them back."
  [run-id]
  (swap! allowed dissoc run-id)
  (swap! exact-allowed dissoc run-id)
  (swap! approved-content (fn [s] (into #{} (remove #(= run-id (first %))) s)))
  (let [gone (filter #(= run-id (:run-id %)) (vals @requests))]
    (swap! requests #(apply dissoc % (map :id gone)))
    (doseq [{:keys [promise]} gone]
      (when promise (deliver promise {:decision :deny :note "the run ended"})))
    (count gone)))

(defn content-approved?
  "Whether a person already allowed exactly this content (`key`, e.g.
  [tool name hash]) on `run-id` (karamazov-0e2c.19, after xi's trust by
  hash)."
  [run-id key]
  (contains? @approved-content [run-id key]))

(defn grant-pattern
  "The pattern an \"allow always\" on `command` (whose head is `head`) grants:
  `head sub *` when the word after the head is a subcommand — `git push *`,
  `cargo run *` — and `head *` otherwise. It was always `head *`, so allowing
  one `git push` allowed every git command for the rest of the session
  (karamazov-3vu1.6). A word is a subcommand when it is a bare lowercase name:
  not a flag, a path, a URL, or a file with an extension."
  [head command]
  (when head
    (let [words (str/split (str/trim (str command)) #"\s+")
          sub (second (drop-while #(not= % head) words))]
      (if (and sub (re-matches #"[a-z][a-z0-9-]*" sub))
        (str head " " sub " *")
        (str head " *")))))

;; --- the policy seam ---------------------------------------------------------

(defn resolve-ask
  "Give a person the chance to turn an `:ask` into an `:allow`.

  Returns the decision map, with `:effect` possibly changed and `:note`
  carrying whatever the person said. Anything that is not an `:ask` passes
  straight through, and so does an `:ask` when the project has not asked for
  a person in the loop — which is the default.

  Never throws. This sits in the path of every shell command, and an
  approval registry that could fail would be a registry that can stop a run
  from doing anything at all."
  [{:keys [run-id branch-id]} {:keys [effect input details reason gaps content-key] :as decision}]
  (let [{:keys [mode wait-ms on-timeout]} (policy run-id)]
    (cond
      (not= :ask effect) decision
      ;; The same text a person already allowed on this run: trust is of
      ;; the content, so only a change asks again (karamazov-0e2c.19).
      (and content-key (content-approved? run-id content-key))
      (assoc decision :effect :allow :approved-before true)
      ;; Exactly this text, allowed always earlier on this run.
      (and input (contains? (get @exact-allowed run-id) (str input)))
      (assoc decision :effect :allow :approved-before true)
      ;; Nobody is asked and everything that would have been is allowed.
      ;; Flagged, so the journal tells a yolo allow from a person's.
      (= :yolo mode) (assoc decision :effect :allow :yolo true)
      (not= :block mode) decision
      :else
      (try
        (let [;; A compound command is never grantable (the policy downgrades
              ;; it to :ask over any grant), so there is no pattern to offer.
              ;; Nor for a flow gap: a grant clears a command, not what the
              ;; branch has read.
              pattern (when (and (:head decision) (not (:complex? decision))
                                 (not (:flow? decision)))
                        (grant-pattern (:head decision) input))
              ;; With no pattern to widen to, always is this exact text.
              exact (when (and (nil? pattern) (not (str/blank? (str input))))
                      (str input))
              id (request! {:run-id run-id :branch-id branch-id :kind :shell
                            :input input :details details :reason reason
                            :gaps gaps :always (or pattern exact)
                            :always-exact (some? exact)})
              answer (await! id wait-ms {:decision (or on-timeout :deny)})]
          (cond
            (= :allow (:decision answer))
            (do (when content-key
                  (swap! approved-content conj [run-id content-key]))
                (when (and (:always answer) pattern)
                  (swap! allowed update run-id (fn [ps] (vec (distinct (conj (vec ps) pattern)))))
                  (log/info "approval: allowed" pattern "always, for this session, on run" run-id))
                (when (and (:always answer) exact)
                  (swap! exact-allowed update run-id (fnil conj #{}) exact)
                  (log/info "approval: allowed exactly" (pr-str exact) "always, for this session, on run" run-id))
                (assoc decision :effect :allow :note (:note answer)))

            ;; An expired wait leaves the ask as the refusal it always was,
            ;; and says that is why — so the refusal the model reads can
            ;; mention that nobody was there, rather than reading as a rule.
            (:timed-out answer)
            (cond-> (assoc decision :timed-out true)
              (= :allow on-timeout) (assoc :effect :allow)
              (:note answer) (assoc :note (:note answer)))

            :else
            (assoc decision :note (:note answer))))
        (catch Throwable e
          (log/warn "approval: asking failed, refusing as usual:" (ex-message e))
          decision)))))
