;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; The finalization critic cell, ported from dirge's unified judge. It is NOT
;; wired into the factory `loop` manifest; the `critic` manifest routes a
;; branch's :done through it. When a branch tries to finish, an LLM judge —
;; fed a deterministic evidence block and judged against the agent's own rules
;; — decides whether the task is really complete. If not, the done is undone:
;; the branch is re-activated, a single [critic] note is injected, and the loop
;; re-enters. Bounded (so it cannot block forever) and fail-open (a judge that
;; errors or abstains-into-silence ships), because a backstop that can wedge
;; the loop is worse than no backstop.
(ns cells.critic
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [mycelium.cell :as cell]
            [samizdat.agent.gitdiff :as gitdiff]
            [samizdat.agent.judge :as judge]
            [samizdat.agent.loop :as turn]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools :as tools]
            [samizdat.llm.client :as llm]
            [samizdat.store.journal :as journal]))

(def max-critic-attempts
  "How many times the critic may send a branch back before it ships anyway.
  A calibrated judge blocks rarely; this caps the cost when it is wrong."
  2)

(defn- parse-args [r]
  (update r :args #(try (json/read-str (str %) :key-fn keyword)
                        (catch Throwable _ {}))))

(defn- ship [data] (assoc data :critic/decision :ship))

(defn- revise
  "Undo the done and re-enter the loop: re-activate the branch, inject one note,
  and do the same per-turn bookkeeping :loop/route does on a :continue (route
  did none — it routed us here on :done)."
  [data branch msg attempts]
  (let [branch' (-> branch
                    (assoc :status :active :final-answer nil)
                    (state/add-message "user" msg))]
    (-> data
        (assoc :branch branch' :critic/decision :revise :critic/attempts attempts)
        (update :turn inc)
        (dissoc :before :call :parsed :signals :said :result :tool :settled)
        (update :mycelium/trace #(vec (take-last 20 %))))))

(cell/defcell :gate/critic
  {:doc "The finalization gate: on a genuine :done, first the cheap
        deterministic checks (edited code but ran no test; an answer that
        claims it tested when nothing ran); if none fire, an LLM judge for
        completeness and a review of the run's diff. COMPLETE with no critical
        finding ships; anything else re-activates the branch, injects one
        [critic] note, and re-enters. Bounded and fail-open."
   :effects [:net :db]
   :requires [:conn :git-baseline :llm-adapter :llm-config :root :run-id]
   ;; :critic/attempts is read before it is ever written — the first pass
   ;; defaults it to 0 — so it is optional coming IN and produced going out on
   ;; the :revise side. That asymmetry is the bound: the count only exists
   ;; once the gate has sent the branch back at least once.
   :input  [:map [:branch :map] [:turn :int]
            [:critic/attempts {:optional true} :int]]
   ;; Per-transition, because ship and revise are genuinely different writes.
   ;; ship stamps the decision and nothing else; revise re-activates the
   ;; branch and does :loop/route's per-turn bookkeeping itself, which route
   ;; did not do — it routed here on :done rather than on :continue.
   :output [:per-transition
            {:ship   [:map [:critic/decision :keyword]]
             :revise [:map [:critic/decision :keyword] [:critic/attempts :int]
                      [:branch :map] [:turn :int]]}]}
  (fn [{:keys [conn run-id root git-baseline llm-adapter llm-config]}
       {:keys [branch turn] :as data}]
    (let [attempts (or (:critic/attempts data) 0)
          note! (fn [m] (journal/note! conn run-id :critic
                                       {:data (merge {:branch-id (:id branch)
                                                      :turn turn
                                                      :attempt (inc attempts)} m)}))]
      (if (>= attempts max-critic-attempts)
        (ship data)
        (let [rows (map parse-args (journal/turns conn run-id))
              det (judge/deterministic-block (:final-answer branch) rows
                                              (tools/tool-names))]
          (if det
            ;; A cheap, specific gate fired — block without paying for the judge.
            (do (note! {:verdict :deterministic :blocked true})
                (revise data branch (str "[critic] " det) (inc attempts)))
            (let [evidence (judge/evidence rows)
                  diff (gitdiff/diff root git-baseline)
                  prompt (judge/critic-prompt {;; WHAT WAS ASKED, which this
                                               ;; never used to carry: the
                                               ;; critic was asked whether the
                                               ;; task was complete with the
                                               ;; task absent from the message,
                                               ;; and inferred it from the
                                               ;; implementer's own transcript.
                                               :requirement (:problem branch)
                                               :evidence evidence
                                               :diff diff
                                               :answer (:final-answer branch)})
                  reply (try (:content (llm/chat llm-adapter llm-config
                                                 [{:role "user" :content prompt}]))
                             (catch Throwable _ nil))
                  verdict (if reply (judge/parse-verdict reply) :complete)
                  ;; A COMPLETE verdict still blocks on a critical/high finding.
                  blocking (when reply (judge/blocking-findings reply))]
              (note! {:verdict verdict :blocked (boolean blocking)})
              (if (and (= :complete verdict) (not blocking))
                (ship data)
                (revise data branch
                        (if (= :complete verdict)
                          (str "[critic] The diff review found defects to fix"
                               " before shipping.\n\n" blocking
                               "\n\nAddress these, then finish again.")
                          (judge/critique-message verdict (judge/findings reply)))
                        (inc attempts))))))))))
