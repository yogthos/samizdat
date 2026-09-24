;; samizdat - a claim-first verification harness
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

(ns samizdat.api.control
  "Starting runs, intervening in them, and stopping them.

  Two paths on purpose. A directive goes on a queue and is drained at the next
  branch boundary, because a branch mid provider-call is not something to
  mutate. An abort goes straight to the supervisor, because a wedged run is
  exactly the one that will never reach another boundary — that is the RAX
  manager pattern, and it is why the stop path does not share machinery with
  the steer path."
  (:require [samizdat.lexicon :as lexicon]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [samizdat.agent.beam :as beam]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.live :as live]
            [samizdat.agent.resume :as resume]
            [samizdat.approval :as approval]
            [samizdat.cancel :as cancel]
            [samizdat.config :as config]
            [samizdat.llm.client :as llm]
            [samizdat.llm.registry :as registry]
            [samizdat.prompt :as prompt]
            [samizdat.store.grants :as grants]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.journal :as journal]
            [samizdat.store.runs :as runs]))

;; run-id -> {:future f :abort (atom false)}. A run outlives the request that
;; started it, so something has to hold it.
(defonce active (atom {}))

(defn run-llm-config
  "The llm config this run should use, after the request's own overrides.

  The model used to come only from HARNESS_MODEL at startup, so putting a run
  on a different arm meant restarting the server — which kills whatever run is
  in flight, hours of provider spend, plus another Mathlib import for the Lean
  pool. Comparing arms was therefore gated on the box being idle, which is the
  one thing it never is during a campaign.

  Per-run instead. beam/run! already records (:model llm-config) on the run
  row, so the arm becomes provenance on the result rather than something to
  remember about the environment when reading it back months later.

  `reasoning_effort` is passed to the provider verbatim. It matters because
  whether a model thinks was otherwise a property of which one was configured:
  deepseek-v4-pro thinks by default, deepseek-v4-flash does not, and neither
  says so in the run record.

  Blank is not a value — an unset select posts \"\" — so it leaves the
  configured default standing rather than asking for a model with no name.
  The model may name its provider as `provider:model`, or be a provider
  config.edn declares on its own (its declared model) — what `/model` keeps
  for the next run — and the run then goes to that provider. A colon whose
  prefix names no provider is part of the model's name (`qwen3:32b`)."
  ([llm-config body] (run-llm-config nil llm-config body))
  ([config llm-config body]
   (let [pick (fn [& ks]
                (some #(let [x (get body %)]
                         (when-not (str/blank? (str x)) x))
                      ks))
         model (pick :model "model")
         effort (pick :reasoning_effort :reasoning-effort "reasoning_effort")
         known (set (config/provider-names config))
         declared (set (keep #(some-> % name str/lower-case keyword)
                             (keys (:providers config))))
         [p m] (when model (str/split (str model) #":" 2))
         p (some-> p str/lower-case keyword)
         [provider model] (cond
                            (and m (known p)) [p m]
                            (and (nil? m) (declared p)) [p nil]
                            :else [nil model])
         llm-config (if (and provider
                             (not (#{(:provider llm-config) (:provider-name llm-config)} provider)))
                      (config/provider-llm config provider
                                           (select-keys llm-config [:reasoning-effort]))
                      llm-config)]
     (cond-> llm-config
       model (assoc :model model)
       effort (assoc :reasoning-effort effort)))))

;; A run's endpoint is asked what it is before the run starts (llm/with-
;; discovery — remembered per endpoint once it answers), and one that does not
;; answer at all refuses the run here rather than letting every branch retry
;; into :provider-error-limit (karamazov-rhrf). Whether to refuse is policy.
;;
;; A project's gates.edn and role map are its OWN copies, and what a later
;; release adds reaches one only when it adopts the new file — endless-flight
;; had neither the key nor the prompt, and a throwing read made every run
;; start a 500. So an absent key is gates/threshold's nil, the behaviour from
;; before the key existed, and absent prose falls back to what the endpoint
;; itself said.
(defn- unreachable-message [url why]
  (try (str/trim (prompt/render "endpoint-unreachable" {:url url :reason why}))
       (catch Throwable e
         (log/warn "endpoint-unreachable prompt:" (ex-message e))
         (str url " - " why))))

(defn discovered
  "`llm-config` with what its endpoint said about itself, and the refusal to
  answer with when it said nothing and gates.edn :endpoint-preflight is on:
  `[llm-config refusal-or-nil]`. The llm config never carries :unreachable
  onward — that is this check's, not the run's."
  [llm-config]
  (let [probed (llm/with-discovery llm-config)
        why (:unreachable probed)]
    [(dissoc probed :unreachable)
     (when (and why (gates/threshold :endpoint-preflight))
       (log/warn "refusing a run: model endpoint" (:base-url probed) "-" why)
       {:status 503
        :body {:error {:message (unreachable-message (:base-url probed) why)
                       :type "endpoint_unreachable"}}})]))

(defn start-run!
  "Kick off a run in the background and return its id immediately.

  `POST /v1/chat/completions` blocks on the same machinery for OpenAI
  compatibility; this is the path for everything else."
  ;; JSON bodies arrive with underscored keys; accept both so a caller is
  ;; never silently given the config default when they asked for something
  ;; specific. The first API call made here asked for beam_width 2 and got 5.
  [{:keys [conn config]} body]
  (let [problem (or (:problem body) (get body "problem"))
        max-turns (or (:max_turns body) (:max-turns body))
        beam-width (or (:beam_width body) (:beam-width body))
        token-budget (or (:token_budget body) (:token-budget body))
        seed-run (or (:seed_run body) (:seed-run body))
        quarantine (or (:quarantine body) (get body "quarantine"))]
  ;; A {} body used to start a REAL run on a nil problem — a selection model
  ;; call plus a full beam of provider spend answering nothing, while
  ;; /v1/chat/completions 400s the same input (blt.38).
  (if (str/blank? (str problem))
    {:status 400
     :body {:error {:message "a run needs a non-blank `problem`"
                    :type "invalid_request_error"}}}
  (let [[llm-config refusal] (discovered (run-llm-config config (:llm config) body))]
  (or refusal
  (let [adapter (registry/adapter-for (:provider llm-config))
        abort (atom false)
        promised (promise)
        cancel* (atom nil)
        ;; The run is a TASK (RFC-013): abort cancels it, and the cancel is
        ;; observed at the round's next step or a turn's next check. The abort
        ;; flag stays beside it for the waits a cancel cannot reach.
        started (cancel/start!
                 (cancel/spawn
                  (fn []
                    (try
                      (let [r (beam/run! {:conn conn :config config
                                          :llm-adapter adapter :llm-config llm-config
                                          :problem problem
                                          :max-turns max-turns
                                          :beam-width beam-width
                                          :token-budget token-budget
                                          :seed-run seed-run
                                          :quarantine quarantine
                                          :abort abort
                                          :on-start (fn [rid]
                                                      (swap! active assoc rid
                                                             {:abort abort
                                                              :cancel (fn [] (some-> @cancel* (apply [])))})
                                                      (deliver promised rid))})]
                        (swap! active dissoc (:run-id r))
                        ;; Release anything parked on a question this run
                        ;; asked. Without it an aborted or finished run
                        ;; leaves threads waiting on an answer nobody will
                        ;; ever give, and the process never gets them back.
                        (approval/abandon! (:run-id r))
                        r)
                      (catch Throwable e
                        (if (cancel/control-signal? e)
                          (log/info "run aborted:" (ex-message e))
                          (log/error "run failed:" (ex-message e)))
                        (when-let [rid (deref promised 0 nil)]
                          (swap! active dissoc rid)
                          (approval/abandon! rid))
                        {:status :error :error (ex-message e)})))))
        _ (reset! cancel* (:cancel started))
        ;; How long the request waits for the run row before answering 503
        ;; (gates.edn :run-start-deadline-ms). The selection model call used
        ;; to run BEFORE the row existed — 28 s on GLM-5.3 with thinking, up
        ;; to 36 s on a local thinking model — and a client that gave up
        ;; re-posted a run that had started anyway (karamazov-5fyo). beam/run!
        ;; now creates the row and fires on-start before that call, so the
        ;; wait here is the insert and the opening-context walk.
        start-deadline (lexicon/policy :run-start-deadline-ms)
        run-id (deref promised start-deadline nil)]
    (if run-id
      ;; Wrapped in :body like resume, so one route shape serves both the
      ;; success and the refusal and neither has to be special-cased.
      {:body {:run_id run-id :status "running"
              :beam_width (or beam-width (get-in config [:run :beam-width]))
              :max_turns (or max-turns (get-in config [:run :max-turns]))
              :token_budget (or token-budget (get-in config [:run :token-budget]))}}
      ;; 503, not 200: the request was well formed and the server could not
      ;; service it. Answering 200 with an error body made a caller that checks
      ;; the status code read this as a started run, which is why gui.api's
      ;; start-run! had to unwrap the body to find out otherwise.
      {:status 503
       :body {:error {:message (str/trim
                                (prompt/render "run-start-timeout"
                                               {:seconds (quot start-deadline 1000)}))}}})))))))

(defn abort!
  "Stop a run without asking it to cooperate. Cancels the run task, which is
  observed at the round's next step or a turn's next check (RFC-013), and sets
  the flag the waits a cancel cannot reach still read. The run's finally block
  disposes every engine session regardless of how it ended."
  [conn run-id]
  (if-let [{:keys [abort cancel]} (get @active run-id)]
    (do (reset! abort true)
        (when cancel (cancel))
        (if (pos? (runs/finish-run! conn run-id :aborted nil))
          ;; :body, not a bare map: the run's own :status is the string
          ;; "aborting", and a route reading (:status r) as an HTTP code would
          ;; have sent that.
          {:body {:run_id run-id :status "aborting"}}
          ;; The run finished between the registry read and the store write;
          ;; the row guard refused the rewrite (provenance R2-4). Same refusal
          ;; shape as an unknown run — the abort did not land.
          {:status 409
           :body {:error {:message (str "run " run-id " already finished")}
                  :run_id run-id}}))
    ;; 409, matching resume's "not resumable": the run may well exist, it is
    ;; just not in a state that can be aborted. This answered 200 with an error
    ;; body, so a caller reading the status code alone saw a refusal as a
    ;; successful abort.
    {:status 409
     :body {:error {:message (str "no active run " run-id)}
            :run_id run-id}}))

(defn resume!
  "Resume a crashed run from its journal, in the background like start-run!.

  Returns {:status 409 :body ...} when the run is not resumable — aborted runs
  stay aborted, completed runs shipped — else a success map the caller turns
  into an HTTP 200. The resumed run is registered under `active` with a fresh
  abort flag, so abort! can stop it like any other.

  `body` may carry max_turns: an explicit budget extension that reopens
  branches closed as exhausted. Omitted, the original budget stands."
  [{:keys [conn config]} run-id body]
  (let [refuse (fn [why] {:status 409 :body {:error {:message (str "run " run-id " " why)
                                                     :run_id run-id}}})
        run (runs/get-run conn run-id)
        ;; The model the run was ON, from its row: the provider it recorded
        ;; (an alias config.edn declares, or a built-in) and the model.
        recorded (when (not-empty (str (:provider run)))
                   (try (config/provider-llm config (:provider run)
                                             (if (not-empty (str (:model run)))
                                               {:model (:model run)}
                                               {}))
                        (catch Exception e e)))]
    (cond
      (not (resume/resumable? conn run-id)) (refuse "is not resumable")
      (instance? Exception recorded) (refuse (str "ran on " (:provider run) ": "
                                                  (ex-message recorded)))
      :else
    ;; A resume may name an arm too — a run that crashed on one model can be
    ;; picked up on another, and saying nothing keeps the original.
    (let [[llm-config refusal] (discovered (run-llm-config config (or recorded (:llm config)) body))]
    (if refusal
      (assoc-in refusal [:body :run_id] run-id)
    (let [adapter (registry/adapter-for (:provider llm-config))
          abort (atom false)
          max-turns (or (:max_turns body) (:max-turns body))
          ;; A run already driven by this process is not resumed: a second
          ;; driver over the same branches wrote duplicate turns, every call
          ;; on whatever model the resume resolved. Claimed HERE, atomically,
          ;; not by the spawned thread, or two resumes a double click apart
          ;; would both get in before either thread registered.
          [before _] (swap-vals! active #(if (contains? % run-id) % (assoc % run-id {:abort abort})))]
      (if (contains? before run-id)
        (refuse "is still running")
      (do
      (let [cancel* (atom nil)
            started (cancel/start!
                     (cancel/spawn
                      (fn []
                        (try
                          (swap! active assoc run-id
                                 {:abort abort
                                  :cancel (fn [] (some-> @cancel* (apply [])))})
                          (let [r (resume/resume! {:conn conn :config config
                                                   :llm-adapter adapter
                                                   :llm-config llm-config
                                                   :run-id run-id :abort abort
                                                   :max-turns max-turns})]
                            (swap! active dissoc run-id)
                            r)
                          (catch Throwable e
                            (if (cancel/control-signal? e)
                              (log/info "resume aborted:" (ex-message e))
                              (log/error "resume failed:" (ex-message e)))
                            (swap! active dissoc run-id)
                            {:status :error :error (ex-message e)})))))]
        (reset! cancel* (:cancel started)))
      ;; The budget this resume is running under, from what the caller asked
      ;; for, falling back to the row as it stood BEFORE the future started.
      ;; Reading the row here unconditionally raced the resume that is
      ;; rewriting it: the answer was whichever thread won, and an explicit
      ;; max_turns extension was reported as the old budget more often than
      ;; not.
      {:body {:run_id run-id :status "resuming"
              :max_turns (or max-turns (:max_turns (runs/get-run conn run-id)))}}))))))))
(defn- grant-pattern
  "The pattern from a grant payload. Accepts a map (what body-json yields), a
  bare string, or nil. Blank is not a pattern — an unset form posts empty
  strings."
  [payload]
  (let [p (cond
            (map? payload) (or (:pattern payload) (get payload "pattern"))
            (string? payload) payload
            :else nil)]
    (when-not (str/blank? (str p)) (str p))))

(def ^:private live-kinds
  "The kinds that change a running run's model rather than what it is told.
  Applied ON ARRIVAL, like `grant`: loop/call-model reads them on every
  request (samizdat.agent.live), so there is no boundary to queue for."
  {"model" :model "effort" :reasoning-effort})

(defn- parse-switch
  "A switch's payload: `value`, or `role value`. A model may carry its
  provider as `provider:model`, and a provider config.edn declares may be
  named alone, meaning its declared model. {:role :value :provider} or
  {:error …}."
  [config kind payload]
  (let [words (remove str/blank? (str/split (str/trim (str payload)) #"\s+"))
        [role v] (case (count words) 1 [nil (first words)] 2 words [nil nil])
        known (set (config/provider-names config))
        declared (set (keep #(some-> % name str/lower-case keyword) (keys (:providers config))))
        [p m] (when (and v (= "model" kind) (str/includes? v ":")) (str/split v #":" 2))
        alone (when (and v (= "model" kind) (nil? p))
                (declared (keyword (str/lower-case v))))
        provider (or (some-> p str/lower-case keyword) alone)]
    (cond
      (nil? v) {:error (str "a " kind " switch takes `" kind "` or `role " kind "`")}
      (and provider (not (contains? known provider)))
      {:error (str "unknown provider " p "; known: "
                   (str/join ", " (sort (map name known))))}
      :else {:role (some-> role str/lower-case keyword)
             :value (cond alone nil provider m :else v)
             :provider provider})))

(defn- live-switch!
  [conn config run-id {:keys [kind payload]}]
  (let [{:keys [error role value provider]} (parse-switch config kind payload)
        k (get live-kinds kind)]
    (if error
      {:status 400 :body {:error {:message error :run_id run-id}}}
      (let [m (cond-> {} value (assoc k value) provider (assoc :provider provider))
            summary (str (if role (name role) "every role") " → "
                         (when provider (str (name provider) (when value ":")))
                         value)]
        (live/set! run-id role m)
        ;; On the record, so the run's own account says when it changed
        ;; and a front end's conversation can show it.
        (journal/note! conn run-id :llm-switch
                       {:data (cond-> {:summary summary}
                                value (assoc k value)
                                role (assoc :role (name role))
                                provider (assoc :provider (name provider)))})
        (log/info "run" run-id kind "switched:" summary)
        {:body (cond-> {:status "switched" :role (some-> role name) :run_id run-id}
                 value (assoc (name k) value)
                 provider (assoc :provider (name provider)))}))))

(defn intervene!
  "Record a human intervention. Queued kinds (message, cull, fork, …) go on
  the directive queue and apply at the next branch boundary. The exception is
  `grant`, which applies ON ARRIVAL: it writes the run-scoped permission grant
  the shell policy consults on every command, so there is no boundary to wait
  for. This is the one production write path into the grants table — a human
  surface, never a tool — and without it every deliberate `ask` (interpreters,
  git push, curl, installs) blocked a run forever (provenance A-2, docs/provenance.md).

  `config` is the running config, for the providers a model switch may name
  beyond the built-ins (config.edn :providers)."
  ([conn run-id body] (intervene! conn nil run-id body))
  ([conn config run-id body]
  (if (= "grant" (:kind body))
    (if-let [pattern (grant-pattern (:payload body))]
      (do (grants/grant! conn run-id pattern)
          (log/info "grant" pattern "recorded for run" run-id)
          {:body {:status "granted" :pattern pattern :run_id run-id
                  :note "Applied now. Commands matching the pattern are allowed for the rest of this run; a hard deny still wins."}})
      {:status 400
       :body {:error {:message "a grant intervention needs payload.pattern — the shell glob to allow"
                     :run_id run-id}}})
    (if-let [run (let [r (runs/get-run conn run-id)]
                   (when (or (nil? r) (runs/terminal? r))
                     (or r ::absent)))]
      ;; A directive against a run that does not exist or has ended would sit
      ;; `pending` forever — the UI showing an intervention that will never
      ;; resolve (blt.38).
      (if (= ::absent run)
        {:status 404 :body {:error {:message (str "no run " run-id)}}}
        {:status 409 :body {:error {:message (str "run " run-id " is already "
                                                  (:status run))
                                    :run_id run-id}}})
    (if (contains? live-kinds (:kind body))
      (live-switch! conn config run-id body)
    (if-not (contains? interventions/kinds (:kind body))
      ;; provenance R3-12: this reached submit!'s throw and surfaced as the
      ;; server's catch-all 500. An unknown kind is the client's mistake.
      {:status 400
       :body {:error {:message (str "Unknown intervention kind " (pr-str (:kind body))
                                    "; known: "
                                    (str/join ", " (sort interventions/kinds)))}
              :run_id run-id}}
      (let [id (interventions/submit! conn run-id
                                      {:branch-id (:branch_id body)
                                       :kind (:kind body)
                                       :payload (:payload body)
                                       :issued-by (or (:issued_by body) "human")})]
        {:body
         {:id id
          :status "pending"
          ;; Said plainly rather than implied, because the difference between
          ;; accepted and applied is the thing a UI most easily lies about.
          :note "Queued. It applies at the branch's next turn boundary, not now."}})))))))

(defn kinds
  "Every directive kind with what it does — the names from the store, the
  words from wordlists.edn :directive-kinds."
  []
  {:kinds (lexicon/wordlist :directive-kinds)})
