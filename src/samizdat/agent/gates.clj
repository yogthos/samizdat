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

(ns samizdat.agent.gates
  "Gate definitions: the conditions under which the harness says something to
  the model, and what it expects to happen next.

  A gate is data. It has a precondition re-evaluated every tick, a message, a
  budget, and a prediction that a later turn settles deterministically. The
  arbiter picks at most one per boundary; nothing here decides to fire.

  Preconditions are re-evaluated rather than latched by one-shot counters,
  which is the behavior-tree property worth taking from Kelley (arXiv
  2404.07439): a condition that stopped holding should stop firing, and a
  counter cannot express that.

  Every gate declares a prediction because a gate that cannot say what should
  change is one whose effect nobody can check. Settling them is what makes the
  gate tally worth reading (AHE decision observability)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [samizdat.agent.state :as state]
            [samizdat.agent.supervisor :as supervisor]
            [samizdat.prompt :as sp]
            [samizdat.userspace :as userspace]
            [samizdat.util :as util]))

(defn load-config
  "The gate thresholds for the current project.

  Through the userspace seam: gates.edn is a policy TABLE, which is userspace —
  a project that learned its cull threshold is too tight should be able to move
  it for itself without moving it for every other project on the same binary.
  The shipped file is the template a project seeds from. Unbound (a test, a
  bare REPL) this is the resource read it always was."
  []
  (userspace/edn-body! :policy "gates"))

(defonce ^:private config-cache (atom nil))

;; Bumped by reload-config!, watched by everything COMPILED from the config
;; (the gate table below, ship.clj's rungs, arbiter's forceable schemas). A
;; reload used to swap the config atom and leave every derived table frozen at
;; namespace load; the generation is what makes reload mean what it says.
(defonce ^:private generation (atom 0))

(defn gen
  "The config's generation. Derived tables cache against this."
  []
  @generation)

(defn config []
  (or @config-cache (reset! config-cache (load-config))))

(defn reload-config! []
  (reset! config-cache (load-config))
  (swap! generation inc)
  nil)

(defn threshold [k]
  (get-in (config) [k :value]))

(defn tool-vocab
  "The tool vocabulary `k` (:verification, :shipping, :file-write,
  :settle-called) from gates.edn. The vocabularies the gates read are
  runtime-tunable data, like the thresholds; the vocabulary test in
  agent-test walks every name against the registered run-tools (provenance R3-6)."
  [k]
  (get-in (config) [:tool-vocab k]))

(defn storm-policy
  "The storm guard's policy, assembled from the thresholds and vocabularies
  above (karamazov-ekk). One map so the detector (samizdat.agent.storm, pure)
  and its consumers — the phases.edn refusal rules, tool-step's window
  bookkeeping, resume's window rebuild — all read the same tunable values.
  Every number here is a gates.edn edit away from different behaviour, which
  is the point: dirge hardcodes its window and threshold, and the standing
  rule says a decision the agent cannot retune at runtime is in the wrong
  place."
  []
  {:enabled? (boolean (threshold :storm-enabled))
   :window-size (threshold :storm-window-size)
   :threshold (threshold :storm-threshold)
   :timeout-floor (threshold :storm-timeout-floor)
   :min-cycles (threshold :storm-min-cycles)
   :strikes-to-force (threshold :storm-strikes-to-force)
   :verify-exempt? (boolean (threshold :storm-verify-exempt))
   :exempt-tools (or (tool-vocab :storm-exempt) #{})
   :mutating-tools (or (tool-vocab :storm-mutating) #{})})

(defn- prompt [name]
  (sp/prompt name))

(defn- fired-count [branch gate]
  (count (filter #(= gate (:gate %)) (:gate-history branch))))



;; --- data-driven gates (tier 3a) ---------------------------------------------
;;
;; The steer policy as data: gates.edn :gates entries carry :when as EDN
;; forms, compiled HERE at load into the closure shape above — the manifest
;; :dispatches are the precedent (EDN predicates evaluated at compile time).
;; The form sees exactly the context keys the loop passes; anything else
;; fails to compile at load, which is the fail-fast. Inside the compiled fn
;; the accessors are ordinary calls, so (threshold k) reads the config atom
;; at FIRE time — tuning a threshold stays runtime-editable; only the form
;; structure compiles at load.

(defn- compile-form
  "Compile an EDN form into (fn [ctx] form) with the gate-context keys bound
  as plain locals — the environment both :when and :message-form build on.
  prompt/threshold/state and the required namespaces resolve at compile, in
  this namespace; the config atom is still read at FIRE time.

  `*ns*` is bound explicitly because `eval` resolves the form's free symbols
  against whatever namespace is current when it runs. That used to be this
  one for free: the table was a top-level `def`, so the compile happened at
  namespace load. Now that it is memoized and compiled on FIRST USE, the
  caller could be anything — jolt.main, a test namespace — and `threshold`,
  `prompt`, `state/…` and `supervisor/…` resolve in none of them."
  [form]
  (binding [*ns* (the-ns 'samizdat.agent.gates)]
    (eval `(fn [~'ctx]
           (let [~'directive            (get ~'ctx :directive)
                 ~'done-block           (get ~'ctx :done-block)
                 ~'branch               (get ~'ctx :branch)
                 ~'max-turns            (get ~'ctx :max-turns)
                 ~'branch-count         (get ~'ctx :branch-count)
                 ~'safe-state-coverage  (get ~'ctx :safe-state-coverage)]
             ~form)))))

(defn- compile-when
  [form]
  (compile-form form))

(defn- compile-message
  "A prompts/ file plus an optional suffix, selmer-rendered at fire time —
  the same {{...}} convention and the same engine (samizdat.prompt) as
  every other prompt seam."
  [{:keys [message-file message-suffix]}]
  (fn [{:keys [branch max-turns]}]
    (let [ctx {:turn-count (state/turn-count branch) :max-turns max-turns}]
      (str (some-> message-file sp/prompt (sp/render-str ctx))
           (some-> message-suffix (sp/render-str ctx))))))

(defn- compile-gate
  [entry]
  (assoc entry
         :when (compile-when (:when entry))
         :message (if (:message-form entry)
                    (compile-form (:message-form entry))
                    (compile-message entry))
         :prediction (let [p (:prediction entry)] (fn [_] p))))

(def gates
  "The steer table, compiled from gates.edn :gates — all data since tier 3b.
  Priorities, not table order, decide arbitration.

  A FUNCTION, not a value: the compile is memoized against the config
  generation, so `reload-config!` genuinely re-reads and re-compiles the
  table. As a top-level `def` it was compiled once at namespace load and a
  reload changed only the thresholds the forms read at fire time — editing a
  gate's :when, adding a gate, or removing one needed a process restart, in
  the one namespace whose whole premise is that the steer policy is data."
  (util/generation-cache gen #(mapv compile-gate (:gates (config)))))

(def ^:private by-name*
  (util/generation-cache gen #(into {} (map (juxt :gate identity)) (gates))))

(defn by-name
  "The compiled gate `k`, or nil."
  [k]
  (get (by-name*) k))

(defn crossed-fractions
  "Which turn-budget notice thresholds this branch has now passed. The loop
  folds these into the branch so the gate stops re-firing."
  [branch max-turns]
  (let [used (/ (double (state/turn-count branch)) (max 1 max-turns))]
    (set (filter #(>= used %) (threshold :turn-budget-notices)))))

(defn budget-exceeded?
  "Whether this gate has already fired as often as it may."
  [gate branch]
  (when-let [k (:budget gate)]
    (>= (fired-count branch (:gate gate)) (threshold k))))

(defn describe
  "The gate table, for docs and for /v1/harness/gates."
  []
  (for [g (gates)]
    {:gate (:gate g) :priority (:priority g)
     :budget (:budget g)
     :budget-kind (some-> (:budget g) (#(get-in (config) [% :kind])))
     :doc (str/replace (str/trim (:doc g)) #"\s+" " ")}))
