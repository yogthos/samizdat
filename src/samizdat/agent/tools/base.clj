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
;;

(ns samizdat.agent.tools.base
  "The tool multimethod and what every tool method shares: result helpers,
  missing-argument complaints, the :default method, tool-names, and the
  per-phase refusal the branch loop consults before dispatch. Tool groups
  require this namespace; nothing here requires a group back."
  (:require [clojure.string :as str]
            [jolt.fs :as fs]
            [samizdat.agent.files :as files]
            ;; gates and storm are not used by name here: the phases.edn
            ;; refusal :when forms reference them fully qualified, and the
            ;; table compiles lazily on the first phase-refusal call — which
            ;; lands in this namespace. Requiring them here is what makes
            ;; that compile find them loaded wherever the caller came from.
            [samizdat.agent.gates :as gates]
            [samizdat.agent.phases :as phases]
            [samizdat.agent.storm :as storm]
            [samizdat.prompt :as prompt]))

(defmulti run-tool
  (fn [ctx] (:tool-name ctx)))

(defn ok [branch result & {:as extra}]
  (merge {:result result :category :neutral :progress? false :branch branch} extra))

(defn fail [branch result & {:as extra}]
  (merge {:result result :category :failure :progress? false :branch branch} extra))

(defn refusal
  "A policy refusal: a WELL-FORMED call the harness declined.

  :mechanics, not :failure — no claim was produced and nothing was tested, so
  there is no evidence here about the branch's line of inquiry — plus
  :policy-refusal? true, which is the pair `state/record-outcome` feeds the
  refusal counter from. Refusals used to go out as `fail`, so no production
  path ever produced that pair: the refusal counter could never move, the
  cull record's 'every call declined by policy' arm was unreachable, and a
  branch refused N times by the task-required rule died as 'N consecutive
  failures' — the vf-jki mistake in a sixth place (karamazov-blt.15)."
  [branch result & {:as extra}]
  (merge {:result result :category :mechanics :progress? false
          :policy-refusal? true :branch branch}
         extra))

(defn malformed
  "A call the harness could not act on because its arguments were wrong.

  NOT a failure. The branch produced no claim and tested nothing, so there is
  no evidence here about its line of inquiry — the same reasoning `unavailable`
  makes about an engine outage and the branch loop makes about a malformed
  fence. Charging it to the counter that decides whether a branch lives is the
  vf-jki mistake, and this is the fifth place it turned up: fences,
  expectedVerdict, proof_start, outages, and argument shape.

  `:mechanics` rather than `:neutral`, deliberately: the count is still kept
  and still bounds a branch looping on malformed calls, which is real spend.
  It just stops being read as substance."
  [branch result]
  {:result result :category :mechanics :progress? false :branch branch})

(defn unavailable
  "An external capability could not be reached. Not the branch's fault, so not
  its failure: the failure counter neither rises nor resets, and
  turns-since-progress still ticks because nothing was established."
  [branch capability e]
  (ok branch (str capability " is unavailable: " (ex-message e))))

(defn arg [ctx k] (get-in ctx [:args k]))

(defn save-body
  "The body for a save action: the inline argument `k` when present, else the
  contents of the `file` argument read from under the run root.

  The file arm exists because of what a live supervisor actually did with a
  fix it had authored (karamazov-lf0): it wrote the new cell body to a file —
  the thing models do reliably — and then never landed it, because save
  demanded the whole body inline in one JSON string. The natural write-a-file
  workflow has to END in the validated save, not die beside it.

  Returns {:body s}, {:error msg} (a path outside the root, or no such file),
  or nil when neither argument was given — the caller then issues its own
  missing-argument complaint."
  [ctx k]
  (let [inline (arg ctx k)
        file (some-> (arg ctx :file) str str/trim not-empty)]
    (cond
      ;; presence, not non-blankness: each tool keeps its own judgement about
      ;; an explicitly empty inline body (the prompt tool allows one)
      (some? inline) {:body (str inline)}

      file
      (if-let [p (and (:root ctx) (files/resolve-under-root (:root ctx) file))]
        (if (fs/exists? p)
          {:body (slurp p)}
          {:error (prompt/render "file-tool" {:no-file true :path file})})
        {:error (prompt/render "file-tool" {:outside-root true :path file
                                            :verb "read"})})

      :else nil)))

(defn rationale
  "The stated reason for a userspace save or revert, trimmed, or nil.

  The mutation tools REQUIRE one (karamazov-c58): a live supervisor reverted
  its predecessor's tuning thirteen minutes after it landed, because the
  version history showed bodies and timestamps but never why — so a successor
  confronted with an unfamiliar delta restored what it recognized. The
  rationale is the commit message of self-modification; demanding it at the
  tool seam keeps the store free for seeding and tests."
  [ctx]
  (some-> (arg ctx :rationale) str str/trim not-empty))

(defn version-line
  "One line of a userspace edit history: when, why, and what the version has
  survived. The reader is the NEXT supervisor deciding whether to keep it."
  [{:keys [version created_at rationale success_count failure_count]}]
  (let [green (long (or success_count 0))
        red (long (or failure_count 0))]
    (str "v" version "  " created_at
         (when-let [r (some-> rationale str not-empty)] (str "  — " r))
         (when (or (pos? green) (pos? red))
           (str "  [" green " green run" (when (not= 1 green) "s")
                (when (pos? red) (str ", " red " failed")) "]")))))

(defn missing
  "The complaint for absent required arguments, WITH the call it wanted.

  This used to be a bare list of names. gen-20 B1 called `proof_start` without
  its arguments five times — three producing the byte-identical message — and
  was culled for it; a model that did not understand the call the first time
  learns nothing from being told the same names again. The skeleton costs
  nothing and needs no schema registry, because the tool name and the keys it
  requires are exactly what this function is already handed."
  [ctx & ks]
  (let [absent (remove #(let [v (arg ctx %)]
                          (and (some? v) (not (and (string? v) (str/blank? v)))))
                       ks)]
    (when (seq absent)
      (str "Missing required argument(s): " (str/join ", " (map name absent)) "."
           "\n\nA call to `" (:tool-name ctx) "` looks like:\n"
           "```tool-call\n"
           "{\"name\": \"" (:tool-name ctx) "\", \"args\": {"
           (str/join ", " (for [k ks]
                            (str "\"" (name k) "\": \"<" (name k) ">\"")))
           "}}\n```"))))

(defn- when-fn-holds?
  "Whether a compiled `:when` form holds for this call. A rule with no
  condition always holds, so an unconditional withhold needs no ceremony."
  [when-fn ctx]
  (or (nil? when-fn) (boolean (when-fn ctx))))

(defn phase-refusal
  "The one place that owns tool-withholding policy, consulted by the branch
  loop BEFORE run-tool dispatch. Returns a result map refusing the call, or
  nil when it may proceed.

  Two tables, both phases.edn data (drg-4026 #34), because the question has
  two shapes:

  `:withholds` is per PHASE — which tools this phase forbids outright. Empty
  today; the proof harness's explore/build policy left with its tool surface,
  and the table is still consulted so a coding loop's phase policy plugs back
  in as a data edit rather than as new code.

  `:refusals` is per BRANCH — an ordered table of conditions, each a form
  seeing `branch` and `tool-name`. This is what the phase table could not
  express and what RFC-008's gap needed: the board was `encouraged, not
  enforced` because nothing could refuse a call from a branch holding no
  task, and whether a branch holds a task is a fact about the branch and not
  about its phase. Adding it here rather than in the task tool keeps every
  withholding decision in one place and one table.

  Any refusal from either path carries `:policy-refusal? true`, so a cull
  record can tell a declined call from a malformed fence — the branch made a
  well-formed call and the harness declined it, which is not evidence about
  its line of inquiry."
  [{:keys [branch tool-name] :as ctx}]
  (or
   (when (contains? (phases/withholds (:phase branch)) tool-name)
     (refusal branch
           (str "`" tool-name "` is not available in the "
                (name (:phase branch))
                " phase. The phase-valve message says when that changes.")))

   ;; `condition` rather than destructuring `:when` — a local named `when`
   ;; shadows clojure.core/when for the whole body, and the shadowing is
   ;; silent until the form it swallows is evaluated.
   (some (fn [{:keys [tools message-file] :as rule}]
           (let [condition (:when rule)]
             (clojure.core/when
              ;; :all opts a rule in for every tool, so a rule whose scoping
              ;; lives in its own policy (the storm guard's exempt vocabulary)
              ;; does not need a second tool list here to drift out of date.
              (and (or (= :all tools) (contains? (set tools) tool-name))
                   (when-fn-holds? condition ctx))
              (refusal branch
                       (prompt/render message-file
                                      {:tool-name tool-name
                                       :phase (some-> (:phase branch) name)})
                       :refusal-rule (:rule rule)))))
         (phases/refusals))))


;; --- unknown ----------------------------------------------------------------

(defmethod run-tool :default [{:keys [branch tool-name]}]
  ;; (fnil inc 0), not inc: the counter starts absent, and `inc` on nil throws.
  ;; state/new-branch seeds the tally so a production branch survived it, but
  ;; a resumed branch, a hand-built one, or any branch whose mechanics map has
  ;; not been touched yet did not — and an unknown tool name is exactly what a
  ;; model produces when it hallucinates a capability, so the one path that
  ;; exists to handle a bad call was itself the crash. The dispatch seam turns
  ;; a throw into a :mechanics result now (RFC-008), which would have masked
  ;; this rather than fixed it.
  (fail (update-in branch [:mechanics :unknown-tools] (fnil inc 0))
        (str "No tool named `" tool-name "`. Available: "
             (str/join ", " (sort (remove #{:default} (keys (methods run-tool)))))
             ".")))

(defn tool-names []
  (sort (remove keyword? (keys (methods run-tool)))))