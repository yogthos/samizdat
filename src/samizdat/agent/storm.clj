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

(ns samizdat.agent.storm
  "The storm window: detection of a branch repeating the same call.

  Every guard before this keyed on failure — repeating-failure? needs two
  identical errors, the stuck gate needs a failure streak, over-studying?
  needs an inspection-only stretch. A model repeating the same SUCCESSFUL
  call, or alternating between two calls, tripped nothing and burned the run
  (karamazov-ekk; dirge's storm.rs was built for the same hole, and
  karamazov-j5t watched a branch re-read one file twenty turns running).

  Mechanism only, and pure: signatures, a bounded window, and predicates over
  it. The DECISION to withhold a call is a phases.edn refusal rule; the
  thresholds and the tool classification are gates.edn policy read through
  gates/storm-policy; the words the model sees are resources/prompts/storm*.md.
  Nothing here reads config, so a different policy is a data edit.

  The window holds one entry per DISPATCHED tracked call: exempt (read-only)
  tools never enter, and a withheld call never ran so it is not noted either —
  which is what keeps the originals in the window and the repeat withheld
  until the branch actually changes course."
  (:require [clojure.data.json :as json]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn- normalize
  "One canonical shape for argument values, so signature equality means call
  identity rather than serialization accident. Keys become strings (live args
  are keywordized JSON; resume re-parses the journal's verbatim JSON), map
  order is sorted, and a double that holds an integer reads as that integer —
  the same normalizations dirge's canonical_json does, and for the same
  reason: 1 vs 1.0 or key order must not let a repeat dodge the window."
  [v]
  (walk/postwalk
   (fn [x]
     (cond
       (map? x) (into (sorted-map)
                      (map (fn [[k v]]
                             [(if (keyword? k) (name k) (str k)) v]))
                      x)
       (and (double? x)
            (== x (Math/rint x))
            (<= (- Long/MAX_VALUE) x Long/MAX_VALUE)) (long x)
       (set? x) (vec (sort (map pr-str x)))
       (seq? x) (vec x)
       :else x))
   v))

(defn signature
  "The identity of one call: tool name plus canonicalized args, as a readable
  string — readable because the reflexion log quotes it back to the model."
  [tool args]
  (str tool " " (pr-str (normalize args))))

(defn tracked?
  "Whether the storm guard watches this tool at all. Exempt tools — the
  read-only surface, plus done/give_up whose repetition is the done-blocked
  and last-call rungs' business — never enter the window and are never
  withheld, at any repetition count."
  [policy tool]
  (and (:enabled? policy)
       (not (contains? (:exempt-tools policy) tool))))

(defn mutating?
  [policy tool]
  (contains? (:mutating-tools policy) tool))

(defn note-call
  "Append one dispatched call to the window, bounded at :window-size.

  A mutating call first clears the non-mutating entries: the mutation changed
  the world, so a repeated read-like call after it may legitimately answer
  differently — clearing keeps a check-fix-check cycle out of the window.
  Mutators still count amongst themselves: three identical edits IS a storm."
  [window {:keys [sig mutating? timeout? failed? error]} {:keys [window-size]}]
  ;; No default sizes or thresholds anywhere in this namespace: the numbers
  ;; are policy, and the ONLY place they live is gates.edn (storm-policy
  ;; assembles them). A missing policy key is a caller bug, not a case to
  ;; paper over with a literal src/ would then own.
  (let [w (if mutating? (filterv :mutating? window) (vec window))
        w (conj w (cond-> {:sig sig
                           :mutating? (boolean mutating?)
                           :timeout? (boolean timeout?)}
                    failed? (assoc :failed? true :error (str error))))]
    (vec (take-last window-size w))))

(defn mark-timeout
  "Flag every window entry with this signature as having timed out, dropping
  its effective threshold to the floor: a timed-out call burned its whole
  budget, so it does not get the full retry allowance."
  [window sig]
  (mapv #(if (= sig (:sig %)) (assoc % :timeout? true) %) window))

(defn repeats
  [window sig]
  (count (filter #(= sig (:sig %)) window)))

(defn blocked?
  "Whether the NEXT call with this signature should be withheld, judged
  against the prior copies in the window. At :threshold 3 the first two run
  and the third is withheld; a signature that has timed out is judged at
  :timeout-floor instead."
  [window sig {:keys [threshold timeout-floor]}]
  (let [n (repeats window sig)
        timed? (boolean (some #(and (= sig (:sig %)) (:timeout? %)) window))
        effective (if timed? timeout-floor threshold)]
    (>= n (dec effective))))

(defn oscillating?
  "Whether this call would extend an A-B-A-B alternation between exactly two
  distinct signatures for at least :min-cycles full cycles. A pure repeat is
  never oscillation — the repeat rule owns it. The two-signature requirement
  is the dead band: near-alternation among three or more calls is divergence,
  not a cycle, and counting it would manufacture phantom oscillation
  (Physics-of-Agents split-band lesson, research/2608.16578)."
  [window sig {:keys [min-cycles]}]
  (let [need (* 2 min-cycles)
        sigs (conj (mapv :sig window) sig)]
    (boolean
     (and (>= (count sigs) need)
          (let [tail (take-last need sigs)
                evens (take-nth 2 tail)
                odds (take-nth 2 (rest tail))]
            (and (apply = evens)
                 (apply = odds)
                 (not= (first evens) (first odds))))))))

(defn verify-call?
  "Whether this call runs the project's configured verify command. The verify
  loop IS repetition — red, edit, rerun the identical suite command — and the
  best run of the GLM campaign (4e785664) contained exactly one identical
  repeat: `jolt -M:test`. Blocking the third suite run would punish TDD
  itself, so a verify call is invisible to the storm guard: never counted,
  never withheld. The degenerate cousin — rerunning a red suite with no edit
  in between — stays the failure-keyed guards' business (repeating-failure?
  sees the identical error; the stuck gate sees the streak)."
  [tool args verify-cmd]
  (boolean (and (= "shell" tool)
                (not (str/blank? (str verify-cmd)))
                (str/includes? (str (or (:command args) (get args "command")))
                               (str verify-cmd)))))

(defn- ctx-verify-cmd
  [ctx]
  (get-in ctx [:config :run :verify-cmd]))

(defn repeat-blocked?
  "The phases.edn :storm rule's predicate: withhold this call as an identical
  repeat. ctx carries :branch, :tool-name, :args exactly as phase-refusal
  hands them to every refusal rule (plus the loop's :config, which supplies
  the verify command the exemption reads)."
  [{:keys [branch tool-name args] :as ctx} policy]
  (and (tracked? policy tool-name)
       (not (and (:verify-exempt? policy)
                 (verify-call? tool-name args (ctx-verify-cmd ctx))))
       (blocked? (:storm-window branch) (signature tool-name args) policy)))

(defn oscillation-blocked?
  "The phases.edn :storm-oscillation rule's predicate: withhold this call as
  the extension of a two-call alternation."
  [{:keys [branch tool-name args] :as ctx} policy]
  (and (tracked? policy tool-name)
       (not (and (:verify-exempt? policy)
                 (verify-call? tool-name args (ctx-verify-cmd ctx))))
       (oscillating? (:storm-window branch) (signature tool-name args) policy)))

(defn last-failure-of
  "The most recent window entry for this signature that failed, or nil — the
  seam retry-carrying-diagnosis reads (karamazov-g86, after J-Space's rule
  that a retry must inherit the diagnosis, never go in blank)."
  [window sig]
  (->> (rseq (vec window))
       (filter #(and (= sig (:sig %)) (:failed? %)))
       first))

;; --- the file-touch streak (karamazov-g86, dirge context_depth.rs) ----------

(defn touched-paths
  "The file paths one call names, as a set of strings — the :path/:file/:paths
  arguments, both key spellings. Empty when the call touches no file."
  [args]
  (let [args (if (map? args) args {})
        one (fn [k] (or (get args k) (get args (name k))))
        ps (concat (some-> (one :path) vector)
                   (some-> (one :file) vector)
                   (let [v (one :paths)] (if (coll? v) v (some-> v vector))))]
    (into #{} (comp (map str) (remove str/blank?)) ps)))

(defn note-file-touch
  "One dispatched call's effect on the same-file streak: consecutive
  file-touching calls whose path sets overlap. The tracked set NARROWS to the
  intersection, so divergent touches eventually break the streak on their
  own; a call touching no file resets it. Counts calls, not turns — several
  edits to one file in one turn advance it by several, which is the thrash
  being watched for."
  [{:keys [streak files] :or {streak 0 files #{}}} paths]
  (cond
    (empty? paths) {:streak 0 :files #{}}
    (empty? files) {:streak 1 :files paths}
    :else (let [common (set/intersection files paths)]
            (if (seq common)
              {:streak (inc streak) :files common}
              {:streak 1 :files paths}))))

(defn window-from-turns
  "Rebuild the window from journal turn rows on resume, so a resumed branch
  keeps its protection — unlike repeating-failure?, which resume.clj documents
  comes back blind. Rows are journal/branch-turns projections: :tool_name,
  :args (the model's JSON verbatim), :category, :result.

  A timed-out row is recognized by its result text — the flag itself is not
  journalled, and the timeout message is machine-written so the match is
  stable. Mechanics rows are included even though a withheld call was never
  noted live: the journal cannot tell a refusal from a malformed call, and
  over-counting a repeat after resume errs toward protection."
  [rows policy]
  (reduce
   (fn [w {:keys [tool_name args category result]}]
     (let [parsed (when (tracked? policy tool_name)
                    (try (json/read-str (str args) :key-fn keyword)
                         (catch Throwable _ nil)))]
       (if (or (not (tracked? policy tool_name))
               (and (:verify-exempt? policy)
                    (verify-call? tool_name parsed (:verify-cmd policy))))
         w
         (let [sig (signature tool_name parsed)
               failed? (= "failure" (str category))
               digest-chars (:error-digest-chars policy)
               w (note-call w (cond-> {:sig sig
                                       :mutating? (mutating? policy tool_name)}
                                failed?
                                (assoc :failed? true
                                       :error (let [s (str result)]
                                                (if (and digest-chars
                                                         (> (count s) digest-chars))
                                                  (subs s 0 digest-chars)
                                                  s))))
                            policy)]
           (if (str/includes? (str result) "[timed out after")
             (mark-timeout w sig)
             w)))))
   []
   rows))
