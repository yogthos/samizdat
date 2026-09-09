;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.phases
  "The branch phase machine, result transitions, and winner rubric from
  resources/phases.edn (drg-4026 #3/#30/#34).

  Same shape as gates.clj's config machinery — io/resource so the path works
  interpreted and inside an AOT binary, cached, reloadable — but a separate
  namespace for the same reason wordlists.clj is one: state.clj sits BELOW
  gates in the require graph (gates requires state) and cannot read its
  accessor, while the phase table and the winner rubric are consumed down
  there. system.clj calls reload! on every start."
  (:require [samizdat.userspace :as userspace]
            [samizdat.util :as util]))

(defn- load-phases
  "The phase table for the current project, through the userspace seam. Like
  gates.edn this is a policy table: a project whose work does not divide into
  an explore prologue and a build phase should be able to say so for itself."
  []
  (userspace/edn-body! :policy "phases"))

(def ^:private cache (atom (load-phases)))

;; Watched by the winner rubric, which state.clj COMPILES from :finished-key.
;; Without it reload! swapped this atom and left the compiled rubric frozen at
;; namespace load — the one part of phases.edn that is forms rather than
;; lookups was the one part a reload could not reach.
(def ^:private generation (atom 0))

(defn gen
  "The phase table's generation. Derived tables cache against this."
  []
  @generation)

(defn reload! []
  (reset! cache (load-phases))
  (swap! generation inc)
  nil)

(defn table
  "The whole phases.edn map — :initial-phase, :phases, :transitions,
  :finished-key."
  []
  @cache)

(defn initial-phase
  "The phase a new branch starts in (:initial-phase)."
  []
  (:initial-phase @cache))

(defn phase
  "The phase table entry for `p` — {:cap-key ... :next ... :withholds ...}
  or nil for an unknown phase."
  [p]
  (get-in @cache [:phases p]))

(defn next-phase
  "The phase that follows `p` per the table, or nil when it has none."
  [p]
  (:next (phase p)))

(defn withholds
  "The tool names phase `p` withholds (base/phase-refusal consults this)."
  [p]
  (:withholds (phase p)))

;; --- conditional withholding ------------------------------------------------
;;
;; `:withholds` answers "which tools does THIS PHASE forbid", and that is the
;; only question it can answer, because a phase is all it is handed. RFC-008's
;; gap needed a different one: the board is encouraged and not enforced, and
;; nothing refuses a tool call from a branch holding no task — which is a fact
;; about the BRANCH, not about its phase. `phases.edn :refusals` is the table
;; that can say it.
;;
;; Same compilation discipline as gates.edn's `:when` forms, and for the same
;; reason: the STRUCTURE compiles once so a broken form fails at load rather
;; than mid-run, while everything the form reads is read at fire time so the
;; policy stays runtime-editable. The form sees `branch`, `tool-name` and the
;; whole `ctx` as plain locals and nothing else; what it calls it names fully
;; qualified.
;;
;; AND IT LOADS WHAT IT NAMES. This namespace requires none of the namespaces
;; the shipped rules call — storm, gates, files — because it sits below gates
;; in the require graph, and until now it relied on tools/base having loaded
;; them before anyone compiled the table. The table is memoized on first use,
;; so the first caller decided: a run's eval image that required phases before
;; tools compiled rules that threw "No such var: samizdat.agent.gates/
;; storm-policy" on every call, after the namespaces had loaded too. Requiring
;; the namespaces a form names, at compile time, makes the table correct
;; whoever compiles it first (karamazov-b76m's validation run found it).

(defn namespaces-named
  "The namespaces a form calls, as symbols: every qualified symbol in it,
  by its namespace part, once. Pure; a keyword's namespace is not a
  namespace and is not counted."
  [form]
  (into #{}
        (comp (filter symbol?) (keep namespace) (map symbol))
        (tree-seq coll? seq form)))

(defn- compile-refusal
  [entry]
  (let [form (:when entry)]
    (doseq [ns-sym (namespaces-named form)]
      (require ns-sym))
    (assoc entry
           :when-form form
           :when (binding [*ns* (the-ns 'samizdat.agent.phases)]
                   (eval `(fn [~'ctx]
                            (let [~'branch    (get ~'ctx :branch)
                                  ~'tool-name (get ~'ctx :tool-name)]
                              ~form)))))))

(def refusals
  "The compiled conditional withholds, in table order — first match wins.

  A function memoized against the generation, not a top-level value: as a
  `def` the table would be compiled once at namespace load and `reload!`
  would move the thresholds the forms read without moving the forms, which is
  the bug gates.clj's own table had and fixed."
  (util/generation-cache gen #(mapv compile-refusal (:refusals @cache))))

(defn transitions
  "The result-signal transitions: get-in paths into the turn envelope,
  each mapping to the effect names loop.clj applies."
  []
  (:transitions @cache))

(defn finished-key-forms
  "The winner-rubric component forms, compiled by state.clj at load."
  []
  (:finished-key @cache))
