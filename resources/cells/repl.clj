;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; THE REPL SESSION as cells. A session is orient -> declare -> explore ->
;; land, and the `repl` manifest is where that order is written down.
;;
;; Why it exists, from run bd56a286: a strong model spent 238 turns in the
;; REPL hunting a defect that was in its own tests, and a sibling branch read
;; for 316 turns and wrote nothing at all. Neither was reachable by steering —
;; nudges are 0-for-6 across two model tiers. What was missing was not
;; encouragement but a CONTRACT: name the files you are about to change before
;; you start exploring, and land them before you stop.
;;
;; The two conditions are enforced as phases.edn refusals (eval needs a plan;
;; done needs the plan landed) because a withhold holds and advice does not.
;; These cells compute the session's SHAPE — which step a branch is in and
;; what it still owes — so the digest and any workflow can read it, and so the
;; order is data an agent can rewrite rather than control flow in src/.
(ns cells.repl
  (:require [mycelium.cell :as cell]
            [samizdat.agent.state :as state]))

(cell/defcell :repl/orient
  {:doc "Entry. The branch has no plan yet: reading is open (read_file, grep,
        lsp, shell) and the REPL is closed. This is where the hypothesis comes
        from — read the failing assertion and the code it calls, and decide
        which of the two is lying."
   :pure true
   :requires []
   ;; The entry node, so it requires only what the driver seeds. Every cell in
   ;; this file writes :repl/step — the session's position, which the phases
   ;; refusals read — and each adds the one fact its own step establishes.
   :input  [:map [:branch :map]]
   :output [:map [:repl/step :keyword] [:repl/may-eval? :boolean]]}
  (fn [_ data]
    (assoc data :repl/step :orient
                :repl/may-eval? false)))

(cell/defcell :repl/declare
  {:doc "The commitment, made with the `plan` tool:

          plan({\"files\": [\"src/…\"], \"tests\": [\"test/…\"], \"goal\": \"one line\"})

        `files` is what will be created or edited, `tests` what will be
        written; both land in the same debt, so a declared test nobody writes
        blocks the exit exactly as a declared source file does. At least one
        path is required — an empty declaration is not one, and naming no file
        is exactly the state this step exists to rule out.

        Enforced by the phases.edn refusal :repl-needs-a-plan, which withholds
        `eval` until this has happened. Calling plan again REPLACES the
        previous declaration rather than adding to it: the contract is commit
        to a hypothesis, not never change your mind."
   :pure true
   :requires []
   :input  [:map [:branch :map]]
   ;; :repl/planned? is what the manifest dispatches on — :planned to the
   ;; REPL, :empty back to :start. The cell writes the key and the edge reads
   ;; it, which is the rule the manifests are held to.
   :output [:map [:repl/step :keyword] [:repl/planned? :boolean]
            [:repl/plan :any]]}
  (fn [_ {:keys [branch] :as data}]
    (assoc data :repl/step :declare
                :repl/planned? (state/planned? branch)
                :repl/plan (state/plan branch))))

(cell/defcell :repl/explore
  {:doc "The REPL, open. Bounded by having said what it is for rather than by
        a turn budget: a branch exploring against a named file is doing the
        work, and a branch exploring against nothing is the failure this
        workflow was built after."
   :pure true
   :requires []
   :input  [:map [:branch :map]]
   :output [:map [:repl/step :keyword] [:repl/may-eval? :boolean]]}
  (fn [_ {:keys [branch] :as data}]
    (assoc data :repl/step :explore
                :repl/may-eval? (state/planned? branch))))

(cell/defcell :repl/land
  {:doc "Exit. What the branch said it would change and has not yet written.
        Empty means the session may close; anything else is the debt the
        phases.edn refusal :plan-not-landed withholds `done` on, naming the
        outstanding paths back. Discharged by a SUCCESSFUL write from any
        :file-write tool (write_file, edit_file, patch) — reading a file you
        promised to change is not changing it. The REPL dies with the branch;
        only a file survives it."
   :pure true
   :requires []
   :input  [:map [:branch :map]]
   ;; :repl/complete? is the exit condition the manifest loops :land on until
   ;; it goes true — the debt is empty and the session may end.
   :output [:map [:repl/step :keyword] [:repl/unwritten :any]
            [:repl/complete? :boolean]]}
  (fn [_ {:keys [branch] :as data}]
    (let [owed (state/unwritten branch)]
      (assoc data :repl/step :land
                  :repl/unwritten owed
                  :repl/complete? (empty? owed)))))
