;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.supervisor
  "Watching a branch's own turn pattern for trouble the failure- and
  artifact-keyed gates miss, and the correction for it.

  The failure gates (stuck) key on errors; the progress gate keys on confirmed
  artifacts. A run that INSPECTS without shipping — reads, greps, evals for turn
  after turn, all succeeding, producing nothing — trips neither, and a live run
  did exactly that (karamazov-j5t): it re-read the same file twenty turns
  running and had to be steered by hand.

  This namespace is the seat of a SUPERVISORY ROLE. Today a single gate reads
  over-studying? each turn; the same primitives are what the multi-agent
  orchestrator's supervisor sub-loop (karamazov-ioo.hc7) will read to watch a
  worker — so the detection and the correction live here, apart from any one
  consumer, and grow as the role does."
  (:require [clojure.string :as str]))

;; The shipping vocabulary lives in gates.edn (:tool-vocab :shipping, tier
;; 1a) and is passed in by the caller, keeping this namespace pure and free
;; of the config layer. Shipping tools change or run something — real
;; movement toward finishing; anything else counts as studying.

(defn shipped? [tools entry] (contains? tools (:tool entry)))

(defn over-studying?
  "The branch shipped something at some point, then spent the last `threshold`
  turns only inspecting. The arm-after-shipping guard is deliberate: a run's
  opening exploration — reading its way into an unfamiliar area — is never
  nagged; the stall is when a branch that WAS making changes has lapsed into
  reading and stopped moving."
  [tools turns threshold]
  (let [turns (vec turns)]
    (and (>= (count turns) threshold)
         (some (partial shipped? tools) turns)
         (not-any? (partial shipped? tools) (take-last threshold turns)))))

(defn recent-studying-tools
  "The distinct inspection tools the branch has leaned on in its last
  `threshold` turns — named back to it so the nudge is concrete."
  [tools turns threshold]
  (->> (take-last threshold turns) (keep :tool) (remove tools) distinct vec))

(defn stall-nudge
  "The correction: stop studying, commit and test. Names the tools it has been
  cycling so the message is not generic."
  [tools turns threshold]
  (str "You have spent " threshold " turns inspecting ("
       (str/join ", " (recent-studying-tools tools turns threshold))
       ") without changing anything. Commit your best current version to a file"
       " and test it — a rough version you refine beats more reading. If you were"
       " re-checking something already done, it is done: move to the next step"
       " or finish."))

;; --- looking in the wrong place ---------------------------------------------
;;
;; Every other guard here measures whether the branch is DOING something.
;; This one measures whether it is looking in the right PLACE, which is a
;; different question and the one run bd56a286 could not answer for itself.

(defn- same-failure? [a b] (= (str (:error a)) (str (:error b))))

(defn since-last-write
  "The branch's turns after its most recent file write — the window in which
  the implementation has not changed. The whole history when it has never
  written."
  [writes turns]
  (->> (vec turns)
       reverse
       (take-while #(not (contains? writes (str (:tool %)))))
       reverse
       vec))

(defn repeating-one-failure?
  "The branch has run the code `n` times since it last changed the code, and
  got back the SAME failure every time.

  THE STALL THIS EXISTS FOR, from run bd56a286: a strong model wrote five
  namespaces and five test namespaces, reached ten failures, and then spent
  238 turns reading the implementation, re-running the suite, and reading the
  implementation again. The ten failures were in its own TESTS — two
  assertions inverted against their own descriptions, one bare float equality
  — and its single edit in that whole span went to the implementation, the one
  place the bug was not.

  It could not escape because it trusted its tests and doubted its code, so
  every re-read CONFIRMED the code was fine and justified reading again. The
  evidence it kept gathering was evidence against a hypothesis it was not
  entertaining.

  No existing gate reaches it. `:no-edits` says write a file — which file? the
  code is correct. `:studying` says commit and test — it tested thirteen
  times. Both measure activity, and the branch was active.

  THE SAME failure, not merely repeated failures: a branch working through
  different errors is making progress, and telling it to doubt its tests would
  be worse than silence. Unchanged implementation plus an unchanging failure
  is what distinguishes searching in the wrong place from searching."
  [writes verifiers turns n]
  (let [window (since-last-write writes turns)
        fails (filterv #(and (contains? verifiers (str (:tool %)))
                             (= :failure (:category %))
                             (not (str/blank? (str (:error %)))))
                       window)]
    (boolean (and (>= (count fails) n)
                  (every? (partial same-failure? (peek fails)) fails)))))

(defn unchanged-failure
  "The failure the branch keeps getting, clipped — so the steer can quote what
  it is actually staring at rather than describing it. nil when there is none."
  [writes verifiers turns chars]
  (when-let [f (peek (filterv #(and (contains? verifiers (str (:tool %)))
                                    (= :failure (:category %)))
                              (since-last-write writes turns)))]
    (let [e (str/replace (str (:error f)) #"\s+" " ")]
      (subs e 0 (min chars (count e))))))
