;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.model.interleave
  "Exhaustive interleaving of a few thread programs over a shared state, so a
  property is PROVED over the model rather than sampled by a run.

  A concurrency bug that shows up one run in seven is one the suite samples;
  running it more often explores more schedules and never says it has seen
  them all. `schedules` uses core.logic to enumerate every interleaving of the
  programs (each a vector of actions, its own order preserved). A model then
  supplies a `step` of [state action] -> state, deterministic by construction,
  and `violations` threads every schedule through it and keeps the ones whose
  final state fails the property. Models stay deliberately small: they cover
  an arbitration, not a subsystem, and a model that says sound is a statement
  about what it models.

  Adapted from jlt-commons/ebb model/ebb/model/interleave.clj (EPL-2.0)."
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [defne run*]]))

(defne interleaveo
  "zs is an interleaving of xs and ys, preserving each one's own order."
  [xs ys zs]
  ([() _ ys])
  ([_ () xs])
  ([[x . xr] [y . _] [x . zr]] (interleaveo xr ys zr))
  ([[x . _] [y . yr] [y . zr]] (interleaveo xs yr zr)))

(defn schedules
  "Every interleaving of the given thread programs (a seq of action vectors)."
  [progs]
  (distinct
   (reduce (fn [acc prog] (mapcat (fn [a] (run* [z] (interleaveo a prog z))) acc))
           [(first progs)]
           (rest progs))))

(defn run-schedule
  "The state reached by applying every action of `sched` to `s0` through `step`."
  [step s0 sched]
  (reduce step s0 sched))

(defn violations
  "Every schedule of `progs` whose final state fails `ok?`, as
  [schedule final-state] pairs. Empty means the property holds over the model."
  [step s0 ok? progs]
  (for [sched (schedules progs)
        :let [end (run-schedule step s0 sched)]
        :when (not (ok? end))]
    [(vec sched) end]))
