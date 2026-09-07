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

(ns samizdat.memory
  "How a memory earns its place: the salience model, as pure arithmetic.

  Adopted from dirge's `src/extras/salience.rs`, which ports the converged
  LangMem/MemoryOS taxonomy. The shape is theirs and the reasoning behind each
  term is theirs; what is different here is where the numbers live.

  In dirge they are Rust constants. Here they are `gates.edn :memory`, because
  the role that reads memories and acts on them is the supervisor, and the
  supervisor is the role that can edit policy. A harness that learns from
  experience but cannot adjust what it learns FROM is only half a loop.

  Everything in this namespace is pure and takes its policy as an argument or
  reads it through the lexicon at call time. Nothing here decides WHEN to
  remember or what to do about a recalled memory — that is the loop's business
  (RFC-001)."
  (:require [samizdat.lexicon :as lexicon]))

(defn policy
  "The memory model's constants, from gates.edn."
  []
  (lexicon/policy :memory))

(defn base-salience
  "The starting importance for a memory of `kind`.

  The ordering is the claim: who we are outranks what is true, which outranks
  how to do things, which outranks what happened once, which outranks what we
  are doing right now. An unrecognised kind gets the `:note` default rather
  than zero — a memory whose kind nobody thought to classify is still a
  memory, and starting it at the floor would bury it before anyone read it."
  ([kind] (base-salience kind (policy)))
  ([kind p]
   (let [table (:kind-salience p)]
     ;; No compiled fallback beyond the table's own :note entry — a default
     ;; hidden in code is the value nobody can retune, which is the whole thing
     ;; this model puts in gates.edn.
     (or (get table (keyword (name (or kind :note))))
         (get table :note)))))

(defn effectiveness
  "The signed, log-damped, bounded contribution of a memory's track record.

  Zero for an even record, and for no record at all — which is the important
  case, because most memories are never reported on and must not be penalised
  for it. Log-damped so the FIRST confirmation buys most of what confirmation
  can buy and a memory cannot be voted to the top by repetition; capped so a
  hot playbook cannot outrank a durable identity fact on its record alone."
  ([successes failures] (effectiveness successes failures (policy)))
  ([successes failures p]
   (let [net (- (or successes 0) (or failures 0))]
     (if (zero? net)
       0.0
       (let [magnitude (min (* (Math/log10 (+ 1.0 (Math/abs (double net))))
                               (:effectiveness-weight p))
                            (:effectiveness-cap p))]
         (if (pos? net) magnitude (- magnitude)))))))

(defn confidence-bonus
  "How much a memory's truth-likelihood moves its ranking.

  Centred on the default so an unremarked memory is neutral, and weighted low
  so the full [0,1] range is a ±0.1 swing — smaller than the gaps between kind
  tiers. Confidence is a TIEBREAK within a tier, not a way for a contested
  claim to jump above a durable one. Salience is importance and confidence is
  truth-likelihood; a fact can be important but contested, or trivial but
  certain, and a model that collapsed them would lose exactly the distinction
  a supervisor needs when two memories disagree."
  ([confidence] (confidence-bonus confidence (policy)))
  ([confidence p]
   (* (- (or confidence (:default-confidence p)) (:default-confidence p))
      (:confidence-weight p))))

(defn- recently-used?
  "Used, and within the window — a window counted in RUNS, not days.

  A run is an opportunity for a memory to be needed; a day is not. The
  wall-clock window this replaced never fired across a whole campaign (every
  row was younger than it) while the store had sunk to the floor by another
  path (karamazov-4ay9). `idle_runs` is bumped at each run end the memory
  was not used in and reset when it is; a memory never used has no recency
  to speak of, whatever its count says."
  [{:keys [last_used_at idle_runs]} p]
  (boolean (and last_used_at
                (<= (long (or idle_runs 0))
                    (long (:recent-use-window-runs p))))))

(defn corroboration-bonus
  "The bounded contribution of having been seen again by other runs.

  A bounded associative memory ranks a slot by the attention it accumulates;
  here a pattern accumulates the DISTINCT runs that re-observed it, which the
  store already counted and ranked by nowhere — so a finding confirmed by
  seven runs sat at the floor under a command that worked once
  (karamazov-4ay9). Same shape as `effectiveness`: zero for the first
  sighting, which is an observation and not a pattern; log-damped so the
  second buys the most and repetition cannot buy the top; capped the same."
  ([corroborations] (corroboration-bonus corroborations (policy)))
  ([corroborations p]
   (let [n (max 1 (long (or corroborations 1)))]
     (min (* (Math/log10 (double n)) (:corroboration-weight p))
          (:corroboration-cap p)))))

(defn effective-salience
  "What a memory is worth right now: its stored importance, plus what its use
  and its record say about it.

    salience + recent-use bonus + effectiveness + corroboration + confidence

  This is a RANKING number, not a stored one. `salience` moves slowly, by
  reinforcement and decay; the other terms are read off the row every time,
  so a memory's standing reflects what has happened to it without anything
  having to rewrite it."
  ([row] (effective-salience row (policy)))
  ([row p]
   (+ (double (or (:salience row) (base-salience (:kind row) p)))
      (if (recently-used? row p) (:recent-use-bonus p) 0.0)
      (effectiveness (:success_count row) (:failure_count row) p)
      (corroboration-bonus (:corroborations row) p)
      (confidence-bonus (:confidence row) p))))

(defn reinforced
  "The salience a memory should carry after being USED — being looked up is
  itself the relevance signal. Capped, so use alone never reaches the top of
  the scale: the ceiling belongs to the kinds that earned it."
  ([salience] (reinforced salience (policy)))
  ([salience p]
   (min (:salience-cap p)
        (+ (double (or salience (base-salience :note p))) (:use-reinforcement p)))))

(defn decayed
  "The salience a memory should carry after going unused past the window.

  Floored rather than allowed to reach zero: a memory that decayed to nothing
  would be indistinguishable from one that was never important, and the
  difference — this WAS worth writing down, and has not been needed since — is
  worth keeping. Pinned memories are the caller's business to exclude."
  ([salience] (decayed salience (policy)))
  ([salience p]
   (max (:decay-floor p)
        (- (double (or salience (base-salience :note p))) (:disuse-decay p)))))

(defn rank
  "Memories, most worth reading first. TIES KEEP THE CALLER'S ORDER: sort-by
  is stable, so memories of equal standing stay in the order the text search
  ranked them — bm25 relevance from the FTS path, newest-first from the LIKE
  scan. The old created_at tie key threw that second opinion away (and
  delivered oldest-first while its docstring promised recency —
  karamazov-blt.38): a two-term bm25 winner lost its place to a one-term
  match that happened to be newer."
  ([rows] (rank rows (policy)))
  ([rows p]
   (->> rows
        (sort-by #(- (effective-salience % p)))
        vec)))
