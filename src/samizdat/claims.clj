;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.claims
  "Whether a claim about the world survives the run's own record.

  THE ASYMMETRY THIS CLOSES (karamazov-ei6t). A claim about the CODE faces a
  deterministic evidence block, a focused verify, four lexical ship rungs and
  a two-pass critic. A claim about the WORLD faced nothing: knowledge/remember!
  checks that content is non-blank, and an intervene directive is checked
  against nothing at all. So the harness is rigorous about `this function
  works` and credulous about `requires do not work in this project`.

  THE RECORDED HARM is karamazov-ko5b. The supervisor had ONE failing require,
  at its own turn 2, and wrote a standing directive saying requires of any
  flight.* namespace fail and should not be retried. Its own turns 5 and 12-14
  required four flight namespaces successfully and ran a 19,254-frame loop
  with them. A general claim from one observation, contradicted by the run's
  own turn record, with nothing in the path to notice.

  TWO CHECKS, AND THEY FAIL DIFFERENTLY, which is why both are here. The
  mechanical one (`contradicted-by-record`) reads the turns and cannot
  hallucinate, but only sees the conflicts somebody wrote a rule for. The
  judge (RFC-014's two-pass critic, wired by the caller) catches semantic error
  and costs a model call. lemmalog derives contradictions by rule at read time;
  brainapi2 verifies at write with a Janitor loop. Both were read for this and
  neither is adopted whole — the rules transfer, the engines do not.

  WHY samizdat.symbolic IS THE SUBSTRATE. A run's turns already ARE tuples —
  [:called tool], [:succeeded tool], [:refused command] — and the question `is
  there a turn that contradicts this claim` is a join, not a scan with a
  special case per claim shape. It is also the same engine the gates and the
  procedural graph run on, so a project retuning what counts as a contradiction
  edits rules rather than code.

  IT IS A MATCHER, NOT A DEDUCTIVE DATABASE, and that bounds what is ported.
  `fire` reports which rules matched; it never adds derived facts back into the
  db, so there is no fixpoint, no stratification and no negation-as-absence
  here. Contradiction is a single-pass join and needs none of them — but a
  reader coming from lemmalog should not expect the closure machinery, because
  it is not there."
  (:require [clojure.string :as str]
            [samizdat.lexicon :as lexicon]
            [samizdat.symbolic :as sym]))

;;; ------------------------------------------------------------ the record

(defn record-facts
  "A run's turns as tuples the rules can join over.

  Three relations and no more, because each is derived from a column rather
  than from prose: `:called` that a tool was dispatched, `:worked` that it came
  back a success, `:failed` that it did not. Category is what the loop already
  decided about the turn, so this adds no judgement of its own — which is the
  property that makes the mechanical check worth having beside a judge."
  [turns]
  (into []
        (mapcat (fn [t]
                  (let [tool (some-> (:tool_name t) str not-empty)
                        cat (some-> (:category t) str)]
                    (when tool
                      (cond-> [[:called tool]]
                        (= "failure" cat) (conj [:failed tool])
                        (not= "failure" cat) (conj [:worked tool]))))))
        turns))

;;; ------------------------------------------------------------- the rules

(defn subject-tools
  "The tool names a claim is ABOUT, from the tool vocabulary.

  A claim naming no tool is not one this check can judge, and says so by
  returning nothing — the honest answer for a mechanical test, and the reason
  a judge stays in the ladder beside it."
  [claim tools]
  (let [l (str/lower-case (str claim))]
    (into [] (filter #(str/includes? l (str/lower-case (str %)))) tools)))

(defn denial?
  "Whether a claim asserts that something does NOT work.

  The vocabulary is wordlists.edn :negation-markers, matched with a trailing
  space so `no ` cannot read out of `note`. Shared with memory/contradicts?
  deliberately: two places asking the same question of the same language
  should not drift apart on what counts as a denial."
  [claim]
  (let [l (str/lower-case (str claim))]
    (boolean (some #(str/includes? l (str % " ")) (lexicon/wordlist :negation-markers)))))

(defn contradicted-by-record
  "Every way the run's own turns contradict `claim`, as data.

  Only the shape the harm had: the claim says a tool does not work, and the
  record shows that tool working in this very run. That narrowness is the
  point — a check that fired on every disagreement would refuse the ordinary
  case of a run learning something new, and a check nobody trusts gets turned
  off.

  Returns [{:tool :worked n :claim}], empty when nothing contradicts — which
  includes the case where the claim names no tool at all, since a mechanical
  test that cannot see the subject must not report an opinion about it.

  `tools` is INJECTED rather than read from the registry here. This namespace
  is pure and knows nothing about the tool tree, which is what lets a test
  drive it with three names and what keeps a store-level check from dragging
  in the agent's whole capability surface."
  [claim turns tools]
  (if-not (denial? claim)
    []
    (let [tuples (record-facts turns)
          db (sym/facts tuples)
          ;; HOW OFTEN, counted from the tuples rather than from the query.
          ;; sym/query distincts its bindings, so a var-free clause answers
          ;; [{}] whether the tool worked once or forty times — which reads as
          ;; a count and is not one. The join decides WHETHER; the tuples say
          ;; how much, and an escalation saying \"it worked 14 times in this
          ;; run\" is a different argument from one saying it worked.
          n-worked (frequencies (keep (fn [[rel t]] (when (= :worked rel) t)) tuples))
          tools (subject-tools claim (map str tools))
          hits (for [t tools
                     :when (seq (sym/query db [[:worked t]]))]
                 {:tool t :worked (get n-worked t 0) :claim (str claim)})]
      (vec hits))))
