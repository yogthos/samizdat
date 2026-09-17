;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.llm.grammar
  "GBNF for llama.cpp's sampler: the fenced tool-call convention as a grammar
  (karamazov-fp21.1).

  WHY. On a llama.cpp endpoint the only force a branch had was the
  tools-array + tool_choice fallback, which the chat template renders into
  the PROMPT — a prefix rewrite, so a cache miss — and which needs a template
  that knows tool calls at all. A grammar is applied at sampling: it never
  touches the prompt, so it is cache-safe, it works under any template, and
  it can say 'any tool but this one' as easily as 'this tool', which no
  tool_choice can. Prefill, the other cache-safe force, continues a trailing
  assistant message and only DeepSeek's beta endpoint does that.

  WHAT IT GUARANTEES. Every ```tool-call fence OUTSIDE a think block is a
  well-formed call whose name is on the list — and, when required, that at
  least one exists, so the reply cannot END without a call (the sampler
  accepts end-of-generation only in an accepting state). The parser takes the
  last fence outside the reasoning (fence/parse-tool-call), which is exactly
  the fence this constrains. Inside <think>…</think> the text is free: a
  model drafts fences while reasoning, and pinning those would either make
  the draft the call or forbid the draft.

  HOW. 'Text that does not contain the opener' is a regular language whose
  DFA is the KMP automaton of the opener with its accepting state removed —
  one rule per state, an optional alternation over the pattern's alphabet
  plus a negated class for everything else. Written out rather than as a
  wildcard, because `any* opener` is ambiguous: the sampler keeps a second
  stack on which the opener is just more prefix, and on THAT stack the fence
  is unconstrained.

  Measured 2026-09-17 on VibeThinker-3B under llama.cpp b10809: the grammar
  cost nothing (49.4 tok/s against 49.6 unconstrained) and every forced reply
  ended in a well-formed fence.

  Mechanism only. Which turns send one is samizdat.agent.infer reading
  gates.edn :local-grammar; whether the endpoint takes it is the adapter's
  llama-cpp-endpoint? check."
  (:require [clojure.string :as str]))

(def opener "```tool-call\n")
(def think-open "<think>")
(def think-close "</think>")

;; --- 'text that does not contain P' as GBNF rules ----------------------------

(defn- failure-table
  "KMP failure function of `p`: fail[i] is the length of the longest proper
  prefix of p[0..i] that is also its suffix."
  [p]
  (let [n (count p)]
    (loop [i 1 k 0 fail (vec (repeat n 0))]
      (if (>= i n)
        fail
        (let [k (loop [k k]
                  (if (and (pos? k) (not= (nth p i) (nth p k)))
                    (recur (nth fail (dec k)))
                    k))
              k (if (= (nth p i) (nth p k)) (inc k) k)]
          (recur (inc i) k (assoc fail i k)))))))

(defn next-state
  "The KMP state after reading character `c` in state `k` — `k` characters of
  `p` matched so far. Answers `(count p)` when `c` completes the pattern,
  overlaps included: four backticks before `tool-call` still contain the
  opener."
  [p k c]
  (let [fail (failure-table p)
        n (count p)]
    (loop [k k]
      (cond
        (and (< k n) (= (nth p k) c)) (inc k)
        (zero? k) 0
        :else (recur (nth fail (dec k)))))))

(defn- escape-literal
  "One character inside a GBNF double-quoted literal."
  [c]
  (case c
    \newline "\\n"
    \return "\\r"
    \tab "\\t"
    \\ "\\\\"
    \" "\\\""
    (str c)))

(defn- escape-class
  "One character inside a GBNF `[...]` class."
  [c]
  (case c
    \newline "\\n"
    \return "\\r"
    \tab "\\t"
    \\ "\\\\"
    \] "\\]"
    \^ "\\^"
    \- "\\-"
    (str c)))

(defn no-substring-rules
  "GBNF rules `<name>0` … `<name>(n-1)` recognising the strings that do not
  contain `p`: one per KMP state, each an optional alternation over p's
  alphabet with a negated class for every other character. No rule exists for
  state n, so the transition that would complete `p` does not exist either."
  [name p]
  (let [n (count p)
        alphabet (sort (distinct (seq p)))
        other (str "[^" (str/join (map escape-class alphabet)) "] " name "0")]
    (vec
     (for [k (range n)]
       (let [alts (into [other]
                        (keep (fn [c]
                                (let [t (next-state p k c)]
                                  (when (< t n)
                                    (str "\"" (escape-literal c) "\" " name t))))
                              alphabet))]
         (str name k " ::= ( " (str/join " | " alts) " )?"))))))

;; --- the fence ---------------------------------------------------------------

(defn- literal [s]
  (str "\"" (str/join (map escape-literal s)) "\""))

(def ^:private json-rules
  ["object ::= \"{\" ws ( member ( \",\" ws member )* )? \"}\""
   "member ::= string ws \":\" ws value ws"
   "value ::= object | array | string | number | \"true\" | \"false\" | \"null\""
   "array ::= \"[\" ws ( value ( \",\" ws value )* )? \"]\""
   "string ::= \"\\\"\" ( [^\"\\\\\\x7F\\x00-\\x1F] | \"\\\\\" ( [\"\\\\bfnrt/] | \"u\" [0-9a-fA-F]{4} ) )* \"\\\"\""
   "number ::= \"-\"? ( \"0\" | [1-9] [0-9]* ) ( \".\" [0-9]+ )? ( [eE] [-+]? [0-9]+ )?"
   "ws ::= [ \\t\\n\\r]*"])

(defn fence-grammar
  "The GBNF under which a reply's tool calls are well-formed fences naming one
  of `tools`. `:require? true` demands at least one call outside the
  reasoning — the force; false leaves the call optional but still constrained
  — the restriction. nil when there is nothing to allow: an empty list would
  be a grammar no reply can satisfy."
  [{:keys [tools require?]}]
  (let [names (vec (distinct (map str tools)))]
    (when (seq names)
      (str/join
       "\n"
       (concat
        [(str "root ::= think? ( prefix call )" (if require? "+" "*") " prefix")
         (str "think ::= " (literal think-open) " t0 " (literal think-close))
         "prefix ::= p0"]
        (no-substring-rules "p" opener)
        (no-substring-rules "t" think-close)
        [(str "call ::= " (literal opener)
              " ws \"{\" ws \"\\\"name\\\"\" ws \":\" ws name ws \",\" ws"
              " \"\\\"args\\\"\" ws \":\" ws object ws \"}\" ws \"```\"")
         (str "name ::= " (str/join " | " (map #(literal (str "\"" % "\"")) names)))]
        json-rules)))))
