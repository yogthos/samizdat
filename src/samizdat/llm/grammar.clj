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

  WHAT IT GUARANTEES. Every ```tool-call fence AFTER the reasoning is a
  well-formed call whose name is on the list — and, when required, that at
  least one exists, so the reply cannot END without a call (the sampler
  accepts end-of-generation only in an accepting state). The parser takes the
  last fence outside the reasoning (fence/parse-tool-call), which is exactly
  the fence this constrains.

  THE REASONING IS FREE, and the grammar has to be TOLD it is there. Under a
  reasoning split (llama.cpp --jinja with a thinking template) the sampled
  text begins INSIDE the think block — the template put the opener in the
  prompt — so there is no `<think>` to match on, only the closer at the end.
  Measured on Ternary-Bonsai-2-27B, 2026-09-18: a grammar that waited for a
  literal `<think>` read the reasoning as ordinary prefix, and at the first
  fence the model DRAFTED while thinking it began forcing the draft's name;
  the model fought it for the whole 4000-token budget and never closed the
  block. With `:think-close` set, the grammar is free text up to that closer,
  then the constrained part: 56 tokens, one fence, the forced name. Whether
  thinking is on for a call is the caller's to say (infer/grammar-for reads
  the config); a closer for a call that thinks not would leave the reply
  unable to end.

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
  "One character inside a GBNF `[...]` class.

  `-` and `^` are NOT escaped: llama.cpp's grammar parser knows only the
  escapes \\x \\u \\U \\t \\r \\n \\\\ \\\" \\[ \\], and an older build (PrismML's, at
  mainline 10687) rejects `\\-` and `\\^` outright where a newer one (10809)
  let them through. `class-members` places them where they are literal."
  [c]
  (case c
    \newline "\\n"
    \return "\\r"
    \tab "\\t"
    \\ "\\\\"
    \] "\\]"
    (str c)))

(defn- class-members
  "The characters of a negated class `[^...]`, ordered so each is literal:
  `-` last (anywhere else it ranges), `^` never first (after the negating
  caret a literal one is fine, but last but one keeps it unambiguous)."
  [chars]
  (let [plain (remove #{\- \^} chars)]
    (str/join (map escape-class
                   (concat plain
                           (when (some #{\^} chars) [\^])
                           (when (some #{\-} chars) [\-]))))))

(defn no-substring-rules
  "GBNF rules `<name>0` … `<name>(n-1)` recognising the strings that do not
  contain `p`: one per KMP state, each an optional alternation over p's
  alphabet with a negated class for every other character. No rule exists for
  state n, so the transition that would complete `p` does not exist either."
  [name p]
  (let [n (count p)
        alphabet (sort (distinct (seq p)))
        other (str "[^" (class-members alphabet) "] " name "0")]
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
  of `tools`. `:require? true` demands at least one call after the reasoning
  — the force; false leaves the call optional but still constrained — the
  restriction. `:think-close` is the reasoning closer (`</think>`) when the
  call THINKS: the sampled text is then free up to it and constrained after;
  nil when it does not, and the whole reply is constrained. nil when there is
  nothing to allow: an empty list would be a grammar no reply can satisfy."
  [{:keys [tools require? think-close]}]
  (let [names (vec (distinct (map str tools)))
        calls (str "( prefix call )" (if require? "+" "*") " prefix")]
    (when (seq names)
      (str/join
       "\n"
       (concat
        (if (seq think-close)
          [(str "root ::= t0 " (literal think-close) " " calls)]
          [(str "root ::= " calls)])
        ["prefix ::= p0"]
        (no-substring-rules "p" opener)
        (when (seq think-close) (no-substring-rules "t" think-close))
        [(str "call ::= " (literal opener)
              " ws \"{\" ws \"\\\"name\\\"\" ws \":\" ws name ws \",\" ws"
              " \"\\\"args\\\"\" ws \":\" ws object ws \"}\" ws \"```\"")
         (str "name ::= " (str/join " | " (map #(literal (str "\"" % "\"")) names)))]
        json-rules)))))
