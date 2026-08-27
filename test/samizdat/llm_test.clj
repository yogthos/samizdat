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

(ns samizdat.llm-test
  "Phase 2: the model plumbing, offline.

  The fence parser gets the most tests here because it is the component whose
  bugs are invisible in a live run. A parser that quietly drops a tool call
  looks exactly like a model that chose not to make one."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is are]]
            [jolt.http-client :as http]
            [samizdat.llm.adapter :as adapter]
            [samizdat.llm.adapter.openai :as openai]
            [samizdat.llm.client :as client]
            [samizdat.llm.fence :as fence]
            [samizdat.llm.message :as message]
            [samizdat.llm.registry :as registry]
            [samizdat.tape :as tape]))

;; --- fence extraction -------------------------------------------------------

(defn- fenced [body] (str "prose before\n```tool-call\n" body "\n```\nprose after"))

(deftest the-repair-ladder-handles-commas-and-dangling-keys
  ;; karamazov-avk, dirge's remaining rungs, each validated by the caller's
  ;; re-parse. Nothing here invents content: a filled key is null, which the
  ;; tool's missing-argument check then names precisely.
  (testing "a trailing comma before a closer is dropped, interior ones too"
    (let [p (fence/parse-tool-call
             "```tool-call\n{\"name\": \"task\", \"args\": {\"op\": \"list\",}}\n```" {})]
      (is (= "task" (:name p)))
      (is (:auto-repaired? p)))
    (let [p (fence/parse-tool-call
             "```tool-call\n{\"name\": \"t\", \"args\": {\"a\": [1, 2,], \"b\": 3,}}\n```" {})]
      (is (= "t" (:name p)))
      (is (= [1 2] (get-in p [:args :a])))))
  (testing "a comma inside a string is text and is kept"
    (let [p (fence/parse-tool-call
             "```tool-call\n{\"name\": \"t\", \"args\": {\"s\": \",}\"}}\n```" {})]
      (is (= ",}" (get-in p [:args :s])))
      (is (not (:auto-repaired? p)))))
  (testing "a body stopping after a key is filled with null and closed —
            the tool's own missing-argument complaint then names the loss"
    (let [p (fence/parse-tool-call
             "```tool-call\n{\"name\": \"task\", \"args\": {\"op\":\n```" {})]
      (is (= "task" (:name p)))
      (is (contains? (:args p) :op))
      (is (nil? (get-in p [:args :op])))
      (is (:auto-repaired? p))))
  (testing "a body stopping INSIDE a string stays a parse error — closing it
            would ship half a file as a success, the guard close-unbalanced's
            docstring calls load-bearing"
    (let [p (fence/parse-tool-call
             (str "```tool-call\n{\"name\": \"write_file\","
                  " \"args\": {\"content\": \"half\n```") {})]
      (is (= "__parse_error__" (:name p))))))

(deftest a-context-overflow-is-recognized-from-the-body
  ;; karamazov-d41: a 500 wearing this message is deterministic — the same
  ;; oversized prompt fails identically every time — so post-once classifies
  ;; it :fatal with :reason :context-overflow instead of walking the backoff
  ;; ladder. The wording is wordlists.edn data, because it is the endpoints'
  ;; to change.
  (is (client/context-overflow?
       "{\"error\":{\"message\":\"Context size has been exceeded.\"}}")
      "llama-server's wording")
  (is (client/context-overflow?
       "This model's maximum context length is 32768 tokens"))
  (is (client/context-overflow? "code: context_length_exceeded"))
  (is (not (client/context-overflow? "internal server error")))
  (is (not (client/context-overflow? nil))))

(deftest a-fence-marker-inside-a-json-string-is-content-not-a-closer
  ;; Qwen baseline run b8a2b72c (karamazov-hpv): the mdlite task builds a
  ;; MARKDOWN CONVERTER, so the file being written contains literal ``` —
  ;; (not= l "```") in its code-block handling — and the closer scan took the
  ;; first ``` after the opener, cutting the call mid-string. 6 of that run's
  ;; 8 parse errors were this shape, each at a quarter of the token cap: the
  ;; JSON was valid and the harness cut it. The closer scan must be
  ;; string-aware, same state machine as close-unbalanced.
  (let [resp (str "```tool-call\n"
                  "{\"name\": \"write_file\", \"args\": {\"path\": \"a.clj\","
                  " \"content\": \"(= l \\\"```\\\")\\n\"}}\n"
                  "```")
        p (fence/parse-tool-call resp {})]
    (is (= "write_file" (:name p)))
    (is (str/includes? (str (get-in p [:args :content])) "```")
        "the backticks arrive as file content"))
  (testing "an XML closer inside a string is content too"
    (let [p (fence/parse-tool-call
             (str "```tool-call\n"
                  "{\"name\": \"write_file\", \"args\": {\"path\": \"a.md\","
                  " \"content\": \"about </tool-call> tags\"}}\n"
                  "```")
             {})]
      (is (= "write_file" (:name p)))))
  (testing "an unterminated string ahead of a raw closer still earns its
            parse error — the raw-closer fallback keeps the truncation shape
            reported rather than silently becoming a no-call"
    (let [p (fence/parse-tool-call
             (str "```tool-call\n"
                  "{\"name\": \"write_file\", \"args\": {\"content\": \"half a file\n"
                  "```")
             {})]
      (is (= "__parse_error__" (:name p))))))

(deftest prefill-plus-think-plus-real-call-parses
  ;; The live self-building run (2026-08-21) thrashed on this: after a parse
  ;; error the loop prefills "```tool-call\n" to force a bare call, but GLM is
  ;; a reasoning model and continued with a <think> block and prose, then
  ;; opened a SECOND ```tool-call with the real JSON. The closing-fence regex
  ;; matched the ``` prefix of the second opener as the first's closer, so the
  ;; body was the think-block, not the JSON — every recovery attempt lost the
  ;; real call. The parser must strip the reasoning and anchor on the LAST
  ;; opener.
  (let [reattached (str "```tool-call\n"
                        "<think>The human intervened. Let me start small.</think>\n"
                        "Understood — one small form first.```tool-call\n"
                        "{\"name\": \"eval\", \"args\": {\"code\": \"(+ 1 2)\"}}\n"
                        "```")
        p (fence/parse-tool-call reattached {})]
    (is (= "eval" (:name p)))
    (is (= "(+ 1 2)" (get-in p [:args :code]))))
  (testing "a think block before a single clean call is stripped"
    (let [p (fence/parse-tool-call
             (str "<think>reasoning here with a ) stray paren</think>\n"
                  "```tool-call\n{\"name\": \"task\", \"args\": {\"action\": \"list\"}}\n```")
             {})]
      (is (= "task" (:name p))))))

(deftest a-plural-tool-calls-closer-still-closes-the-fence
  ;; gen-31 B1.2 (2026-08-18), three turns running. It opened with the
  ;; documented ```tool-call fence and closed with </tool-calls> — plural — so
  ;; the closer alternation `</tool-call>` did not match, the `s` sitting where
  ;; the `>` was expected. Same class as the gen-30 loss that cost thirteen
  ;; turns and a 30,658-character proof: right call, wrong bracket, discarded.
  ;; What it was discarding here was edge-choice injectivity, one of the three
  ;; gaps the run was formulated to cross.
  (let [resp (str "```tool-call\n"
                  "{\"name\": \"proof_step\", \"args\": {\"tactic\": \"exact htail i\"}}\n"
                  "</tool-calls>")
        p (fence/parse-tool-call resp {})]
    (is (= "proof_step" (:name p)))
    (is (= "exact htail i" (get-in p [:args :tactic]))))

  (testing "every spelling of the closer, since the model mixes them freely"
    (doseq [closer ["```" "</tool-call>" "</tool-calls>" "</tool_call>" "</tool_calls>"]]
      (let [p (fence/parse-tool-call
               (str "```tool-call\n{\"name\": \"review\", \"args\": {}}\n" closer) {})]
        (is (= "review" (:name p)) (str "closer " closer " must close the fence")))))

  (testing "and the XML path is untouched"
    ;; <tool_calls> must NOT become a fence opener: xml-call is the last rung
    ;; and only runs when no fence matched, so capturing XML into the fence
    ;; path would turn a good call into a parse error.
    (let [p (fence/parse-tool-call
             (str "<tool_calls>\n<invoke name=\"lean_search\">\n"
                  "<parameter name=\"query\" string=\"true\">Chain' get</parameter>\n"
                  "</invoke>\n</tool_calls>") {})]
      (is (= "lean_search" (:name p)))
      (is (= "Chain' get" (get-in p [:args :query]))))))

(deftest xml-parameters-carry-lists-as-json
  ;; The XML parameter form has no way to express an array, so a model handing
  ;; over subClaims writes the JSON text as the parameter body. Returning that
  ;; verbatim gave downstream code a String where it expected a collection,
  ;; which seq'd into Characters and died in the journal writer with "Don't
  ;; know how to write JSON of class java.lang.Character" — killing gen-31's
  ;; B1.3 and B2.2 (2026-08-18), both of which were on TARGET 1 step 4.
  ;; Same reason the number case exists: a value whose TYPE is wrong throws
  ;; somewhere far from here.
  (let [resp (str "<invoke name=\"thesis\">\n"
                  "<parameter name=\"goal\">Close TARGET 1 step 4</parameter>\n"
                  "<parameter name=\"subClaims\">[\"define the adjacency\", \"build the Finset\"]</parameter>\n"
                  "</invoke>")
        parsed (fence/parse-tool-call resp {})]
    (is (= ["define the adjacency" "build the Finset"] (get-in parsed [:args :subClaims]))
        "a JSON array in a parameter body is a list, not a string")
    (is (= "Close TARGET 1 step 4" (get-in parsed [:args :goal]))
        "and ordinary prose is untouched")))

(deftest xml-parameter-that-merely-starts-with-a-bracket-stays-a-string
  ;; Lean and prose both contain brackets. Only something that actually parses
  ;; as JSON is treated as structure; everything else is verbatim, which is the
  ;; whole reason a model reaches for this form when handing over a proof.
  (let [resp (str "<invoke name=\"verify_lean\">\n"
                  "<parameter name=\"claim\">[1,2,3] is not sorted descending</parameter>\n"
                  "<parameter name=\"lean\">theorem t : True := by\n  simp [List.mem_cons]</parameter>\n"
                  "</invoke>")
        parsed (fence/parse-tool-call resp {})]
    (is (string? (get-in parsed [:args :claim])))
    (is (str/includes? (get-in parsed [:args :lean]) "List.mem_cons")
        "a Lean body with brackets survives verbatim")))

(deftest xml-parameters-tolerate-extra-attributes
  ;; Observed live in gen-31, run f4b53e8f, branch B3 (2026-08-18).
  ;; deepseek-v4-pro emits `<parameter name="query" string="true">`. The
  ;; parameter regex required the tag to close immediately after the name, so
  ;; the extra attribute made every parameter fail to match while <invoke>,
  ;; which carried no extra attribute, matched fine. The branch was answered
  ;; "Missing required argument(s): query" five times with the query sitting in
  ;; the tag, then culled for a malformed fence it had not emitted. Same class
  ;; as the gen-30 loss: a real call discarded and the cull reason untrue.
  (let [resp (str "reasoning about the goal\n"
                  "<tool_calls>\n<invoke name=\"lean_search\">\n"
                  "<parameter name=\"query\" string=\"true\">List.Chain'.get consecutive relation</parameter>\n"
                  "</invoke>\n</tool_calls>")
        parsed (fence/parse-tool-call resp {})]
    (is (= "lean_search" (:name parsed)))
    (is (= "List.Chain'.get consecutive relation" (get-in parsed [:args :query]))
        "the argument is right there in the tag")))

(deftest xml-numeric-parameter-still-coerces-with-extra-attributes
  (let [resp (str "<invoke name=\"lean_search\">\n"
                  "<parameter name=\"query\" string=\"true\">chain</parameter>\n"
                  "<parameter name=\"top_k\" type=\"int\">5</parameter>\n"
                  "</invoke>")
        parsed (fence/parse-tool-call resp {})]
    (is (= 5 (get-in parsed [:args :top_k])) "top_k reaches (take k) and a string throws there")))

(deftest a-long-branch-sends-a-digest-of-its-early-turns-not-the-turns
  ;; What a branch needs from its own distant past is which approaches it
  ;; already tried and how each came out — not the prose it wrote at the time.
  ;; A 19-turn branch carries ~26KB on the wire against ~1.5KB of digest, and
  ;; the longest branch on record is 86 turns.
  ;;
  ;; LR-4 changed the MECHANISM, not the goal. The digest used to be appended
  ;; to the PROBLEM message and the aged-out pairs dropped; now each aged-out
  ;; message is replaced IN PLACE, so roles, order and count are identical and
  ;; the shared prefix stops being rewritten on every compaction. The frame
  ;; and the recent window are as protected as they ever were.
  (let [pair (fn [i] [{:role "assistant" :turn i
                       :content (str "long reasoning " i (apply str (repeat 400 "x")))}
                      {:role "user" :turn i
                       :content (str "result " i (apply str (repeat 400 "y")))}])
        msgs (into [{:role "system" :content "SYS"}
                    {:role "user" :content "## Problem\n\nsolve it"}]
                   (mapcat pair (range 1 21)))
        turns (mapv (fn [i] {:turn i :tool (str "tool" i)
                             :category (if (even? i) :failure :success)
                             :error (when (even? i) "boom")})
                    (range 1 21))
        out (message/compact msgs turns {:keep-pairs 4 :threshold-chars 1000})]
    (testing "the frame survives untouched — the whole prefix cache rests on it"
      (is (= (first msgs) (first out)))
      (is (= (second msgs) (second out))))
    (testing "the shape is identical, so alternation needs no provider to be forgiving"
      (is (= (count msgs) (count out)))
      (is (= (mapv :role msgs) (mapv :role out))))
    (testing "recent turns survive verbatim"
      (is (str/includes? (:content (nth out (- (count out) 8))) "long reasoning 17"))
      (is (str/includes? (:content (last out)) "result 20")))
    (testing "early turns are unloaded as prose but retained as what was tried"
      (let [all (str/join "\n" (map :content out))]
        (is (not (str/includes? all "long reasoning 3"))
            "the prose of an unloaded turn is gone from the wire")
        (is (str/includes? all "tool3") "what it tried is retained")
        (is (str/includes? all "tool16"))
        (is (not (str/includes? all "tool17"))
            "turns kept verbatim are not also digested")))
    (testing "a message's own turn stamp is what picks its digest, not its position"
      ;; The positional guess was unsound: a provider error or a no-call turn
      ;; appends messages without appending a turn row.
      (is (str/includes? (:content (nth out 2)) "t1 tool1")))
    (testing "the original prose is kept on the branch's copy for the record"
      (is (str/includes? (:original (nth out 2)) "long reasoning 1")))
    (testing "it is smaller"
      (is (< (count (str/join (map :content out)))
             (quot (count (str/join (map :content msgs))) 2))))
    (testing "compacting twice is idempotent — one attempt per message, ever"
      (is (= out (message/compact out turns {:keep-pairs 4 :threshold-chars 1000}))))
    (testing "THE POINT: the prefix before the newest compaction never moves"
      ;; Two consecutive turns' worth of wire messages. Under the old
      ;; append-to-the-problem-message scheme, message 1 differed between
      ;; these two and every cached token after it was invalidated.
      (let [later (into msgs (mapcat pair [21 22]))
            turns' (into turns [{:turn 21 :tool "tool21" :category :success}
                                {:turn 22 :tool "tool22" :category :success}])
            out' (message/compact later turns' {:keep-pairs 4 :threshold-chars 1000})
            ;; The region BOTH calls had aged out: everything before the
            ;; FIRST call's verbatim window. Two more turns move that window
            ;; forward, so the messages between the old boundary and the new
            ;; one get compacted for the first time in out' — the boundary
            ;; advancing is the one place a rewrite is supposed to happen.
            settled (tape/window-index msgs 4)]
        (is (= (take settled out) (take settled out'))
            "everything already compacted is byte-identical between turns")
        (is (= (take 2 msgs) (take 2 out'))
            "and the frame is still the frame — the old scheme rewrote index 1 here")))
    (testing "a short history is left exactly alone"
      (let [short-msgs (into [{:role "system" :content "SYS"}
                              {:role "user" :content "P"}]
                             (mapcat pair (range 1 3)))]
        (is (= short-msgs (message/compact short-msgs
                                           [{:turn 1 :tool "t" :category :success}]
                                           {:keep-pairs 4 :threshold-chars 1000000})))))))

(deftest only-the-newest-settled-state-block-goes-over-the-wire
  ;; The settled-state ledger is regenerated every turn and appended to that
  ;; turn's user message, so without this every copy accumulates: gen-18's
  ;; ledger is ~6,800 tokens and an 80-turn branch would carry 80 copies of it,
  ;; which is far more context than the whole transcript.
  ;;
  ;; A ledger is STATE, not narrative — only the current one is true, and an
  ;; older copy is a strictly worse version of the newest. Same reasoning as
  ;; strip-think-blocks, which drops accumulated reasoning for the same reason.
  ;; The branch keeps every copy in its own history so the journal and resume
  ;; stay faithful; only the wire sees one.
  (let [led (fn [n] (str message/ledger-open "\nsettled: " n "\n" message/ledger-close))
        msgs [{:role "system" :content "sys"}
              {:role "user" :content (str "result 1\n" (led 1))}
              {:role "assistant" :content "call 1"}
              {:role "user" :content (str "result 2\n" (led 2))}
              {:role "assistant" :content "call 2"}
              {:role "user" :content (str "result 3\n" (led 3))}]
        out (mapv :content (message/prepare msgs))]
    (is (not (str/includes? (nth out 1) "settled: 1")) "the first ledger is dropped")
    (is (not (str/includes? (nth out 3) "settled: 2")) "and the second")
    (is (str/includes? (nth out 5) "settled: 3") "the newest survives")
    (testing "the surrounding turn result is untouched"
      (is (str/includes? (nth out 1) "result 1"))
      (is (str/includes? (nth out 3) "result 2")))
    (testing "the markers never reach the model"
      (is (not-any? #(str/includes? % message/ledger-open) out)))
    (testing "a conversation with one ledger keeps it"
      (let [one (mapv :content (message/prepare
                                [{:role "user" :content (str "r\n" (led 9))}]))]
        (is (str/includes? (first one) "settled: 9"))))))

(deftest reattach-rebuilds-the-whole-assistant-turn
  ;; Not only the parser needs this. The completion is just the TAIL of what
  ;; the assistant said, and both the message history and the journal's
  ;; assistant_text must carry the opener too — a stored turn that begins
  ;; mid-fence misrepresents the required format back to the model on every
  ;; subsequent turn, and to anyone reading the run afterwards.
  (let [prefix "```tool-call\n"]
    (is (= "```tool-call\n{\"name\": \"verify\"}"
           (fence/reattach "{\"name\": \"verify\"}" prefix)))
    (testing "a completion that already repeats the opener is left alone"
      (is (= "```tool-call\n{\"name\": \"verify\"}"
             (fence/reattach "```tool-call\n{\"name\": \"verify\"}" prefix))))
    (testing "no prefill means the completion stands as the whole turn"
      (is (= "just prose" (fence/reattach "just prose" nil)))
      (is (= "just prose" (fence/reattach "just prose" ""))))))

(deftest a-prefilled-response-parses-as-if-it-carried-its-own-fence
  ;; The trap in prefilling the opening fence: the model does not repeat it,
  ;; so the response body starts INSIDE the fence and fence-re — which matches
  ;; on the ```tool-call opener — finds nothing. Every prefilled turn would
  ;; parse as a no-call, turning a fix for __no_call__ into a generator of
  ;; them. The prefix has to be reattached before matching.
  (let [prefix "```tool-call\n"]
    (testing "the model continues inside the fence and closes it"
      (let [p (fence/parse-tool-call "{\"name\": \"verify\", \"args\": {\"claim\": \"x\"}}\n```"
                                     {:prefill prefix})]
        (is (= "verify" (:name p)))
        (is (= {:claim "x"} (:args p)))))

    (testing "a model that repeats the opener anyway does not get a doubled fence"
      ;; Providers differ on whether the prefix comes back in the completion.
      ;; Reattaching blindly would produce ```tool-call\n```tool-call\n{...},
      ;; whose first fence body is empty — a parse error on a turn that was
      ;; actually fine.
      (let [p (fence/parse-tool-call (str prefix "{\"name\": \"verify\"}\n```")
                                     {:prefill prefix})]
        (is (= "verify" (:name p)))
        (is (= 1 (:fences p)) "one fence, not two")))

    (testing "a prefilled response that never closes the fence still parses"
      ;; Hitting the token cap mid-call is common; the closing fence is the
      ;; first casualty. The unfenced-tail path already handles this shape
      ;; without a prefill and must keep doing so with one.
      (let [p (fence/parse-tool-call "{\"name\": \"proof_state\"}" {:prefill prefix})]
        (is (= "proof_state" (:name p)))))

    (testing "without a prefill nothing changes"
      ;; The whole non-prefilled surface must be untouched, including the
      ;; mechanics signals the capability tier is built from.
      (is (nil? (fence/parse-tool-call "just prose")))
      (is (nil? (fence/parse-tool-call "just prose" {})))
      (let [a (fence/parse-tool-call (fenced "{\"name\": \"verify\"}"))
            b (fence/parse-tool-call (fenced "{\"name\": \"verify\"}") {})]
        (is (= a b))))))

(deftest fence-basics
  (testing "no fence at all is nil, not an error"
    (is (nil? (fence/parse-tool-call "I think the answer is 42.")))
    (is (nil? (fence/parse-tool-call ""))))

  (testing "a well-formed call"
    (let [p (fence/parse-tool-call (fenced "{\"name\": \"verify\", \"args\": {\"claim\": \"x\"}}"))]
      (is (= "verify" (:name p)))
      (is (= {:claim "x"} (:args p)))
      (is (= 1 (:fences p)))
      (is (not (:auto-repaired? p)))))

  (testing "args is optional and defaults to an empty map"
    (is (= {} (:args (fence/parse-tool-call (fenced "{\"name\": \"proof_state\"}"))))))

  (testing "a non-map args is ignored rather than propagated as garbage"
    (is (= {} (:args (fence/parse-tool-call (fenced "{\"name\": \"x\", \"args\": [1,2]}")))))))

(deftest fence-rejects-bad-shapes
  (are [body] (= "__parse_error__"
                 (:name (fence/parse-tool-call (fenced body))))
    "[1, 2, 3]"
    "\"just a string\""
    "{\"args\": {}}"
    "{\"name\": \"\"}"
    "{\"name\": 42}"
    "{not json at all")

  (testing "the error text names the causes the repair pass does not cover"
    (let [p (fence/parse-tool-call (fenced "{\"name\": \"x\", \"args\": {\"a\": \"un\"escaped\"}"))]
      (is (= "__parse_error__" (:name p)))
      (is (str/includes? (:parse-error p) "unescaped quote")))))

(deftest fence-control-char-repair
  (testing "a raw newline inside a string value is repaired"
    ;; The dominant real failure: the model writes multi-line SMT-LIB or Lean
    ;; straight into a string value. 5 of 35 turns in the n=500 Sidon run.
    (let [body "{\"name\": \"verify_smt\", \"args\": {\"smtlib\": \"(declare-const x Int)\n(assert (> x 2))\"}}"
          p (fence/parse-tool-call (fenced body))]
      (is (= "verify_smt" (:name p)))
      (is (:auto-repaired? p))
      (is (= "(declare-const x Int)\n(assert (> x 2))"
             (get-in p [:args :smtlib]))
          "the repaired value must round-trip to the original text")))

  (testing "tabs and carriage returns too"
    (let [p (fence/parse-tool-call (fenced "{\"name\": \"x\", \"args\": {\"c\": \"a\tb\r\nc\"}}"))]
      (is (= "a\tb\r\nc" (get-in p [:args :c])))
      (is (:auto-repaired? p))))

  (testing "control chars OUTSIDE strings are left alone, since they are legal"
    (let [p (fence/parse-tool-call (fenced "{\n  \"name\": \"x\",\n  \"args\": {}\n}"))]
      (is (= "x" (:name p)))
      (is (not (:auto-repaired? p)) "pretty-printed JSON is not a repair case")))

  (testing "an escaped backslash before a quote does not confuse the state machine"
    ;; "a\\" ends the string; a naive scanner reads \\" as an escaped quote and
    ;; thinks it is still inside one, then escapes every later newline.
    (let [body "{\"name\": \"x\", \"args\": {\"p\": \"a\\\\\"}}"]
      (is (= "a\\" (get-in (fence/parse-tool-call (fenced body)) [:args :p])))))

  (testing "a body that ends INSIDE a string is not repaired — that is what a
            reply cut off by the token cap looks like, and completing it would
            hand write_file half a file to overwrite the whole one with"
    (let [p (fence/parse-tool-call (fenced "{\"name\": \"x\", \"args\": {\"c\": \"a\nb"))]
      (is (= "__parse_error__" (:name p)))
      (is (:auto-repaired? p) "the repair was attempted and is recorded even though it failed"))))

(deftest fence-multiple
  (testing "the LAST fence wins, and the count is recorded rather than hidden"
    (let [resp (str "```tool-call\n{\"name\": \"example\", \"args\": {}}\n```\n"
                    "now the real one\n"
                    "```tool-call\n{\"name\": \"real\", \"args\": {\"k\": 1}}\n```")
          p (fence/parse-tool-call resp)]
      (is (= "real" (:name p)))
      (is (= 2 (:fences p)))))

  (testing "the model drafting inside <think> before the real call"
    ;; Not an edge case. Measured at 20.5% of 200 live deepseek-v4-flash
    ;; responses, and in all 41 the first fence was inside <think> and the last
    ;; carried at least as many args. Taking the first — which is what
    ;; String.match without /g does in the TypeScript original — loses one turn
    ;; in five.
    ;;
    ;; This body is trimmed from a real captured response. The first fence is
    ;; the model quoting the system prompt's own template back at itself, which
    ;; is not even valid JSON, so first-fence would spend the turn on a parse
    ;; error while a perfectly good call sat further down the same response.
    (let [resp (str "<think>the format says:\n"
                    "```tool-call\n{\"name\": \"...\", \"args\": {...}}\n```\n"
                    "so I should emit</think>\n"
                    "```tool-call\n"
                    "{\"name\": \"add_rule\", \"args\": {\"name\": \"transitive_closure\","
                    " \"code\": \"tc(X, Y) :- edge(X, Y).\\ntc(X, Y) :- edge(X, Z), tc(Z, Y).\"}}"
                    "\n```")
          p (fence/parse-tool-call resp)]
      (is (= "add_rule" (:name p)))
      (is (str/includes? (get-in p [:args :code]) "tc(X, Y) :- edge(X, Y)."))
      ;; The example fence lived inside <think>, which is now stripped before
      ;; parsing — so this is correctly ONE real fence, not two. The point of
      ;; the case (don't spend the turn on the quoted-template example) is met
      ;; even more cleanly: the example never reaches the fence scanner.
      (is (= 1 (:fences p)))
      (is (not (:multiple-fences (fence/signals {:finish-reason "stop"} p)))))))

(deftest fence-signals
  (testing "a truncated reply is not the same as a model that skipped the call"
    ;; deepseek-v4-flash spent its whole budget inside <think> on the first
    ;; live call here. Treating that as a steering problem would be wrong: the
    ;; fix is more tokens.
    (is (= {:no-fence false :truncated true :parse-error false
            :auto-repaired false :multiple-fences false}
           (fence/signals {:finish-reason "length"} nil)))
    (is (= {:no-fence true :truncated false :parse-error false
            :auto-repaired false :multiple-fences false}
           (fence/signals {:finish-reason "stop"} nil)))))

;; --- messages ---------------------------------------------------------------

(deftest think-blocks
  (testing "think blocks are stripped from prior assistant turns"
    (is (= "the answer" (message/strip-think-blocks "<think>musing</think>\nthe answer")))
    (is (= "" (message/strip-think-blocks "<think>only musing</think>"))))

  (testing "multiple blocks and content across newlines"
    (is (= "a\nb" (message/strip-think-blocks "<think>x</think>a\nb<think>y\nz</think>"))))

  (testing "only assistant turns are touched"
    (let [out (message/prepare [{:role :system :content "<think>keep</think>sys"}
                                {:role :user :content "<think>keep</think>usr"}
                                {:role :assistant :content "<think>drop</think>asst"}])]
      (is (= ["<think>keep</think>sys" "<think>keep</think>usr" "asst"]
             (mapv :content out)))
      (is (= ["system" "user" "assistant"] (mapv :role out)))))

  (testing "reasoning is folded into <think> framing so one parser handles both"
    (is (= "<think>r</think>\nc" (message/merge-reasoning "c" "r")))
    (is (= "<think>r</think>" (message/merge-reasoning nil "r")))
    (is (= "c" (message/merge-reasoning "c" nil)))))

;; --- adapters ---------------------------------------------------------------

(deftest force-tool-uses-native-tool-choice-not-prefill
  ;; karamazov-9se: assistant prefill only forces a call where the provider
  ;; continues a trailing assistant message (DeepSeek /beta); GLM ignores it. A
  ;; gate that must land a `done` forces it with native tool_choice instead.
  (let [glm (registry/adapter-for :glm)
        cfg {:base-url "https://open.bigmodel.cn/api/coding/paas/v4" :model "glm-5.3" :api-key "k"}
        done-spec {:name "done" :description "Finish."
                   :parameters {:type "object"
                                :properties {:answer {:type "string"}}
                                :required ["answer"]}}]
    (testing "on GLM (no prefill) it exposes only the forced tool with tool_choice"
      (let [body (adapter/chat-body glm cfg
                                    {:messages [{:role "user" :content "x"}]
                                     :prefill "```tool-call\n{\"name\": \"done\""
                                     :force-tool done-spec})]
        (is (= [{:type "function" :function done-spec}] (:tools body)))
        (is (= {:type "function" :function {:name "done"}} (:tool_choice body)))
        (is (not-any? #(= "assistant" (:role %)) (:messages body))
            "GLM cannot continue a trailing assistant message, so none is sent")))
    (testing "on DeepSeek /beta (prefill works) it prefixes and does NOT send tool_choice"
      ;; DeepSeek's thinking mode rejects tool_choice with a 400, so where the
      ;; prefill can force the call it must be preferred.
      (let [ds (registry/adapter-for :deepseek)
            body (adapter/chat-body ds {:base-url "https://api.deepseek.com/beta"
                                        :model "deepseek-v4-flash" :api-key "k"}
                                    {:messages [{:role "user" :content "x"}]
                                     :prefill "```tool-call\n{\"name\": \"done\""
                                     :force-tool done-spec})]
        (is (nil? (:tool_choice body)) "no tool_choice where a prefill forces the call")
        (is (nil? (:tools body)))
        (is (some #(and (= "assistant" (:role %)) (:prefix %)) (:messages body))
            "the trailing assistant prefix does the forcing instead")))
    (testing "parse-chat folds a native tool_calls response back into the fence convention"
      (let [reply {:choices [{:message {:tool_calls
                                        [{:function {:name "done"
                                                     :arguments "{\"answer\":\"shipped the partial\"}"}}]}
                              :finish_reason "tool_calls"}]}
            p (adapter/parse-chat glm reply)]
        (is (str/includes? (:content p) "```tool-call"))
        (is (str/includes? (:content p) "\"name\":\"done\""))
        (is (str/includes? (:content p) "shipped the partial")
            "so the loop's fence parser reads it exactly like a normal tool call")))))

(deftest the-local-endpoint-gets-prefix-cache-reuse-and-nobody-else-does
  ;; LR-5. An inherited fork and a fan of probes off one tape are only cheap if
  ;; the server reuses the warm prefix instead of re-prefilling it; without
  ;; cache_prompt that is most of their cost. Design copied from llm-repl's
  ;; llama-wire: cache_prompt on the local path, id_slot ONLY from an explicit
  ;; slots table, because a slot count is a property of how the server was
  ;; launched and a guessed index evicts somebody else's warm prefix.
  (let [local (registry/adapter-for :local)
        cfg {:base-url "http://127.0.0.1:8080/v1" :model "local-model"}
        opts {:messages [{:role "user" :content "x"}] :cache-key "B1"}]
    (testing "the local endpoint asks for prefix reuse"
      (is (true? (:cache_prompt (adapter/chat-body local cfg opts)))))
    (testing "no id_slot without an explicit slots table — the server picks"
      (is (nil? (:id_slot (adapter/chat-body local cfg opts)))))
    (testing "a configured slot pins the conversation"
      (is (= 2 (:id_slot (adapter/chat-body local (assoc cfg :slots {"B1" 2}) opts)))))
    (testing "a cache key with no matching slot entry still gets reuse, unpinned"
      (let [body (adapter/chat-body local (assoc cfg :slots {"B9" 0}) opts)]
        (is (true? (:cache_prompt body)))
        (is (nil? (:id_slot body)))))
    (testing "no cache key, no wire change"
      (is (nil? (:cache_prompt (adapter/chat-body local cfg (dissoc opts :cache-key))))))
    (testing "every hosted provider's body is byte-identical with or without the key"
      (doseq [p [:deepseek :glm :openai]]
        (let [a (registry/adapter-for p)]
          (is (= (adapter/chat-body a (assoc cfg :api-key "k") (dissoc opts :cache-key))
                 (adapter/chat-body a (assoc cfg :api-key "k") opts))
              (str (name p) " must ignore a knob it has nowhere to put")))))
    (testing "Ollama ignores it too"
      (let [a (registry/adapter-for :ollama)]
        (is (= (adapter/chat-body a cfg (dissoc opts :cache-key))
               (adapter/chat-body a cfg opts)))))))

(deftest adapters-differ-only-where-they-should
  (let [cfg {:base-url "https://api.example.com/v1" :model "m" :api-key "k"}]
    (testing "the OpenAI family"
      (let [a (registry/adapter-for :deepseek)]
        (is (= "https://api.example.com/v1/chat/completions" (adapter/chat-url a cfg)))
        (is (= {"Authorization" "Bearer k"} (adapter/auth-headers a cfg)))
        (is (= {:model "m" :messages [] :max_tokens 10 :temperature 0.5}
               (adapter/chat-body a cfg {:messages [] :max-tokens 10 :temperature 0.5})))))

    (testing "an endpoint with no key sends no auth header"
      (is (= {} (adapter/auth-headers (registry/adapter-for :local) (dissoc cfg :api-key)))))

    (testing "Ollama's native shape: one object, options nested, stream off"
      (let [a (registry/adapter-for :ollama)
            body (adapter/chat-body a {:model "q"} {:messages [] :max-tokens 10 :temperature 0.5})]
        (is (false? (:stream body)) "without this the reply is newline-delimited JSON")
        (is (= {:num_predict 10 :temperature 0.5} (:options body)))))

    (testing "reasoning lives under a different key per provider"
      (let [reply {:choices [{:message {:content "c" :reasoning_content "r"}
                              :finish_reason "stop"}]}]
        (is (= "r" (:reasoning (adapter/parse-chat (registry/adapter-for :deepseek) reply))))
        (is (nil? (:reasoning (adapter/parse-chat (registry/adapter-for :openai) reply))))))

    (testing "Ollama reads content and usage from its own field names"
      (let [reply {:message {:content "c" :thinking "t"} :done_reason "stop"
                   :prompt_eval_count 3 :eval_count 7}
            p (adapter/parse-chat (registry/adapter-for :ollama) reply)]
        (is (= "c" (:content p)))
        (is (= "t" (:reasoning p)))
        (is (= {:prompt-tokens 3 :completion-tokens 7 :total-tokens 10} (:usage p)))))

    (testing "cache token counts are kept when the provider reports them"
      ;; Providers that do prefix caching return the split alongside the
      ;; totals. samizdat threw it away, so nothing could answer whether a
      ;; wide beam thrashes the cache — every branch carries its own growing
      ;; message list, so the beam holds N diverging prefixes at once and
      ;; whether that is cheap was unknowable.
      (let [reply {:choices [{:message {:content "c"} :finish_reason "stop"}]
                   :usage {:prompt_tokens 100 :completion_tokens 20 :total_tokens 120
                           :prompt_cache_hit_tokens 80 :prompt_cache_miss_tokens 20}}
            u (:usage (adapter/parse-chat (registry/adapter-for :deepseek) reply))]
        (is (= 100 (:prompt-tokens u)))
        (is (= 80 (:cache-hit-tokens u)))
        (is (= 20 (:cache-miss-tokens u)))))

    (testing "a provider that reports no cache split omits the keys rather than zeroing them"
      ;; Zero and absent are different claims. A zero would say the cache was
      ;; missed on every token; absent says the provider did not tell us. The
      ;; whole point of capturing this is to reason about cache behaviour, and
      ;; a fabricated zero would poison exactly that.
      (let [reply {:choices [{:message {:content "c"} :finish_reason "stop"}]
                   :usage {:prompt_tokens 100 :completion_tokens 20 :total_tokens 120}}
            u (:usage (adapter/parse-chat (registry/adapter-for :deepseek) reply))]
        (is (= 100 (:prompt-tokens u)))
        (is (not (contains? u :cache-hit-tokens)))
        (is (not (contains? u :cache-miss-tokens)))))

    (testing "prefill is offered only by providers that actually support it"
      ;; A fenced tool-call protocol lets the model answer in prose instead,
      ;; which is samizdat's dominant mechanical failure. Sending the opening
      ;; fence as a partial assistant message removes that option — but only
      ;; some providers continue a trailing assistant turn. OpenAI does not,
      ;; and asking it to would either be ignored or rejected, so the
      ;; capability is declared rather than assumed.
      (let [beta {:base-url "https://api.deepseek.com/beta"}
            v1   {:base-url "https://api.deepseek.com/v1"}]
        (is (adapter/prefill-support? (registry/adapter-for :deepseek) beta))
        ;; Not a property of the provider alone. On /v1 DeepSeek REJECTS the
        ;; request — "prefix is only available when using beta api" — so a
        ;; misconfigured endpoint would fail every steered turn. Checked here
        ;; so it degrades to today's behaviour instead.
        (is (not (adapter/prefill-support? (registry/adapter-for :deepseek) v1)))
        (is (not (adapter/prefill-support? (registry/adapter-for :openai) beta)))
        (is (not (adapter/prefill-support? (registry/adapter-for :ollama) beta)))))

    (testing "a supporting adapter appends the prefix as a trailing assistant turn"
      (let [a (registry/adapter-for :deepseek)
            body (adapter/chat-body a {:model "m" :base-url "https://api.deepseek.com/beta"}
                                    {:messages [{:role "user" :content "go"}]
                                     :prefill "```tool-call\n"})
            msgs (:messages body)]
        (is (= 2 (count msgs)))
        (is (= "assistant" (:role (last msgs))))
        (is (= "```tool-call\n" (:content (last msgs))))
        ;; DeepSeek continues a trailing assistant message only when it is
        ;; flagged; without this the message is treated as a completed turn
        ;; and the model replies after it rather than inside it.
        (is (true? (:prefix (last msgs))))))

    (testing "chat-body gates prefill through the protocol, not a private twin"
      ;; provenance R3-14: chat-body consulted a private supports-prefill? while
      ;; the protocol method delegated to it — two paths deciding one
      ;; question, so a provider update touching one left the other behind.
      ;; The protocol method is the only gate now: overriding it must change
      ;; what chat-body sends, in BOTH directions, whatever the config says.
      (let [v1 {:model "m" :base-url "https://api.deepseek.com/v1"}
            a (registry/adapter-for :deepseek)
            opts {:messages [{:role "user" :content "go"}] :prefill "```tool-call\n"}]
        (with-redefs [adapter/prefill-support? (fn [_ _] true)]
          (is (some #(and (= "assistant" (:role %)) (true? (:prefix %)))
                    (:messages (adapter/chat-body a v1 opts)))
              "protocol says yes on a /v1 config → prefill is sent anyway"))
        (with-redefs [adapter/prefill-support? (fn [_ _] false)]
          (is (= [{:role "user" :content "go"}]
                 (:messages (adapter/chat-body a v1 opts)))
              "protocol says no → no trailing assistant message"))))

    (testing "the models listing does not follow chat onto the beta path"
      ;; /beta is a chat-completions variant: it serves prefix completion and
      ;; returns 404 for /beta/models. Pointing the listing at it turned the
      ;; startup model check into "provider listed no models", downgrading a
      ;; real check to a warning on every start.
      (let [a (registry/adapter-for :deepseek)]
        (is (= "https://api.deepseek.com/v1/models"
               (adapter/models-url a {:base-url "https://api.deepseek.com/beta"})))
        (is (= "https://api.deepseek.com/v1/models"
               (adapter/models-url a {:base-url "https://api.deepseek.com/beta/"}))
            "a trailing slash is the same endpoint")
        (is (= "https://api.deepseek.com/v1/models"
               (adapter/models-url a {:base-url "https://api.deepseek.com/v1"}))
            "and a non-beta base is untouched")
        ;; Chat still goes where it was told.
        (is (= "https://api.deepseek.com/beta/chat/completions"
               (adapter/chat-url a {:base-url "https://api.deepseek.com/beta"})))))

    (testing "the client threads prefill through to the adapter"
      ;; The plumbing gap that would make all of the above dead code: chat
      ;; builds the request map from its opts, so a key it does not name never
      ;; reaches chat-body at all, and the prefill would silently never happen.
      (let [seen (atom nil)
            a (reify adapter/Adapter
                (id [_] :probe)
                (display-name [_] "probe")
                (chat-url [_ _] "http://localhost/x")
                (models-url [_ _] nil)
                (auth-headers [_ _] {})
                (chat-body [_ _ req] (reset! seen req) {})
                (parse-chat [_ _] nil)
                (parse-models [_ _] [])
                (error-message [_ _] nil)
                (prefill-support? [_ _] true)
                (usage-cap? [_ _ _] false))]
        (try (client/chat a {:max-retries 0} [{:role "user" :content "hi"}]
                          {:prefill "```tool-call\n"})
             (catch Throwable _ nil))
        (is (= "```tool-call\n" (:prefill @seen))
            "chat must name :prefill in its opts destructuring or it is dropped")))

    (testing "a non-supporting adapter ignores prefill entirely"
      ;; Must be byte-identical to the no-prefill body: a provider that does
      ;; not support this has to be left on exactly the path it is on today.
      (let [a (registry/adapter-for :openai)
            req {:messages [{:role "user" :content "go"}] :max-tokens 5}]
        (is (= (adapter/chat-body a {:model "m"} req)
               (adapter/chat-body a {:model "m"} (assoc req :prefill "```tool-call\n"))))))

    (testing "an unknown provider names what is available"
      (is (thrown? Throwable (registry/adapter-for :nope))))))

;; --- retry policy -----------------------------------------------------------

(deftest every-provider-call-bounds-its-connect
  ;; http-client honours :conn-timeout as of v0.0.3 (a variadic-fcntl fix);
  ;; before that it was ignored and a connect was bounded only by the
  ;; kernel's SYN retry limit, about 75s on macOS. Now that the option has
  ;; teeth, a call that omits it is the one that hangs — and list-models,
  ;; the boot-time reachability probe, was exactly that call.
  (let [cfg {:base-url "https://api.example.com/v1" :model "m" :api-key "k"
             :conn-timeout-ms 4321}
        a (registry/adapter-for :deepseek)
        seen (atom nil)]
    (testing "the chat call"
      (with-redefs [http/post
                    (fn [_ opts]
                      (reset! seen opts)
                      {:status 200
                       :body (json/write-str
                              {:choices [{:message {:content "ok"}
                                          :finish_reason "stop"}]})})]
        (client/chat a cfg [{:role "user" :content "hi"}])
        (is (= 4321 (:conn-timeout @seen)))))
    (testing "the models probe"
      (with-redefs [http/get (fn [_ opts] (reset! seen opts)
                               {:status 200 :body "{\"data\":[]}"})]
        (client/list-models a cfg)
        (is (= 4321 (:conn-timeout @seen)))))
    (testing "a config with no explicit value still bounds it"
      (with-redefs [http/get (fn [_ opts] (reset! seen opts)
                               {:status 200 :body "{\"data\":[]}"})]
        (client/list-models a (dissoc cfg :conn-timeout-ms))
        (is (pos? (:conn-timeout @seen)))))))

(deftest error-classification
  (let [a (registry/adapter-for :deepseek)]
    (testing "transient statuses retry"
      (are [status] (= :retry (client/classify a status nil))
        429 500 502 503 408))

    (testing "client errors do not"
      (are [status] (= :fatal (client/classify a status nil))
        400 401 403 404 422))

    (testing "a 429 that means out of credit is a wall, not a window"
      ;; dirge PR 689: retrying a usage cap spends the run's budget against
      ;; something that will not move before the run ends.
      (is (= :fatal (client/classify a 429 {:error {:message "Insufficient Balance"}})))
      (is (= :fatal (client/classify a 429 {:error {:message "You exceeded your current quota"}})))
      ;; GLM/Zhipu signal a daily cap as code 1308 + a Chinese message, not in
      ;; English — the provider we actually run on.
      (is (= :fatal (client/classify a 429 {:error {:code "1308" :message "达到使用上限"}})))
      (is (= :fatal (client/classify a 429 {:error {:message "per-day quota reached"}})))
      (is (= :retry (client/classify a 429 {:error {:message "Rate limit reached, slow down"}}))))))

(deftest backoff-carries-jitter
  ;; The exponential base is jittered up to +25% so a beam that all trips the
  ;; same 429 does not retry in lockstep and re-collide.
  (let [waits (repeatedly 40 #(#'client/backoff-ms 1 nil))]
    (is (every? #(<= 8000 % 10000) waits) "attempt 1 base 8s, +0-25%")
    (is (> (count (distinct waits)) 1) "actually jittered, not constant")))

(deftest retry-after-headers
  ;; dirge PR 719: waiting exactly as long as the provider asked beats
  ;; doubling a guess.
  (testing "retry-after in seconds"
    (is (= 5000 (client/retry-after-ms {"retry-after" "5"})))
    (is (= 5000 (client/retry-after-ms {"Retry-After" " 5 "}))))

  (testing "the x-ratelimit-reset family, with or without a unit suffix"
    (is (= 3000 (client/retry-after-ms {"x-ratelimit-reset-requests" "3s"})))
    (is (= 2000 (client/retry-after-ms {"x-ratelimit-reset-tokens" "2"}))))

  (testing "the ask is unclamped — the ceiling lives at the sleep"
    ;; The clamp used to sit in retry-after-ms, which made the in-run cap
    ;; check compare a 60s-bounded number against a 300s threshold and so
    ;; never fire (provenance CR1-2).
    (is (= 3600000 (client/retry-after-ms {"retry-after" "3600"})))
    (is (= client/max-backoff-ms (#'client/backoff-ms 0 {"retry-after" "3600"}))
        "what we actually sleep is still bounded by our ceiling"))

  (testing "no header means fall back to the ladder"
    (is (nil? (client/retry-after-ms {})))
    (is (nil? (client/retry-after-ms {"retry-after" "not-a-number"})))))

;; --- a call with no fence ---------------------------------------------------

(deftest an-unfenced-trailing-call-is-accepted
  ;; Measured at 23 of 34 turns in one run: the model emitted exactly the right
  ;; JSON, omitted the fence, and the harness threw it away and asked it to try
  ;; again, which it did the same way. A whole run lost to punctuation.
  (let [r (fence/parse-tool-call
           "I'll check positive definiteness.\n\n{\"name\": \"verify_octave\", \"args\": {\"claim\": \"A is PD\", \"expr\": \"all(eig(A) > 0)\"}}")]
    (is (= "verify_octave" (:name r)))
    (is (= "A is PD" (get-in r [:args :claim])))
    (is (:unfenced? r) "the signal is recorded rather than silently normalised")
    (is (= 0 (:fences r)))))

(deftest a-fence-still-wins-over-trailing-json
  ;; The fallback must not change how a well-formed response is read. A model
  ;; that fences its call and then prints data after it gets the fenced call.
  (let [r (fence/parse-tool-call
           "```tool-call\n{\"name\": \"verify\", \"args\": {\"claim\": \"real\"}}\n```\n\n{\"name\": \"decoy\", \"args\": {}}")]
    (is (= "verify" (:name r)))
    (is (not (:unfenced? r)))))

(deftest trailing-json-that-is-not-a-call-is-not-a-call
  ;; Reporting this as a malformed call would send the model looking for a
  ;; mistake it did not make. It has no tool call, which is a different thing.
  (is (nil? (fence/parse-tool-call "Here is the matrix:\n\n{\"rows\": 3, \"cols\": 3}")))
  (is (nil? (fence/parse-tool-call "no json at all here")))
  (is (nil? (fence/parse-tool-call "{\"name\": \"\", \"args\": {}}"))))

(deftest an-unfenced-call-still-gets-control-char-repair
  (let [r (fence/parse-tool-call
           "{\"name\": \"verify_lean\", \"args\": {\"lean\": \"theorem t : True := by\ntrivial\"}}")]
    (is (= "verify_lean" (:name r)))
    (is (:unfenced? r))
    (is (:auto-repaired? r))))

(deftest reasoning-effort-is-sent-only-when-asked-for
  ;; deepseek-v4-pro thinks by default and deepseek-v4-flash does not, so
  ;; "thinking is on" was a property of which model happened to be configured
  ;; rather than something the run stated. reasoning_effort makes it explicit:
  ;; the API honours "high" and treats "none" as thinking disabled — a probe
  ;; with "none" came back with no reasoning_content and one completion token.
  ;;
  ;; Omitted from the body entirely when unset, because a provider that has
  ;; never heard of the field rejects the request rather than ignoring it.
  (let [a (registry/adapter-for :deepseek)
        req {:messages [{:role "user" :content "go"}] :max-tokens 10}]
    (testing "absent from the body when the config says nothing"
      (is (not (contains? (adapter/chat-body a {:model "m"} req) :reasoning_effort))))

    (testing "sent when the config asks for it"
      (is (= "high" (:reasoning_effort
                     (adapter/chat-body a {:model "m" :reasoning-effort "high"} req)))))

    (testing "\"none\" is a real setting and not the same as unset"
      ;; It is how thinking gets turned OFF, so dropping it as falsy-looking
      ;; would silently leave thinking on for a run that asked for neither.
      (is (= "none" (:reasoning_effort
                     (adapter/chat-body a {:model "m" :reasoning-effort "none"} req)))))))

(deftest an-xml-style-tool-call-is-accepted-rather-than-discarded
  ;; gen-30, the first run on deepseek-v4-pro. The model emits Anthropic's XML
  ;; tool syntax instead of the fenced JSON this harness documents, and the
  ;; parser saw no fence and reported a no-call. Eight of the run's first
  ;; twelve no-calls were this, on a branch that alternated failing turn and
  ;; prefill-recovered turn all the way to turn 13.
  ;;
  ;; Same reasoning as the unfenced-JSON path already here: the model said
  ;; exactly what it wanted, unambiguously, and throwing it away over
  ;; punctuation costs a turn and teaches nothing — it retries the same way,
  ;; because that IS its native format.
  ;;
  ;; Narrow, like that path. Only when no fence and no trailing JSON was
  ;; found, and only for a complete <invoke name="..."> … </invoke>.
  (testing "a plain call becomes name and args"
    (let [p (fence/parse-tool-call
             (str "Let me look.\n<tool_calls>\n<invoke name=\"lean_search\">\n"
                  "<parameter name=\"query\">List.Chain' dropLast</parameter>\n"
                  "</invoke>\n</tool_calls>"))]
      (is (= "lean_search" (:name p)))
      (is (= {:query "List.Chain' dropLast"} (:args p)))
      (is (:xml-call? p) "recorded, so it stays visible rather than silently normalised")))

  (testing "a value that is a number arrives as one"
    ;; top_k reaches (take k) and a string throws there.
    (let [p (fence/parse-tool-call
             (str "<invoke name=\"lean_search\">"
                  "<parameter name=\"query\">chain</parameter>"
                  "<parameter name=\"top_k\">8</parameter></invoke>"))]
      (is (= {:query "chain" :top_k 8} (:args p)))))

  (testing "an id that merely contains digits stays a string"
    (let [p (fence/parse-tool-call
             "<invoke name=\"fetch_artifact\"><parameter name=\"id\">s#1392</parameter></invoke>")]
      (is (= {:id "s#1392"} (:args p)))))

  (testing "a Lean body keeps its newlines, quotes and backslashes verbatim"
    ;; The whole point of the XML form is that values are not JSON-escaped.
    (let [lean "theorem t : True := by\n  simp [\"a\\b\"]\n  trivial"
          p (fence/parse-tool-call
             (str "<invoke name=\"verify_lean\">"
                  "<parameter name=\"claim\">a claim</parameter>"
                  "<parameter name=\"lean\">" lean "</parameter></invoke>"))]
      (is (= lean (get-in p [:args :lean])))))

  (testing "a real fence still wins"
    ;; A model that shows the XML while reasoning and then issues a proper
    ;; fenced call must not have the reasoning parsed as its call.
    (let [p (fence/parse-tool-call
             (str "<invoke name=\"lean_search\"><parameter name=\"query\">x</parameter></invoke>\n"
                  "```tool-call\n{\"name\": \"verify_lean\", \"args\": {\"claim\": \"c\"}}\n```"))]
      (is (= "verify_lean" (:name p)))
      (is (not (:xml-call? p)))))

  (testing "the last invoke wins, as the last fence does"
    (let [p (fence/parse-tool-call
             (str "<invoke name=\"lean_search\"><parameter name=\"query\">first</parameter></invoke>\n"
                  "<invoke name=\"fetch_artifact\"><parameter name=\"id\">827</parameter></invoke>"))]
      (is (= "fetch_artifact" (:name p)))))

  (testing "prose about the format is not a call"
    (is (nil? (fence/parse-tool-call
               "Do not use <invoke name=...> syntax; use the fenced form.")))
    (is (nil? (fence/parse-tool-call "<invoke name=\"lean_search\">unterminated"))
        "an opener with no closer is not a call")
    (is (nil? (fence/parse-tool-call "<invoke>no name here</invoke>")))))

(deftest retry-after-is-the-providers-ask-unclamped
  ;; provenance CR1-2: the value was clamped to max-backoff-ms (60s)
  ;; BEFORE the in-run cap check compared it against max-in-run-retry-wait-ms
  ;; (300s) — a 60s ceiling under a 300s guard made the "usage cap wearing a
  ;; rate limit" branch unreachable. The clamp belongs at the sleep, not here.
  (is (= 3600000 (client/retry-after-ms {"x-ratelimit-reset-tokens" "3600"})))
  (is (= 301000 (client/retry-after-ms {"retry-after" "301"})))
  (is (= 0 (client/retry-after-ms {"retry-after" "0"})))
  (is (nil? (client/retry-after-ms {}))))

(deftest a-reset-beyond-the-retry-window-is-fatal-not-slept
  ;; The guard in `chat` fires on the UNCLAMPED ask: a 429 whose reset is an
  ;; hour out is a cap in a rate-limit's clothing, and retrying it just burns
  ;; the run's budget against a wall that will not move (dirge PR 689).
  (let [adapter (reify adapter/Adapter
                  (id [_] :fake)
                  (display-name [_] "Fake")
                  (chat-url [_ _] "http://example.invalid/chat")
                  (auth-headers [_ _] {})
                  (chat-body [_ _ _] {})
                  (prefill-support? [_ _] false)
                  (error-message [_ _] nil)
                  (usage-cap? [_ _ _] false)
                  (parse-chat [_ _] {:content "x" :finish-reason "stop"}))]
    (with-redefs [http/post (fn [& _] {:status 429
                                        :headers {"x-ratelimit-reset-tokens" "3600"}})]
      (is (thrown-with-msg? Exception #"cap"
                            (client/chat adapter {:max-retries 1}
                                         [{:role "user" :content "go"}]))))))

(deftest the-reasoning-stream-survives-onto-the-response
  ;; turns.reasoning_text was empty for every run ever recorded. Not because
  ;; nothing reasoned — agent/loop writes :reasoning-text (:reasoning response)
  ;; on every turn — but because the client folds the provider's reasoning into
  ;; <think> framing on :content and then drops the key, so that write always
  ;; stored nil. Querying the column to ask whether a model reasoned returned
  ;; absence, which reads as "it did not".
  ;;
  ;; The fold stays: it is what lets one fence parser work across providers.
  ;; The key is carried alongside it, which is additive.
  (let [adapter (reify adapter/Adapter
                  (id [_] :fake)
                  (display-name [_] "Fake")
                  (chat-url [_ _] "http://example.invalid/chat")
                  (auth-headers [_ _] {})
                  (chat-body [_ _ _] {})
                  (prefill-support? [_ _] false)
                  (error-message [_ _] nil)
                  (usage-cap? [_ _ _] false)
                  (parse-chat [_ _] {:content "1183"
                                     :reasoning "91*10=910, 91*3=273"
                                     :finish-reason "stop"}))]
    (with-redefs [http/post (fn [& _] {:status 200 :body "{}"})]
      (let [r (client/chat adapter {:model "m"} [{:role "user" :content "go"}])]
        (is (= "<think>91*10=910, 91*3=273</think>\n1183" (:content r))
            "the fold is unchanged — one parser still sees one string")
        (is (= "91*10=910, 91*3=273" (:reasoning r))
            "and the reasoning is still reachable on its own, for the column that stores it")))))

(deftest the-other-two-wrappers-pro-reaches-for-are-accepted
  ;; Every one of gen-30's first twelve no-calls was a valid call in the wrong
  ;; wrapper: 8 in Anthropic's <invoke> syntax, 3 in <tool-call> tags, 1 in a
  ;; ```json fence. Not prose, not confusion about what to do next — the call
  ;; was right there each time.
  (testing "<tool-call> tags carry the documented body"
    ;; Unambiguous intent, so treated exactly like the ```tool-call fence:
    ;; a malformed body here should still earn a parse error rather than
    ;; silence, which is what a branch needs to correct itself.
    (let [p (fence/parse-tool-call
             "<tool-call>\n{\"name\": \"fetch_artifact\", \"args\": {\"id\": 1421}}\n</tool-call>")]
      (is (= "fetch_artifact" (:name p)))
      (is (= {:id 1421} (:args p))))
    (let [p (fence/parse-tool-call "<tool-call>\n{not json}\n</tool-call>")]
      (is (= "__parse_error__" (:name p))
          "a broken body in an unmistakable wrapper is still worth reporting")))

  (testing "a ```json fence counts only when the body is actually a call"
    (let [p (fence/parse-tool-call
             "```json\n{\"name\": \"lean_search\", \"args\": {\"query\": \"chain\"}}\n```")]
      (is (= "lean_search" (:name p))))

    ;; Guarded, unlike the two wrappers above, because ```json is a general
    ;; purpose fence a model also uses to show data. gen-30 emitted
    ;; {"lean_search": {...}} — the tool name as the KEY — which is NOT the
    ;; documented shape, and guessing that a one-key object is a call would
    ;; mean the parser deciding what is a tool name.
    (is (nil? (fence/parse-tool-call
               "```json\n{\"lean_search\": {\"query\": \"chain\"}}\n```"))
        "an unrecognised shape is a no-call, not a parse error")
    (is (nil? (fence/parse-tool-call
               "Here is the data I used:\n```json\n{\"vertices\": 4, \"edges\": 6}\n```"))
        "and quoted data is left alone"))

  (testing "the documented fence still wins over both"
    (let [p (fence/parse-tool-call
             (str "<tool-call>\n{\"name\": \"wrong\"}\n</tool-call>\n"
                  "```tool-call\n{\"name\": \"right\"}\n```"))]
      (is (= "right" (:name p))))))

(deftest an-opener-and-closer-that-disagree-still-delimit-one-call
  ;; gen-30 again, and the same failure as before wearing different clothes:
  ;; 13 of 14 no-calls in a 60-turn window CONTAINED ```tool-call and were
  ;; recorded as having made no call. The model opens with the documented
  ;; fence and closes with </tool-call> — one delimiter from each of the two
  ;; forms it knows. Neither pattern matched: the fence wants a closing ```,
  ;; the tag wants an opening <tool-call>.
  ;;
  ;; One of these was 30,658 characters carrying a complete recursive
  ;; extraction proof, well inside the token cap. It was thrown away for
  ;; using the wrong bracket at the end.
  (testing "fence open, tag close"
    (let [p (fence/parse-tool-call
             "```tool-call\n{\"name\": \"verify_lean\", \"args\": {\"claim\": \"c\"}}\n</tool-call>")]
      (is (= "verify_lean" (:name p)))
      (is (= {:claim "c"} (:args p)))))

  (testing "tag open, fence close"
    (let [p (fence/parse-tool-call
             "<tool-call>\n{\"name\": \"lean_search\", \"args\": {\"query\": \"q\"}}\n```")]
      (is (= "lean_search" (:name p)))))

  (testing "the matching pairs still work"
    (is (= "a" (:name (fence/parse-tool-call "```tool-call\n{\"name\": \"a\"}\n```"))))
    (is (= "b" (:name (fence/parse-tool-call "<tool-call>{\"name\": \"b\"}</tool-call>")))))

  (testing "the last call still wins when several appear"
    (is (= "second"
           (:name (fence/parse-tool-call
                   (str "```tool-call\n{\"name\": \"first\"}\n```\n"
                        "```tool-call\n{\"name\": \"second\"}\n</tool-call>"))))))

  (testing "an opener with no closer falls through to the other rungs"
    ;; Not special-cased: a response ending in a well-formed call is accepted
    ;; by the unfenced rung whether or not a fence was opened, and flagged
    ;; :unfenced? so it stays visible.
    (let [p (fence/parse-tool-call "```tool-call\n{\"name\": \"x\"}")]
      (is (= "x" (:name p)))
      (is (:unfenced? p)))
    (is (nil? (fence/parse-tool-call "```tool-call\nI will search for it"))
        "and an opener over prose is still no call at all")))

;; --- identifying a llama.cpp endpoint ---------------------------------------

(deftest a-llama-cpp-endpoint-is-identified-by-asking-not-by-config-key
  ;; RFC-005 recorded that :local was decided by which config key an endpoint
  ;; sat under, so a llama-server configured as :openai silently got no prefix
  ;; pinning. The naive repair — send cache_prompt everywhere and let servers
  ;; ignore it — is worse than the gap: dirge measured strict OpenAI-compatible
  ;; servers answering 422 on the whole request over one unknown field, so a
  ;; field sent hopefully is a session that cannot make a single request.
  (let [hosted (openai/openai-family {:id :openai :label "O"})
        req {:messages [] :max-tokens 10 :cache-key "B1"}]
    (testing "a hosted endpoint's body is untouched"
      (let [body (adapter/chat-body hosted {:base-url "u"} req)]
        (is (nil? (:cache_prompt body)))
        (is (nil? (:chat_template_kwargs body)))
        (is (nil? (:id_slot body)))))

    (testing "the same adapter, once /props identified it, gets the knobs"
      (let [body (adapter/chat-body hosted {:base-url "u" :llama-cpp? true} req)]
        (is (true? (:cache_prompt body)))))))

(deftest thinking-is-off-by-default-on-a-local-endpoint
  ;; llama.cpp has Qwen-family reasoning ON by default and `/no_think` in the
  ;; PROMPT does not disable it — the chat template decides, not the text. A
  ;; local model can therefore spend its whole output budget thinking and
  ;; return a reply with neither content nor a tool call. This layer already
  ;; treats that reply as an error rather than an empty answer, which is the
  ;; right reading and does nothing to prevent it.
  (let [local (openai/openai-family {:id :local :label "L"})
        req {:messages [] :max-tokens 10 :cache-key "B1"}]
    (is (= {:enable_thinking false}
           (:chat_template_kwargs (adapter/chat-body local {:base-url "u"} req))))
    (testing "and a reasoning model asked to reason is still a valid config"
      (is (nil? (:chat_template_kwargs
                 (adapter/chat-body local {:base-url "u" :thinking? true} req)))))))

(deftest an-id-slot-is-pinned-only-from-an-explicit-table
  ;; A slot count is a property of how the server was launched; inventing an
  ;; index evicts another conversation's warm prefix to serve a guess.
  (let [local (openai/openai-family {:id :local :label "L"})
        req {:messages [] :max-tokens 10 :cache-key "B1"}]
    (is (nil? (:id_slot (adapter/chat-body local {:base-url "u"} req))))
    (is (= 3 (:id_slot (adapter/chat-body local {:base-url "u" :slots {"B1" 3}} req))))
    (is (nil? (:id_slot (adapter/chat-body local {:base-url "u" :slots {"other" 3}} req))))))

(deftest the-probe-answers-nil-for-anything-that-is-not-llama-cpp
  ;; Unreachable, not-llama.cpp and malformed are the same answer, and none of
  ;; them is a reason not to start.
  (with-redefs [http/get (fn [& _] {:status 404 :body "not found"})]
    (is (nil? (client/probe-llama-cpp {:base-url "http://x/v1"}))))
  (with-redefs [http/get (fn [& _] {:status 200 :body "{\"object\":\"list\"}"})]
    (is (nil? (client/probe-llama-cpp {:base-url "http://x/v1"}))
        "a 200 without total_slots is some other server"))
  (with-redefs [http/get (fn [& _] (throw (ex-info "connection refused" {})))]
    (is (nil? (client/probe-llama-cpp {:base-url "http://x/v1"}))))
  (with-redefs [http/get (fn [& _] {:status 200 :body "{\"total_slots\": 4}"})]
    (is (= {:llama-cpp? true :total-slots 4}
           (client/probe-llama-cpp {:base-url "http://x/v1"}))
        "and total_slots is the server saying how it was launched")))

;; --- repairing a tool call the model nearly got right -----------------------

(deftest a-missing-closing-brace-is-repaired-not-refused
  ;; Observed live, TWICE in one fourteen-turn run, on the two calls carrying
  ;; the run's actual work: the model emitted a complete write_file whose args
  ;; object closed and whose outer object did not. The reply was not truncated
  ;; — it finished cleanly inside the fence — the model simply miscounted,
  ;; which is what happens when the closing braces are eight hundred
  ;; characters of escaped Clojure away from their openers.
  ;;
  ;; Fourteen percent of that run's turns died on one absent character the
  ;; harness could supply deterministically.
  (let [reply (str "```tool-call\n"
                   "{\"name\": \"write_file\", \"args\": {\"path\": \"src/todo/core.clj\","
                   " \"content\": \"(ns todo.core)\\n\\n(defn add [ts t] (conj ts {:t t}))\"}"
                   "\n```")
        parsed (fence/parse-tool-call reply)]
    (is (= "write_file" (:name parsed)))
    (is (= "src/todo/core.clj" (get-in parsed [:args :path])))
    (is (str/includes? (get-in parsed [:args :content]) "(defn add"))
    (is (true? (:auto-repaired? parsed))
        "a branch whose calls need repairing is a fact the mechanics tally
         should see, so the repair is never silent")))

(deftest the-brace-repair-only-adds-and-only-outside-strings
  (testing "a brace inside a content string is text, not structure — the case
            that matters, since the argument is usually source code"
    (is (= "{\"content\": \"{{{ [[[ \"}"
           (fence/close-unbalanced "{\"content\": \"{{{ [[[ \"}"))))

  (testing "an escaped quote does not end the string"
    (is (= "{\"a\": \"say \\\"{\\\" here\"}"
           (fence/close-unbalanced "{\"a\": \"say \\\"{\\\" here\"}"))))

  (testing "already balanced is untouched"
    (is (= "{\"a\": [1 2]}" (fence/close-unbalanced "{\"a\": [1 2]}"))))

  (testing "too many closers is a different mistake and is left to be reported"
    (is (= "{\"a\": 1}}" (fence/close-unbalanced "{\"a\": 1}}"))))

  (testing "nested openers close in the right order — a stack, not a count"
    (is (= "{\"a\": {\"b\": [1]}}" (fence/close-unbalanced "{\"a\": {\"b\": [1"))))

  (testing "a body ending inside a string is refused, however unbalanced"
    (is (= "{\"a\": \"unfinished" (fence/close-unbalanced "{\"a\": \"unfinished")))))

(deftest a-body-that-cannot-be-repaired-still-explains-itself
  (let [parsed (fence/parse-tool-call "```tool-call\n{\"name\": not-json}\n```")]
    (is (= "__parse_error__" (:name parsed)))
    (is (str/includes? (:parse-error parsed) "brace")
        "the complaint names the missing-closer cause alongside the others")))
