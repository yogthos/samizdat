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

(ns samizdat.llm.fence
  "Parse the model's tool call out of a ```tool-call fenced JSON block.

  The harness deliberately does not use provider-native tool calling. A fence
  works identically on every OpenAI-compatible endpoint, including ones whose
  tool-calling implementation is broken or absent, and it survives a provider
  that puts the call inside its reasoning stream.

  The cost is that malformed JSON is now the harness's problem. One repair pass
  handles the dominant failure — a multi-line SMT-LIB or Lean snippet written
  into a string value with raw newlines, which is invalid JSON per RFC 8259 and
  accounted for 5 of 35 turns in the n=500 Sidon run. Anything else is reported
  back to the model as a parse error rather than guessed at.

  Every parse records mechanics signals: whether a repair was needed, how many
  fences appeared, whether it failed outright. These feed the capability tier,
  and only the capability tier. Per the rule from dirge PR 740, a signal may
  tune a guard that fires on the same thing the signal measures, so these may
  adjust repair budgets and may never relax a verification gate."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

;; Any opener paired with any closer.
;;
;; The model knows two spellings of the wrapper — the documented ```tool-call
;; fence and <tool-call> tags — and mixes them: it opens with one and closes
;; with the other. Requiring a matching pair cost gen-30 thirteen turns in a
;; sixty-turn window, one of them 30,658 characters carrying a complete
;; recursive extraction proof, thrown away for the wrong bracket at the end.
;;
;; Both spellings are unmistakable in intent, so the body between them is
;; treated exactly like the documented fence's, malformed ones included: those
;; earn a parse error, which is what lets a branch correct itself.
;;
;; The CLOSER also takes the plural and the underscore — </tool-calls>,
;; </tool_call>, </tool_calls>. gen-31 B1.2 opened with the documented fence
;; and closed with </tool-calls> on three consecutive turns; `</tool-call>` did
;; not match, the `s` sitting where the `>` was expected, and what was thrown
;; away was a proof of edge-choice injectivity, one of the three gaps that run
;; existed to cross. Same shape as the gen-30 loss, one letter over.
;;
;; The OPENER is deliberately NOT widened to <tool_calls>. That is the wrapper
;; of the XML tool syntax, and xml-call is the last rung — reached only when no
;; fence matched — so treating it as a fence opener would capture a good XML
;; call into the fence path and turn it into a parse error.
(def fence-re
  #"(?s)(?:```tool-call\s*\r?\n|<tool-call>\s*)(.*?)(?:```|</tool[-_]calls?>)")

;; A general-purpose ```json fence, which a model also uses to show data. Only
;; counts when the body is the DOCUMENTED shape, checked in json-fence below.
(def ^:private json-fence-re #"(?s)```json\s*\r?\n(.*?)```")

(defn close-unbalanced
  "Append the `}` and `]` a tool-call body is missing, or return it unchanged.

  Counts braces and brackets OUTSIDE string literals, with the same state
  machine `repair-control-chars` uses — a `{` inside a content string is text,
  not structure, and a naive count is wrong in exactly the case that matters
  (a tool call whose argument is source code).

  WHY THIS EXISTS. Observed live, twice in one fourteen-turn run: the model
  emitted a complete `write_file` call whose body ended
  `…\\n\"}` — the args object closed and the outer object did not. One
  character missing, a whole turn lost, twice, on the two calls that carried
  the run's actual work. The reply was not truncated (it finished cleanly
  inside the fence); the model simply miscounted, which is what a model does
  when the closing braces are eight hundred characters of escaped Clojure away
  from their openers.

  REFUSES TO REPAIR A BODY THAT ENDS INSIDE A STRING, and that guard is the
  load-bearing half. A reply cut off by the token cap stops mid-content — the
  string never closes — and appending braces there would produce a perfectly
  valid `write_file` carrying HALF A FILE, which the tool would then write over
  the whole one and report as a success. A parse error costs a turn; a silently
  truncated file costs the work. Ending inside a string is precisely the shape
  of a truncated reply, so it is where the repair stops.

  Only ever ADDS closers: an unbalanced-the-other-way body (more closers than
  openers) is a different mistake and is reported rather than guessed at. The
  caller must still parse the result — this makes a parse possible, it does not
  assert one succeeded — and a repaired call is flagged `:auto-repaired?`,
  because a branch whose calls need repairing is a fact the mechanics tally
  should see."
  [^String input]
  (let [n (count input)]
    (loop [i 0, in-string? false, escaped? false, stack []]
      (if (>= i n)
        (if (and (seq stack) (not in-string?))
          (str input (str/join (reverse stack)))
          input)
        (let [ch (.charAt input i)]
          (cond
            escaped? (recur (inc i) in-string? false stack)
            (= \\ ch) (recur (inc i) in-string? true stack)
            (= \" ch) (recur (inc i) (not in-string?) false stack)
            in-string? (recur (inc i) true false stack)
            (= \{ ch) (recur (inc i) false false (conj stack "}"))
            (= \[ ch) (recur (inc i) false false (conj stack "]"))
            (or (= \} ch) (= \] ch))
            ;; A closer with nothing open is the other kind of imbalance and
            ;; is not repairable by appending: bail out unchanged.
            (if (empty? stack) input (recur (inc i) false false (pop stack)))
            :else (recur (inc i) in-string? false stack)))))))

(defn repair-control-chars
  "Escape literal control characters appearing INSIDE JSON string literals.

  A small state machine rather than a regex, because whether a newline is
  invalid depends on being inside a string, and that is not a regular
  property. Outside strings everything is copied through; inside, bare
  newline, carriage return and tab become their escape sequences.

  Does not fix unmatched backslashes or smart quotes. Those get reported."
  [^String input]
  (let [n (count input)
        sb (StringBuilder.)]
    (loop [i 0, in-string? false, escaped? false]
      (if (>= i n)
        (.toString sb)
        (let [ch (.charAt input i)]
          (cond
            escaped?
            (do (.append sb ch) (recur (inc i) in-string? false))

            (= \\ ch)
            (do (.append sb ch) (recur (inc i) in-string? true))

            (= \" ch)
            (do (.append sb ch) (recur (inc i) (not in-string?) false))

            (and in-string? (= \newline ch))
            (do (.append sb "\\n") (recur (inc i) in-string? false))

            (and in-string? (= \return ch))
            (do (.append sb "\\r") (recur (inc i) in-string? false))

            (and in-string? (= \tab ch))
            (do (.append sb "\\t") (recur (inc i) in-string? false))

            :else
            (do (.append sb ch) (recur (inc i) in-string? false))))))))

(defn repair-json
  "One repair pass over a tool-call body: control characters inside strings,
  then unbalanced closers. Order matters — the brace scan has to see the
  string boundaries the control-char pass leaves intact."
  [^String input]
  (close-unbalanced (repair-control-chars input)))

(defn- parse-error [msg extra]
  (merge {:name "__parse_error__" :args {} :parse-error msg} extra))

(defn- read-json [s]
  (try
    {:ok true :value (json/read-str s :key-fn keyword)}
    (catch Throwable e
      {:ok false :error (ex-message e)})))

(def ^:private opener-re #"```tool-call\s*\r?\n|<tool-call>\s*")
(def ^:private closer-re #"```|</tool[-_]calls?>")

(def ^:private closer-tokens
  ["```" "</tool-call>" "</tool_call>" "</tool-calls>" "</tool_calls>"])

(defn- closer-token-at
  "The closer token starting at index i and ending by `bound`, or nil."
  [^String s i bound]
  (some (fn [^String t]
          (let [e (+ i (count t))]
            (when (and (<= e bound) (= t (subs s i e)))
              t)))
        closer-tokens))

(defn- string-aware-closer
  "Index of the first closer OUTSIDE a JSON string literal in s[from..to),
  or nil.

  Same state machine as close-unbalanced, for the same reason: a ``` inside
  a content string is text, not structure. The mdlite dogfood run
  (karamazov-hpv) made this concrete — the file being written was a MARKDOWN
  CONVERTER, its code contained literal ``` in its own fence handling, and
  the raw closer scan cut six valid calls mid-string, each reported back as
  a parse error the model had not made. Any task whose files mention code
  fences (markdown tooling, docs, READMEs) produces this shape."
  [^String s from to]
  (loop [i from, in-string? false, escaped? false]
    (when (< i to)
      (let [ch (.charAt s i)]
        (cond
          escaped? (recur (inc i) in-string? false)
          (= \\ ch) (recur (inc i) in-string? true)
          (= \" ch) (recur (inc i) (not in-string?) false)
          (and (not in-string?) (closer-token-at s i to)) i
          :else (recur (inc i) in-string? false))))))

(defn extract-fences
  "Every tool-call fence body in the response, in order.

  Opener-anchored rather than a single opener…closer regex: that regex pairs
  an opener with the next ``` — but a second `` ```tool-call `` opener BEGINS
  with ```` ``` ````, so a reasoning model that emits one call, then reasons,
  then emits the real call (or is prefilled into an opener and then thinks)
  had its first opener paired with the second opener's ``` prefix, capturing
  the reasoning instead of the JSON.

  Each opener yields a body only when a closer is found before the NEXT opener.
  An opener whose body has no closer (an opener over prose, or a call whose
  closer the model dropped) contributes nothing here — it falls through to the
  trailing-call and XML rungs, exactly as before. So the count stays a
  faithful mechanics signal, the LAST body is the real call, and an unclosed
  opener is still not a fence."
  [response]
  (let [s (or response "")
        opens (let [m (re-matcher opener-re s)]
                (loop [out []] (if (.find m) (recur (conj out [(.start m) (.end m)])) out)))
        n (count s)]
    (vec
     (for [[i [_ body-start]] (map-indexed vector opens)
           :let [next-open (if (< (inc i) (count opens))
                             (first (nth opens (inc i)))
                             n)
                 ;; String-aware first: a closer inside a JSON string literal
                 ;; is content (karamazov-hpv). The raw scan stays as the
                 ;; fallback for a body whose string never closes — an
                 ;; unterminated string ahead of a real closer is the
                 ;; truncation shape, and it must keep earning its parse
                 ;; error rather than quietly becoming a no-call.
                 closer (or (string-aware-closer s body-start next-open)
                            (let [m (re-matcher closer-re
                                                (subs s body-start next-open))]
                              (when (.find m) (+ body-start (.start m)))))]
           :when closer]
       (str/trim (subs s body-start closer))))))

(defn- json-fence
  "A ```json fence whose body is the documented call shape, or nil.

  Guarded where the two wrappers above are not, because ```json is what a
  model reaches for to show data as well. gen-30 emitted
  {\"lean_search\": {…}} — the tool name as the KEY — and accepting that would
  mean this parser deciding which strings are tool names, which is the
  registry's business and not punctuation's. An unrecognised shape stays a
  no-call rather than becoming a parse error, on the same reasoning as
  trailing-call: telling a model its call is broken when it never made one
  sends it looking in the wrong place."
  [response]
  (when-let [body (some-> (last (re-seq json-fence-re (or response "")))
                          second str/trim)]
    (let [{:keys [ok value]} (read-json (repair-control-chars body))]
      (when (and ok (map? value) (string? (:name value)) (not (str/blank? (:name value))))
        body))))

;; A trailing JSON object, for a response that ends in a well-formed call and
;; simply omits the fence. Anchored to the END of the response and required to
;; balance from a `{` that starts a line, so a JSON example quoted mid-argument
;; cannot be mistaken for the call.
(def ^:private trailing-object-re #"(?s)(?:^|\n)\s*(\{.*\})\s*\z")

(defn- trailing-call
  "The response's final top-level JSON object, but only if it is plausibly a
  tool call: it must parse and carry a non-blank `name`.

  Deliberately silent otherwise. A response ending in some other JSON should
  still be reported as having no tool call, rather than as a malformed one,
  because telling a model its call is broken when it never made one sends it
  looking in the wrong place."
  [response]
  (when-let [candidate (some-> (re-find trailing-object-re (or response ""))
                               second str/trim)]
    (let [{:keys [ok value]} (read-json (repair-control-chars candidate))]
      (when (and ok (map? value) (string? (:name value)) (not (str/blank? (:name value))))
        candidate))))

;; Anthropic's XML tool syntax, which deepseek-v4-pro emits in place of the
;; fenced JSON this harness documents. Both parts are required — an opener with
;; no closer is not a call — so prose ABOUT the format does not become one.
;; `[^>]*` rather than `\s*` before the tag closes: the attribute list does
;; not stop at `name`. deepseek-v4-pro writes
;; `<parameter name="query" string="true">`, and requiring the tag to close
;; immediately after the name made every such parameter fail to match while
;; `<invoke>`, which carries no extra attribute, matched fine — so the call
;; arrived with the right name and NO arguments, and the branch was told
;; "Missing required argument(s): query" with the query sitting in the tag.
;; gen-31 B3 was culled for a malformed fence it had not emitted (2026-08-18).
(def ^:private invoke-re
  #"(?s)<invoke\s+name=\"([^\"]+)\"[^>]*>(.*?)</invoke>")

(def ^:private parameter-re
  #"(?s)<parameter\s+name=\"([^\"]+)\"[^>]*>(.*?)</parameter>")

(defn- xml-value
  "A parameter's value. Verbatim, except that something which is entirely a
  number becomes one.

  Values here are NOT JSON-escaped — that is the whole reason a model reaches
  for this form when handing over a Lean proof — so the text is kept exactly
  as written, newlines, quotes and backslashes included. The number case is
  not cosmetic: `top_k` reaches `(take k)` and a string throws there. Anchored
  and strict, so `s#1392` and a claim that merely mentions a figure stay
  strings."
  [s]
  (let [t (str/trim s)]
    (cond
      (re-matches #"-?\d+" t) (parse-long t)

      ;; A JSON array or object. The XML parameter form has no way to express
      ;; one, so a model handing over `subClaims` writes the JSON text as the
      ;; body. Returned verbatim that is a String where the caller wants a
      ;; collection, which seqs into Characters and dies far from here — in the
      ;; journal writer, with "Don't know how to write JSON of class
      ;; java.lang.Character", taking the branch with it. gen-31 lost B1.3 and
      ;; B2.2 that way, both of them on the run's actual target.
      ;;
      ;; Only when it really parses. Lean bodies and prose contain brackets,
      ;; and a value that merely starts with one stays the string it is.
      (and (or (str/starts-with? t "[") (str/starts-with? t "{"))
           (str/includes? t "\""))
      (let [{:keys [ok value]} (read-json t)]
        (if (and ok (coll? value)) value s))

      :else s)))

(defn- xml-call
  "The response's last complete <invoke>, as {:name :args}, or nil.

  Last rather than first, matching the fence rule and for the same reason: a
  model that drafts one call while reasoning and then issues the real one puts
  the real one last."
  [response]
  (when-let [m (last (re-seq invoke-re (or response "")))]
    (let [[_ nm body] m]
      (when-not (str/blank? nm)
        {:name nm
         :args (reduce (fn [acc [_ k v]] (assoc acc (keyword k) (xml-value v)))
                       {} (re-seq parameter-re (or body "")))}))))

(defn reattach
  "The complete assistant turn, given what the request was prefilled with.

  A prefilled request ends mid-fence and the model continues from there
  WITHOUT repeating the opener, so the raw completion is only the tail of what
  the assistant actually said. Both the parser and the transcript need the
  whole thing: the parser because it matches on the opener, and the message
  history because an assistant turn that begins mid-fence misrepresents the
  format back to the model on every later turn.

  Providers differ on whether the prefix comes back in the completion, so a
  response that already starts with it is left alone — reattaching blindly
  would produce two openers whose first fence body is empty."
  [response prefill]
  (if (and (seq prefill)
           (not (str/starts-with? (str/triml (str response)) (str/triml prefill))))
    (str prefill response)
    (str response)))

(declare parse-tool-call* strip-think)

(defn parse-tool-call
  "Parse a model response into a tool call.

  Returns nil when there is no fence at all, a map with `:name` and `:args` on
  success, or a `__parse_error__` map whose `:parse-error` is written for the
  model to read and correct.

  When several fences appear, the LAST one wins. A model that shows an example
  call while reasoning and then issues the real one puts the real one last, and
  a model that issues a call and then rambles rarely emits a second fence. The
  count is recorded either way rather than silently resolved, because
  `:fences > 1` is exactly the sort of tool-call mechanics the capability tier
  is built from.

  `opts` may carry `:prefill`, the partial assistant text the request ended
  with. The model continues from it and does not repeat it, so the response
  begins INSIDE the fence and the opener this function matches on is missing
  from the text — every prefilled turn would read as a no-call, which is the
  opposite of what prefilling is for. The prefix is reattached first, unless
  the response already repeats it: providers differ on that, and reattaching
  blindly would produce two openers whose first fence body is empty."
  ([response] (parse-tool-call response nil))
  ([response {:keys [prefill]}]
   (parse-tool-call* (strip-think
                      (if (seq prefill) (reattach response prefill) response)))))

(def ^:private think-re #"(?s)<think>.*?</think>")

;; An opener whose closer never came, to end of text. A reply truncated
;; mid-thought (finish_reason "length") emits `<think>` and stops, and the
;; balanced pattern above then matches nothing — so the whole reasoning
;; stream stayed in the text, inside the prefilled fence, and corrupted the
;; JSON with exactly the stray parens and quotes the strip exists to remove.
;; Applied only after the balanced pass, so a well-formed reply is untouched
;; and nothing after a real `</think>` is ever swallowed.
(def ^:private open-think-re #"(?s)<think>.*\z")

(defn- strip-think
  "Remove <think>…</think> reasoning blocks before parsing. A reasoning model
  puts its thinking in the content, and after a prefilled `` ```tool-call `` it
  lands INSIDE the fence — its stray parens and quotes then corrupt the JSON.
  The reasoning is not part of the call, so it is dropped before the fence is
  read (the durable transcript still keeps the full text, stripped only on the
  way to the wire)."
  [s]
  (-> (str s)
      (str/replace think-re "")
      (str/replace open-think-re "")))

(defn- parse-tool-call* [response]
  (let [fenced (extract-fences response)
        ;; A response that ends in a well-formed call but omits the fence is
        ;; accepted, and recorded as :unfenced? so it stays visible rather than
        ;; being quietly normalised. Measured at 23 of 34 turns in one run: the
        ;; model emitted exactly the right JSON and the harness discarded it
        ;; over formatting, then told it to try again, which it did the same
        ;; way. That is a whole run lost to punctuation.
        ;;
        ;; Narrow on purpose. It applies only when NO fence was found, only to
        ;; the very end of the response, and only if the object carries a
        ;; `name` — the same validation a fenced body gets. A drafted example
        ;; followed by a real fenced call is unaffected, because the fence wins.
        bodies (if (seq fenced)
                 fenced
                 (when-let [t (or (trailing-call response) (json-fence response))]
                   [t]))
        unfenced? (and (empty? fenced) (seq bodies))]
    ;; Third rung, and last: only when neither a fence nor a trailing JSON
    ;; object was found. Recorded rather than silently normalised, for the same
    ;; reason :unfenced? is — a run where the model never once used the
    ;; documented format is a fact about the arm, not a detail.
    (if (empty? bodies)
      (when-let [x (xml-call response)]
        (assoc x :fences 0 :xml-call? true))
      (when (seq bodies)
      (let [body (peek bodies)
            n (count fenced)
            repaired (repair-json body)
            ;; Computed from the TEXT, not from which parse path succeeded.
            ;; clojure.data.json accepts raw newlines and tabs inside string
            ;; values where JSON.parse rejects them, so keying this off the
            ;; fallback firing would leave the counter permanently zero — a
            ;; signal that is never fed reads identically to a behaviour that
            ;; never happens (dirge PR 740). What we want to measure is that
            ;; the model emitted a body needing repair — unescaped control
            ;; characters, or a missing closer — which is true whichever
            ;; parser tolerated it.
            needed-repair? (not= repaired body)
            base (cond-> {:fences n}
                   needed-repair? (assoc :auto-repaired? true)
                   unfenced? (assoc :unfenced? true))
            first-try (read-json body)]
        (if (:ok first-try)
          (let [parsed (:value first-try)]
            (cond
              (not (map? parsed))
              (parse-error "tool-call body must be a JSON object, not an array or scalar" base)

              (not (string? (:name parsed)))
              (parse-error "tool-call body must have a `name` string" base)

              (str/blank? (:name parsed))
              (parse-error "tool-call `name` must not be empty" base)

              :else
              (merge base
                     {:name (:name parsed)
                      :args (let [a (:args parsed)] (if (map? a) a {}))})))

          ;; One repair pass. If the repair changed nothing there is no point
          ;; re-parsing, and the error message should name the causes the
          ;; repair does not cover.
          (if-not needed-repair?
            (parse-error
             (str (:error first-try)
                  ". Common causes: (a) a raw newline inside a string value — use \\n,"
                  " (b) an unescaped quote inside a string — use \\\","
                  " (c) an unescaped backslash — use \\\\,"
                  " (d) a missing closing brace — count the `{` and `}`,"
                  " the outer object needs one of its own after `args` closes.")
             base)
            (let [second-try (read-json repaired)]
              (if-not (:ok second-try)
                (parse-error
                 (str (:error first-try)
                      ". The harness auto-repaired what it could (control"
                      " characters inside strings, missing closers) and the"
                      " result still did not parse — escape \\n, \\r, \\t,"
                      " \\\\ and \\\" inside string values, and check the braces.")
                 base)
                (let [parsed (:value second-try)]
                  (if (and (map? parsed)
                           (string? (:name parsed))
                           (not (str/blank? (:name parsed))))
                    (merge base
                           {:name (:name parsed)
                            :args (let [a (:args parsed)] (if (map? a) a {}))})
                    (parse-error "tool-call body must be a JSON object with a non-empty `name` string"
                                 base))))))))))))

(defn signals
  "The mechanics signals from one parse, for the capability tier.

  `:no-fence` and `:truncated` are separated on purpose. A reply that hit the
  token cap mid-thought produced no fence because it never got that far, and
  reading that as a model too weak to emit a tool call would be wrong — the
  fix is more tokens, not more steering. It was the first thing a live
  deepseek-v4-flash call did here, so it is not a hypothetical."
  [{:keys [finish-reason]} parsed]
  (let [truncated (= "length" finish-reason)]
    {:no-fence (and (nil? parsed) (not truncated))
     :truncated truncated
     :parse-error (= "__parse_error__" (:name parsed))
     :auto-repaired (boolean (:auto-repaired? parsed))
     :multiple-fences (> (or (:fences parsed) 0) 1)}))
