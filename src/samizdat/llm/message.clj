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

(ns samizdat.llm.message
  "Message shaping shared by every adapter.

  Two jobs, both of which have to happen the same way for every provider or
  the harness's context management differs by which model it is talking to."
  (:require [clojure.string :as str]
            [samizdat.lexicon :as lexicon]
            [samizdat.tape :as tape]))

(defn strip-think-blocks
  "Remove <think>…</think> from content.

  Applied to PRIOR assistant turns before they go back over the wire. Both
  DeepSeek and Zhipu document that `reasoning_content` must not be fed back on
  later turns — the model regenerates its thinking each turn — and
  accumulating it burns context for nothing. Tolerant of nesting and of an
  unmatched open tag."
  [content]
  (-> (or content "")
      (str/replace #"(?s)<think>.*?</think>" "")
      str/trim))

(defn merge-reasoning
  "Fold a provider's separate reasoning stream into the visible content with
  <think> framing.

  This is what lets one fence parser work across providers: a model that emits
  its tool call inside its reasoning is handled identically to one that emits
  it in the content, because after this they are the same string."
  [content reasoning]
  (let [c (or content "") r (or reasoning "")]
    (cond
      (and (seq r) (seq c)) (str "<think>" r "</think>\n" c)
      (seq r) (str "<think>" r "</think>")
      :else c)))

(defn default-keep-pairs
  "How many recent turns stay verbatim when a branch's history is compacted.

  This is the branch's working memory: what it just tried, what the engine
  said, and the goal state it is mid-way through.

  `:context-budget :keep-pairs`, not a constant. `infer/render` already passes
  the budget's value in, so a constant here was a SECOND answer to the same
  question that applied on exactly the path nobody was watching — a direct
  caller passing no opts — and disagreed with the first as soon as anyone
  retuned the table."
  []
  (lexicon/budget :keep-pairs))

(defn default-compaction-threshold
  "Total content characters before compaction engages.

  Deliberately high. A 19-turn branch carries about 26,000 characters and does
  not need this; the longest branch on record is 86 turns and about 211,000.
  Compaction is for the tail, and a branch that never reaches it is sent
  byte-identical messages to what it would have been sent before.

  `:context-budget :compaction-chars`, for the reason above."
  []
  (lexicon/budget :compaction-chars))

(def ^:private frame-size
  "Messages at the head of a tape that are never compaction candidates: the
  system prompt and the problem statement.

  Not a tunable — it is the SHAPE of a tape, fixed by `loop/seed-messages`,
  and the two things whose byte-stability the whole prefix cache rests on
  (RFC-004's caching rule). Named so the guard below reads as what it is
  rather than as a magic 3."
  2)

(defn- digest-line
  "One line for a turn that has been unloaded: what was tried, how it came out.

  The minimum a branch needs about its own distant past. Anything more — the
  encoding, the engine's full output, the reasoning — is in the journal, and
  confirmed results are in the settled-state block with ids to fetch."
  [{:keys [turn tool category error]}]
  (str "t" turn " " (or tool "?") " → " (name (or category :neutral))
       (when (seq error)
         (let [e (first (str/split-lines (str error)))]
           (str ": " (if (> (count e) 90) (str (subs e 0 90) "…") e))))))

(def ^:private compactable-roles
  "The roles compaction may rewrite here. Both, unlike llm-repl's
  assistant-only default, because a \"user\" message in this harness is a tool
  result rather than a human prompt — and tool results are the bulk of a long
  branch's context. The FRAME (system prompt, problem statement) is excluded
  by index, not by role."
  #{"assistant" "user"})

(def unloaded-marker
  "The marker every compacted message carries, so the model can tell a summary
  from something that was actually said.

  LEADS the line, and that position is the fix for a real failure. It used to
  trail — `t69 cell → neutral [unloaded]` — and on a 119-turn supervisor
  branch, whose context was by then almost entirely these lines standing in
  for its OWN past turns, the model concluded that a digest line is what an
  assistant writes here and emitted eight of them in a row instead of a tool
  call (karamazov-068). A model reads left to right: with the marker last,
  everything it sees first is imitable content. With the marker first, the
  line announces itself as harness bookkeeping before it says anything else.

  Still deliberately TERSE. It repeats on every compacted message, so a long
  sentence here is a per-message tax on the very context compaction exists to
  shrink. Where the detail went, and how to get it back with `fetch_turn`,
  belongs in the system prompt, which says it once. Constant, so a compacted
  message never changes again once written."
  "[unloaded] ")

(defn unloaded?
  "Whether `s` is (or begins as) one of the harness's own compaction digests.

  The loop uses this to recognize a reply that is nothing but an imitation of
  the marker, which needs a different complaint from an ordinary missing
  fence — telling a model that copied our bookkeeping to 'emit a tool call'
  produces another copy (karamazov-068)."
  [s]
  (str/includes? (str s) (str/trim unloaded-marker)))

(defn- self-summary
  "A message summarised from its OWN content: the first non-blank line,
  bounded, plus the pointer.

  The fallback when no turn record can be attributed to the message. It is
  never a lie about which turn a message was — which is the failure mode the
  positional guess had, since a provider error or a no-call turn appends
  messages without appending a turn row, so the k-th message is not the k-th
  turn."
  [{:keys [content]}]
  (let [line (or (first (remove str/blank? (str/split-lines (str content)))) "")
        line (str/trim line)]
    (str unloaded-marker
         (if (> (count line) 90) (str (subs line 0 90) "…") line))))

(defn- replacement-for
  "What an unloaded message becomes: its turn's digest where the message
  carries a turn stamp and that turn is on the log, else a summary of its own
  content."
  [m turns-by-number]
  (if-let [t (get turns-by-number (:turn m))]
    (str unloaded-marker (digest-line t))
    (self-summary m)))

(defn compact
  "Compact a branch's older turns IN PLACE, so the array's shape never changes.

  A branch's context is its own narrative, and past a certain length most of
  it is prose it will never consult again — while the part it genuinely needs,
  which approaches are already spent, is buried in that prose. This inverts
  that: recent turns stay whole, older ones become one line each.

  WHY IN PLACE. The previous version appended its digest to the PROBLEM
  message, to keep the conversation strictly alternating after it. That works,
  but it rewrites message 1 every time compaction fires, so the shared prefix
  changes on every turn and the upstream prompt cache is invalidated from
  index 1 onward — every turn re-prefills the whole conversation. Replacing
  each aged-out message's content in place keeps roles, order and count
  identical, so alternation holds with no provider needing to be forgiving AND
  the prefix stays stable: each message is rewritten once, ever, and the
  prefix before the newest rewrite is byte-identical from turn to turn. This
  is llm-repl's chat-memory design; see samizdat.tape for the primitives and
  docs/RFCS/RFC-004-tape-and-inference.md for the argument.

  ONE ATTEMPT PER MESSAGE. A replacement is accepted only inside the
  compression band (|new| ≤ max(|original|, floor)); outside it the message is
  marked declined and left verbatim, and it never returns to the due set.
  Every outcome changes the array, which is what makes a loop impossible — a
  rejection that marked nothing decremented no measure, and llm-repl logged 31
  attempts against one message before finding that out.

  Nothing is lost, only unloaded: every turn is in the journal, every
  confirmed and refuted artifact is in the settled-state block, and the
  encodings are one `fetch_artifact` away. Applied on the way to the wire, so
  the branch's own history is untouched and a resume replays what was really
  sent at the time."
  ([messages turns] (compact messages turns nil))
  ([messages turns {:keys [keep-pairs threshold-chars floor]}]
   (let [messages (vec messages)
         keep-pairs (or keep-pairs (default-keep-pairs))
         threshold (or threshold-chars (default-compaction-threshold))
         total (reduce + 0 (map (comp count str :content) messages))]
     ;; Nothing to do below the threshold, and nothing to do when the tape is
     ;; the frame plus at most nothing: there is no message past it.
     (if (or (< total threshold) (<= (count messages) frame-size))
       messages
       ;; The frame is never a candidate: the system prompt and the problem
       ;; statement are the two things whose stability the whole prefix cache
       ;; rests on, and they are also the two a branch cannot do without.
       (let [turns-by-number (into {} (map (juxt :turn identity)) (or turns []))
             ;; Everything past the last `keep-pairs` exchanges, counted in
             ;; ASSISTANT turns — the tape's own definition of a turn, and one
             ;; that does not shift when the harness inserts a message of its
             ;; own.
             ;; Both roles: in samizdat a "user" message is usually a TOOL
             ;; RESULT, and those are the bulk of a long branch's context.
             ;; Compacting assistant turns alone would leave almost all of it
             ;; verbatim.
             opts {:floor (or floor tape/default-floor) :roles compactable-roles}
             ;; Indices 0 and 1 are the frame — the system prompt and the
             ;; problem. tape/due-indices deliberately knows nothing about
             ;; which leading messages are load-bearing, so the frame is
             ;; protected here, by the caller that owns it.
             due (remove #(< % frame-size)
                         (tape/due-indices messages keep-pairs compactable-roles))]
         (reduce (fn [ms i]
                   (tape/compact-at ms i
                                    (replacement-for (nth ms i) turns-by-number)
                                    opts))
                 messages
                 due))))))

(def ledger-open "<!--settled-state-->")
(def ledger-close "<!--/settled-state-->")

(def ^:private ledger-re
  (re-pattern (str "(?s)" (java.util.regex.Pattern/quote ledger-open)
                   ".*?" (java.util.regex.Pattern/quote ledger-close))))

(defn strip-stale-ledgers
  "Drop every settled-state block except the most recent.

  The ledger is regenerated from the artifacts table each turn and appended to
  that turn's result, so without this a branch accumulates one copy per turn.
  gen-18's ledger is roughly 6,800 tokens; eighty turns of it would dwarf the
  transcript it exists to summarise.

  A ledger is STATE. Only the newest is true, and an older copy is a strictly
  worse version of it — the same argument strip-think-blocks makes about
  reasoning, and the reason both live here rather than at the call site.

  Applied on the way to the wire only. The branch keeps every copy in its own
  message list, so the journal and a resume see exactly what was sent at the
  time."
  [messages]
  (let [last-idx (last (keep-indexed (fn [i m]
                                       (when (re-find ledger-re (str (:content m))) i))
                                     messages))]
    (if (nil? last-idx)
      messages
      (map-indexed (fn [i m]
                     (if (= i last-idx)
                       m
                       (update m :content #(str/replace (str %) ledger-re ""))))
                   messages))))

(defn prepare
  "Normalize a conversation for the wire: keyword roles become strings, prior
  assistant turns lose their think blocks, and every settled-state block but
  the newest is dropped. System and user messages are otherwise left alone."
  [messages]
  (mapv (fn [{:keys [role content] :as m}]
          (let [role (if (keyword? role) (clojure.core/name role) (str role))]
            (assoc (select-keys m [])
                   :role role
                   :content (-> (if (and (= "assistant" role) content)
                                  (strip-think-blocks content)
                                  (or content ""))
                                ;; The markers are harness framing and must not
                                ;; reach the model, which would otherwise learn
                                ;; to emit them.
                                (str/replace ledger-open "")
                                (str/replace ledger-close "")
                                str/trim))))
        (strip-stale-ledgers messages)))
