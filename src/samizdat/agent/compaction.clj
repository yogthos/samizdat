;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Ported from dirge's src/agent/compression.rs and
;; src/agent/agent_loop/context_manager.rs, which are themselves ports of
;; Hermes's context_compressor.py and DeepSeek-Reasonix's context-manager.ts.
;; The budget ladder, the protected head/tail, the user-boundary snapping, the
;; summary sizing, the structural validation and the tool-output pruning are
;; all that lineage; what is new here is the samizdat vocabulary (tool names,
;; the journal, the knowledge store) and the fact that every number and every
;; model-facing sentence is injected rather than compiled in.

(ns samizdat.agent.compaction
  "WHAT THE BRANCH IS ALLOWED TO FORGET, and how it gets it back.

  Compaction here was one thing — replace old messages with a one-line digest
  once the transcript passes a character count — and it fired REACTIVELY, at a
  fixed size, with no notion of how full the model's window actually was. That
  has two failure modes and this project has met both: a branch that overflows
  before the threshold is reached, and a branch that is compacted long before
  it needed to be.

  THE LADDER replaces the single threshold. Every rung is a fraction of the
  model's context window compared against the current token count, so the same
  policy works on an 8k local model and a 200k hosted one:

    0.60  tighten the per-result cap        head off an overflow before folding
    0.75  fold older history into a summary keep a live tail
    0.78  fold harder                       the normal fold bought too little
    0.80  force a summary and end the turn  defence in depth
    0.90  fold at turn START                a huge paste, or a restored session

  PROACTIVE, which is the point. A fold at 0.75 happens while the branch is
  still working well; a fold at 0.98 happens after the model has already lost
  the thread. The cheapest rung — capping one oversized tool result — runs
  first and often makes the rest unnecessary.

  NOTHING IS LOST, and that is what makes folding safe to do early. Three
  paths back:

    the JOURNAL      every turn is a row; `fetch_turn` reopens any of them
    the KNOWLEDGE    what a folded region established is distilled into
                     durable memories before the region goes
    the BREADCRUMBS  those memories are indexed into every later turn, one
                     line each, and `recall {id}` expands one

  So a fold moves detail from the transcript into stores the branch can query,
  rather than destroying it. That is why the summary prompt can afford to be
  terse and why `validate-summary` may refuse: the caller acts on `true` by
  dropping the folded region, so a false positive costs real history.

  PURE, with the numbers and the prose injected. Every threshold is a
  gates.edn entry and every sentence the model reads is a prompt, because a
  compaction policy compiled into `src/` is one the supervisor cannot tune —
  and the supervisor is the role that watches runs run out of context."
  (:require [clojure.string :as str]))

;; --- measuring ---------------------------------------------------------------

(defn content-chars
  "Characters a message contributes, across both content shapes: a plain
  string, and the block vector `[{:type \"text\" :text \"…\"}]` that a tool
  result arrives as. Counting only the string shape is how dirge's own prune
  pass was a silent no-op on live results for a while."
  [content]
  (cond
    (string? content) (count content)
    (sequential? content) (reduce + 0 (map #(count (str (or (:text %) (:content %)))) content))
    (nil? content) 0
    :else (count (str content))))

(defn estimate-tokens
  "Rough token count for a message seq: total content characters over
  `chars-per-token`.

  An ESTIMATE, used at the pre-send measurement point. Where the provider
  reports real usage the caller should prefer that; this exists so the ladder
  can be consulted before a request is built, which is the only place the
  cheap rungs can act."
  [messages chars-per-token]
  (long (Math/ceil (/ (double (reduce + 0 (map #(content-chars (:content %)) messages)))
                      (double (max 1 chars-per-token))))))

(defn pressure
  "Estimated tokens as a fraction of the window, or 0.0 when the window is
  unknown. A model whose context size nobody recorded gets no ladder rather
  than a guessed one."
  [tokens window]
  (if (and window (pos? window)) (/ (double tokens) (double window)) 0.0))

;; --- the ladder --------------------------------------------------------------

(defn tier
  "Which rung the current pressure has reached, or nil below all of them.

  `ladder` is the gates.edn map of fraction thresholds. Ordered most-severe
  first so the highest rung reached is the one that answers — a branch at 0.92
  wants the turn-start fold, not the per-result cap it also passed."
  [ratio {:keys [turn-start force aggressive-fold fold aggressive-cap]}]
  (cond
    (and turn-start (>= ratio turn-start)) :turn-start-fold
    (and force (>= ratio force)) :force-summary
    (and aggressive-fold (>= ratio aggressive-fold)) :aggressive-fold
    (and fold (>= ratio fold)) :fold
    (and aggressive-cap (>= ratio aggressive-cap)) :aggressive-cap
    :else nil))

(defn fold?
  "Whether this tier folds history into a summary."
  [t]
  (contains? #{:fold :aggressive-fold :force-summary :turn-start-fold} t))

(defn aggressive?
  "Whether this tier is past the point where a normal fold is enough. An
  aggressive fold halves the tail it keeps, and no snip is allowed to skip it."
  [t]
  (contains? #{:aggressive-fold :force-summary :turn-start-fold} t))

(defn result-cap
  "The per-tool-result token cap for this pressure: the tight one once the
  aggressive-cap rung is reached, the normal one below it.

  The model that CALLED the tool already saw the whole result on its dispatch
  turn. Later turns see a head-and-tail excerpt with the dropped count, so it
  can call again if it genuinely needs the middle."
  [ratio {:keys [aggressive-cap]} {:keys [normal aggressive]}]
  (if (and aggressive-cap (> ratio aggressive-cap)) aggressive normal))

(defn snip-bought-enough?
  "Whether capping oversized results freed enough that a normal fold can be
  skipped this turn. Never true for an aggressive tier: past that point the
  summary is the thing that is needed, and a snip that looks sufficient is the
  reason a branch arrives at 0.9 having never folded."
  [freed window fraction aggressive]
  (boolean (and (not aggressive)
                window (pos? window)
                (> (/ (double freed) (double window)) fraction))))

;; --- capping one oversized result -------------------------------------------

(defn- clip-middle
  "Head and tail of `s` with the dropped middle accounted for. Keeping both
  ends matters: the head of a tool result says what ran and the tail says how
  it ended, and a plain head truncation drops every error message."
  [s cap-chars note-fn]
  (if (<= (count s) cap-chars)
    s
    (let [half (quot cap-chars 2)
          head (subs s 0 half)
          tail (subs s (max half (- (count s) half)))]
      (str head (note-fn (- (count s) cap-chars)) tail))))

(defn cap-oversized-results
  "Every tool result over `cap` tokens clipped to it, outside the protected
  tail. Returns `{:messages … :freed …}` — the caller needs the saving to
  decide whether a fold is still required."
  [messages {:keys [cap-tokens chars-per-token protect-tail roles note-fn]}]
  (let [n (count messages)
        cutoff (max 0 (- n protect-tail))
        cap-chars (* cap-tokens chars-per-token)
        before (reduce + 0 (map #(content-chars (:content %)) messages))
        out (vec (map-indexed
                  (fn [i m]
                    (if (or (>= i cutoff)
                            (not (contains? roles (str (:role m))))
                            (not (string? (:content m))))
                      m
                      (update m :content clip-middle cap-chars note-fn)))
                  messages))
        after (reduce + 0 (map #(content-chars (:content %)) out))]
    {:messages out
     :freed (long (Math/ceil (/ (double (max 0 (- before after)))
                                (double (max 1 chars-per-token)))))}))

;; --- pruning tool output ------------------------------------------------------

(defn summarize-result
  "One line standing in for a tool result: what the tool was, and enough shape
  to decide whether it is worth reopening.

  Keyed by TOOL, because the useful line differs: a shell result's first line
  is the command, a grep's shape is its match count, and a file read's is its
  size. The generic arm keeps a bounded preview. `templates` is the prose,
  from a prompt — these sentences are read by the model on every folded turn."
  [tool content templates]
  (let [text (str content)
        lines (count (str/split-lines text))
        chars (count text)
        first-line (str/trim (or (first (str/split-lines text)) ""))
        clip (fn [s n] (if (> (count s) n) (str (subs s 0 n) "…") s))
        ctx {:tool tool :lines lines :chars chars
             :first-line (clip first-line (:preview-chars templates))
             :preview (clip (str/replace text #"\s+" " ") (:preview-chars templates))}]
    ((:render templates) (get (:by-tool templates) tool (:default templates)) ctx)))

(defn prune-tool-outputs
  "Replace the BODY of older tool results with one line each, keeping the call
  itself and the protected tail whole.

  A different act from folding, and cheaper. A fold replaces a whole region of
  history with a summary of it; this keeps every turn exactly where it is and
  removes only the bulk — which is overwhelmingly tool output, the least
  re-read and most voluminous thing in a transcript. The shape of what the
  branch did survives; what it saw is one `fetch_turn` away.

  Only results over `min-chars`: summarising a short result costs more than it
  saves and loses something the model can read at a glance."
  [messages {:keys [protect-tail min-chars roles tool-of templates]}]
  (let [n (count messages)
        cutoff (max 0 (- n protect-tail))]
    (vec (map-indexed
          (fn [i m]
            (if (or (>= i cutoff)
                    (not (contains? roles (str (:role m))))
                    (<= (content-chars (:content m)) min-chars))
              m
              (assoc m :content (summarize-result (tool-of m) (:content m) templates)
                     :pruned? true)))
          messages))))

;; --- choosing the window to fold ---------------------------------------------

(defn- user-msg? [m] (= "user" (str (:role m))))

(defn- snap-forward [messages idx]
  (let [n (count messages)]
    (loop [i (min idx n)]
      (cond (>= i n) n
            (user-msg? (nth messages i)) i
            :else (recur (inc i))))))

(defn- snap-backward [messages idx]
  (loop [i (min idx (dec (count messages)))]
    (cond (neg? i) 0
          (user-msg? (nth messages i)) i
          :else (recur (dec i)))))

(defn compress-window
  "The `[start end)` index range to fold, or `[0 0]` when there is nothing
  worth folding.

  Two protections and one alignment. The HEAD is protected because the system
  prompt and the opening statement of the problem are what the branch is for;
  folding them leaves it working on a summary of its own instructions. The
  TAIL is protected because recent turns are the ones still in play.

  Both cuts SNAP TO USER-MESSAGE BOUNDARIES, and that is a correctness
  requirement rather than tidiness: a user message never carries a dangling
  tool call and is never itself an orphaned tool result, so cutting there
  removes whole turns. Cutting mid-turn leaves half of a call/result pair
  behind, which some providers reject outright. The head snaps forward and the
  tail backward, so both only ever protect more."
  [messages protect-head protect-tail]
  (let [n (count messages)]
    (if (< n (+ protect-head protect-tail 1))
      [0 0]
      (let [raw-start protect-head
            raw-end (max 0 (- n protect-tail))]
        (if (>= raw-start raw-end)
          [0 0]
          (let [start (snap-forward messages raw-start)
                end (snap-backward messages raw-end)]
            (if (>= start end) [0 0] [start end])))))))

(defn summary-budget
  "How many tokens the summary of a folded region may spend: a fraction of
  what is being folded, floored and capped.

  The floor exists because a small fold still has to carry its load-bearing
  facts, and the ceiling because a summary approaching the size of what it
  replaces has not compacted anything."
  [folded-tokens {:keys [ratio floor ceiling]}]
  (-> (long (* ratio folded-tokens))
      (max floor)
      (min ceiling)))

;; --- is that actually a summary? ---------------------------------------------

(defn- placeholder?
  "A section body that says nothing — what a model emits when it has no
  material, or is not really summarising."
  [line empties]
  (contains? empties (-> (str line) str/trim (str/replace #"\.+$" "") str/trim str/lower-case)))

(defn validate-summary
  "Whether `summary` is structurally a summary rather than a stub.

  THE CALLER ACTS ON TRUE BY DROPPING HISTORY, so a false positive costs real
  turns permanently. The test is whether at least `min-sections` of the
  template's own `## ` headings carry a non-placeholder body.

  Counting POPULATED sections — not headings, not length — is what makes this
  both safe and permissive. `## Active Task` followed by `None.` is rejected
  however many empty headings accompany it, while a terse but real summary
  with one-line sections passes. That permissiveness matters: rejecting a
  usable summary forces prune-only folding, which walks the branch into the
  overflow the fold existed to prevent.

  Anchoring on `## ` also means prose that merely uses the word `Goal` is not
  mistaken for a summary."
  [summary {:keys [sections min-sections empties]}]
  (if (str/blank? (str summary))
    false
    (let [known (set sections)]
      (loop [lines (str/split-lines (str summary))
             in? false body? false populated 0]
        (if-let [line (first lines)]
          (let [t (str/trim line)]
            (if-let [head (second (re-find #"^##\s+(.*)$" t))]
              (recur (rest lines)
                     (contains? known (str/trim head))
                     false
                     (cond-> populated (and in? body?) inc))
              (recur (rest lines) in?
                     (or body? (and in? (not (placeholder? t empties))))
                     populated)))
          (>= (cond-> populated (and in? body?) inc) min-sections))))))

;; --- applying it -------------------------------------------------------------

(defn find-previous-summary
  "Index and body of the most recent fold marker, or nil. A second fold
  summarises the previous summary along with what came after it, so the marker
  has to be findable."
  [messages marker]
  (->> (map-indexed vector messages)
       reverse
       (some (fn [[i m]]
               (when (and (= "system" (str (:role m)))
                          (str/starts-with? (str (:content m)) marker))
                 [i (subs (str (:content m)) (count marker))])))))

(defn apply-summary
  "The messages with `[start end)` replaced by one system message carrying the
  summary.

  The protected head keeps its place, the summary stands where the folded
  region was, and the tail follows — so the conversation's shape is unchanged
  and the summary sits in chronological position rather than being prepended
  as a preamble to everything."
  [messages [start end] marker summary]
  (if (>= start end)
    messages
    (vec (concat (take start messages)
                 [{:role "system" :content (str marker summary) :compaction? true}]
                 (drop end messages)))))
