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

(ns samizdat.tui.widgets
  "The widgets the layout arranges.

  Each is `(fn [state props] -> hiccup)` and registers itself under a
  `:widget/*` tag. Two rules hold the whole namespace together:

  PURE FUNCTIONS OF STATE. A widget reads the view state and returns hiccup.
  It never polls, never writes, and never reaches for a connection. What it
  can DO arrives in the state as `:on` handlers, so a test drives a click by
  passing a recording function and the suite covers every interaction with
  no terminal and no server.

  DRAW SOMETHING FROM NOTHING. The first frame — before any poll has
  answered — is the state every user sees, and the layout that decides which
  widgets exist is a file the agent edits at runtime. So every widget renders
  an element for an empty state and for the half-loaded one between them.
  There is a test that walks the registry and holds this for all of them.

  Toolkit-free: hiccup is data. `ftxui` appears nowhere in this file."
  (:require [clojure.string :as str]
            [samizdat.tui.layout :as layout]
            [samizdat.tui.state :as st]
            [samizdat.tui.commands :as cmd]
            [samizdat.tui.timeline :as tl]))

;; --- shared shapes -----------------------------------------------------------

(defn- clip
  "`s` on one line, no longer than `n`. Panels are narrow and a tool result
  carries newlines."
  [s n]
  (let [one (str/join " " (str/split (str s) #"\s+"))]
    (if (> (count one) n) (str (subs one 0 (max 0 (dec n))) "…") one)))

(defn- panel
  "A titled box. Every side panel is one, so a layout that puts two of them
  in a column gets borders that line up without saying so.

  `:height` is forwarded like `:width` because a column of panels is a
  competition for rows: without a ceiling on the ones that do not need many,
  the flexed panel below them is the one that gets squeezed off the screen."
  [props & children]
  (into [:vbox (cond-> {:class (into [:panel] (let [c (:class props)] (if (keyword? c) [c] c)))
                        :border :rounded}
                 ;; The VALUE, not a coerced true: ftxui's flex kinds are
                 ;; `:grow` (may take spare room, may not be squeezed) as
                 ;; well as plain `true` (both), and flattening them cost a
                 ;; layout the distinction that keeps a panel on screen.
                 (:flex props) (assoc :flex (:flex props))
                 (:width props) (assoc :width (:width props))
                 (:height props) (assoc :height (:height props))
                 ;; Inline style from the layout, over the theme's :panel.
                 (:style props) (assoc :style (:style props)))]
        (cond-> []
          (:title props) (conj [:text {:class :panel-title} (str " " (:title props) " ")]
                               [:separator {:class :muted}])
          :always (into (remove nil? children)))))

(defn- empty-note
  "What a panel says when it has nothing. Dim rather than blank: a panel that
  drew nothing is indistinguishable from one that failed to draw."
  [s]
  [:text {:class :dim} (str "  " s)])

(defn fmt-tokens
  "A token count short enough for a footer: 9475 -> \"9k\", 1250000 -> \"1.2M\".
  Ported from dirge's status line, which learned that the exact figure is
  noise at a glance and the magnitude is the signal."
  [n]
  (let [n (or n 0)]
    (cond
      (>= n 1000000) (format "%.1fM" (double (/ n 1000000)))
      (>= n 1000) (str (quot n 1000) "k")
      :else (str n))))

(defn project-label
  "`project:branch`, or just the project when there is no branch to name — a
  detached HEAD, or a harness outside a git tree. Never a dangling separator."
  [{:keys [project branch]}]
  (let [p (str (or project ""))]
    (cond
      (str/blank? p) nil
      (not-empty (str branch)) (str p ":" branch)
      :else p)))

(defn fold-id
  "The identity of one foldable section of a turn, stable across frames: the
  fold samizdat.tui.timeline gives that turn's :thinking or :result.

  Keyed by the turn and the section, never by a position in a list — the
  conversation grows, and an id that moved would shut every open fold on the
  turn after it was opened."
  [turn section]
  (case section
    :thinking (str "t" turn "/thinking")
    (str "t" turn "/tool/more")))

(defn- toggle-fn
  "The click handler for a fold. Absent handlers are tolerated: a widget
  rendered by a test, or in a frame before the loop wired itself, still
  draws."
  [state id]
  (fn [_open?] (when-let [f (get-in state [:on :toggle])] (f id))))

(defn- fold
  "A collapsible section: header always visible, body a click away.

  ftxui's own `:collapsible`, so the mouse works on it for free — this is
  the widget FTXUI already opens and closes on a click, and wiring our own
  hit-testing over a `:text` would be re-implementing it worse.

  `body-fn` is a THUNK, and it is called only when the fold is open. Passing
  the body itself built every tool result on the branch into one element per
  line on every frame whether or not anyone could see it: 175ms a frame at
  two hundred turns with every fold shut, and ftxui redraws on a keystroke.
  A shut fold is not on screen and its body does not get built."
  [state id label body-fn]
  (let [open? (contains? (set (:expanded state)) id)]
    [:collapsible {:label label
                   :show open?
                   :on-change (toggle-fn state id)}
     (if open? (body-fn) [:empty])]))

;; --- the activity log --------------------------------------------------------

(defn- step-line [{:keys [node cell transition ms failed]}]
  [:hbox
   [:text {:class (if failed :step-failed :step)} (if failed " ✗ " " · ")]
   [:text {:bold true} (clip node 12)]
   [:text {:class :dim} (str " " (clip (or cell "") 16)
                           (when transition (str " →" (clip transition 8)))
                           (when ms (str " " ms "ms")))]])

(defn activity
  "The ACTIVITY LOG: the manifest states the agent is walking.

  This is the one deliberate departure from the UI it was ported from.
  dirge's activity panel is a ticker of tool calls; samizdat's workflow is a
  state machine written in a file the agent can edit, so what an operator
  wants to watch is the graph being walked — :start, :measure, :infer,
  :parse, :dispatch, :journal, :settle, :arbiter, :route — with the cell
  behind each state, because the cell is the thing they would edit.

  Newest LAST, so it grows downward toward the compose box the way the
  conversation does."
  [state props]
  (let [trace (vec (:trace state))
        dropped (:trace-dropped state)]
    (panel props
           (when (and dropped (pos? dropped))
             [:text {:class :warn} (str "  ⚠ " dropped " step(s) dropped")])
           (if (seq trace)
             (into [:vbox {:flex true}] (map step-line trace))
             (empty-note "no steps yet")))))

;; --- the conversation --------------------------------------------------------

(defn- diff-line
  "One line of a write result, coloured when it reads as a diff. How a reader
  tells an edit that landed from one that replaced the wrong thing."
  [line]
  (cond
    (str/starts-with? line "+") [:text {:class :diff-add} line]
    (str/starts-with? line "-") [:text {:class :diff-del} line]
    (str/starts-with? line "@@") [:text {:class :diff-hunk} line]
    :else [:text line]))

(defn- request-cost
  "`  ctx 43k · hit 93%`, and what busted the cache when something did: the
  tool a native tool_choice forced, or a history rewritten behind the tail.
  The conversation is where a reader follows a run turn by turn, so it is
  where a cache that stopped serving should show at the turn it happened
  (karamazov-pdes). nil for a turn that carried no usage — a replay, a
  provider error — rather than a row of zeros."
  [{:keys [prompt_tokens cache_hit_tokens prefix_change forced_tool]}]
  (when (number? prompt_tokens)
    (str "  ctx " (fmt-tokens prompt_tokens)
         (when (and (number? cache_hit_tokens) (pos? prompt_tokens))
           (str " · hit " (quot (* 100 cache_hit_tokens) prompt_tokens) "%"))
         (when (not-empty (str forced_tool)) (str " · forced " forced_tool))
         (when (= "rewritten" (str prefix_change)) " · rewritten"))))

(def default-turns-shown
  "How many turns the conversation draws when the layout does not say.

  A default, not a policy: `:turns` in the layout is the number that counts,
  and this is what a layout written before the prop existed gets."
  60)

(def ^:private writing-tools #{"write_file" "edit_file" "patch"})

(defn- say-entry
  "A line in a role's voice, dirge-style: `<agent> ` and the words wrapped
  under it with a hanging indent."
  [{:keys [role text pending?]} handles]
  [:hbox
   [:text {:class role} (str "<" (get handles role (name role)) "> ")]
   [:paragraph {:class role :flex true}
    (str text (when pending? "  (not applied yet)"))]])

(defn- thinking-entry [state e]
  (fold state (tl/fold-id e)
        (str "◇ thinking (" (count (:text e)) " chars)")
        #(vector :paragraph {:class :thinking} (:text e))))

(defn- tool-entry
  "A tool call as a CHAMBER, dirge's word: a box headed by the tool and the
  one argument it is known by, the first lines of what came back, and the
  rest a click (or Ctrl+O) away."
  [state {:keys [tool arg result failed? turn turn-row] :as e} {:keys [result-lines]}]
  (let [lines (str/split-lines (str result))
        shown (take (or result-lines 4) lines)
        more (- (count lines) (count shown))
        line-of (if (contains? writing-tools tool) diff-line (fn [l] [:text {:class :result} l]))]
    (into [:vbox {:class (if failed? [:tool-box :error-box] :tool-box) :border :rounded}
           (cond-> [:hbox
                    [:text {:class (if failed? :error :tool-name)}
                     (str (if failed? "✗ " "") (str/upper-case tool))]]
             arg (conj [:text {:class :tool} (str " ─ \"" (clip arg 60) "\"")])
             :always (conj [:filler]
                           [:text {:class :dim}
                            (str "turn " turn (or (request-cost turn-row) "") " ")]))]
          (cond-> (if (str/blank? (str result))
                    [[:text {:class :dim} "(no output)"]]
                    (mapv line-of shown))
            (pos? more)
            (conj (fold state (tl/fold-id e) (str "↓ " more " more lines")
                        #(into [:vbox] (map line-of (drop (count shown) lines)))))))))

(defn- shown-entries
  "The entries for the newest `n` turns — and everything said between them."
  [es n]
  (let [turns (distinct (keep :turn es))
        keep-from (when (> (count turns) n) (nth turns (- (count turns) n)))]
    (if keep-from
      (let [start (first (keep-indexed (fn [i e] (when (= keep-from (:turn e)) i)) es))]
        (subvec es start))
      es)))

(defn conversation
  "The branch's story: the problem, what the agent said and called, and what
  the person, the critic and the supervisor said to it — each in its own
  voice (samizdat.tui.timeline), in the order it happened.

  A tool call is a chamber showing the first lines of its result; the rest,
  and the model's thinking, fold, closed by default.

  FOLLOWS THE BOTTOM. The newest entry holds the frame's focus, so the pane
  scrolls as the run speaks; scrolling up anchors it on an entry and it
  stays there as more arrive, until End (or scrolling back down) follows
  again.

  BOUNDED: `:turns` in the layout, the newest that many."
  [state props]
  (let [cfg (get-in state [:settings :conversation])
        es (shown-entries (tl/entries state cfg) (or (:turns props) default-turns-shown))
        focus (or (:scroll-anchor state) (:key (peek es)))
        handles (:handles cfg)]
    (panel (assoc props :title (or (:title props)
                                   (some->> (:branch-id state) (str "BRANCH "))))
           (if (seq es)
             (into [:vbox {:flex true :frame :y :scroll-indicator :v}]
                   (for [e es]
                     [:vbox (cond-> {:key (:key e)} (= focus (:key e)) (assoc :focus true))
                      (case (:kind e)
                        :say (say-entry e handles)
                        :thinking (thinking-entry state e)
                        :tool (tool-entry state e cfg))]))
             (empty-note "no turns yet — pick a run")))))

;; --- the side panels ---------------------------------------------------------

(def ^:private task-glyph
  {"in_progress" "▶" "open" "○" "blocked" "⊘" "done" "✓"})

(defn tasks
  "The board: what is open, what is being worked on, what is blocked."
  [state props]
  (let [board (vec (get-in state [:detail :tasks]))]
    (panel props
           (if (seq board)
             (into [:vbox {:flex true :frame :y}]
                   (for [{:keys [title status]} board]
                     [:hbox
                      [:text {:class (if (= "in_progress" status) :selected :result)}
                       (str " " (get task-glyph (str status) "·") " ")]
                      [:text (clip title 26)]
                      [:text {:class :dim} (str " " status)]]))
             (empty-note "board is empty")))))

(defn files
  "What this run has changed, most recently first, naming every branch that
  touched a path — which is the question a beam raises."
  [state props]
  (let [mods (vec (get-in state [:detail :modified]))]
    (panel props
           (if (seq mods)
             (into [:vbox {:flex true :frame :y}]
                   (for [{:keys [path turn branches]} mods]
                     [:hbox
                      [:text " "]
                      [:text (clip path 24)]
                      [:text {:class :dim} (str " t" turn
                                              (when (seq branches)
                                                (str " " (str/join "," branches))))]]))
             (empty-note "nothing written yet")))))

(defn- ratio [used total]
  (if (and (number? used) (number? total) (pos? total))
    (double (min 1.0 (/ used total)))
    0.0))

(def ^:private fold-warn-fraction
  "Where the footer starts flagging a fold, as a fraction of the window.

  dirge's numbers and its reasoning: the denominator is the WINDOW, so the
  gauge reads 0-100 rather than running past 100 once usage passes the
  fold-trigger budget — a fold is flagged by a marker instead (dirge-l4rp,
  dirge-cx7t). samizdat's own ladder rungs are gates.edn :compaction; these
  two are the front end's warning line, not the policy, which is why they are
  not read from there."
  0.75)

(def ^:private fold-urgent-fraction 0.90)

(defn- fill-segment
  "`used / window (pct%)` with a fold marker as the window fills, or nil when
  there is no window to measure against."
  [used window]
  (when (and window (pos? window))
    (let [pct (quot (* 100 (or used 0)) window)
          frac (/ (double (or used 0)) window)]
      (str (fmt-tokens used) " / " (fmt-tokens window) " (" pct "%"
           (cond (>= frac fold-urgent-fraction) " fold!"
                 (>= frac fold-warn-fraction) " fold"
                 :else "")
           ")"))))

(defn- selected-branch
  "The branch row the conversation is showing, or nil when none is chosen or
  the detail has not landed."
  [state]
  (when-let [id (:branch-id state)]
    (some #(when (= (str (:id %)) (str id)) %)
          (get-in state [:detail :branches]))))

(defn- branch-fill
  "The prompt tokens of the selected branch's newest measured request — how
  full its context is now — or nil. NOT the run's total: on a beam of five
  that is every branch's every turn summed, and a gauge fed that read
  \"fold!\" a handful of turns into any run and never stopped
  (karamazov-pdes)."
  [state]
  (get-in (selected-branch state) [:context :prompt-tokens]))

(defn- cache-miss-clause
  "` · 33 low: forced 19, rewritten 3` — how many turns the cache failed
  and why, biggest cause first — or nothing when no turn missed."
  [{:keys [low by-cause]}]
  (when (and (number? low) (pos? low))
    (str " · " low " low"
         (when (seq by-cause)
           (str ": " (str/join ", " (map (fn [[k n]] (str (name k) " " n))
                                          (sort-by (comp - val) by-cause))))))))

(defn context
  "What the run is spending: tokens against its budget, turns against its
  ceiling. Gauges rather than numbers alone — a ratio is not a number to
  read."
  [state props]
  (let [run (get-in state [:detail :run])
        ;; KEBAB keys: run-usage builds a Clojure map and the JSON writer
        ;; emits the keywords it was handed, so the wire carries
        ;; :total-tokens. The snake spelling read every run as having spent
        ;; nothing.
        used (get-in run [:usage :total-tokens])
        budget (:token_budget run)
        turns (get-in run [:usage :turns])
        max-turns (:max_turns run)
        ;; The window and the cache (karamazov-pdes): the selected branch's
        ;; last request against the model's context window, and the run's
        ;; hit rate with the turns that missed, by cause. Each line draws
        ;; only when its number was measured — nil is unknown, not zero.
        window (get-in state [:project :context_window])
        fill (branch-fill state)
        rate (get-in run [:usage :cache-hit-rate])
        misses (get-in run [:usage :cache-misses])
        block (get-in run [:usage :context-block :avg-total])
        fill? (and (number? fill) (number? window) (pos? window))]
    (panel props
           (into [:vbox
                  [:text {:class :dim} (str "  tokens " (or used 0)
                                          (when budget (str " / " budget)))]
                  [:gauge {:value (ratio used budget)
                           :class (if (> (ratio used budget) 0.85) :gauge-hot :gauge)}]
                  [:text {:class :dim} (str "  turns  " (or turns 0)
                                          (when max-turns (str " / " max-turns)))]
                  [:gauge {:value (ratio turns max-turns) :class :gauge}]]
                 (remove nil?
                         [(when fill?
                            [:text {:class :dim} (str "  ctx    " (fill-segment fill window))])
                          (when fill?
                            [:gauge {:value (ratio fill window)
                                     :class (if (>= (ratio fill window) fold-warn-fraction)
                                              :gauge-hot :gauge)}])
                          (when (number? rate)
                            [:text {:class :dim}
                             (str "  cache  " (Math/round (* 100.0 rate)) "% hit"
                                  (cache-miss-clause misses))])
                          ;; What the harness adds to each request itself —
                          ;; the ledger, the memories, the task — averaged
                          ;; over the run, so a block that has grown to a
                          ;; third of every request is a number and not a
                          ;; feeling.
                          (when (number? block)
                            [:text {:class :dim}
                             (str "  block  " block " chars/turn added by the harness")])])))))

(defn gates
  "Which gates have fired, and how many of their predictions are still open —
  the run's own account of advice it was given and has not yet answered."
  [state props]
  (let [gs (vec (get-in state [:detail :gates]))]
    (panel props
           (if (seq gs)
             (into [:vbox {:frame :y}]
                   (for [{:keys [gate fired open]} gs]
                     [:hbox
                      [:text " "]
                      [:text (clip gate 18)]
                      [:text {:class :dim} (str "  fired " fired)]
                      [:text {:class (if (pos? (or open 0)) :warn :dim)}
                       (str "  open " (or open 0))]]))
             (empty-note "no gates fired")))))

(def ^:private claim-class
  {"confirmed" :claim-confirmed "refuted" :claim-refuted
   "ambiguous" :claim-ambiguous "existential" :claim-existential})

(defn artifacts
  "The claims the run has made and how each was judged. The harness is
  claim-first, so this is its actual output — the panel that says whether
  any of the work is standing up."
  [state props]
  (let [as (vec (get-in state [:detail :artifacts]))]
    (panel props
           (if (seq as)
             (into [:vbox {:frame :y}]
                   (for [{:keys [claim claim_status]} as]
                     [:hbox
                      [:text {:class (get claim-class (str claim_status) :dim)}
                       (str " " (clip (str claim_status) 11) " ")]
                      [:text (clip claim 20)]]))
             (empty-note "no claims yet")))))

(defn runs
  "The run picker."
  [state props]
  (let [rs (vec (:runs state))]
    (panel props
           (if (seq rs)
             [:menu {:entries (mapv #(clip (str (:id %) "  " (:problem %)) 40) rs)
                     :selected (max 0 (.indexOf (mapv :id rs) (:run-id state)))
                     :on-enter (fn [i]
                                 (when-let [f (get-in state [:on :select-run])]
                                   (f (:id (nth rs i nil)))))}]
             (empty-note "no runs")))))

(defn branches
  "The beam: every branch on this run and what became of it.

  A run here is a BEAM, so the branch being read is one of several and which
  one is a choice. Without this the auto-picked branch was the only one
  reachable — the state fold could switch and nothing on screen asked it to."
  [state props]
  (let [bs (vec (get-in state [:detail :branches]))]
    (panel props
           (if (seq bs)
             [:menu {:entries (mapv #(clip (str (:id %) " " (:status %) "  "
                                                (get-in % [:thesis :claim]))
                                           32)
                                    bs)
                     :selected (max 0 (.indexOf (mapv :id bs) (:branch-id state)))
                     :on-enter (fn [i]
                                 (when-let [f (get-in state [:on :select-branch])]
                                   (f (:id (nth bs i nil)))))}]
             (empty-note "no branches")))))

;; --- the modals --------------------------------------------------------------

(defn- lines-of
  "A blob as its own lines, uncut. Used only by the approval dialog, which is
  the one place in this UI where clipping is the wrong answer."
  [s]
  (into [:vbox] (map (fn [l] [:text l]) (str/split-lines (str s)))))

(defn- permission-dialog
  "dirge's permission prompt: what wants to run, why it was stopped, and the
  answers — y allow once, a allow always (this session), n deny, d deny and
  say what to do instead, Esc abort."
  [state {:keys [id kind input reason details always]}]
  (let [decide (fn [d] (fn [] (when-let [f (get-in state [:on :decide])] (f id d))))
        reply (fn [] (when-let [f (get-in state [:on :reply])] (f :deny-note id)))
        noting? (= {:kind :deny-note :id id} (:reply state))]
    [:vbox {:class [:perm :perm-box]}
     [:text {:class :perm :bold true} " ⚠ PERMISSION REQUIRED "]
     [:separator]
     [:text {:class :dim} (str " tool: " kind)]
     ;; NOT clipped, unlike every other panel. A person cannot judge
     ;; `rm -rf "$BUILD"/*` from its first three characters, and a dialog
     ;; that hid the rest would be asking them to approve something they
     ;; were not shown.
     (lines-of input)
     (when (not-empty (str reason)) [:text {:class :dim} (str " why: " reason)])
     (when (not-empty (str details)) [:text {:class :dim} (str " " details)])
     (when always [:text {:class :dim} (str " always would allow: " always ", for this session")])
     [:separator]
     (if noting?
       [:text {:class :perm}
        " denying — type what to do instead below (Enter sends · Esc goes back)"]
       (cond-> [:hbox
                [:button {:label "allow once (y)" :style :ascii
                          :on-click (decide {:decision :allow})}]]
         always (conj [:button {:label "allow always (a)" :style :ascii
                                :on-click (decide {:decision :allow :always true})}])
         :always (conj [:button {:label "deny (n)" :style :ascii
                                 :on-click (decide {:decision :deny})}]
                       [:button {:label "deny + note (d)" :style :ascii :on-click reply}])))]))

(defn- questionnaire
  "ask_human's questions, one at a time: pick an option (tick several when
  the question allows it), or write your own answer, or reject the lot."
  [state {:keys [id questions]}]
  (let [qs (vec questions)
        i (min (or (:question-cursor state) 0) (max 0 (dec (count qs))))
        q (nth qs i nil)
        so-far (vec (:question-answers state))
        opts (vec (:options q))
        sel (set (:question-selected state))
        ;; Carry what has already been answered. Handing back only the
        ;; latest would lose every earlier answer on the way to the last
        ;; question.
        answer (fn [a] (when-let [f (get-in state [:on :answer])]
                         (f id i (conj so-far a))))
        custom (fn [] (when-let [f (get-in state [:on :reply])] (f :custom-answer id)))
        reject (fn [] (when-let [f (get-in state [:on :decide])]
                        (f id {:decision :deny :note "rejected"})))
        writing? (= {:kind :custom-answer :id id} (:reply state))]
    [:vbox {:class [:question :question-box]}
     [:text {:class :question :bold true}
      (str " ? QUESTION " (inc i) " of " (count qs) (when (:multi q) " — pick any") " ")]
     [:separator]
     [:paragraph (str (:question q))]
     (cond
       writing?
       [:text {:class :question} " your answer — type it below (Enter sends · Esc goes back)"]

       (and (seq opts) (:multi q))
       (into [:vbox]
             (concat
              (map-indexed (fn [n o]
                             [:checkbox {:label (str o) :checked (contains? sel n)
                                         :on-change (fn [_] (when-let [f (get-in state [:on :toggle-option])]
                                                              (f n)))}])
                           opts)
              [[:button {:label "confirm" :style :ascii
                         :on-click (fn [] (when (seq sel) (answer (mapv #(str (nth opts %)) (sort sel)))))}]]))

       (seq opts)
       [:menu {:entries (mapv str opts)
               :on-enter (fn [n] (answer (str (nth opts n nil))))}]

       ;; No options is not a malformed question — `ask_human` takes a bare
       ;; string and an open-ended question is the ordinary use of a tool by
       ;; that name. Drawn as an empty menu it could not be answered AT ALL,
       ;; and the branch parked until the deadline for want of a text box.
       :else
       [:input {:placeholder "type an answer — Enter sends"
                :on-enter (fn [a] (answer (str a)))}])
     (when-not writing?
       [:hbox
        (when (seq opts) [:button {:label "your own answer" :style :ascii :on-click custom}])
        [:button {:label "reject (Esc)" :style :ascii :on-click reject}]])]))

(defn approvals
  "Questions waiting on a person: the permission gate and ask_human.

  One at a time. A wall of every pending question at once is how somebody
  answers the second one thinking it was the first — and with the branch
  parked on an answer, a mis-click is not a cosmetic mistake.

  Draws nothing when there is nothing pending: it sits in the layout
  permanently, so an idle run must not carry an empty box, and there must be
  no button to hit by accident."
  [state props]
  (let [pending (vec (:approvals state))
        a (first pending)]
    (cond
      (nil? a) [:empty]
      (seq (:questions a)) (questionnaire state a)
      :else (permission-dialog state a))))

(defn git
  "The working tree at a glance: branch, what is dirty, and the last commit.

  Ported from dirge's left-panel GIT box. The three counts are kept separate
  because git keeps them separate — a path can be staged AND edited again
  since, so one \"dirty\" number would hide the case a reader most needs to
  see before shipping.

  Everything comes from GET /v1/harness/project: this process reads no git of
  its own, so a TUI pointed at a harness on another machine shows THAT
  machine's tree rather than confidently mislabelling its own."
  [state props]
  (let [{:keys [branch staged unstaged untracked last_commit] :as p} (:project state)
        counts (mapv #(or % 0) [staged unstaged untracked])
        dirty (reduce + counts)]
    (panel props
           (cond
             (nil? p) (empty-note "no harness yet")
             (nil? branch) (empty-note (if (some? staged)
                                         "detached HEAD"
                                         "not a git working tree"))
             :else
             [:vbox
              [:text {:class :git-branch} (str " \u2387 " (clip branch 24))]
              (if (zero? dirty)
                [:text {:class :dim} "  clean"]
                [:hbox
                 [:text {:class (if (pos? (nth counts 0)) :ok :dim)}
                  (str "  +" (nth counts 0))]
                 [:text {:class (if (pos? (nth counts 1)) :warn :dim)}
                  (str " ~" (nth counts 1))]
                 [:text {:class (if (pos? (nth counts 2)) :warn :dim)}
                  (str " ?" (nth counts 2))]])
              (when (not-empty (str last_commit))
                [:text {:class :dim} (str "  " (clip last_commit 30))])]))))

;; --- the bottom strip --------------------------------------------------------

(defn input
  "The compose box, and the three things done to a run rather than said to it.

  ONE BOX, TWO MEANINGS, and `state/enter-action` decides which. With a run
  selected what it sends is an INTERVENTION, not a chat message — samizdat
  runs are autonomous and a person steers them at a turn boundary, so that is
  the same seam the supervisor uses. With no run selected there is nothing to
  steer, so the words are the problem statement for a new run: the TUI could
  reach every run the server already had and could not make one, which made
  the first thing anybody typed into a fresh harness an error message.

  `start` is a button as well as a key, because with a run already selected
  Enter belongs to steering and starting another needs somewhere to click.
  Abort and resume sit beside it because they are the rest of what is done
  to a run rather than said to it.

  NO TITLE unless a layout asks for one. A bordered panel spent two rows
  captioning a single input line whose own placeholder already says what it
  takes; the caption is userspace like the rest of the arrangement, so
  `{:title \"STEER\"}` brings it back."
  [state props]
  (let [starting? (= :start (st/enter-action state))
        ;; Spelled out rather than built by a helper taking the key as an
        ;; argument: `every-handler-the-loop-offers-has-a-caller` reads these
        ;; literally, and a key assembled at runtime is a key that ratchet
        ;; cannot see. That is also why the branch is over two whole handlers
        ;; rather than over one name.
        on-enter (if starting?
                   (fn [s] (when-let [f (get-in state [:on :start])] (f s)))
                   (fn [s] (when-let [f (get-in state [:on :submit])] (f s))))
        ;; The layout's own sizing still applies to the bare row — it is the
        ;; element that stands where the panel used to.
        row [:hbox (select-keys props [:flex :width :height])
             [:input {:flex true
                      :value (or (:input state) "")
                      :placeholder (case (:kind (:reply state))
                                     :deny-note "what the agent should do instead — Enter sends"
                                     :custom-answer "your answer — Enter sends"
                                     (if starting?
                                       "the problem to work on — Enter starts a run, /help for commands"
                                       "a directive for the run — Enter sends, /help for commands"))
                      :on-change (fn [s] (when-let [f (get-in state [:on :input])] (f s)))
                      :on-enter on-enter}]
             [:button {:label "start" :style :ascii
                       :on-click (fn [] (when-let [f (get-in state [:on :start])]
                                          (f (or (:input state) ""))))}]
             [:button {:label "abort" :style :ascii
                       :on-click (fn [] (when-let [f (get-in state [:on :abort])] (f)))}]
             [:button {:label "resume" :style :ascii
                       :on-click (fn [] (when-let [f (get-in state [:on :resume])] (f)))}]]]
    (cond
      (:title props) (panel props row)
      ;; A border and nothing else: the one-row box beside the avatar.
      (:boxed props) [:hbox (merge {:class :panel :border :rounded}
                                   (select-keys props [:flex :width :style]))
                      (assoc row 1 {:flex true})]
      :else row)))

(defn status
  "The footer: where the harness is pointed, what is answering, what the run
  has spent, and what it is doing.

  Ported from dirge's status line, which reads
  `project:branch | model | used/ctx (pct%) | Nmsgs | state`. Samizdat's said
  only the run id and the base URL — nothing about WHICH project or WHICH
  model, which are the two things that say whether the harness is pointed
  where the reader thinks it is. A stale serve process answering on the
  expected port is exactly the confusion these segments remove.

  Every segment is optional and the strip draws with none of them: the first
  frame has no project, no run and no connection. The connection dot stays
  leftmost and is samizdat's own addition — dirge's UI is the process doing
  the work, where this one is a client that can be pointed anywhere and has
  to say when it has lost the thing it is watching."
  [state props]
  (let [run (get-in state [:detail :run])
        p (:project state)
        model (or (:model run) (:model p))
        turns (get-in run [:usage :turns])
        ;; The branch's last request, not the run's total (karamazov-pdes,
        ;; see branch-fill); no measured request, no segment.
        fill (when-let [used (branch-fill state)]
               (fill-segment used (:context_window p)))
        sep [:text {:class :dim} " \u2502 "]]
    (into [:hbox {:class :status}]
          (remove nil?
                  [(if (:connected? state)
                     [:text {:class :status-live} " \u25cf connected "]
                     [:text {:class :status-down} " \u25cb offline "])
                   (when-let [l (project-label p)]
                     [:text {:bold true} (str " " (clip l 34))])
                   (when model sep)
                   (when model [:text {:class :model} (clip (str model) 20)])
                   (when fill sep)
                   (when fill [:text {:class :dim} fill])
                   (when turns sep)
                   (when turns [:text {:class :dim}
                                (str turns (when-let [m (:max_turns run)]
                                             (str " / " m))
                                     " turns")])
                   (when run sep)
                   (when run [:text (str (:status run))])
                   ;; The approval mode: what happens when the run needs a
                   ;; person (dirge shows its permission mode the same way).
                   (when (:approval_mode p) sep)
                   (when-let [m (:approval_mode p)]
                     [:text {:class (if (= "block" (str m)) :perm :dim)} (str "mode:" m)])
                   ;; Pushed or polled: whether the run on screen is live.
                   (when (:run-id state) sep)
                   (when (:run-id state)
                     [:text {:class (if (:live? state) :status-live :dim)}
                      (if (:live? state) "live" "polling")])
                   sep
                   [:text {:class :dim} (if-let [r (:run-id state)]
                                        (subs (str r) 0 (min 8 (count (str r))))
                                        "no run")]
                   (when-let [e (:error state)]
                     [:text {:class :error} (str " " (clip e 40) " ")])
                   ;; Cyan, not red, and only when there is no error to report
                   ;; instead: a notice says what is happening ("starting…"),
                   ;; and a strip carrying both at once does not tell the
                   ;; reader which of them is the news.
                   (when-let [n (and (not (:error state)) (:notice state))]
                     [:text {:class :info} (str " " (clip n 40) " ")])
                   (when-let [e (:layout-error state)]
                     [:text {:class :warn} (str " " (clip e 60) " ")])
                   [:filler]
                   ;; Scheme stripped: "http://" is seven columns that say
                   ;; nothing, and with the project, model, fill and turn
                   ;; segments now on this line the strip overran an
                   ;; ordinary terminal and clipped the run id into the URL.
                   [:text {:class :dim}
                    (str " " (str/replace (str (or (:base state) "")) #"^https?://" "") " ")]]))))

;; --- slash commands ------------------------------------------------------------

(defn command-hints
  "While a slash command is being typed: the commands it could be, each with
  its arguments and what it does — and once the name is typed, that one's
  usage. Draws nothing otherwise. Tab completes (dirge's ghost, as a list)."
  [state props]
  (let [cs (get-in state [:settings :commands])
        text (str (:input state))
        matches (cmd/candidates text cs)
        parsed (when (and (str/starts-with? text "/") (re-find #"\s" text))
                 (cmd/parse text cs))
        lines (cond
                (seq matches) (map cmd/usage (take (or (:max props) 8) matches))
                (:error parsed) [(:error parsed)]
                parsed [(cmd/usage parsed)])]
    (if (seq lines)
      (into [:vbox {:class :panel :border :rounded}]
            (map-indexed (fn [i l] [:text {:class (if (zero? i) :selected :dim)} (str " " l)])
                         lines))
      [:empty])))

;; --- the frame -----------------------------------------------------------------

(defn rule
  "A horizontal line with a title set into it: `──[ AGENT LOG ]──`. dirge's
  top frame is three of these, one over each column, and a layout lines them
  up by giving each the width of the column below it."
  [_state props]
  (let [line [:vbox {:flex true} [:separator {:class :frame}]]]
    [:hbox (select-keys props [:flex :width])
     [:vbox {:width 2} [:separator {:class :frame}]]
     [:text {:class :panel-title} (str "[" (:title props) "]")]
     line]))

(defn- avatar-mood
  "Which face: somebody has to answer, the run failed, it finished, it is
  using a tool the face table names, it is thinking, or there is no run."
  [state tools]
  (let [status (str (get-in state [:detail :run :status]))
        tool (str (:tool_name (last (get-in state [:branch :turns]))))]
    (cond
      (seq (:approvals state)) :alert
      (not (:run-id state)) :idle
      (#{"failed" "aborted" "error"} status) :error
      (and (not= "running" status) (not= "" status)) :done
      (contains? tools tool) (keyword (get tools tool))
      :else :thinking)))

(defn avatar
  "A face that says what the agent is doing, from dirge. The faces and which
  tool makes which one are settings — tui.edn :avatar — so a person who
  would rather not have a face can give it an empty string."
  [state props]
  (let [{:keys [faces tools]} (get-in state [:settings :avatar])
        mood (avatar-mood state (or tools {}))
        face (get faces mood (get faces :idle "(o o)"))]
    [:vbox (merge {:class :panel :border :rounded} (select-keys props [:width :flex :style]))
     [:filler]
     [:text {:class (case mood :alert :perm :error :error :done :accent :agent)
             :align :hcenter}
      (str face)]
     [:filler]]))

;; --- registration ------------------------------------------------------------
;;
;; Last, so every widget above is defined. The layout names these tags and
;; nothing else resolves them; a tag with no entry here draws a complaint in
;; its own box rather than taking the frame down (samizdat.tui.layout).

(doseq [[tag f] {:widget/rule         rule
                 :widget/command-hints command-hints
                 :widget/avatar       avatar
                 :widget/activity     activity
                 :widget/git          git
                 :widget/approvals    approvals
                 :widget/conversation conversation
                 :widget/tasks        tasks
                 :widget/files        files
                 :widget/context      context
                 :widget/gates        gates
                 :widget/artifacts    artifacts
                 :widget/runs         runs
                 :widget/branches     branches
                 :widget/input        input
                 :widget/status       status}]
  (layout/register! tag f))
