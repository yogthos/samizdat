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
            [samizdat.tui.layout :as layout]))

;; --- shared shapes -----------------------------------------------------------

(defn- clip
  "`s` on one line, no longer than `n`. Panels are narrow and a tool result
  carries newlines."
  [s n]
  (let [one (str/join " " (str/split (str s) #"\s+"))]
    (if (> (count one) n) (str (subs one 0 (max 0 (dec n))) "…") one)))

(defn- panel
  "A titled box. Every side panel is one, so a layout that puts two of them
  in a column gets borders that line up without saying so."
  [props & children]
  (into [:vbox (cond-> {:border :rounded}
                 (:flex props) (assoc :flex true)
                 (:width props) (assoc :width (:width props)))]
        (cond-> []
          (:title props) (conj [:text {:bold true} (str " " (:title props) " ")]
                               [:separator])
          :always (into (remove nil? children)))))

(defn- empty-note
  "What a panel says when it has nothing. Dim rather than blank: a panel that
  drew nothing is indistinguishable from one that failed to draw."
  [s]
  [:text {:dim true} (str "  " s)])

(defn fold-id
  "The identity of one foldable section, stable across frames.

  Keyed by the turn and the section, never by a position in a list — the
  conversation grows, and an id that moved would shut every open fold on the
  turn after it was opened."
  [turn section]
  (str turn "/" (name section)))

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
  hit-testing over a `:text` would be re-implementing it worse."
  [state id label body]
  [:collapsible {:label label
                 :show (contains? (set (:expanded state)) id)
                 :on-change (toggle-fn state id)}
   body])

;; --- the activity log --------------------------------------------------------

(defn- step-line [{:keys [node cell transition ms failed]}]
  [:hbox
   [:text {:color (if failed :red :cyan)} (if failed " ✗ " " · ")]
   [:text {:bold true} (clip node 12)]
   [:text {:dim true} (str " " (clip (or cell "") 16)
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
             [:text {:color :yellow} (str "  ⚠ " dropped " step(s) dropped")])
           (if (seq trace)
             (into [:vbox {:flex true}] (map step-line trace))
             (empty-note "no steps yet")))))

;; --- the conversation --------------------------------------------------------

(defn- diff-line
  "One line of a write result, coloured when it reads as a diff. How a reader
  tells an edit that landed from one that replaced the wrong thing."
  [line]
  (cond
    (str/starts-with? line "+") [:text {:color :green} line]
    (str/starts-with? line "-") [:text {:color :red} line]
    :else [:text line]))

(defn- body-block
  "A result or an argument blob, as lines. Diff-coloured when `diff?`."
  [s diff?]
  (let [lines (str/split-lines (str s))]
    (into [:vbox] (map (if diff? diff-line (fn [l] [:text l])) lines))))

(defn- failed? [turn]
  (contains? #{"failure" "mechanics"} (str (:category turn))))

(defn- turn-entry
  [state {:keys [turn tool_name args result] :as t}]
  (let [writing? (contains? #{"write_file" "edit_file" "patch"} (str tool_name))
        ;; The prose arrives per turn, because the branch listing drops it —
        ;; it is the bulk of a long run (5.5MB against 62KB of results) and
        ;; fetching it wholesale timed the branch panel out entirely. So the
        ;; words come from :turn-text, filled in for the turns on screen.
        text (get-in state [:turn-text turn])
        assistant_text (:assistant_text text)
        reasoning_text (:reasoning_text text)]
    [:vbox {:key (str turn)}
     ;; What the model said, in full. Prose is the one thing never folded:
     ;; it is short, and it is the part a reader is actually following.
     (when (not-empty (str assistant_text))
       [:paragraph (str assistant_text)])

     (when (not-empty (str reasoning_text))
       (fold state (fold-id turn :thinking)
             (str "thinking (" (count (str reasoning_text)) " chars)")
             [:paragraph {:dim true} (str reasoning_text)]))

     (when tool_name
       [:hbox
        [:text {:color (if (failed? t) :red :green)}
         (if (failed? t) " ✗ " " → ")]
        [:text {:bold true} (str tool_name)]
        [:text {:dim true} (str "  turn " turn)]])

     (when (not-empty (str args))
       (fold state (fold-id turn :args) "arguments" (body-block args false)))

     (when (not-empty (str result))
       (fold state (fold-id turn :result)
             (str "result (" (count (str/split-lines (str result))) " lines)")
             (body-block result writing?)))

     [:separator {:dim true}]]))

(defn conversation
  "The agent's turns: what it said, what it called, and what came back.

  Thinking, arguments and results fold, closed by default — a log that
  opened every tool result is unreadable after three turns, and the header
  alone is what a reader scans."
  [state props]
  (let [turns (vec (get-in state [:branch :turns]))]
    (panel (assoc props :title (or (:title props)
                                   (some->> (:branch-id state) (str "BRANCH "))))
           (if (seq turns)
             (into [:vbox {:flex true :frame :y :scroll-indicator :v}]
                   (map #(turn-entry state %) turns))
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
                      [:text {:color (if (= "in_progress" status) :yellow :default)}
                       (str " " (get task-glyph (str status) "·") " ")]
                      [:text (clip title 26)]
                      [:text {:dim true} (str " " status)]]))
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
                      [:text {:dim true} (str " t" turn
                                              (when (seq branches)
                                                (str " " (str/join "," branches))))]]))
             (empty-note "nothing written yet")))))

(defn- ratio [used total]
  (if (and (number? used) (number? total) (pos? total))
    (double (min 1.0 (/ used total)))
    0.0))

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
        max-turns (:max_turns run)]
    (panel props
           [:vbox
            [:text {:dim true} (str "  tokens " (or used 0)
                                    (when budget (str " / " budget)))]
            [:gauge {:value (ratio used budget)
                     :color (if (> (ratio used budget) 0.85) :red :cyan)}]
            [:text {:dim true} (str "  turns  " (or turns 0)
                                    (when max-turns (str " / " max-turns)))]
            [:gauge {:value (ratio turns max-turns)}]])))

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
                      [:text {:dim true} (str "  fired " fired)]
                      [:text {:color (if (pos? (or open 0)) :yellow :default)}
                       (str "  open " (or open 0))]]))
             (empty-note "no gates fired")))))

(def ^:private claim-color
  {"confirmed" :green "refuted" :red "ambiguous" :yellow "existential" :cyan})

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
                      [:text {:color (get claim-color (str claim_status) :default)}
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

;; --- the modals --------------------------------------------------------------

(defn- lines-of
  "A blob as its own lines, uncut. Used only by the approval dialog, which is
  the one place in this UI where clipping is the wrong answer."
  [s]
  (into [:vbox] (map (fn [l] [:text l]) (str/split-lines (str s)))))

(defn- permission-dialog
  [state {:keys [id kind input reason details]}]
  (let [decide (fn [d] (fn [] (when-let [f (get-in state [:on :decide])] (f id d))))]
    [:vbox {:border :heavy :color :yellow}
     [:text {:bold true :color :yellow} " ⚠ PERMISSION REQUIRED "]
     [:separator]
     [:text {:dim true} (str " tool: " kind)]
     ;; NOT clipped, unlike every other panel. A person cannot judge
     ;; `rm -rf "$BUILD"/*` from its first three characters, and a dialog
     ;; that hid the rest would be asking them to approve something they
     ;; were not shown.
     (lines-of input)
     (when (not-empty (str reason)) [:text {:dim true} (str " why: " reason)])
     (when (not-empty (str details)) [:text {:dim true} (str " " details)])
     [:separator]
     [:hbox
      [:button {:label "allow (y)" :on-click (decide :allow)}]
      [:button {:label "deny (n)" :on-click (decide :deny)}]]]))

(defn- questionnaire
  [state {:keys [id questions]}]
  (let [qs (vec questions)
        i (min (or (:question-cursor state) 0) (max 0 (dec (count qs))))
        q (nth qs i nil)
        so-far (vec (:question-answers state))]
    [:vbox {:border :heavy :color :cyan}
     [:text {:bold true :color :cyan}
      (str " ? QUESTION " (inc i) " of " (count qs) " ")]
     [:separator]
     [:paragraph (str (:question q))]
     [:menu {:entries (mapv str (:options q))
             :on-enter (fn [n]
                         (when-let [f (get-in state [:on :answer])]
                           ;; Carry what has already been answered. Handing
                           ;; back only the latest would lose every earlier
                           ;; answer on the way to the last question.
                           (f id i (conj so-far (str (nth (:options q) n nil))))))}]]))

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

;; --- the bottom strip --------------------------------------------------------

(defn input
  "The compose box. What it sends is an INTERVENTION, not a chat message —
  samizdat runs are autonomous and a person steers them at a turn boundary,
  so this is the same seam the supervisor uses."
  [state props]
  (panel (assoc props :title (or (:title props) "STEER"))
         [:input {:value (or (:input state) "")
                  :placeholder "a directive for the run — Enter sends"
                  :on-change (fn [s] (when-let [f (get-in state [:on :input])] (f s)))
                  :on-enter (fn [s] (when-let [f (get-in state [:on :submit])] (f s)))}]))

(defn status
  "The status line: whether there is a server, which run, and what it is
  doing. The first thing to look at when nothing else is moving."
  [state props]
  (let [run (get-in state [:detail :run])]
    [:hbox {:bg :gray-dark}
     (if (:connected? state)
       [:text {:color :green} " ● connected "]
       [:text {:color :red} " ○ offline "])
     [:text {:dim true} (str " " (or (:run-id state) "no run") " ")]
     (when run [:text (str " " (:status run) " ")])
     (when-let [e (:error state)] [:text {:color :red} (str " " (clip e 40) " ")])
     (when-let [e (:layout-error state)] [:text {:color :yellow} (str " " (clip e 60) " ")])
     [:filler]
     [:text {:dim true} (str (or (:base state) "") " ")]]))

;; --- registration ------------------------------------------------------------
;;
;; Last, so every widget above is defined. The layout names these tags and
;; nothing else resolves them; a tag with no entry here draws a complaint in
;; its own box rather than taking the frame down (samizdat.tui.layout).

(doseq [[tag f] {:widget/activity     activity
                 :widget/approvals    approvals
                 :widget/conversation conversation
                 :widget/tasks        tasks
                 :widget/files        files
                 :widget/context      context
                 :widget/gates        gates
                 :widget/artifacts    artifacts
                 :widget/runs         runs
                 :widget/input        input
                 :widget/status       status}]
  (layout/register! tag f))
