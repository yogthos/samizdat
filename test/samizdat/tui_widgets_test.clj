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

(ns samizdat.tui-widgets-test
  "The widgets, as data.

  Each is (fn [state props] -> hiccup), so the whole set is covered here
  without a terminal, without ftxui, and without a running server — the
  reason the TUI is written this way rather than as paint calls.

  What these tests hold: that a widget draws from the state it is given and
  nothing else, that every one of them survives an EMPTY state (the first
  frame, before any poll has answered, is the state every user sees), and
  that the folds are wired to the toggle handler with ids stable across
  frames — a fold whose id moved would close itself every time the run
  advanced."
  (:require [clojure.set]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [clojure.walk :as walk]
            [samizdat.tui.layout :as layout]
            [samizdat.tui.state :as st]
            [samizdat.tui.widgets :as w]))

(defn- render [tag state props]
  ((get @layout/widgets tag) state props))

(defn- texts
  "Every string anywhere in a rendered tree, concatenated — what the panel
  says, independent of how it is boxed."
  [form]
  (let [acc (atom [])]
    (walk/postwalk (fn [x] (when (string? x) (swap! acc conj x)) x) form)
    (str/join " " @acc)))

(defn- nodes-of
  "Every element in `form` whose tag is `tag`."
  [tag form]
  (let [acc (atom [])]
    (walk/postwalk (fn [x]
                     (when (and (vector? x) (= tag (first x))) (swap! acc conj x))
                     x)
                   form)
    @acc))

(defn- props-of [el] (when (map? (second el)) (second el)))

;; --- the activity log --------------------------------------------------------

(def ^:private trace
  [{:seq 1 :node "start" :cell "loop/assemble" :transition "ok" :turn 1 :ms 2}
   {:seq 2 :node "infer" :cell "llm/infer" :transition "ok" :turn 1 :ms 900}
   {:seq 3 :node "parse" :cell "llm/parse" :transition "tool" :turn 1 :ms 1
    :failed true}])

(deftest the-activity-log-is-the-manifest-states-not-a-tool-ticker
  ;; The one deliberate departure from what was ported. dirge's ACTIVITY
  ;; panel is a list of tool calls; here it is the state graph being walked,
  ;; because that is the thing samizdat has and a tool ticker is already the
  ;; conversation's job.
  (let [out (render :widget/activity {:trace trace} {:title "ACTIVITY LOG"})
        said (texts out)]
    (is (str/includes? said "ACTIVITY LOG"))
    (doseq [n ["start" "infer" "parse"]]
      (is (str/includes? said n) (str "names the manifest state " n)))
    (is (str/includes? said "llm/infer")
        "and the cell behind the state, which is what an operator edits")))

(deftest a-failed-step-is-marked
  (let [said (texts (render :widget/activity {:trace trace} {}))]
    (is (re-find #"(?i)fail|✗|!" said))))

(deftest the-activity-log-says-when-it-fell-behind
  ;; The ring drops the oldest under load and reports how many. A panel that
  ;; swallowed that would draw a continuous trace with an invisible hole.
  (let [said (texts (render :widget/activity {:trace trace :trace-dropped 7} {}))]
    (is (str/includes? said "7"))))

(deftest the-newest-step-is-the-one-nearest-the-input
  ;; Reading order: a log you watch grows downward, like the conversation
  ;; above the compose box.
  (let [out (render :widget/activity {:trace trace} {})
        said (texts out)]
    (is (< (str/index-of said "start") (str/index-of said "parse")))))

;; --- the conversation --------------------------------------------------------

(def ^:private turns
  [{:turn 1 :tool_name "read_file" :args "{\"path\":\"src/a.clj\"}"
    :result "(ns a)" :category "neutral"}
   {:turn 2 :tool_name "write_file" :args "{\"path\":\"src/a.clj\"}"
    :result "+ (defn go [])\n- (defn old [])" :category "success"}])

(def ^:private turn-text
  ;; Prose arrives per turn on its own feed — the branch listing drops it.
  {1 {:assistant_text "Looking at the namespace first."
      :reasoning_text "I should read before I write."}
   2 {:assistant_text "Replacing the entry point."}})

(deftest the-conversation-shows-what-was-said-and-what-was-called
  (let [said (texts (render :widget/conversation
                            {:branch {:turns turns} :turn-text turn-text} {}))]
    (is (str/includes? said "Looking at the namespace first."))
    (is (str/includes? said "read_file"))
    (is (str/includes? said "write_file"))))

(deftest a-turn-whose-prose-has-not-arrived-still-draws-its-call
  ;; The prose is a second fetch, so there are always frames where a turn is
  ;; listed and its words are not back yet. That has to read as a turn with
  ;; no prose, not as a blank entry or a crash.
  (let [said (texts (render :widget/conversation {:branch {:turns turns}} {}))]
    (is (str/includes? said "read_file"))
    (is (not (str/includes? said "Looking at the namespace first.")))))

(deftest thinking-arguments-and-results-fold-and-are-closed-by-default
  ;; A conversation that opened every tool result would be unreadable after
  ;; three turns. The header is always there; the body is a click away.
  (let [out (render :widget/conversation
                    {:branch {:turns turns} :turn-text turn-text} {})
        folds (nodes-of :collapsible out)]
    (is (seq folds) "folds are ftxui collapsibles, so the mouse works on them")
    (is (every? #(false? (:show (props-of %))) folds)
        "closed until the operator opens one")
    (let [labels (str/join " " (keep #(:label (props-of %)) folds))]
      (is (str/includes? labels "thinking"))
      (is (str/includes? labels "result")))))

(deftest a-closed-fold-does-not-carry-its-body
  ;; Cost, not appearance. `fold` handed the body to :collapsible whether or
  ;; not it was open, so every frame split every tool result on the branch
  ;; into one element per line — 175ms a frame at 200 turns, with every fold
  ;; SHUT, and ftxui redraws on every keystroke. A shut fold's body is not
  ;; on screen and must not be built.
  (let [out (render :widget/conversation
                    {:branch {:turns turns} :turn-text turn-text} {})]
    (is (not (str/includes? (texts out) "(ns a)"))
        "the body of a closed fold is not in the tree at all")
    (is (str/includes? (texts out) "result")
        "but its header is, which is what a reader scans"))
  (testing "and the body is back the moment it is opened"
    (let [out (render :widget/conversation
                      {:branch {:turns turns} :turn-text turn-text
                       :expanded #{(w/fold-id 1 :result)}} {})]
      (is (str/includes? (texts out) "(ns a)")))))

(deftest the-conversation-draws-a-bounded-tail-of-a-long-branch
  ;; Every other panel is bounded — the trace at 500, the file list at
  ;; :modified-files-shown. The biggest one was not, so a 400-turn branch
  ;; built 400 entries per frame. How many is userspace, like :prose-turns:
  ;; how far back a reader scrolls is their business.
  (let [turns (mapv (fn [n] {:turn n :tool_name "read_file" :result "x"})
                    (range 1 401))
        out (render :widget/conversation {:branch {:turns turns}} {:turns 40})
        drawn (keep #(:key (props-of %)) (nodes-of :vbox out))]
    (is (= 40 (count drawn)) "the tail the layout asked for")
    (is (= "400" (last drawn)) "and it is the NEWEST forty, which is what is being read")
    (is (= "361" (first drawn))))
  (testing "a branch shorter than the bound is drawn whole"
    (let [turns (mapv (fn [n] {:turn n :tool_name "read_file"}) (range 1 4))
          out (render :widget/conversation {:branch {:turns turns}} {:turns 40})]
      (is (= 3 (count (keep #(:key (props-of %)) (nodes-of :vbox out))))))))

(deftest an-expanded-fold-is-open-and-its-body-is-drawn
  (let [id (w/fold-id 1 :result)
        out (render :widget/conversation {:branch {:turns turns} :turn-text turn-text :expanded #{id}} {})
        open (filter #(true? (:show (props-of %))) (nodes-of :collapsible out))]
    (is (= 1 (count open)) "exactly the one that was expanded")
    (is (str/includes? (texts open) "(ns a)") "and its body is rendered")))

(deftest a-fold-id-is-stable-across-frames-and-unique-per-section
  ;; The set of open folds is keyed by these. An id derived from a position
  ;; in the list would move as the run advanced and every open fold would
  ;; shut itself on the next turn.
  (is (= (w/fold-id 3 :thinking) (w/fold-id 3 :thinking)))
  (is (not= (w/fold-id 3 :thinking) (w/fold-id 3 :result)))
  (is (not= (w/fold-id 3 :result) (w/fold-id 4 :result))))

(deftest clicking-a-fold-toggles-it-through-the-handler
  ;; Widgets are pure functions of state, so the handler arrives IN the
  ;; state. That is what keeps them testable without a running loop.
  (let [toggled (atom nil)
        state {:branch {:turns turns} :turn-text turn-text :on {:toggle #(reset! toggled %)}}
        fold (first (nodes-of :collapsible (render :widget/conversation state {})))]
    ((:on-change (props-of fold)) true)
    (is (some? @toggled) "the toggle handler was called with the fold's id")))

(deftest a-write-result-is-drawn-as-a-diff
  ;; +/- lines are the one result shape worth colouring: it is how a reader
  ;; tells an edit that landed from one that replaced the wrong thing.
  (let [id (w/fold-id 2 :result)
        out (render :widget/conversation {:branch {:turns turns} :turn-text turn-text :expanded #{id}} {})
        coloured (filter #(:color (props-of %)) (nodes-of :text out))
        by-colour (group-by #(:color (props-of %)) coloured)]
    (is (seq (get by-colour :green)) "additions")
    (is (seq (get by-colour :red)) "removals")))

(deftest a-failed-turn-reads-as-failed
  (let [said (texts (render :widget/conversation
                            {:branch {:turns [{:turn 1 :tool_name "bash"
                                               :category "failure" :result "boom"}]}}
                            {}))]
    (is (re-find #"(?i)fail|✗" said))))

;; --- the side panels ---------------------------------------------------------

(deftest the-task-panel-shows-the-board-with-its-statuses
  (let [said (texts (render :widget/tasks
                            {:detail {:tasks [{:id "t1" :title "wire it" :status "in_progress"}
                                              {:id "t2" :title "then test" :status "open"}]}}
                            {:title "TASKS"}))]
    (is (str/includes? said "wire it"))
    (is (str/includes? said "then test"))
    (is (re-find #"(?i)progress" said) "the one being worked on is marked")))

(deftest the-files-panel-lists-what-the-run-changed
  (let [said (texts (render :widget/files
                            {:detail {:modified [{:path "src/a.clj" :turn 3 :branches ["B1"]}
                                                 {:path "src/b.clj" :turn 1 :branches ["B1" "B2"]}]}}
                            {:title "MODIFIED"}))]
    (is (str/includes? said "src/a.clj"))
    (is (str/includes? said "B2") "and who touched it, which is the beam's question")))

(deftest the-context-panel-shows-fill-against-the-window
  ;; The usage map comes off the wire with the KEBAB keys run-usage builds —
  ;; :total-tokens, not :total_tokens, because the JSON writer emits the
  ;; keyword it was given. Reading the snake spelling drew a run that had
  ;; spent 8000 tokens as having spent none, which is the one number an
  ;; operator watching a metered provider is actually there for.
  (let [out (render :widget/context
                    {:detail {:run {:usage {:total-tokens 4000 :turns 4}
                                    :token_budget 10000
                                    :max_turns 20}}}
                    {:title "CONTEXT"})]
    (is (seq (nodes-of :gauge out)) "a gauge, because a ratio is not a number to read")
    (is (str/includes? (texts out) "4000"))
    (is (str/includes? (texts out) "10000"))
    (is (str/includes? (texts out) "4") "turns taken, which lives on the usage map")
    (is (str/includes? (texts out) "20"))))

(deftest the-gate-panel-separates-what-fired-from-what-is-still-open
  (let [said (texts (render :widget/gates
                            {:detail {:gates [{:gate "stuck" :fired 2 :open 1}]}}
                            {:title "GATES"}))]
    (is (str/includes? said "stuck"))
    (is (str/includes? said "2"))
    (is (str/includes? said "1"))))

(deftest the-claims-panel-shows-how-each-was-judged
  (let [said (texts (render :widget/artifacts
                            {:detail {:artifacts [{:claim "it compiles" :claim_status "confirmed"}
                                                  {:claim "it is fast" :claim_status "refuted"}]}}
                            {:title "CLAIMS"}))]
    (is (str/includes? said "it compiles"))
    (is (re-find #"(?i)confirm" said))
    (is (re-find #"(?i)refut" said))))

(deftest the-branch-picker-lists-the-beam-and-switches-to-one
  ;; samizdat runs a BEAM. Without this widget the auto-picked branch was the
  ;; only one a reader could ever see: :select-branch was in the handler map
  ;; with nothing on screen calling it.
  (let [picked (atom nil)
        state {:detail {:branches [{:id "B1" :status "active" :thesis {:claim "parser first"}}
                                   {:id "B2" :status "culled" :thesis {:claim "lexer first"}}]}
               :branch-id "B1"
               :on {:select-branch #(reset! picked %)}}
        out (render :widget/branches state {})]
    (is (str/includes? (texts out) "B1"))
    (is (str/includes? (texts out) "culled") "and what became of each")
    (let [menu (first (nodes-of :menu out))]
      (is (zero? (:selected (props-of menu))) "open on the one being read")
      ((:on-enter (props-of menu)) 1)
      (is (= "B2" @picked)))))

(deftest the-compose-box-can-also-abort-and-resume
  ;; Both were in the handler map and documented in tui.edn's widget table,
  ;; and neither had anything on screen that called it.
  (let [hit (atom [])
        state {:on {:abort #(swap! hit conj :abort)
                    :resume #(swap! hit conj :resume)}}
        out (render :widget/input state {})
        by-label (into {} (map (juxt #(:label (props-of %)) #(:on-click (props-of %))))
                       (nodes-of :button out))]
    (is (contains? by-label "abort"))
    (is (contains? by-label "resume"))
    ((get by-label "abort"))
    ((get by-label "resume"))
    (is (= [:abort :resume] @hit))))

(deftest the-compose-box-can-start-a-run
  ;; The gap this whole change is about: with no run selected the box's Enter
  ;; went to :submit, which has nothing to steer, so a fresh harness could not
  ;; be given its first problem from the UI at all. Enter now means :start
  ;; while nothing is selected, and the button means it either way.
  (let [hit (atom [])
        state {:input "build a parser"
               :on {:start #(swap! hit conj [:start %])
                    :submit #(swap! hit conj [:submit %])}}
        out (render :widget/input state {})
        inp (first (nodes-of :input out))
        by-label (into {} (map (juxt #(:label (props-of %)) #(:on-click (props-of %))))
                       (nodes-of :button out))]
    (is (contains? by-label "start") "and it is on screen, not only on a key")
    ((get by-label "start"))
    (is (= [[:start "build a parser"]] @hit)
        "the button starts on what is in the box, not on an empty string")
    (reset! hit [])
    ((:on-enter (props-of inp)) "build a parser")
    (is (= [[:start "build a parser"]] @hit)
        "Enter with no run selected starts one")
    (is (re-find #"(?i)problem|start" (str (:placeholder (props-of inp))))
        "and the box says so rather than offering to steer nothing"))
  (testing "with a run selected Enter steers it instead"
    (let [hit (atom [])
          state {:run-id "r1"
                 :on {:start #(swap! hit conj [:start %])
                      :submit #(swap! hit conj [:submit %])}}
          out (render :widget/input state {})
          inp (first (nodes-of :input out))]
      ((:on-enter (props-of inp)) "try the other parser")
      (is (= [[:submit "try the other parser"]] @hit))
      (is (re-find #"(?i)directive|steer" (str (:placeholder (props-of inp))))))))

(deftest the-compose-box-is-just-the-box-unless-a-layout-asks-for-a-title
  ;; A titled, bordered panel around one input line spent two rows saying
  ;; "STEER" above a box whose own placeholder already says what it is. The
  ;; header is userspace like every other piece of the arrangement: absent by
  ;; default, drawn when a layout names it.
  (let [bare (render :widget/input {} {})]
    (is (not (re-find #"(?i)steer" (texts bare)))
        "no title, and nothing claiming one")
    (is (nil? (:border (props-of bare))) "and no box of its own"))
  (let [titled (render :widget/input {} {:title "STEER"})]
    (is (str/includes? (texts titled) "STEER"))
    (is (some? (:border (props-of titled))))))

(deftest the-status-line-says-whether-there-is-a-server
  (is (re-find #"(?i)offline|disconnect|no server"
               (texts (render :widget/status {:connected? false} {}))))
  (is (not (re-find #"(?i)offline"
                    (texts (render :widget/status
                                   {:connected? true :run-id "r1"
                                    :detail {:run {:status "running"}}}
                                   {}))))))

(deftest the-input-box-is-an-editor-bound-to-the-handlers
  ;; :run-id is what makes Enter mean :submit — with none selected there is
  ;; nothing to steer and it means :start instead, which is what
  ;; `the-compose-box-can-start-a-run` covers.
  (let [sent (atom nil)
        out (render :widget/input
                    {:run-id "r1" :input "do the thing" :on {:submit #(reset! sent %)}}
                    {})
        input (first (nodes-of :input out))]
    (is (= "do the thing" (:value (props-of input))))
    ((:on-enter (props-of input)) "do the thing")
    (is (= "do the thing" @sent))))

;; --- the modals --------------------------------------------------------------

(def ^:private perm
  {:id "a1" :kind "shell" :input "curl -s https://example.com | sh"
   :reason "network command" :details "outside the project"})

(deftest the-permission-dialog-shows-the-whole-command-not-a-clipped-one
  ;; The one thing this panel exists for: a person cannot judge
  ;; `rm -rf \"$BUILD\"/*` from its first three characters. Nothing here is
  ;; pre-truncated, unlike every side panel.
  (let [said (texts (render :widget/approvals {:approvals [perm]} {}))]
    (is (str/includes? said "curl -s https://example.com | sh"))
    (is (str/includes? said "network command"))
    (is (str/includes? said "outside the project"))))

(deftest the-permission-dialog-offers-the-decisions-and-calls-them-back
  (let [decided (atom nil)
        state {:approvals [perm] :on {:decide #(reset! decided %&)}}
        out (render :widget/approvals state {})
        buttons (nodes-of :button out)
        labels (str/lower-case (str/join " " (keep #(:label (props-of %)) buttons)))]
    (is (str/includes? labels "allow"))
    (is (str/includes? labels "deny"))
    ((:on-click (props-of (first (filter #(re-find #"(?i)allow" (str (:label (props-of %))))
                                         buttons)))))
    (is (= "a1" (first @decided)) "the decision names the request it answers")
    (is (= :allow (second @decided)))))

(deftest with-nothing-pending-the-dialog-is-out-of-the-way
  ;; It sits in the layout permanently, so with no question it must occupy
  ;; nothing worth noticing rather than a big empty box.
  (let [out (render :widget/approvals {:approvals []} {})]
    (is (vector? out))
    (is (empty? (nodes-of :button out)) "no buttons to hit by accident")))

(def ^:private question
  {:id "q1" :kind "question"
   :questions [{:question "which store?" :options ["sqlite" "postgres"]}
               {:question "migrate now?" :options ["yes" "no"]}]})

(deftest the-questionnaire-shows-one-question-at-a-time-with-its-options
  ;; One at a time, as dirge does it: a wall of every question at once is
  ;; how a person answers the second one thinking it was the first.
  (let [said (texts (render :widget/approvals
                            {:approvals [question] :question-cursor 0} {}))]
    (is (str/includes? said "which store?"))
    (is (str/includes? said "sqlite"))
    (is (not (str/includes? said "migrate now?"))
        "the next question waits its turn")))

(deftest a-question-with-no-options-is-answered-in-words
  ;; ask_human's `normalize` accepts a bare string and gives it no options —
  ;; and an open-ended question is the ordinary use of a tool by that name.
  ;; Rendered as a menu that has nothing in it, that question could not be
  ;; answered at all: the branch sat parked for the whole :wait-ms and came
  ;; back "unanswered".
  (let [answered (atom nil)
        free {:id "q9" :kind "question"
              :questions [{:question "what should I name the module?" :options []}]}
        out (render :widget/approvals
                    {:approvals [free] :on {:answer #(reset! answered %&)}} {})]
    (is (str/includes? (texts out) "what should I name the module?"))
    (is (empty? (nodes-of :menu out)) "no menu, because there is nothing to pick from")
    (let [box (first (nodes-of :input out))]
      (is (some? box) "an editor instead")
      ((:on-enter (props-of box)) "mycelium")
      (is (= ["q9" 0 ["mycelium"]] (vec @answered))
          "and what was typed is the answer"))))

(deftest a-question-with-options-still-gets-a-menu-not-a-text-box
  (let [out (render :widget/approvals {:approvals [question]} {})]
    (is (seq (nodes-of :menu out)))
    (is (empty? (nodes-of :input out)))))

(deftest the-questionnaire-advances-and-answers
  (let [answered (atom nil)
        state {:approvals [question] :question-cursor 1
               :question-answers ["sqlite"]
               :on {:answer #(reset! answered %&)}}
        out (render :widget/approvals state {})]
    (is (str/includes? (texts out) "migrate now?") "it is on the second question")
    (let [menu (first (nodes-of :menu out))]
      ((:on-enter (props-of menu)) 0)
      (is (= "q1" (first @answered)))
      (is (= ["sqlite" "yes"] (nth @answered 2))
          "the earlier answer is carried, not lost on the way to the last"))))

;; --- the property that matters most -----------------------------------------

(deftest every-handler-the-loop-offers-has-a-caller
  ;; The ratchet for the defect that made this review worth doing. :abort,
  ;; :resume and :select-branch sat in the handler map, were documented in
  ;; tui.edn's widget table, and had nothing on screen that called them —
  ;; and no test could see it, because both halves were correct alone. Read
  ;; off the source because that is where the seam is: a widget reaches the
  ;; loop only by naming a key, and a key nothing names is dead.
  (let [src (slurp "tui/samizdat/tui/widgets.clj")
        called (set (map (comp keyword second)
                         (re-seq #"\[:on :([a-z-]+)\]" src)))]
    (is (= st/handler-keys called)
        (str "handlers with no widget calling them: "
             (pr-str (clojure.set/difference st/handler-keys called))
             "; widgets calling handlers the loop does not offer: "
             (pr-str (clojure.set/difference called st/handler-keys))))))

(deftest every-widget-draws-something-from-an-empty-state
  ;; The first frame, before any poll has answered, is the state every user
  ;; sees. A widget that assumed a run was selected would make starting the
  ;; TUI a crash — and these are drawn from a file the agent can edit, so
  ;; "just don't put that one in the layout" is not a defence.
  (doseq [[tag f] @layout/widgets]
    (testing (str tag)
      (let [out (f {} {})]
        (is (vector? out) "renders an element")
        (is (keyword? (first out)) "with a tag")))))

(deftest every-widget-tolerates-a-state-full-of-nils
  ;; The shape between "nothing yet" and "loaded": a run selected, its detail
  ;; not back. Half the panels were reading through keys that are legitimately
  ;; nil for a frame or two.
  (let [half {:run-id "r1" :detail nil :branch nil :trace nil
              :runs nil :expanded nil :input nil :on nil}]
    (doseq [[tag f] @layout/widgets]
      (testing (str tag)
        (is (vector? (f half {})))))))
