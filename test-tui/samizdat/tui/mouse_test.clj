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

(ns samizdat.tui.mouse-test
  "The half the main suite cannot see: what the TOOLKIT does with the hiccup.

  Everywhere else the TUI is tested as data — a widget returns a
  `:collapsible` with an `:on-change`, and that is checked without ftxui.
  What that cannot tell you is whether a CLICK reaches it. `[:collapsible]`
  producing the right map is not the same claim as a person being able to
  open a diff with the mouse, and the second one is the requirement.

  So these drive real FTXUI components headlessly: mount the tree, render a
  frame, find the row the header landed on, send a real mouse press at those
  coordinates, and check the fold opened. No terminal needed — but the
  compiled shim is, which is why this is its own alias."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [ftxui.core :as ui]
            [samizdat.tui.state :as st]
            [samizdat.tui.widgets :as w]))

(def ^:private turns
  [{:turn 1 :tool_name "read_file" :args "{\"path\":\"src/a.clj\"}"
    ;; Six lines: the chamber shows four, and the last two — (defn go []) the
    ;; last of them — fold behind "↓ 2 more lines".
    :result "(ns a)\n(def x 1)\n(def y 2)\n(def z 3)\n(def w 4)\n(defn go [])"
    :category "neutral"}])

(defn- row-of
  "The 0-based row `needle` appears on in a rendered frame, or nil. How a
  test finds where to click without hard-coding a layout that will move."
  [text needle]
  (first (keep-indexed (fn [i line] (when (str/includes? line needle) i))
                       (str/split-lines text))))

(deftest clicking-a-fold-header-opens-it
  ;; The requirement, end to end through FTXUI's own hit testing: a person
  ;; with a mouse can open a tool result. Nothing here simulates the widget
  ;; contract — the click goes in as coordinates and the fold opens or it
  ;; does not.
  (let [state (atom (assoc (st/initial "b") :branch {:turns turns}))
        app (fn []
              (w/conversation
               (assoc @state :on {:toggle #(swap! state st/toggle-fold %)})
               {}))]
    (ui/with-screen [s app]
      (let [before (ui/render-text s 60 12)]
        (is (str/includes? before "more lines") "the header is drawn")
        (is (not (str/includes? before "(defn go [])"))
            "and its body is not, until it is opened")
        (let [y (row-of before "more lines")]
          (is (some? y) "found the header's row")
          (ui/send-mouse! s {:button :left :motion :pressed :x 2 :y y})
          (ui/send-mouse! s {:button :left :motion :released :x 2 :y y})
          (let [after (ui/render-text s 60 12)]
            (is (str/includes? after "(defn go [])")
                "clicking the header opened the fold and drew the body")
            (is (contains? (:expanded @state) (w/fold-id 1 :result))
                "and the open state is in the app's own state, not the widget's,
                 so it survives the next poll redrawing the tree")))))))

(deftest clicking-it-again-closes-it
  (let [state (atom (assoc (st/initial "b")
                           :branch {:turns turns}
                           :expanded #{(w/fold-id 1 :result)}))
        app (fn []
              (w/conversation
               (assoc @state :on {:toggle #(swap! state st/toggle-fold %)})
               {}))]
    (ui/with-screen [s app]
      (let [open (ui/render-text s 60 12)]
        (is (str/includes? open "(defn go [])") "starts open")
        (let [y (row-of open "more lines")]
          (ui/send-mouse! s {:button :left :motion :pressed :x 2 :y y})
          (ui/send-mouse! s {:button :left :motion :released :x 2 :y y})
          (is (not (str/includes? (ui/render-text s 60 12) "(defn go [])"))
              "and the same click shuts it"))))))

(deftest the-permission-dialog-decides-on-a-click
  ;; The other place a click has consequences, and the one where getting it
  ;; wrong is expensive: a branch is parked on this answer.
  (let [decided (atom nil)
        app (fn []
              (w/approvals {:approvals [{:id "a1" :kind "shell" :input "curl x | sh"}]
                            :on {:decide (fn [id d] (reset! decided [id d]))}}
                           {}))]
    (ui/with-screen [s app]
      (let [frame (ui/render-text s 70 14)
            y (row-of frame "allow")]
        (is (some? y) "the allow button is on screen")
        (ui/send-mouse! s {:button :left :motion :pressed :x 3 :y y})
        (ui/send-mouse! s {:button :left :motion :released :x 3 :y y})
        (is (= ["a1" {:decision :allow}] @decided)
            "the click answered the question the dialog was showing")))))

(deftest an-open-ended-question-can-be-typed-into-and-sent
  ;; The other half of the permission dialog's claim, and the one the data
  ;; tests cannot make: that a person can actually ANSWER. `ask_human` takes
  ;; a bare string, which has no options, and an empty menu is not a thing
  ;; anybody can click — the branch sat parked for the whole deadline. So
  ;; the keys go in as keys and the answer comes out or it does not.
  (let [answered (atom nil)
        q {:id "q9" :kind "question"
           :questions [{:question "what should I name the module?" :options []}]}
        app (fn [] (w/approvals {:approvals [q]
                                 :on {:answer (fn [& xs] (reset! answered (vec xs)))}}
                                {}))]
    (ui/with-screen [s app]
      (let [frame (ui/render-text s 70 12)]
        (is (str/includes? frame "what should I name the module?"))
        ;; Focus the editor, type, send.
        (let [y (row-of frame "type an answer")]
          (is (some? y) "the text box is on screen with its prompt")
          (ui/send-mouse! s {:button :left :motion :pressed :x 3 :y y})
          (ui/send-mouse! s {:button :left :motion :released :x 3 :y y}))
        (ui/send-char! s "mycelium")
        (ui/send-key! s :return)
        (is (= ["q9" 0 ["mycelium"]] @answered)
            "what was typed reached the handler that unparks the branch")))))

(defn- at-of
  "The 0-based [row col] `needle` starts at in a rendered frame, or nil. The
  buttons sit at the right end of a flexed row, so a click needs the column
  as well as the line."
  [text needle]
  (first (keep-indexed (fn [i line]
                         (when-let [c (str/index-of line needle)] [i c]))
                       (str/split-lines text))))

(defn- button-at
  "Where to click a button labelled `label`, found on the row the buttons
  share.

  Anchored on \"abort\" rather than searching the frame for the label: the
  compose box's own placeholder reads \"…Enter starts a run\", so a plain
  search for \"start\" lands in the text field and focuses it instead —
  which looks exactly like a dead button. No placeholder says \"abort\"."
  [text label]
  ;; The buttons sit at the END of the input's own line, after a placeholder
  ;; that says "starts", so the last match on the line is the button.
  (let [lines (str/split-lines text)]
    (first (keep-indexed (fn [i line]
                           (when (str/includes? line "abort")
                             (when-let [c (str/last-index-of line label)] [i c])))
                         lines))))

(defn- click!
  [s [y x]]
  (ui/send-mouse! s {:button :left :motion :pressed :x x :y y})
  (ui/send-mouse! s {:button :left :motion :released :x x :y y}))

(deftest the-compose-box-buttons-answer-a-click
  ;; The data half checks that each button carries the right :on-click. What
  ;; it cannot say is that a click REACHES one — three components share a row
  ;; with a flexed input, and FTXUI routes a press by hit-testing the
  ;; container it built. So every button on the strip is pressed for real.
  (doseq [label ["start" "abort" "resume"]]
    (testing label
      (let [hit (atom [])
            app (fn [] (w/input {:on {:start  (fn [& _] (swap! hit conj :start))
                                      :abort  #(swap! hit conj :abort)
                                      :resume #(swap! hit conj :resume)
                                      :input  (fn [_])
                                      :submit (fn [_])}}
                                {}))]
        (ui/with-screen [s app]
          (let [frame (ui/render-text s 80 8)
                at (button-at frame label)]
            (is (some? at) (str label " is on screen: " (pr-str frame)))
            (click! s at)
            (is (= [(keyword label)] @hit)
                (str "clicking " label " reached the handler"))))))))

(deftest typing-a-problem-and-pressing-enter-starts-a-run
  ;; End to end through the toolkit for the thing the TUI could not do: no
  ;; run selected, type a statement, press Enter, and the loop is asked to
  ;; start a run with exactly those words.
  (let [started (atom nil)
        text (atom "")
        app (fn [] (w/input {:input @text
                             :on {:input  #(reset! text %)
                                  :submit (fn [_] (reset! started :STEERED))
                                  :start  #(reset! started %)
                                  :abort  (fn [])
                                  :resume (fn [])}}
                            {}))]
    (ui/with-screen [s app]
      (let [frame (ui/render-text s 80 8)]
        ;; Focus the box by clicking its placeholder, then type.
        (is (some? (at-of frame "problem")) (str "the box says what it takes: " frame))
        (click! s (at-of frame "problem")))
      (ui/send-char! s "build a parser")
      (ui/send-key! s :return)
      (is (= "build a parser" @started)
          "Enter started a run on what was typed, rather than steering nothing"))))

(deftest the-buttons-still-answer-a-click-inside-the-whole-layout
  ;; The widget alone is not the configuration anybody runs. In the shipped
  ;; layout the strip sits under a flexed row, two panel columns and a
  ;; scrolling :frame, and FTXUI routes a press by hit-testing the containers
  ;; it built out of all of that — so the claim "abort and resume respond to
  ;; the mouse" is only worth making about the real tree.
  (let [hit (atom [])
        handlers {:start  (fn [& _] (swap! hit conj :start))
                  :abort  #(swap! hit conj :abort)
                  :resume #(swap! hit conj :resume)
                  :input  (fn [_])
                  :submit (fn [_])
                  :toggle (fn [_])
                  :decide (fn [& _])
                  :answer (fn [& _])
                  :select-run (fn [_])
                  :select-branch (fn [_])}
        current (requiring-resolve 'samizdat.tui.layout/current)
        expand (requiring-resolve 'samizdat.tui.layout/expand)
        themed (requiring-resolve 'samizdat.tui.theme/apply-theme)
        ;; Themed, as core/root draws it: the theme pass is part of what
        ;; reaches the toolkit, and it once ate the buttons' own :style.
        app (fn [] (let [spec (current)]
                     (themed (:theme spec)
                             (expand (:layout spec)
                                     (assoc (st/initial "http://x") :on handlers)))))]
    (ui/with-screen [s app]
      (let [frame (ui/render-text s 110 30)]
        (doseq [label ["abort" "resume" "start"]]
          (testing label
            (reset! hit [])
            (let [at (button-at frame label)]
              (is (some? at) (str label " is on the strip"))
              (click! s at)
              (is (= [(keyword label)] @hit)
                  (str "a click at " (pr-str at) " reached " label)))))))))

(deftest the-compose-box-has-the-focus-from-the-start
  ;; Typing into a TUI that has just opened must land in the compose box. The
  ;; focus used to start on whatever focusable widget the layout reached
  ;; first, so what was typed went nowhere until the box had been clicked.
  (let [typed (atom nil)
        handlers {:start (fn [& _]) :abort (fn []) :resume (fn [])
                  :input #(reset! typed %) :submit (fn [_]) :toggle (fn [_])
                  :decide (fn [& _]) :answer (fn [& _]) :select-run (fn [_])
                  :select-branch (fn [_]) :scroll (fn [& _])}
        current (requiring-resolve 'samizdat.tui.layout/current)
        expand (requiring-resolve 'samizdat.tui.layout/expand)
        themed (requiring-resolve 'samizdat.tui.theme/apply-theme)
        turns [{:turn 1 :tool_name "shell" :result "a\nb\nc\nd\ne\nf" :category "success"}]
        app (fn [] (let [spec (current)]
                     (themed (:theme spec)
                             (expand (:layout spec)
                                     (assoc (st/initial "http://x") :on handlers
                                            :run-id "r" :branch-id "B1"
                                            :branch {:turns turns})))))]
    (ui/with-screen [s app]
      (ui/render-text s 200 45)
      (ui/send-char! s "hi")
      (is (= "hi" @typed))))
  (testing "and keeps it as the screen fills in around it"
    ;; The first frame has no runs and no turns; the polls that follow add
    ;; a run menu and a conversation of folds, each of them focusable.
    (let [typed (atom nil)
          data (atom {})
          handlers {:start (fn [& _]) :abort (fn []) :resume (fn [])
                    :input #(reset! typed %) :submit (fn [_]) :toggle (fn [_])
                    :decide (fn [& _]) :answer (fn [& _]) :select-run (fn [_])
                    :select-branch (fn [_]) :scroll (fn [& _])}
          current (requiring-resolve 'samizdat.tui.layout/current)
          expand (requiring-resolve 'samizdat.tui.layout/expand)
          themed (requiring-resolve 'samizdat.tui.theme/apply-theme)
          app (fn [] (let [spec (current)]
                       (themed (:theme spec)
                               (expand (:layout spec)
                                       (merge (assoc (st/initial "http://x") :on handlers)
                                              @data)))))]
      (ui/with-screen [s app]
        (ui/render-text s 200 45)
        (reset! data {:runs [{:id "r1" :problem "p" :status "completed"}]
                      :run-id "r1" :branch-id "B1"
                      :branch {:turns [{:turn 1 :tool_name "shell" :result "a\nb\nc\nd\ne\nf"
                                        :category "success"}]}})
        (ui/refresh! s)
        (ui/render-text s 200 45)
        (ui/send-char! s "yo")
        (is (= "yo" @typed))))))

(deftest the-whole-shipped-layout-renders-through-the-real-toolkit
  ;; The layout is userspace hiccup expanded against the registry, and every
  ;; widget is only ever checked as data. This is the one test that says the
  ;; result is something FTXUI can actually draw — a tag or prop the toolkit
  ;; rejects would otherwise surface as a black screen on first run.
  (let [layout (requiring-resolve 'samizdat.tui.layout/current)
        expand (requiring-resolve 'samizdat.tui.layout/expand)
        ;; A project folded in, because the GIT box's content is what the
        ;; squeeze assertion below is about and an empty state draws its
        ;; empty note instead. Everything else is still the first frame.
        state (assoc (st/initial "http://x")
                     :project {:project "samizdat" :branch "trunk"
                               :staged 1 :unstaged 2 :untracked 0
                               :last_commit "a commit"
                               :model "glm-5.3" :context_window 128000})
        themed (requiring-resolve 'samizdat.tui.theme/apply-theme)
        spec (layout)
        frame (ui/render-text (themed (:theme spec) (expand (:layout spec) state)) 120 30)]
    (is (str/includes? frame "ACTIVITY"))
    (is (str/includes? frame "[AGENT LOG]") "the top frame names the columns")
    (is (str/includes? frame "TASKS"))
    (is (str/includes? frame "offline") "the status line drew too")
    (is (not (str/includes? frame "STEER"))
        "and the compose box is the box, with no row spent captioning it")
    (is (str/includes? frame "start") "with a way to start the first run")
    ;; The TITLE is not the assertion. A box whose content got squeezed to
    ;; zero rows still prints its title, which is exactly how the first
    ;; placement of this panel passed a weaker version of this test while
    ;; drawing an empty box on a real terminal.
    (is (str/includes? frame "GIT") "the GIT box is placed")
    (is (str/includes? frame "trunk")
        "and its CONTENT drew — a titled box with no rows in it is the squeeze
         this layout's own warning is about")
    (is (re-find #"\+1 ~2" frame) "including the dirty counts")
    (is (str/includes? frame "samizdat:trunk") "and the footer's project label")))

(deftest the-conversation-follows-the-bottom-in-the-real-toolkit
  ;; The pane follows the bottom until it is scrolled (ftxui-jolt's
  ;; :scroll): a run that has said more than fits shows what it said LAST.
  (let [turns (mapv (fn [n] {:turn n :tool_name "read_file" :result (str "result " n)})
                    (range 1 41))
        frame (ui/render-text (w/conversation (assoc (st/initial "b") :branch {:turns turns}) {})
                              60 20)]
    (is (str/includes? frame "result 40") "the newest turn is on screen")
    (is (not (str/includes? frame "result 1\n")) "and the oldest has scrolled away"))
  (testing "scrolled to the top, the pane holds it there instead"
    (let [turns (mapv (fn [n] {:turn n :tool_name "read_file" :result (str "result " n)})
                      (range 1 41))
          frame (ui/render-text (w/conversation (assoc (st/initial "b") :branch {:turns turns}
                                                       :scroll {:conversation {:top 0}}) {})
                                60 20)]
      (is (re-find #"result 1\b" frame) "the oldest turn is on screen")
      (is (not (str/includes? frame "result 40"))))))

(deftest a-multi-select-question-ticks-on-a-click
  (let [state (atom (assoc (st/initial "b")
                           :approvals [{:id "q1" :questions [{:question "which?" :multi true
                                                              :options ["alpha" "beta"]}]}]))
        app (fn [] (w/approvals (assoc @state :on {:toggle-option #(swap! state st/toggle-option %)
                                                   :answer (fn [& _]) :decide (fn [& _])
                                                   :reply (fn [& _])})
                                {}))]
    (ui/with-screen [s app]
      (let [frame (ui/render-text s 60 14)
            y (row-of frame "beta")]
        (is (some? y) "the option is on screen")
        (ui/send-mouse! s {:button :left :motion :pressed :x 3 :y y})
        (ui/send-mouse! s {:button :left :motion :released :x 3 :y y})
        (is (= #{1} (:question-selected @state)) "the click ticked it")))))

(def ^:private long-words
  "Long enough to need several rows at 40 columns, first and last word findable."
  (str "FIRSTWORD " (str/join " " (repeat 24 "word")) " LASTWORD"))

(deftest the-compose-box-soft-wraps-and-grows-with-it
  ;; A directive longer than the box used to run off its right edge, one row
  ;; tall. It wraps at the width the box is given, every word on screen, and
  ;; the box is as tall as the rows it took.
  (let [frame (ui/render-text (w/input {:input long-words :on {}} {:boxed true}) 40 12)]
    (is (some? (row-of frame "LASTWORD")) (str "the end of the text is on screen: " frame))
    (is (> (row-of frame "LASTWORD") (row-of frame "FIRSTWORD")) "on a later row than its start")))

(deftest the-conversation-soft-wraps-a-long-line
  (let [turns [{:turn 1 :tool_name "shell" :result long-words}]
        frame (ui/render-text (w/conversation (assoc (st/initial "b") :branch {:turns turns}) {})
                              40 20)]
    (is (some? (row-of frame "LASTWORD")) (str "a tool's long output line wraps rather than clips: " frame))
    (is (> (row-of frame "LASTWORD") (row-of frame "FIRSTWORD")))))
