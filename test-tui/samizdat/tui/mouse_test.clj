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
    :result "(ns a)\n(defn go [])" :category "neutral"}])

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
        (is (str/includes? before "result") "the header is drawn")
        (is (not (str/includes? before "(defn go [])"))
            "and its body is not, until it is opened")
        (let [y (row-of before "result")]
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
        (let [y (row-of open "result")]
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
        (is (= ["a1" :allow] @decided)
            "the click answered the question the dialog was showing")))))

(deftest the-whole-shipped-layout-renders-through-the-real-toolkit
  ;; The layout is userspace hiccup expanded against the registry, and every
  ;; widget is only ever checked as data. This is the one test that says the
  ;; result is something FTXUI can actually draw — a tag or prop the toolkit
  ;; rejects would otherwise surface as a black screen on first run.
  (let [layout (requiring-resolve 'samizdat.tui.layout/current)
        expand (requiring-resolve 'samizdat.tui.layout/expand)
        frame (ui/render-text (expand (:layout (layout)) (st/initial "http://x")) 120 30)]
    (is (str/includes? frame "ACTIVITY LOG"))
    (is (str/includes? frame "TASKS"))
    (is (str/includes? frame "offline") "the status line drew too")))
