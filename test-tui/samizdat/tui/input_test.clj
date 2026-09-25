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

(ns samizdat.tui.input-test
  "The compose box through real FTXUI: that Up on its top row reaches the
  history and a drag over it selects what was typed. The data tests show the
  widget names :on-up-edge; only the toolkit can say a key press gets there."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [ftxui.core :as ui]
            [samizdat.tui.state :as st]
            [samizdat.tui.widgets :as w]))

(defn- box [state]
  (fn []
    (w/input (assoc @state :on {:input #(swap! state st/set-input %)
                                :history-back #(swap! state st/history-back)
                                :history-forward #(swap! state st/history-forward)})
             {})))

(deftest up-and-down-walk-what-was-sent-and-come-back-to-the-draft
  (let [state (atom (-> (st/initial "b")
                        (st/remember-input "first thing")
                        (st/remember-input "second thing")))]
    (ui/with-screen [s (box state)]
      (ui/render-text s 80 3)
      (ui/send-char! s "draft")
      (ui/send-key! s :arrow-up)
      (is (= "second thing" (:input @state)))
      (ui/send-key! s :arrow-up)
      (is (= "first thing" (:input @state)))
      (ui/send-key! s :arrow-down)
      (ui/send-key! s :arrow-down)
      (is (= "draft" (:input @state)) "past the newest, the line being typed")
      (testing "a recalled line is edited from its end"
        (ui/send-key! s :arrow-up)
        (ui/send-char! s "!")
        (is (= "second thing!" (:input @state)))))))

(deftest what-was-typed-can-be-selected
  (let [state (atom (st/set-input (st/initial "b") "select me please"))]
    (ui/with-screen [s (box state)]
      (let [frame (ui/render-text s 80 3)
            y (first (keep-indexed (fn [i l] (when (str/includes? l "select me") i))
                                   (str/split-lines frame)))
            x (str/index-of (nth (str/split-lines frame) y) "select")]
        (is (false? (ui/send-mouse! s {:x (+ x 2) :y y}))
            "a press in the box is left for the selection to start from")
        (is (= "select me" (ui/selection-text s 80 3 [x y (+ x 8) y])))))))
