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

(ns samizdat.tui-theme-test
  "The TUI's colours as data (karamazov-tq7m.2): a `:theme` of named styles
  in tui.edn, and any hiccup node naming one with `:class` or carrying its
  own `:style`, the way a stylesheet and an inline style work."
  (:require [clojure.test :refer [deftest testing is]]
            [samizdat.tui.layout :as layout]
            [samizdat.tui.theme :as theme]))

(def ^:private t
  {:agent {:color "#8ae89c" :bold true}
   :panel {:border-color "#60dc8c"}
   :dim {:color "#567460"}})

(deftest a-class-becomes-its-style
  (is (= [:text {:color "#8ae89c" :bold true} "hi"]
         (theme/apply-theme t [:text {:class :agent} "hi"]))))

(deftest classes-stack-and-the-nodes-own-attrs-win
  (is (= [:text {:color :red :bold true} "x"]
         (theme/apply-theme t [:text {:class [:dim :agent] :color :red} "x"]))
      "later classes over earlier, the node's own attrs over both")
  (is (= [:text {:color "#000" :bold true} "x"]
         (theme/apply-theme t [:text {:class :agent :style {:color "#000"}} "x"]))
      ":style is inline — it wins over the class, like CSS"))

(deftest a-border-colour-joins-the-border
  (is (= [:vbox {:border {:style :rounded :color "#60dc8c"}} [:text "a"]]
         (theme/apply-theme t [:vbox {:class :panel :border :rounded} [:text "a"]])))
  (testing "with no border style named, a colour alone draws the light one"
    (is (= {:border {:style :light :color "#fff"}}
           (second (theme/apply-theme {} [:vbox {:style {:border-color "#fff"}}]))))))

(deftest the-whole-tree-is-themed-and-nothing-else-is-touched
  (let [f (fn [_] nil)
        tree [:vbox {}
              [:text {:class :agent} "a"]
              (list [:text {:class :dim} "b"])
              [:input {:on-change f}]]
        out (theme/apply-theme t tree)]
    (is (= "#8ae89c" (get-in out [2 1 :color])))
    (is (= "#567460" (get-in (vec (nth out 3)) [0 1 :color])) "a seq child stays a seq, themed")
    (is (= f (get-in out [4 1 :on-change])) "handlers pass through")))

(deftest a-widgets-own-style-keyword-is-not-css
  ;; ftxui's :button and :menu take :style as a keyword. Read as an inline
  ;; stylesheet it vanished, and the button drew as its bordered default.
  (is (= [:button {:label "go" :style :ascii :color "#8ae89c" :bold true}]
         (theme/apply-theme t [:button {:label "go" :style :ascii :class :agent}]))))

(deftest an-unknown-class-is-just-no-style
  (is (= [:text {} "x"] (theme/apply-theme t [:text {:class :nope} "x"]))))

(deftest colours-are-checked-before-they-reach-the-terminal
  ;; ftxui throws on a colour it cannot read, at draw time, where it takes
  ;; the whole frame with it. A bad colour in a theme is found when the file
  ;; is read, named, and left out.
  (is (every? theme/color? [:red :gray-dark :default 42 "#abc" "#aabbcc"
                            [:rgb 1 2 3] [:p256 7] [:gradient :red "#fff"]]))
  (is (not-any? theme/color? [:reddish 300 "#abcd" "red" [:rgb 1 2] nil]))
  (let [{:keys [theme problems]} (theme/check {:ok {:color :red :bg "#000"}
                                               :bad {:color "chartreuse" :bold true}})]
    (is (= {:color :red :bg "#000"} (:ok theme)))
    (is (= {:bold true} (:bad theme)) "the bad colour dropped, the rest kept")
    (is (= [[:bad :color "chartreuse"]] problems))))

(deftest a-bad-inline-colour-is-left-out-and-named
  (let [l [:vbox [:text {:style {:color "nope" :bold true}} "x"]]]
    (is (= [:vbox [:text {:bold true} "x"]] (theme/apply-theme {} l)))
    (is (= [[:color "nope"]] (theme/inline-problems l)))))

(deftest the-shipped-theme-is-sound
  (let [shipped (:theme (layout/template))]
    (is (seq shipped))
    (is (empty? (:problems (theme/check shipped))))
    (testing "every class a widget names is one the shipped theme styles"
      (is (empty? (remove (set (keys shipped)) theme/classes))))))
