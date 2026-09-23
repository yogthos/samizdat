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

(ns samizdat.tui.theme
  "The TUI's colours, as data.

  tui.edn carries a `:theme`: a map from a CLASS name to a STYLE — a map of
  the same attributes ftxui hiccup takes (`:color`, `:bg`, `:bold`, `:dim`,
  `:italic`, `:underlined`, `:inverted`, `:strikethrough`, `:blink`), plus
  `:border` (a style: :rounded, :heavy, …) and `:border-color`. Any hiccup
  node names classes with `:class` (a keyword or a vector of them) and may
  carry an inline `:style` of its own:

      [:text {:class :agent} \"…\"]
      [:widget/tasks {:title \"TASKS\" :style {:border-color \"#ffb955\"}}]

  Resolution is the stylesheet one: the classes in order, later over
  earlier, then the node's own attributes, then its `:style`. The widgets
  name classes and never colours, so every colour on screen is the theme's,
  and the layered settings (samizdat.layers) merge a theme key by key — a
  person's global file can change one colour and a project another.

  Pure: the colours are checked here, before ftxui sees them, because ftxui
  throws on a colour it cannot read at DRAW time, where it takes the frame
  with it.")

(def classes
  "Every class the shipped widgets name. The shipped theme styles each one,
  and a test holds it to that."
  #{:frame :panel :panel-title :dim :muted :accent :ok :warn :error :info
    :user :agent :critic :supervisor :system :tool :tool-name :result :thinking
    :diff-add :diff-del :diff-hunk :step :step-failed :gauge :gauge-hot
    :status :status-live :status-down :model :git-branch :claim-confirmed
    :claim-refuted :claim-ambiguous :claim-existential :perm :perm-box
    :question :question-box :complaint :input :selected :tool-box :error-box})

(def ^:private palette
  #{:black :red :green :yellow :blue :magenta :cyan :gray-light :gray-dark
    :red-light :green-light :yellow-light :blue-light :magenta-light
    :cyan-light :white :default})

(defn color?
  "Whether ftxui can read `c` as a colour."
  [c]
  (cond
    (keyword? c) (contains? palette c)
    (int? c) (<= 0 c 255)
    (string? c) (boolean (re-matches #"#(?:[0-9a-fA-F]{3}|[0-9a-fA-F]{6})" c))
    (vector? c) (let [[k & args] c]
                  (case k
                    :rgb (and (= 3 (count args)) (every? #(and (int? %) (<= 0 % 255)) args))
                    :p256 (and (= 1 (count args)) (int? (first args)) (<= 0 (first args) 255))
                    :gradient (and (seq args) (every? color? args))
                    false))
    :else false))

(def ^:private colour-keys #{:color :bg :border-color})

(defn check
  "`theme` with every colour ftxui could not read left out, and the
  problems as [[class key value] …] for the status line to name."
  [theme]
  (reduce-kv
   (fn [acc cls style]
     (if-not (map? style)
       (update acc :problems conj [cls :style style])
       (reduce-kv (fn [acc k v]
                    (if (and (colour-keys k) (not (color? v)))
                      (-> acc
                          (update-in [:theme cls] dissoc k)
                          (update :problems conj [cls k v]))
                      acc))
                  (assoc-in acc [:theme cls] style)
                  style)))
   {:theme {} :problems []}
   (or theme {})))

(defn- readable
  "`style` with any colour ftxui could not read left out."
  [style]
  (reduce-kv (fn [m k v] (if (and (colour-keys k) (not (color? v))) (dissoc m k) m))
             style style))

(defn inline-problems
  "Every inline :style colour in a layout `form` that ftxui could not read,
  as [[key value] …]. Drawing leaves them out; this is so they are named."
  [form]
  (cond
    (and (vector? form) (map? (second form)) (map? (:style (second form))))
    (into (vec (for [[k v] (:style (second form))
                     :when (and (colour-keys k) (not (color? v)))]
                 [k v]))
          (mapcat inline-problems (drop 2 form)))
    (or (vector? form) (seq? form)) (vec (mapcat inline-problems form))
    :else []))

(defn- class-names [c]
  (cond (keyword? c) [c] (sequential? c) (filter keyword? c) :else []))

(defn- with-border
  "Fold :border-color into ftxui's {:style :color} border."
  [attrs]
  (if-let [bc (:border-color attrs)]
    (let [b (:border attrs)
          style (cond (keyword? b) b (map? b) (:style b :light) :else :light)]
      (-> attrs (dissoc :border-color) (assoc :border {:style style :color bc})))
    attrs))

(defn- resolve-attrs
  "A node's attributes with its classes and inline style resolved.

  A `:style` is inline CSS only when it is a MAP: ftxui's own :button and
  :menu take `:style` as a keyword (:ascii, :animated), and that one belongs
  to the widget and is left where it is."
  [theme attrs]
  (if-not (or (contains? attrs :class) (map? (:style attrs)))
    attrs
    (let [from-classes (apply merge (map #(get theme %) (class-names (:class attrs))))
          style (:style attrs)
          own (cond-> (dissoc attrs :class) (map? style) (dissoc :style))
          ;; A class's :border-color joins the :border STYLE the node named
          ;; (with-border), rather than replacing it.
          merged (merge from-classes own (when (map? style) (readable style)))]
      (with-border merged))))

(defn apply-theme
  "`tree` with every node's :class and :style resolved against `theme`.
  Everything that is not a hiccup attribute map — handlers, text, seqs —
  passes through."
  [theme tree]
  (cond
    (and (vector? tree) (keyword? (first tree)))
    (let [[tag a & more] tree]
      (if (map? a)
        (into [tag (resolve-attrs theme a)] (map #(apply-theme theme %)) more)
        (into [tag] (map #(apply-theme theme %)) (rest tree))))

    (vector? tree) (mapv #(apply-theme theme %) tree)
    (seq? tree) (map #(apply-theme theme %) tree)
    :else tree))
