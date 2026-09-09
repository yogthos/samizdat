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

(ns samizdat.tui.layout
  "The seam between what the widgets ARE and how they are ARRANGED.

  The core owns the first: a conversation log knows how to fold a diff, the
  activity log knows the trace is manifest states. `resources/tui.edn` owns
  the second, and it is userspace — read through the same seam as gates.edn
  and the manifests, versioned in the project, and editable by the agent
  while it runs.

  The file is hiccup. Every tag is ftxui's own except `:widget/*`, each of
  which stands in for one widget the core implements; `expand` replaces those
  and leaves the rest alone. That is the whole mechanism, and it is why a
  user can put two conversation panes side by side without anyone having
  added a feature for it.

  DEGRADE, NEVER THROW. This file's whole reason for being userspace is that
  it gets edited at runtime, so every failure mode here is a rendering:

  - a `:widget/*` tag nothing implements draws a named complaint in its own
    box and its neighbours are untouched;
  - a widget that throws is contained to its own box, with the message;
  - a layout that is not hiccup at all falls back to the shipped template and
    reports why.

  The alternative is a UI that black-screens on the edit whose damage it is
  the only tool for seeing.

  Toolkit-free on purpose: hiccup is data, so everything here is covered by
  the suite with no terminal and no ftxui."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [samizdat.userspace :as userspace]))

;; --- the registry ------------------------------------------------------------
;;
;; Populated by samizdat.tui.widgets, which requires this namespace — the
;; dependency runs that way so a widget can be added without editing the
;; loader, and so this namespace stays loadable on its own for the layout
;; tests. Each entry is (fn [state props] -> hiccup).

(defonce widgets (atom {}))

(defn register!
  "Add a widget under its `:widget/*` tag."
  [tag f]
  (swap! widgets assoc tag f)
  nil)

(defn widget-tag?
  "Whether `x` is a tag this namespace resolves rather than passes through."
  [x]
  (and (keyword? x) (= "widget" (namespace x))))

;; --- expansion ---------------------------------------------------------------

(defn- complaint
  "A widget-shaped box saying what went wrong, drawn where the widget was.

  Bordered and titled rather than a bare line: it stands where a panel
  stood, and an operator scanning a broken layout should be able to see
  which panel is missing without reading the text."
  [title detail]
  [:vbox {:border :rounded :color :red}
   [:text {:bold true} title]
   [:text detail]])

(defn- call-widget
  [tag props state registry]
  (if-let [f (get registry tag)]
    (try
      (f state (or props {}))
      (catch Throwable e
        (complaint (str tag " failed")
                   (or (ex-message e) (str (class e))))))
    (complaint (str "no widget " tag)
               "not implemented in this build")))

(defn expand
  "`form` with every `:widget/*` tag replaced by what its widget renders.

  `state` is the whole view state — a widget takes what it needs from it, so
  the layout never has to name data, only widgets. `registry` defaults to the
  registered widgets."
  ([form state] (expand form state @widgets))
  ([form state registry]
   (cond
     ;; A hiccup element whose tag is ours: props are the second element when
     ;; it is a map, exactly as hiccup reads them everywhere else.
     (and (vector? form) (widget-tag? (first form)))
     (let [props (when (map? (second form)) (second form))]
       (call-widget (first form) props state registry))

     (vector? form)
     (mapv #(expand % state registry) form)

     ;; Seqs are spliced by ftxui, so a (for ...) in a layout has to survive
     ;; the walk as a seq rather than becoming a vector in props position.
     (seq? form)
     (map #(expand % state registry) form)

     :else form)))

(defn widget-tags
  "Every `:widget/*` tag named anywhere in `form`, as a set."
  [form]
  (cond
    (and (vector? form) (widget-tag? (first form)))
    (into #{(first form)} (mapcat widget-tags (rest form)))

    (or (vector? form) (seq? form))
    (into #{} (mapcat widget-tags form))

    :else #{}))

;; --- loading -----------------------------------------------------------------

(defn template
  "The shipped layout, straight off the classpath.

  Read here rather than through the userspace seam because this is the
  fallback: a project whose stored layout is broken needs the file, and
  going back through the seam that just served the broken one to get it
  would be circular."
  []
  (some-> (io/resource "tui.edn") slurp edn/read-string))

(defn- drawable?
  "Whether `x` could be an ftxui element. A vector with a keyword tag is the
  only shape the renderer can start from."
  [x]
  (and (vector? x) (seq x) (keyword? (first x))))

(defn validate
  "`spec` if its `:layout` can be drawn; otherwise the template's, with
  `:error` saying what was wrong.

  Returns the spec either way. The caller draws what comes back and shows
  `:error` if there is one — an operator whose edit did not take needs to be
  told, and told without losing the UI."
  [spec]
  (let [l (:layout spec)]
    (if (drawable? l)
      (dissoc spec :error)
      (assoc spec
             :layout (:layout (template))
             :error (str "tui.edn :layout is not a hiccup element ("
                         (if (nil? l) "absent" (pr-str (type l)))
                         "); showing the shipped layout")))))

(defn current
  "The project's layout, validated, falling back to the shipped one.

  Through `userspace/edn-body`, so a project seeds its own copy on first read
  and the agent's edits to it take effect on the next load. Unparseable EDN
  is caught here rather than left to throw: a half-written file is exactly
  what a runtime edit looks like for the instant it is being saved."
  []
  (validate
   (or (try (userspace/edn-body :policy "tui")
            (catch Throwable e
              {:error (str "tui.edn did not parse: " (ex-message e))}))
       {})))
