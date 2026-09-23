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
  the second, and it is userspace — versioned in the project alongside
  gates.edn and the manifests, editable by a person as a file and by the
  agent as a stored version, both of them while it runs.

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

  WHERE THE LAYOUT COMES FROM. tui.edn is a LAYERED settings file — it
  follows a person between projects — so it is assembled by samizdat.layers
  like config.edn, highest first:

  1. the file SAMIZDAT_TUI_FILE (or the older SAMIZDAT_TUI_LAYOUT) names;
  2. `.samizdat/tui.edn` in the project the TUI was started in;
  3. what the HARNESS SERVES, `GET /v1/harness/layout` — the server's own
     project file, which is how the agent rearranges its UI for a front end
     running somewhere else;
  4. `~/.config/samizdat/tui.edn`, a person's own across every project;
  5. the SHIPPED file off the classpath, which is what draws offline and on
     the first frame.

  Each is merged over the ones below: maps key by key, so one file can move
  a colour without restating the rest; a `:layout` whole. Every file is
  re-read when its stamp moves, so an edit shows up on the next frame.

  This used to be one source, `userspace/edn-body`, and it could not work:
  a front end binds no project, so that read fell through to the classpath
  template every time, and userspace's read cache — invalidated only by a
  write, which a front end never makes — then pinned it for the life of the
  process. Every claim about editing the UI while it runs was false, in both
  directions.

  Toolkit-free on purpose: hiccup is data, so everything here is covered by
  the suite with no terminal and no ftxui."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [samizdat.layers :as layers]
            [samizdat.tui.theme :as theme]))

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
  [:vbox {:class :complaint :border :rounded}
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

  The last of the three sources and the fallback for the other two: a
  project whose stored layout is broken needs the file, and asking the
  source that just served the broken one to supply the replacement would be
  circular."
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
  (let [l (:layout spec)
        {th :theme problems :problems} (theme/check (:theme spec))
        inline (theme/inline-problems l)
        colour-error (when (or (seq problems) (seq inline))
                       (str "tui.edn — not a colour: "
                            (str/join ", " (concat
                                            (for [[cls k v] problems]
                                              (str ":theme " cls " " k " " (pr-str v)))
                                            (for [[k v] inline]
                                              (str ":style " k " " (pr-str v)))))))
        spec (assoc spec :theme th)]
    (if (drawable? l)
      (cond-> (dissoc spec :error)
        colour-error (assoc :error colour-error))
      (assoc spec
             :layout (:layout (template))
             :error (str "tui.edn :layout is not a hiccup element ("
                         (if (nil? l) "absent" (pr-str (type l)))
                         "); showing the shipped layout"
                         (when colour-error (str "; " colour-error)))))))

;; --- the layers --------------------------------------------------------------

;; The text the harness last served. Written by the poller through `serve!`;
;; nil until the first successful fetch, and left alone by a failed one — an
;; outage must not cost the layout that is on screen. Text rather than parsed:
;; samizdat.layers parses every layer the same way, this one included.
(defonce ^:private served (atom nil))

(defn serve!
  "Take the layout body `GET /v1/harness/layout` returned. nil clears it."
  [body]
  (reset! served (not-empty (str (or body ""))))
  nil)

(defn forget-file!
  "Drop the file cache, so the next read goes to disk. For tests."
  []
  (layers/forget-files!))

(defn default-opts
  "Where the TUI's layers are: the project it was started in, and the person's
  config home. The TUI binds no project, so the project is the working
  directory — which is where `jolt tui` is run from."
  []
  {:root (System/getProperty "user.dir")
   :global-dir (layers/global-dir)})

(defn current
  "The layout to draw, as the merged settings map plus `:sources` (the layers
  that contributed) and `:error` (what did not, and why).

  Through samizdat.layers, like every other layered settings file:
  SAMIZDAT_TUI_FILE / SAMIZDAT_TUI_LAYOUT, then .samizdat/tui.edn, then what
  the harness serves, then ~/.config/samizdat/tui.edn, then the shipped file —
  each merged over the one below. `opts` override `default-opts`; a test
  passes its own dirs and environment.

  Validated, so a broken one costs its own layer and a line in the status bar
  rather than the screen. Cheap enough to call every frame: each file is
  re-read only when its stamp moves."
  ([] (current nil))
  ([opts]
   (let [r (layers/resolve "tui" (merge (default-opts)
                                        opts
                                        {:served @served
                                         :shipped (some-> (io/resource "tui.edn") slurp)}))
         layer-error (when (seq (:errors r))
                       (str/join "; " (for [{:keys [layer path var error]} (:errors r)]
                                        (str (or path var (name layer)) " " error))))
         v (validate (or (:value r) {}))]
     (cond-> (assoc v :sources (:sources r))
       layer-error (assoc :error (if (:error v)
                                   (str layer-error "; " (:error v))
                                   layer-error))))))
