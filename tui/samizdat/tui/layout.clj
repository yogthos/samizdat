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

  WHERE THE LAYOUT COMES FROM. Three sources, most local first:

  1. a FILE, `SAMIZDAT_TUI_LAYOUT` or `.samizdat/tui.edn` beside the run —
     what a person edits, re-read whenever its mtime moves, so an edit shows
     up on the next frame with no restart;
  2. what the HARNESS SERVES, `GET /v1/harness/layout`, folded in by the
     poller — the project's stored `tui` policy, which is how the agent
     rearranges its own UI: the server is bound to the project and can read
     that row, and this process is a strict HTTP client that cannot;
  3. the SHIPPED template off the classpath, which is what draws offline and
     on the first frame.

  This used to be one source, `userspace/edn-body`, and it could not work:
  a front end binds no project, so that read fell through to the classpath
  template every time, and userspace's read cache — invalidated only by a
  write, which a front end never makes — then pinned it for the life of the
  process. Every claim about editing the UI while it runs was false, in both
  directions.

  Toolkit-free on purpose: hiccup is data, so everything here is covered by
  the suite with no terminal and no ftxui."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

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
  (let [l (:layout spec)]
    (if (drawable? l)
      (dissoc spec :error)
      (assoc spec
             :layout (:layout (template))
             :error (str "tui.edn :layout is not a hiccup element ("
                         (if (nil? l) "absent" (pr-str (type l)))
                         "); showing the shipped layout")))))

;; --- the three sources -------------------------------------------------------

(defn- parse
  "An EDN layout body as a spec, or a spec carrying only the complaint.

  Never throws. A half-written file is exactly what a runtime edit looks like
  for the instant it is being saved, and the tool for seeing the damage must
  not be the thing the damage takes out."
  [body what]
  (try
    (let [v (edn/read-string (str body))]
      (if (map? v) v {:error (str what " is not a map of layout settings")}))
    (catch Throwable e
      {:error (str what " did not parse: " (ex-message e))})))

;; The body the harness last served, already parsed. Written by the poller
;; through `serve!`; nil until the first successful fetch, and left alone by
;; a failed one — an outage must not cost the layout that is on screen.
(defonce ^:private served (atom nil))

(defn serve!
  "Take the layout body `GET /v1/harness/layout` returned. nil clears it."
  [body]
  (reset! served (when (not-empty (str body)) (parse body "the harness's tui.edn")))
  nil)

(defn file-path
  "Where a person's own layout lives, if they have one.

  `SAMIZDAT_TUI_LAYOUT` names it outright; otherwise `.samizdat/tui.edn`
  beside the project, which is where the harness already keeps the files a
  person and the agent both edit in place."
  []
  (or (not-empty (str (System/getenv "SAMIZDAT_TUI_LAYOUT")))
      ".samizdat/tui.edn"))

;; {:path :stamp :spec} — so the common case is a stat and not a parse. The
;; stamp is mtime AND length: `lastModified` is milliseconds, and an edit
;; saved inside one of them would otherwise not be seen.
(defonce ^:private file-cache (atom nil))

(defn forget-file!
  "Drop the file cache, so the next read goes to disk. For tests."
  []
  (reset! file-cache nil)
  nil)

(defn- from-file
  "The layout in `path`, re-read whenever its mtime moves. nil when there is
  no such file.

  A spec that FAILED to parse is not cached: a torn read of a file being
  written would otherwise be remembered as the answer, and the finished
  write — which need not change the mtime again — would never be seen."
  [path]
  (let [f (io/file (str path))]
    (when (.isFile f)
      (let [stamp [(.lastModified f) (.length f)]
            c @file-cache]
        (if (and (= (str path) (:path c)) (= stamp (:stamp c)))
          (:spec c)
          (let [spec (try (parse (slurp f) (str path))
                          (catch Throwable e {:error (str path ": " (ex-message e))}))]
            (when-not (:error spec)
              (reset! file-cache {:path (str path) :stamp stamp :spec spec}))
            spec))))))

(defn current
  "The layout to draw: the local file, else what the harness serves, else the
  shipped template — validated, so a broken one costs its own panel and a
  line in the status bar rather than the screen.

  Cheap enough to call every frame, which is the point: the file is re-read
  only when its mtime moves, and the served body was parsed when it arrived."
  ([] (current (file-path)))
  ([path]
   (validate (or (from-file path) @served (template) {}))))
