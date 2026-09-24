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

(ns samizdat.tui-layout-test
  "The layout seam: WHAT the widgets are is the core's business, HOW they are
  arranged is the user's.

  resources/tui.edn is hiccup with `:widget/*` tags standing in for the
  widgets the core implements. Every other tag is ftxui's own vocabulary and
  passes through untouched, so rearranging the UI is editing EDN — no
  rebuild, and the agent can do it to itself at runtime through the same
  userspace seam that carries gates.edn and the manifests.

  The load-bearing property tested here is that a BAD EDIT CANNOT BLACK-SCREEN
  THE UI. A layout the agent breaks at runtime has to degrade to something
  that still shows the run and still says what is wrong, or the one tool the
  operator would use to see the damage is the tool the damage took out."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.tui.layout :as layout]
            ;; Registers every :widget/* into layout/widgets as a side effect
            ;; of loading — which is what the shipped-layout test below
            ;; checks against, so requiring it here is the test.
            [samizdat.tui.widgets]))

(def ^:private registry
  {:widget/log (fn [_state props] [:text (str "log:" (:title props))])
   :widget/tasks (fn [state _props] [:text (str "tasks:" (count (:tasks state)))])})

(deftest widget-tags-expand-and-everything-else-passes-through
  (is (= [:vbox {:border :rounded}
          [:text "log:ACTIVITY"]
          [:separator]
          [:text "tasks:2"]]
         (layout/expand
          [:vbox {:border :rounded}
           [:widget/log {:title "ACTIVITY"}]
           [:separator]
           [:widget/tasks {}]]
          {:tasks [1 2]}
          registry))
      "ftxui's own tags are the arrangement vocabulary; only :widget/* is ours"))

(deftest a-widget-may-be-written-without-props
  (is (= [:vbox [:text "tasks:0"]]
         (layout/expand [:vbox [:widget/tasks]] {:tasks []} registry))))

(deftest nesting-and-sequences-survive-expansion
  ;; hiccup children may be seqs — ftxui splices them — so the walk must not
  ;; flatten a (for ...) into the parent's props position.
  (is (= [:hbox [:vbox [:text "log:A"]] [:vbox [:text "log:B"]]]
         (layout/expand
          [:hbox [:vbox [:widget/log {:title "A"}]] [:vbox [:widget/log {:title "B"}]]]
          {} registry))))

(deftest an-unknown-widget-renders-a-visible-complaint-rather-than-throwing
  ;; The whole point of the file being userspace is that the agent edits it
  ;; while running. A typo'd tag that threw would take down the UI that shows
  ;; what the agent just did — so it renders in place, named, and the rest of
  ;; the layout still draws.
  (let [out (layout/expand [:vbox [:widget/nope {}] [:widget/tasks {}]] {:tasks []} registry)]
    (is (= :vbox (first out)))
    (is (= [:text "tasks:0"] (nth out 2)) "its neighbour is unaffected")
    (let [bad (nth out 1)]
      (is (vector? bad))
      (is (re-find #"widget/nope" (pr-str bad))
          "and the broken tag is named where the operator can see it"))))

(deftest a-widget-that-throws-is-contained-to-its-own-box
  ;; Same argument one level down: a widget fed a state shape it did not
  ;; expect must not take the frame with it.
  (let [boom {:widget/boom (fn [_ _] (throw (ex-info "kaboom" {})))}
        out (layout/expand [:vbox [:widget/boom {}] [:separator]]
                           {} (merge registry boom))]
    (is (re-find #"kaboom" (pr-str (nth out 1))))
    (is (= [:separator] (nth out 2)))))

(deftest the-shipped-layout-is-valid-and-names-only-widgets-that-exist
  ;; The template is what a project seeds from and what a broken edit falls
  ;; back to, so it is the one layout that must never be wrong.
  (let [spec (layout/template)]
    (is (vector? (:layout spec)) "tui.edn carries a :layout")
    (is (seq (layout/widget-tags (:layout spec))))
    (is (empty? (remove (set (keys @layout/widgets)) (layout/widget-tags (:layout spec))))
        "every :widget/* the shipped layout names is implemented")))

(deftest a-layout-that-is-not-hiccup-falls-back-to-the-template
  (testing "nil, a map, a bare string — none of them can draw"
    (doseq [junk [nil {} "vbox" 42 []]]
      (is (= (:layout (layout/template)) (:layout (layout/validate {:layout junk})))
          (str "fell back for " (pr-str junk)))))
  (testing "and it says why, so the operator is not left guessing"
    (is (string? (:error (layout/validate {:layout nil}))))))

(deftest a-valid-layout-is-returned-unchanged-and-flagged-clean
  (let [spec {:layout [:vbox [:widget/tasks {}]]}]
    (is (= (:layout spec) (:layout (layout/validate spec))))
    (is (nil? (:error (layout/validate spec))))))

;; --- where the layout actually comes from ------------------------------------
;;
;; The bug these pin: `current` read the layout through `userspace/edn-body`,
;; which in the TUI's process can only ever return the SHIPPED template — the
;; TUI binds no project, so there is no stored row to read — and the
;; userspace read cache then pinned that value for the life of the process,
;; because nothing in a front end ever writes and nothing therefore ever
;; invalidates. So `root`'s re-read-every-frame was a no-op after the first
;; frame, an agent's saved version was invisible, and a person editing the
;; file needed a restart. Three sources with a stated precedence instead.

(defn- with-clean-layout [f]
  (layout/serve! nil)
  (layout/forget-file!)
  (try (f) (finally (layout/serve! nil) (layout/forget-file!))))

(def ^:private nowhere
  "No project, no config home, no environment: only what is served and what
  ships. Every test states the layers it means rather than reading whatever
  the machine running the suite has in ~/.config."
  {:root nil :global-dir nil :getenv (constantly nil)})

(defn- file-at
  "The layers with a person's file named by the environment variable."
  [path]
  (assoc nowhere :getenv {"SAMIZDAT_TUI_LAYOUT" path}))

(deftest with-no-file-and-no-server-the-shipped-layout-is-what-draws
  (with-clean-layout
    (fn []
      (is (= (:layout (layout/template)) (:layout (layout/current nowhere)))
          "offline, first frame, nothing configured — it still draws"))))

(deftest what-the-harness-serves-beats-the-shipped-template
  ;; This is the agent editing its own UI. The server IS bound to the project,
  ;; so it can read the stored `tui` policy; the TUI cannot, and asks.
  (with-clean-layout
    (fn []
      (layout/serve! (pr-str {:prose-turns 5 :layout [:vbox [:widget/status {}]]}))
      (is (= [:vbox [:widget/status {}]] (:layout (layout/current nowhere))))
      (is (= 5 (:prose-turns (layout/current nowhere)))))))

(deftest an-unparseable-served-layout-falls-back-and-says-so
  (with-clean-layout
    (fn []
      (layout/serve! "{:layout [:vbox")
      (is (= (:layout (layout/template)) (:layout (layout/current nowhere))))
      (is (string? (:error (layout/current nowhere)))))))

(deftest a-local-file-beats-what-the-harness-serves
  ;; A person editing EDN is the requirement this file exists for, and they
  ;; must not have to go through the harness's store to do it.
  (with-clean-layout
    (fn []
      (let [f (java.io.File/createTempFile "tui-layout" ".edn")]
        (try
          (spit f (pr-str {:layout [:vbox [:widget/activity {:title "MINE"}]]}))
          (layout/serve! (pr-str {:layout [:vbox [:widget/status {}]]}))
          (is (= [:vbox [:widget/activity {:title "MINE"}]]
                 (:layout (layout/current (file-at (.getPath f))))))
          (finally (.delete f)))))))

(deftest an-edit-to-the-file-takes-on-the-next-frame
  ;; The whole claim of `root`: the layout is re-read, so a runtime edit
  ;; shows up without a restart. It was false, and this is what says so.
  (with-clean-layout
    (fn []
      (let [f (java.io.File/createTempFile "tui-layout" ".edn")
            path (.getPath f)]
        (try
          (spit f (pr-str {:prose-turns 1 :layout [:vbox [:widget/status {}]]}))
          (is (= 1 (:prose-turns (layout/current (file-at path)))))
          (spit f (pr-str {:prose-turns 99 :layout [:vbox [:widget/activity {}]]}))
          ;; Pushed forward deliberately: two writes a millisecond apart can
          ;; land on the same mtime, and a test that happened to pass on the
          ;; clock rather than on the code would be no test at all.
          (.setLastModified f (+ 2000 (.lastModified f)))
          (is (= 99 (:prose-turns (layout/current (file-at path))))
              "the edit took, with no restart and no cache to invalidate")
          (is (= [:vbox [:widget/activity {}]] (:layout (layout/current (file-at path)))))
          (finally (.delete f)))))))

(deftest a-half-written-file-does-not-take-the-screen-and-is-retried
  ;; What a file being saved looks like for the instant it is being saved.
  ;; It must not black-screen, and it must not be CACHED as broken either —
  ;; the next frame has to try again or a torn read would be permanent.
  (with-clean-layout
    (fn []
      (let [f (java.io.File/createTempFile "tui-layout" ".edn")
            path (.getPath f)]
        (try
          (spit f "{:layout [:vbox")
          (is (= (:layout (layout/template)) (:layout (layout/current (file-at path)))))
          (is (string? (:error (layout/current (file-at path)))))
          ;; Same mtime as far as the cache is concerned; the retry is what
          ;; makes the finished write visible.
          (spit f (pr-str {:layout [:vbox [:widget/status {}]]}))
          (.setLastModified f 1000)
          (is (= [:vbox [:widget/status {}]] (:layout (layout/current (file-at path))))
              "a failed read is not remembered as the answer")
          (finally (.delete f)))))))

;; --- the layers (karamazov-1a51.5) -------------------------------------------
;;
;; The TUI's file is one of the LAYERED settings files: it follows a person
;; from project to project, so ~/.config/samizdat/tui.edn sits under the
;; project's .samizdat/tui.edn, and each merges over the one below — a theme
;; colour at a time, while a :layout is replaced whole.

(defn- temp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "tui-layers" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- put! [dir rel body]
  (let [f (java.io.File. (str dir "/" rel))]
    (.mkdirs (.getParentFile f))
    (spit f body)
    (.getPath f)))

(deftest a-global-file-reaches-every-project-and-a-project-file-overrides-it
  (with-clean-layout
    (fn []
      (let [root (temp-dir) global (temp-dir)
            o (assoc nowhere :root root :global-dir global)]
        (put! global "tui.edn" (pr-str {:prose-turns 7
                                        :theme {:agent {:color "#111111"}
                                                :user {:color "#222222"}}}))
        (is (= 7 (:prose-turns (layout/current o))))
        (is (= (:layout (layout/template)) (:layout (layout/current o)))
            "a file that says nothing about :layout keeps the shipped one")
        (put! root ".samizdat/tui.edn" (pr-str {:theme {:agent {:color "#999999"}}
                                                :layout [:vbox [:widget/status {}]]}))
        (is (= {:agent {:color "#999999"} :user {:color "#222222"}}
               (select-keys (:theme (layout/current o)) [:agent :user]))
            "colours merge one at a time")
        (is (= {:color "#8ae89c"} (:ok (:theme (layout/current o))))
            "and what no file changed is the shipped theme's")
        (testing "a colour ftxui cannot read is left out and named"
          (put! root ".samizdat/tui.edn" (pr-str {:theme {:agent {:color "greenish"}}}))
          (let [c (layout/current o)]
            (is (nil? (get-in c [:theme :agent :color])))
            (is (re-find #"greenish" (str (:error c))))))
        (put! root ".samizdat/tui.edn" (pr-str {:theme {:agent {:color "#999999"}}
                                                :layout [:vbox [:widget/status {}]]}))
        (is (= [:vbox [:widget/status {}]] (:layout (layout/current o)))
            "an arrangement is replaced whole")
        (is (= 7 (:prose-turns (layout/current o))))))))

(deftest the-sources-are-reported-so-a-surprising-panel-can-be-traced
  (with-clean-layout
    (fn []
      (let [global (temp-dir)]
        (put! global "tui.edn" "{:prose-turns 2}")
        (is (= [:global :shipped]
               (mapv :layer (:sources (layout/current (assoc nowhere :global-dir global))))))))))

(deftest a-broken-layer-costs-itself-and-the-status-line-names-it
  (with-clean-layout
    (fn []
      (let [root (temp-dir) global (temp-dir)
            bad (put! root ".samizdat/tui.edn" "{:prose-turns ")]
        (put! global "tui.edn" "{:prose-turns 3}")
        (let [c (layout/current (assoc nowhere :root root :global-dir global))]
          (is (= 3 (:prose-turns c)) "the global layer still counts")
          (is (str/includes? (str (:error c)) bad)))))))

(deftest an-unchanged-layout-is-not-rebuilt-every-frame
  ;; Every frame, and several keys, ask for the layout. Re-reading the
  ;; shipped file and re-parsing, merging and validating every layer was 2ms
  ;; a call (karamazov-lx14) — for an answer that only changes when a layer
  ;; does.
  (with-clean-layout
    (fn []
      (layout/serve! (pr-str {:prose-turns 5 :layout [:vbox [:widget/status {}]]}))
      (let [a (layout/current nowhere)]
        (is (identical? a (layout/current nowhere)))
        (testing "a new served version is a new answer"
          (layout/serve! (pr-str {:prose-turns 6 :layout [:vbox [:widget/status {}]]}))
          (is (= 6 (:prose-turns (layout/current nowhere)))))))))
