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
  (:require [clojure.test :refer [deftest testing is]]
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

(deftest with-no-file-and-no-server-the-shipped-layout-is-what-draws
  (with-clean-layout
    (fn []
      (is (= (:layout (layout/template)) (:layout (layout/current)))
          "offline, first frame, nothing configured — it still draws"))))

(deftest what-the-harness-serves-beats-the-shipped-template
  ;; This is the agent editing its own UI. The server IS bound to the project,
  ;; so it can read the stored `tui` policy; the TUI cannot, and asks.
  (with-clean-layout
    (fn []
      (layout/serve! (pr-str {:prose-turns 5 :layout [:vbox [:widget/status {}]]}))
      (is (= [:vbox [:widget/status {}]] (:layout (layout/current))))
      (is (= 5 (:prose-turns (layout/current)))))))

(deftest an-unparseable-served-layout-falls-back-and-says-so
  (with-clean-layout
    (fn []
      (layout/serve! "{:layout [:vbox")
      (is (= (:layout (layout/template)) (:layout (layout/current))))
      (is (string? (:error (layout/current)))))))

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
                 (:layout (layout/current (.getPath f)))))
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
          (is (= 1 (:prose-turns (layout/current path))))
          (spit f (pr-str {:prose-turns 99 :layout [:vbox [:widget/activity {}]]}))
          ;; Pushed forward deliberately: two writes a millisecond apart can
          ;; land on the same mtime, and a test that happened to pass on the
          ;; clock rather than on the code would be no test at all.
          (.setLastModified f (+ 2000 (.lastModified f)))
          (is (= 99 (:prose-turns (layout/current path)))
              "the edit took, with no restart and no cache to invalidate")
          (is (= [:vbox [:widget/activity {}]] (:layout (layout/current path))))
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
          (is (= (:layout (layout/template)) (:layout (layout/current path))))
          (is (string? (:error (layout/current path))))
          ;; Same mtime as far as the cache is concerned; the retry is what
          ;; makes the finished write visible.
          (spit f (pr-str {:layout [:vbox [:widget/status {}]]}))
          (.setLastModified f 1000)
          (is (= [:vbox [:widget/status {}]] (:layout (layout/current path)))
              "a failed read is not remembered as the answer")
          (finally (.delete f)))))))
