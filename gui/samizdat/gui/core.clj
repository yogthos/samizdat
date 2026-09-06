;; samizdat - a claim-first verification harness
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

(ns samizdat.gui.core
  "The optional GUI: a live scene graph of a run's solution space.

  Strictly an HTTP client of the server — this process holds no engines, no
  database handle, and no run state of its own. Everything it shows arrives
  through samizdat.gui.api (the journal cursor doing the tailing), the
  event stream folds through samizdat.gui.graph into the node graph the
  GL pane draws, and everything it does (interventions, abort, resume)
  goes back through a POST. The server neither knows nor cares that a GUI
  exists, which is what keeps `jolt serve` headless."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [glimmer.core :as ui]
            ;; Installs the GTK4 backend into glimmer.backend as a side effect
            ;; of loading. glimmer itself is toolkit-agnostic since v0.1.0 —
            ;; it owns the ratom, the component model and the reconciler, and
            ;; renders through whichever backend is registered — so without
            ;; this require `ui/run` throws "no backend registered".
            [glimmer-gtk.core]
            [glimmer.ratom :as r]
            [samizdat.gui.api :as api]
            [samizdat.gui.glpane :as glpane]
            [samizdat.gui.graph :as graph]
            [samizdat.gui.mathtext :as mt]
            [samizdat.gui.newrun :as newrun]
            [samizdat.gui.ops :as ops]
            ;; Registers [:text-view ...] with glimmer as a side effect of
            ;; loading — the problem statement needs more than one line.
            [samizdat.gui.textview]))

(defn default-base-url
  "Where the server is. Defaults to the same port `jolt serve` does, so the
  GUI finds a default server with no configuration; `SAMIZDAT_URL` points
  it elsewhere, and `HARNESS_PORT` alone is enough when only the port moved
  (the server reads that same variable)."
  []
  (or (System/getenv "SAMIZDAT_URL")
      (str "http://127.0.0.1:" (or (System/getenv "HARNESS_PORT") "3985"))))

(defonce state
  (r/atom {:base-url (default-base-url)
           :runs []                     ; known runs, newest first
           :run nil                     ; selected run id
           :graph (graph/empty-graph)   ; folded scene graph
           :event-count 0
           :selected nil                ; selected branch node
           :branch-log nil              ; branch-detail body for :selected
           :branch-log-error nil        ; why it is missing, when it failed
           :connected? false
           :notice nil                  ; transient feedback line
           :budget ""                   ; resume budget-extension entry
           :composing? false            ; the new-run form is open
           :form {:problem "" :max-turns "" :beam-width "" :seed-run ""
                  :model "" :reasoning-effort ""}
           :models []                   ; what the provider serves, for the picker
           :draft ""}))                 ; intervention input text

(defonce poller (atom nil))

;; connect-to! runs on the GTK thread and from background futures alike
;; (refresh-runs!, start-new-run!), and its stop-old/reset-new pair on
;; @poller must not interleave across threads: the orphaned poller that race
;; leaves behind holds no stop! handle anywhere, and keeps folding the old
;; run's events into the new one forever (code-review-2026-08 #5).
(defonce ^:private connect-lock (Object.))

;; One branch-detail fetch at a time. The poll loop refires
;; refresh-branch-log! on every batch touching the selected branch, so a
;; fetch slower than the 1.5s interval used to stack another every tick
;; (same review).
(defonce ^:private branch-log-busy (atom false))
(defonce ^:private branch-log-pending (atom nil))

;; --- selection and the branch log --------------------------------------------

(defn- selected-branch-id
  "The branch a selection belongs to. An artifact node's id is
  `<branch>@<turn>`, so it carries its branch explicitly."
  [{:keys [graph selected]}]
  (when (and selected (not= "seed" selected))
    (or (get-in graph [:nodes selected :branch]) selected)))

(defn- refresh-branch-log!
  "Fetch the selected branch's turns and artifacts, off the main thread.

  A failure is RECORDED, not dropped. It used to be discarded silently, which
  left the panel saying `(loading …)` forever with no way to tell a slow fetch
  from a dead one — the same shape of bug as a crashed run whose row still
  read `running`. Worse, the poll loop refired it on every event touching the
  branch, so a branch too big to fetch queued a new doomed request every
  second."
  []
  (let [{:keys [base-url run] :as st} @state
        branch (selected-branch-id st)
        sel (:selected st)]
    (when (and run branch)
      (if (compare-and-set! branch-log-busy false true)
        (future
          (try
            (let [r (api/branch-detail base-url run branch)]
              (when (= sel (:selected @state))
                (if (:ok r)
                  (swap! state assoc :branch-log (:body r) :branch-log-error nil)
                  (swap! state assoc :branch-log-error
                         (or (:error r) (str "HTTP " (:status r)))))))
            (finally
              (reset! branch-log-busy false)
              ;; A selection that lost the CAS is not dropped (review2 #7):
              ;; refetch for it now that the single-flight slot is free — on
              ;; a finished run no poller event ever would. A same-selection
              ;; request stays coalesced (see ops/refetch-after?): the busy
              ;; guard exists so a poller firing every second for one branch
              ;; queues exactly one fetch.
              (let [pending @branch-log-pending]
                (reset! branch-log-pending nil)
                (when (ops/refetch-after? pending sel)
                  (refresh-branch-log!)))
              (glpane/request-render!))))
        ;; busy: a fetch for an older selection is mid-flight and will
        ;; discard its own result by its completion guard; record this
        ;; selection for its finally (review2 #7).
        (reset! branch-log-pending sel)))))

(defn- select-node! [id]
  (swap! state assoc :selected id :branch-log nil :branch-log-error nil)
  (refresh-branch-log!)
  (glpane/request-render!))

(defn- set-hover!
  "Called only when the hovered node changes, so this repaints the window
  at pointer speed only across node boundaries, not per motion event."
  [id]
  (swap! state assoc :hover id))

;; --- run selection and the poll loop -----------------------------------------

(defn- connect-to!
  "Tail `run-id`, replacing any current tail. The cursor starts at zero so
  the fold sees the run from its first event.

  Serialized on connect-lock (see its comment)."
  [run-id]
  (locking connect-lock
    (let [base (:base-url @state)]
      (when-let [{:keys [stop!]} @poller] (stop!))
      ;; :hover too, and glpane's own mark: a hover from the run just left
      ;; kept describing a node that no longer existed (code-review-2026-08
      ;; #5).
      (swap! state assoc :run run-id :graph (graph/empty-graph) :event-count 0
             :selected nil :branch-log nil :branch-log-error nil :notice nil
             :hover nil)
      (glpane/clear-hover!)
      (glpane/request-render!)
      (when run-id
        (reset! poller
                (api/start-poller!
                 {:base base :run-id run-id
                  :on-events
                  (fn [evs]
                    (swap! state
                           (fn [s] (-> s
                                       (update :graph #(reduce graph/apply-event % evs))
                                       (update :event-count + (count evs)))))
                    ;; Compare against the selected node's BRANCH: an artifact
                    ;; node's id is "<branch>@<turn>", which matches no event's
                    ;; branch_id, so artifact panels never refreshed at all.
                    (let [b (selected-branch-id @state)]
                      (when (and b (some #(= b (:branch_id %)) evs))
                        (refresh-branch-log!)))
                    (glpane/request-render!))
                  :on-status
                  (fn [s] (swap! state assoc :connected? (:connected? s)))}))))))

(defn- refresh-runs!
  "Re-fetch the run list; keep the current selection when it still exists,
  else tail the newest run.

  Off the main thread, like every other call here: GTK's loop owns this
  thread, and a synchronous request would freeze the window for as long as
  the server took to answer — up to the socket timeout when it is down.
  glimmer marshals ratom writes made off the main thread back onto the
  loop, so the swap! is safe from here."
  []
  (future
    (let [base (:base-url @state)
          runs (vec (some-> (api/list-runs base) :body :runs))]
      (swap! state assoc :runs runs)
      ;; No :run-status mirror: nothing ever read it, and the poller's
      ;; :connected? already covers liveness (code-review-2026-08 dead
      ;; code). tail-target keeps the current tail on an empty list — a
      ;; failed fetch (list-runs answers {:ok false}, no throw) is
      ;; "don't know", not "no runs" (review2 #8).
      (when-let [target (ops/tail-target (:run @state) runs)]
        (connect-to! target)))))

(defn- cycle-run!
  "Step through the known runs; the picker with no dropdown widget."
  [delta]
  (let [{:keys [runs run]} @state
        n (count runs)]
    (when (pos? n)
      (let [i (or (first (keep-indexed #(when (= run (:id %2)) %1) runs)) 0)
            j (mod (+ i delta) n)]
        (connect-to! (:id (nth runs j)))))))

;; --- actions ------------------------------------------------------------------

(defn- notice! [msg] (swap! state assoc :notice msg))

(defn- send-intervention!
  "The interjection path: a message for the selected branch (or the whole
  run), applied by the server at the next turn boundary. Bound to both the
  send button and Enter in the entry (:on-activate)."
  []
  (let [{:keys [base-url run draft] :as st} @state
        branch (selected-branch-id st)]
    (cond
      (str/blank? draft) nil
      (nil? run) (notice! "no run selected")
      :else
      ;; Clear the entry immediately so Enter feels instant; the POST
      ;; reports back when it lands.
      (do (swap! state assoc :draft "")
          (notice! (str "sending to " (or branch "the run") "…"))
          (future
            (let [r (api/intervene! base-url run
                                    {:branch-id branch :kind "message"
                                     :payload draft})]
              (notice! (if (:ok r)
                         (str "queued for " (or branch "the run")
                              " — applies at the next turn boundary")
                         (str "failed: " (:error r))))))))))

(defn- abort-run! []
  (when-let [run (:run @state)]
    (future
      (let [r (api/abort! (:base-url @state) run)]
        (notice! (if (:ok r) "abort requested"
                     (str "abort failed: " (:error r))))))))

(defn- resume-run! []
  (when-let [run (:run @state)]
    (future
      (let [budget (parse-long (str (:budget @state)))
            r (api/resume! (:base-url @state) run budget)]
        (notice! (if (:ok r)
                   (str "resuming" (when budget (str " with budget " budget)))
                   (str "resume failed: " (:error r))))))))

;; --- starting a run -----------------------------------------------------------

(defn- form-field! [k v] (swap! state assoc-in [:form k] v))

(defn- refresh-models!
  "Fetch what the provider serves, for the model picker. Off the main thread,
  and a failure leaves the list empty — the picker then offers only the server
  default, which is the behaviour from before there was a picker."
  []
  (future
    (let [r (api/models (:base-url @state))]
      (when (:ok r)
        (swap! state assoc :models (vec (get-in r [:body :models])))))))

(defn- toggle-compose! []
  (let [composing? (:composing? (swap! state update :composing? not))]
    ;; `root` renders a different tree while the form is open, one with no
    ;; :gl-area in it, so opening the form DESTROYS the pane's widget. The
    ;; poller keeps calling request-render! throughout, and glpane holds the
    ;; widget as a raw pointer it has no way to learn is dead, so it has to be
    ;; told. Closing the form mounts a fresh :gl-area whose on-realize
    ;; repopulates it.
    (when composing?
      (glpane/unmount!)
      ;; On open rather than at startup: the list is only needed by the form,
      ;; and a provider that is slow to answer should not delay the window.
      (refresh-models!)))
  (notice! nil))

(defn- copy-current-problem!
  "Fill the statement from the selected run. Each generation of a campaign
  is the previous prompt edited — retyping several paragraphs to change one
  target is the kind of friction that stops the GUI being used at all."
  []
  (let [{:keys [runs run]} @state
        row (first (filter #(= run (:id %)) runs))]
    (if-let [p (:problem row)]
      (do (form-field! :problem p)
          (form-field! :seed-run (str run))
          (notice! "loaded the current run's statement — edit it for the next generation"))
      (notice! "no run selected to copy from"))))

(def ^:private efforts
  "Cycled in this order. \"\" means the run says nothing and each model does
  what it does by default — which for deepseek-v4-pro is to think and for
  deepseek-v4-flash is not to. The other three are what the provider accepts;
  \"none\" is how thinking gets turned off, and is not the same as saying
  nothing."
  ["" "high" "medium" "low" "none"])

(defn- cycle-field!
  "Advance a form field to the next value in `options`, wrapping. The GL
  toolkit has no dropdown, and a free-text entry for a field with four legal
  values invites a typo the server only reports after the run is refused."
  [field options]
  (let [cur (str (get-in @state [:form field] ""))
        opts (vec options)
        i (or (first (keep-indexed #(when (= cur (str %2)) %1) opts)) -1)]
    (form-field! field (str (nth opts (mod (inc i) (count opts)))))))

(defn- start-new-run! []
  (let [{:keys [base-url form]} @state
        {:keys [body error]} (newrun/request form)]
    (if error
      (notice! error)
      (do (notice! (str "starting — " (newrun/summary body) "…"))
          (future
            (let [r (api/start-run! base-url body)
                  id (get-in r [:body :run_id])]
              (if (and (:ok r) id)
                (do (swap! state assoc :composing? false
                           :form {:problem "" :max-turns "" :beam-width ""
                                  :seed-run "" :model "" :reasoning-effort ""})
                    ;; Attach before refreshing the list: the poller is what
                    ;; the user is waiting to see, and the list is cosmetic.
                    (connect-to! id)
                    (notice! (str "started " (subs (str id) 0 8)))
                    (refresh-runs!))
                (notice! (str "could not start: "
                              (or (:error r) "no run id came back"))))))))))

;; --- components ---------------------------------------------------------------

(defn- new-run-panel []
  (let [{:keys [form]} @state]
    [:frame {:label "new run" :vexpand true}
     [:vbox {:spacing 8 :margin 8 :vexpand true}
      [:label {:markup (mt/dim "the problem the beam will work on — state what counts as a shippable answer, and that only engine-confirmed claims count")
               :wrap true :xalign 0.0}]
      [:scrolled {:vexpand true}
       [:text-view {:text (:problem form)
                    :on-text #(form-field! :problem %)}]]
      [:hbox {:spacing 8}
       [:label {:label "max turns"}]
       [:entry {:text (:max-turns form) :placeholder "default" :width-request 90
                :on-change #(form-field! :max-turns %)}]
       [:label {:label "beam width"}]
       [:entry {:text (:beam-width form) :placeholder "default" :width-request 90
                :on-change #(form-field! :beam-width %)}]
       [:label {:label "seed from run"}]
       [:entry {:text (:seed-run form) :hexpand true
                :placeholder "run id — carries its confirmed artifacts in"
                :on-change #(form-field! :seed-run %)}]]
      ;; The arm. Two runs are only comparable if this is recorded, and it now
      ;; is — beam/run! writes the model onto the run row.
      [:hbox {:spacing 8}
       [:label {:label "model"}]
       [:button {:label (let [m (:model form)]
                          (if (str/blank? (str m)) "server default" (str m)))
                 :tooltip "cycle the models this provider serves"
                 :on-click #(cycle-field! :model (cons "" (:models @state)))}]
       [:label {:label "thinking"}]
       [:button {:label (let [e (:reasoning-effort form)]
                          (if (str/blank? (str e)) "model default" (str e)))
                 :tooltip "reasoning_effort — \"none\" turns thinking off, empty leaves the model's own default"
                 :on-click #(cycle-field! :reasoning-effort efforts)}]]
      [:hbox {:spacing 8}
       [:button {:label "start run" :on-click start-new-run!}]
       [:button {:label "from current run" :tooltip "copy the selected run's statement and seed from it"
                 :on-click copy-current-problem!}]
       [:button {:label "cancel" :on-click toggle-compose!}]]]]))

(defn- header []
  (let [{:keys [runs run connected? notice budget]} @state
        row (first (filter #(= run (:id %)) runs))]
    [:hbox {:spacing 8}
     [:label {:markup "<b>samizdat</b>"}]
     [:button {:label (if (:composing? @state) "close" "new run")
               :tooltip "start a fresh run from a problem statement"
               :on-click toggle-compose!}]
     [:button {:label "refresh" :tooltip "re-fetch the run list"
               :on-click refresh-runs!}]
     [:button {:label "◀" :on-click #(cycle-run! 1)}]
     [:label {:label (if run
                       (str (subs (str run) 0 8) " · " (or (:status row) "?"))
                       "no run")}]
     [:button {:label "▶" :on-click #(cycle-run! -1)}]
     [:button {:label "abort" :on-click abort-run!}]
     [:button {:label "resume" :on-click resume-run!}]
     [:entry {:text budget :placeholder "turns" :width-request 70
              :on-change #(swap! state assoc :budget %)}]
     [:label {:label (cond
                       notice (str "  " notice)
                       (not run) ""
                       connected? "  tailing"
                       :else "  disconnected — retrying")
              :xalign 0.0 :hexpand true}]]))

(defn- scene-source []
  {:graph (:graph @state) :selected (:selected @state)})

(defn- graph-pane
  "Deref-free on purpose: the :gl-area widget mounts once and repaints via
  request-render!, not via reconciliation."
  []
  [:frame {:label "solution space" :hexpand true :vexpand true}
   [glpane/pane scene-source select-node! set-hover!]])

(defn- legend []
  (let [{:keys [selected graph event-count]} @state]
    [:hbox {:spacing 12}
     ;; U+2666 rather than U+25C6 for the diamond: the BLACK DIAMOND is
     ;; absent from the default system font here and rendered as a tofu box
     ;; showing its own codepoint, while the card-suit diamond has near
     ;; universal coverage. The glyphs are sized up because they are the
     ;; legend for the graph, not decoration.
     [:label {:markup (str "<b>shape</b> = engine:  "
                           "<span size=\"large\">■</span> prolog   "
                           "<span size=\"large\">♦</span> smt   "
                           "<span size=\"large\">▲</span> lean   "
                           "<span size=\"large\">⬢</span> octave   "
                           "<span size=\"large\">●</span> harness")}]
     ;; A capital O for the ring. U+25CE BULLSEYE and U+25C6 BLACK DIAMOND
     ;; were both absent from the default system font here and rendered as
     ;; tofu boxes showing their own codepoints; ASCII cannot do that.
     [:label {:markup (str "<span size=\"large\" foreground=\"#4CE6F2\">"
                           "<b>O</b></span> working")}]
     [:label {:markup "<b>ring</b> = status"}]
     [:button {:label "recenter" :on-click glpane/reset-pan!}]
     [:label {:label (str (count (:nodes graph)) " nodes · "
                          event-count " events · "
                          (count (graph/working graph)) " working"
                          (when selected (str " · selected " selected)))
              :xalign 1.0 :hexpand true}]]))

(defn- status-line
  "What the pointer is over. Also the fastest read on whether the pane is
  receiving input at all."
  []
  (let [{:keys [selected graph hover]} @state
        node (get-in graph [:nodes (or hover selected)])]
    [:label {:label (cond
                      hover (str "▸ " hover
                                 (when-let [t (:tool node)] (str " · " t))
                                 " — click to inspect, drag to pan")
                      selected (str "selected " selected " — click empty space to clear")
                      :else "hover a node to identify it; drag to pan")
             :xalign 0.0}]))



(defn- thesis-text
  "The branch's plan as it registered it: goal, technique, and the
  sub-claims it committed to. This is the approach a reader follows."
  [detail]
  (let [t (:thesis (:branch detail))
        t (if (string? t)
            (try (json/read-str t :key-fn keyword) (catch Throwable _ nil))
            t)]
    (when t
      (str "\n\n" (mt/heading "THESIS") "\n" (mt/math (:goal t))
           (when-let [tech (:technique t)]
             (str "\n" (mt/dim "Technique: ") (mt/math tech)))
           (when-let [subs (seq (:subClaims t))]
             (str "\n" (mt/dim "Sub-claims:") "\n"
                  (str/join "\n" (map #(str "  " (mt/math %)) subs))))))))

(defn- activity-text
  "What the branch has been doing, from the event stream — available the
  moment an event arrives, where the full detail fetch takes seconds."
  [node]
  (when-let [a (seq (:activity node))]
    (str "\n\n" (mt/heading "LIVE ACTIVITY (most recent last)") "\n"
         (str/join "\n"
                   (for [{:keys [turn tool category]} (take-last 15 a)]
                     (str "T" turn "  " (mt/plain tool) "  "
                          (mt/dim (str "[" (or category "?") "]"))))))))

(defn- claims-text
  "Every attempt, read off the graph rather than the detail response."
  [graph node]
  (when-let [cs (seq (graph/branch-claims graph (:id node)))]
    (str "\n\n" (mt/heading (str "ATTEMPTS (" (count cs) ")")) "\n"
         (str/join "\n\n"
                   (for [{:keys [turn status engine claim]} (take-last 10 cs)]
                     (str (case status
                            :confirmed "✓" :refuted "✗"
                            :existential "∃" :empirical "◆" "?")
                          " T" turn " " (mt/dim (str "[" (some-> engine name) "]")) " " (mt/math claim)))))))

(defn- branch-text [graph node detail err]
  (str "status: " (name (or (:status node) :unknown))
       (when-let [reason (:reason node)] (str "\n" (mt/plain reason)))
       "\nconfirmed artifacts: " (or (:confirmed node) 0)
       (when-let [m (:measured node)] (str " · measurements: " m))
       (when-let [c (:critic node)]
         (str "\ncritic: progress " (:progress c) " · momentum " (:momentum c)
              " · distinctness " (:distinctness c) " · viability " (:viability c)
              (when (:spared? node) "\nspared by Pareto retention")))
       (or (thesis-text detail)
           (when-let [t (:thesis node)] (str "\n\n" (mt/heading "THESIS") "\n" (mt/math t))))
       (activity-text node)
       (claims-text graph node)
       (when-not detail
         (str "\n\n"
              (if err
                (mt/dim (str "(could not load turn results: " err
                             " — the live activity and attempts above are"
                             " from the event stream and are complete)"))
                (mt/dim "(loading full turn results and encodings…)"))))
       (when-let [turns (seq (:turns detail))]
         (str "\n\n" (mt/heading "TURN RESULTS") "\n"
              (->> (take-last 12 turns)
                   (map #(str "T" (:turn %) " " (mt/plain (:tool_name %)) " "
                              (mt/dim (str "[" (:category %) "]")) " "
                              (mt/math (first (str/split-lines (str (:result %)))))))
                   (str/join "\n"))))
       (when-let [arts (seq (filter #(= "confirmed" (:claim_status %))
                                    (:artifacts detail)))]
         (str "\n\n" (mt/heading (str "CONFIRMED, IN FULL (" (count arts) ")")) "\n"
              (->> (take-last 8 arts)
                   (map #(str "✓ " (mt/dim (str "[" (:kind %) "/" (:tier %)
                                                 " T" (:turn %) "]")) " "
                              (mt/math (:claim %))))
                   (str/join "\n\n"))))
       ;; Measurements get their own section rather than falling into the
       ;; catch-all below. "DID NOT HOLD" is a claim about a claim that failed,
       ;; and a measurement did not fail — nothing was decided about it at all.
       (when-let [meas (seq (filter #(= "empirical" (:claim_status %))
                                    (:artifacts detail)))]
         (str "\n\n" (mt/heading (str "MEASURED (" (count meas) ")")) "\n"
              (mt/dim "what a computation produced at the parameters it was run at, not a decision")
              "\n"
              (->> (take-last 6 meas)
                   (map #(str "◆ " (mt/dim (str "[" (:kind %) " T" (:turn %) "]")) " "
                              (mt/math (:claim %))))
                   (str/join "\n"))))
       (when-let [bad (seq (remove #(#{"confirmed" "empirical"} (:claim_status %))
                                   (:artifacts detail)))]
         (str "\n\n" (mt/heading (str "DID NOT HOLD, IN FULL (" (count bad) ")")) "\n"
              (->> (take-last 4 bad)
                   (map #(str "✗ " (mt/dim (str "[" (:claim_status %)
                                                 " T" (:turn %) "]")) " "
                              (mt/math (:claim %))))
                   (str/join "\n"))))))

(defn- attempt-text
  "One verification attempt in full: the claim, the engine and tier, the code
  that was actually run, and the harness's own words about how it came out.
  A graph of attempts is only useful if you can read what each one tried and
  why it did or did not hold."
  [node detail]
  (let [turn (:turn node)
        art (first (filter #(= turn (:turn %)) (:artifacts detail)))
        trn (first (filter #(= turn (:turn %)) (:turns detail)))
        status (or (:claim_status art) (some-> (:status node) name))]
    (str (case status
           "confirmed" "✓ CONFIRMED"
           "refuted" "✗ REFUTED"
           "existential" "∃ EXISTENTIAL — proves something exists, not which"
           "empirical" "◆ MEASURED — a computation at these parameters, not a decision"
           "ambiguous" "? AMBIGUOUS"
           (str status))
         "  ·  " (mt/plain (or (:kind art) (some-> (:engine node) name)))
         (when (:tier art) (mt/plain (str " / " (:tier art))))
         (mt/dim (str "  ·  branch " (:branch node) ", turn " turn))
         "\n\n" (mt/heading "CLAIM") "\n"
         (mt/math (or (:claim art) (:claim node)))
         (when-let [v (:verdict art)]
           (str "\n\n" (mt/heading "ENGINE VERDICT") "\n" (mt/plain v)))
         (when-let [r (:result trn)]
           (str "\n\n" (mt/heading "WHAT THE HARNESS SAID") "\n" (mt/math r)))
         (when-let [w (:witness art)]
           (str "\n\n" (mt/heading "WITNESS") "\n" (mt/mono w)))
         ;; Code is escaped and monospaced but NEVER rewritten: an encoding
         ;; must read as exactly what ran.
         (when-let [c (:code art)]
           (str "\n\n" (mt/heading "CODE THAT RAN") "\n" (mt/mono c))))))

(defonce ^:private last-scroll-sel (atom nil))

(defn- scrolled-props-for
  "The inspector's scrolled props, memoized on the selection the previous
  render saw — :scroll-top rides only the render after a change (review2
  #6, decision in ops/scroll-props)."
  [selected]
  (let [prev @last-scroll-sel]
    (reset! last-scroll-sel selected)
    (ops/scroll-props prev selected)))

(defn- log-panel []
  (let [{:keys [graph selected branch-log branch-log-error]} @state
        node (get-in graph [:nodes selected])]
    [:frame {:label (cond
                      (= :artifact (:kind node)) (str "attempt " selected)
                      selected (str "branch " selected)
                      :else "inspector")
             :vexpand true :width-request 620}
     [:scrolled (scrolled-props-for selected)
      [:label {:markup (cond
                       (nil? selected)
                       (mt/dim "click a branch for its thesis and progress, or an attempt for what it tried")
                       (= "seed" selected)
                       (str (mt/heading "inherited artifacts") "\n" (mt/plain (:label node)))
                       (= :artifact (:kind node))
                       (attempt-text node branch-log)
                       :else (branch-text graph node branch-log branch-log-error))
               :wrap true :xalign 0.0 :margin 8}]]]))

(defn- input-bar []
  [:hbox {:spacing 8}
   [:entry {:text (:draft @state)
            :hexpand true
            :placeholder "interject: a message for the selected branch, applied at its next turn boundary"
            :on-change #(swap! state assoc :draft %)
            :on-activate send-intervention!}]
   [:button {:label "send" :on-click send-intervention!}]])

(defn root []
  (if (:composing? @state)
    ;; The form takes the whole body rather than squeezing in beside the
    ;; graph: a problem statement is paragraphs. The poller keeps running
    ;; while this is open, but the GL pane does NOT survive it — there is no
    ;; :gl-area in this branch, so opening the form destroys the widget and
    ;; closing it builds a new one. toggle-compose! tells glpane so, because
    ;; the pointer it caches would otherwise outlive the widget.
    [:vbox {:spacing 8 :margin 8}
     [header]
     [:separator]
     [new-run-panel]]
    [:vbox {:spacing 8 :margin 8}
     [header]
     [legend]
     [:separator]
     [:hbox {:spacing 8 :vexpand true}
      [graph-pane]
      [log-panel]]
     [status-line]
     [:separator]
     [input-bar]]))

(defn -main [& _]
  (refresh-runs!)
  (ui/run root :title "samizdat" :width 1100 :height 720)
  ;; ui/run returns only after the last window closed and the loop exited.
  ;; Stop the poller there (review2 #16): otherwise it keeps hitting the
  ;; server every ≤30s forever after the app is gone, and in the post-close
  ;; window its swaps render inline on the poller thread against a
  ;; destroyed widget tree.
  (when-let [{:keys [stop!]} @poller] (stop!)))
