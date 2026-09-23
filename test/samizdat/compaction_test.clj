;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.compaction-test
  "The context-budget ladder, ported from dirge.

  Compaction was one threshold on a character count, fired reactively. These
  pin the ported behaviour: fractions of the model's window rather than an
  absolute size, the cheap rungs before the expensive ones, the protected head
  and tail, the user-boundary snapping that keeps a fold from orphaning half a
  tool call, and the structural check that must pass before any history is
  destroyed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.compaction :as cmp]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.loop]
            [samizdat.cells :as cells]
            [samizdat.manifests :as manifests]
            [mycelium.cell :as cell]
            [samizdat.config :as config]
            [samizdat.store.journal]))

(def ^:private ladder
  {:aggressive-cap 0.60 :fold 0.75 :aggressive-fold 0.78 :force 0.80 :turn-start 0.90})

;; --- measuring ---------------------------------------------------------------

(deftest tokens-are-counted-across-both-content-shapes
  ;; A tool result arrives as a block vector, not a string. Counting only the
  ;; string shape is how dirge's own prune pass was a silent no-op on live
  ;; results — the pass ran, matched nothing, and reported success.
  (is (= 4 (cmp/content-chars "abcd")))
  (is (= 6 (cmp/content-chars [{:type "text" :text "abc"} {:type "text" :text "def"}])))
  (is (= 0 (cmp/content-chars nil)))
  (testing "and the estimate divides the total, not each message"
    (is (= 2 (cmp/estimate-tokens [{:content "abcd"} {:content "abcd"}] 4)))))

(deftest an-unknown-window-produces-no-pressure
  ;; A model whose context size nobody recorded gets NO ladder rather than a
  ;; guessed one: every rung is a fraction of that number.
  (is (= 0.0 (cmp/pressure 5000 nil)))
  (is (= 0.0 (cmp/pressure 5000 0)))
  (is (= 0.5 (cmp/pressure 5000 10000))))

;; --- the ladder --------------------------------------------------------------

(deftest the-ladder-answers-with-the-highest-rung-reached
  (is (nil? (cmp/tier 0.4 ladder)) "below every rung, nothing happens")
  (is (= :aggressive-cap (cmp/tier 0.62 ladder)))
  (is (= :fold (cmp/tier 0.76 ladder)))
  (is (= :aggressive-fold (cmp/tier 0.79 ladder)))
  (is (= :force-summary (cmp/tier 0.85 ladder)))
  (testing "a branch at 0.92 wants the turn-start fold, not the per-result cap
            it also passed — most severe wins"
    (is (= :turn-start-fold (cmp/tier 0.92 ladder)))))

(deftest only-the-upper-rungs-fold-and-only-they-are-aggressive
  (is (not (cmp/fold? :aggressive-cap)) "capping a result is not folding history")
  (is (every? cmp/fold? [:fold :aggressive-fold :force-summary :turn-start-fold]))
  (is (not (cmp/aggressive? :fold)))
  (is (every? cmp/aggressive? [:aggressive-fold :force-summary :turn-start-fold])))

(deftest the-result-cap-tightens-under-pressure
  (let [caps {:normal 3000 :aggressive 1000}]
    (is (= 3000 (cmp/result-cap 0.4 ladder caps)))
    (is (= 1000 (cmp/result-cap 0.7 ladder caps))
        "past the aggressive-cap rung one grep result may not eat the window")))

(deftest a-snip-can-excuse-a-normal-fold-but-never-an-aggressive-one
  (is (cmp/snip-bought-enough? 2000 10000 0.10 false)
      "freeing 20% of the window is headroom enough for this turn")
  (is (not (cmp/snip-bought-enough? 500 10000 0.10 false))
      "5% is not")
  (testing "past the aggressive rungs the summary is the thing that is needed,
            and a snip that looks sufficient is how a branch reaches 0.9
            having never folded"
    (is (not (cmp/snip-bought-enough? 9000 10000 0.10 true)))))

;; --- capping and pruning -----------------------------------------------------

(deftest capping-keeps-both-ends-of-an-oversized-result
  ;; The head says what ran and the tail says how it ended. A plain head
  ;; truncation drops every error message.
  (let [big (str "START" (apply str (repeat 4000 "x")) "END")
        {:keys [messages freed]}
        (cmp/cap-oversized-results
         [{:role "user" :content big} {:role "user" :content "recent"}]
         {:cap-tokens 100 :chars-per-token 4 :protect-tail 1
          :roles #{"user"} :note-fn (fn [n] (str "<" n " dropped>"))})]
    (is (str/starts-with? (:content (first messages)) "START"))
    (is (str/ends-with? (:content (first messages)) "END"))
    (is (str/includes? (:content (first messages)) "dropped"))
    (is (pos? freed))
    (is (= "recent" (:content (second messages))) "the protected tail is untouched")))

(deftest pruning-keeps-the-call-and-drops-the-output
  ;; The distinction that makes this cheaper than folding: every turn stays
  ;; where it is, and only the bulk goes. What the branch DID survives; what
  ;; it SAW is one fetch_turn away.
  (let [templates {:preview-chars 20
                   :render (fn [t ctx] (str "[" (:tool ctx) "] " (:chars ctx) " chars"))
                   :by-tool {} :default "x"}
        msgs [{:role "user" :tool "grep" :content (apply str (repeat 900 "m"))}
              {:role "user" :tool "shell" :content "short"}
              {:role "user" :tool "read_file" :content (apply str (repeat 900 "r"))}]
        out (cmp/prune-tool-outputs msgs {:protect-tail 1 :min-chars 500
                                          :roles #{"user"}
                                          :tool-of :tool :templates templates})]
    (is (= "[grep] 900 chars" (:content (first out))))
    (is (:pruned? (first out)))
    (is (= "short" (:content (second out))) "a short result is left readable")
    (is (= 900 (count (:content (last out)))) "the protected tail is untouched")))

;; --- choosing the window -----------------------------------------------------

(defn- convo [n]
  (vec (for [i (range n)]
         {:role (if (even? i) "user" "assistant") :content (str "m" i)})))

(deftest a-short-conversation-is-not-folded
  (is (= [0 0] (cmp/compress-window (convo 5) 2 5))
      "nothing to fold once the head and tail claim everything"))

(deftest both-cuts-snap-to-user-boundaries
  ;; NOT tidiness. A user message never carries a dangling tool call and is
  ;; never itself an orphaned tool result, so cutting there removes whole
  ;; turns. Cutting mid-turn leaves half a call/result pair, which some
  ;; providers reject outright.
  (let [msgs (convo 20)
        [s e] (cmp/compress-window msgs 2 5)]
    (is (= "user" (:role (nth msgs s))) "the head cut lands on a user turn")
    (is (= "user" (:role (nth msgs e))) "and so does the tail cut")
    (is (>= s 2) "the head snap only ever protects more")
    (is (<= e 15) "and so does the tail snap")))

(deftest the-summary-budget-is-a-fraction-floored-and-capped
  (let [b {:ratio 0.20 :floor 2000 :ceiling 12000}]
    (is (= 2000 (cmp/summary-budget 100 b)) "a small fold still carries its facts")
    (is (= 10000 (cmp/summary-budget 50000 b)))
    (is (= 12000 (cmp/summary-budget 500000 b))
        "a summary approaching the size of what it replaced has compacted nothing")))

;; --- the guard on destroying history -----------------------------------------

(def ^:private validation
  {:sections ["Active Task" "Goal" "Completed Actions" "Relevant Files" "Source Coverage"]
   :min-sections 2
   :empties #{"" "none" "n/a" "na" "-" "—" "unknown" "nothing" "todo" "tbd"}})

(deftest a-stub-summary-may-not-destroy-history
  ;; The caller acts on TRUE by dropping the folded region, so a false
  ;; positive costs real turns permanently.
  (is (not (cmp/validate-summary "" validation)))
  (is (not (cmp/validate-summary "## Active Task\nNone.\n## Goal\nN/A" validation))
      "every section a placeholder is a stub however many headings it has")
  (is (not (cmp/validate-summary "## Active Task\nFix the parser." validation))
      "one populated section is below the floor"))

(deftest a-terse-but-real-summary-passes
  ;; Permissiveness matters as much as safety here: rejecting a usable summary
  ;; forces prune-only folding, which walks the branch into the overflow the
  ;; fold existed to prevent.
  (is (cmp/validate-summary
       "## Active Task\nFix step-move.\n## Goal\nShip the game.\n## Goal\nx"
       validation))
  (testing "prose that merely uses the words is not mistaken for a summary"
    (is (not (cmp/validate-summary
              "The goal was to fix it and the active task is done." validation)))))

;; --- applying it -------------------------------------------------------------

(deftest a-fold-replaces-the-window-in-place
  (let [msgs (convo 12)
        out (cmp/apply-summary msgs [2 8] "MARKER:" "the summary")]
    (is (= (- 12 6 -1) (count out)) "six messages became one")
    (is (= "MARKER:the summary" (:content (nth out 2))))
    (is (:compaction? (nth out 2)))
    (is (= (:content (nth msgs 0)) (:content (nth out 0))) "the head keeps its place")
    (is (= (:content (last msgs)) (:content (last out))) "and the tail follows")))

(deftest a-fold-carries-pinned-messages-through-verbatim
  ;; The tape's per-message unload never touches a :pinned? message (the
  ;; current task's statement); the fold must not either, or a long enough
  ;; task gets summarised away exactly when it matters most (karamazov-d5wo.1).
  (let [pinned {:role "user" :content "THE TASK" :pinned? true}
        msgs (assoc (convo 12) 4 pinned)
        out (cmp/apply-summary msgs [2 8] "MARKER:" "the summary")]
    (is (= "MARKER:the summary" (:content (nth out 2))) "the summary stands where the window was")
    (is (= pinned (nth out 3)) "and the pinned message follows it, whole")
    (is (= (- 12 6 -2) (count out)) "the other five folded messages became the one summary")
    (is (= [pinned] (cmp/pinned-in msgs [2 8])))
    (is (= [] (cmp/pinned-in (convo 12) [2 8])))))

;; --- the task fold (karamazov-d5wo.5) -----------------------------------------

(defn- task-convo
  "sys, problem, the claim call, then the task's statement and six turns,
  ending on the call that closes it."
  []
  (vec (concat
        [{:role "system" :content "sys"}
         {:role "user" :content "problem"}
         {:role "assistant" :content "task claim T1" :turn 1}
         {:role "user" :content "[harness] You are now working on T1" :task-id "T1"}
         {:role "user" :content "claimed" :turn 1}]
        (mapcat (fn [t] [{:role "assistant" :content (str "call " t) :turn t}
                         {:role "user" :content (str "result " t) :turn t}])
                (range 2 5))
        [{:role "user" :content "DIR RULES" :pinned? true :instructions "a"}
         {:role "assistant" :content "task close T1" :turn 5}])))

(deftest a-closed-tasks-span-runs-from-its-statement-to-the-closing-call
  (let [msgs (task-convo)]
    (is (= [3 (dec (count msgs))] (cmp/task-span msgs "T1"))
        "the statement starts it, and the closing call stays for its result")
    (is (nil? (cmp/task-span msgs "T9")) "no statement, no span")))

(deftest a-task-fold-leaves-one-line-and-the-pinned-messages
  (let [msgs (task-convo)
        [s e] (cmp/task-span msgs "T1")
        out (cmp/fold-task msgs [s e] "T1 folded")]
    (is (= (take 3 msgs) (take 3 out)) "everything before the task is untouched")
    (is (= {:role "user" :content "T1 folded" :task-fold "T1"} (nth out 3)))
    (is (= "DIR RULES" (:content (nth out 4))) "a pinned directory file rides through")
    (is (= "task close T1" (:content (last out))) "and the closing call is still there")
    (is (= 6 (count out))))
  (testing "the turns folded, for the line to name; the closing turn stays whole"
    (is (= [1 4] (cmp/turn-range (subvec (task-convo) 3 12))))))

(deftest dispatch-folds-the-task-a-call-closed
  (cells/load-cells!)
  (let [before {:id "B1" :task {:id "T1" :title "the task"} :messages (task-convo)}
        dispatch (fn [branch-after policy]
                   (with-redefs [samizdat.agent.loop/tool-step
                                 (fn [_ _ _ _] {:branch branch-after :result {:ok "closed"} :tool "task"})
                                 gates/threshold (let [orig gates/threshold]
                                                   (fn [k] (if (= k :task-fold) policy (orig k))))]
                     (:branch ((:handler (cell/get-cell! :tool/dispatch))
                               {}
                               {:branch before :turn 5
                                :parsed {:name "task" :args {:action "close" :id "T1"}}}))))
        closed (assoc before :task nil)]
    (let [out (dispatch closed {:enabled? true :min-messages 4})]
      (is (some #(= "T1" (:task-fold %)) (:messages out)) "the span folded")
      (is (str/includes? (:content (first (filter :task-fold (:messages out)))) "T1")))
    (testing "a span under the floor is left to age out on its own"
      (is (= (:messages closed) (:messages (dispatch closed {:enabled? true :min-messages 50})))))
    (testing "switched off, nothing folds"
      (is (= (:messages closed) (:messages (dispatch closed {:enabled? false :min-messages 4})))))
    (testing "a close that did not end the held task folds nothing"
      (is (= (:messages before) (:messages (dispatch before {:enabled? true :min-messages 4})))))))

(deftest a-later-fold-can-find-the-earlier-one
  (let [msgs [{:role "user" :content "a"}
              {:role "system" :content "MARKER:first summary"}
              {:role "user" :content "b"}]
        [i body] (cmp/find-previous-summary msgs "MARKER:")]
    (is (= 1 i))
    (is (= "first summary" body)))
  (is (nil? (cmp/find-previous-summary [{:role "user" :content "a"}] "MARKER:"))))

;; --- the shipped policy ------------------------------------------------------

(deftest the-shipped-ladder-is-ordered-and-complete
  ;; A ladder whose rungs are out of order silently disables the ones above
  ;; the misplaced entry, because `tier` answers with the first match.
  (let [p (gates/threshold :compaction)
        l (:ladder p)]
    (is (some? l))
    (is (< (:aggressive-cap l) (:fold l) (:aggressive-fold l) (:force l) (:turn-start l))
        "ascending pressure, or the higher rungs are unreachable")
    (is (every? #(< 0.0 % 1.0) (vals l)) "every rung is a fraction of the window")
    (testing "and the aggressive tail is tighter than the normal one — that is
              what makes an aggressive fold aggressive"
      (is (< (:aggressive-tail p) (:protect-tail p))))
    (testing "the summary floor is below its ceiling"
      (is (< (:floor (:summary p)) (:ceiling (:summary p)))))))

;; --- what the journal records -------------------------------------------------

(deftest a-cap-records-the-pressure-that-triggered-it
  ;; CAP IS THE MOST-USED RUNG AND WAS THE ONLY ONE WHOSE TRIGGER WENT
  ;; UNRECORDED. Run 623ccae8 capped 153 times (86,772 tokens freed) and
  ;; 69880d84 fourteen more, all noting {:tier :action :freed} and never the
  ;; pressure — while fold, which fires far less often, recorded :before and
  ;; :after all along.
  ;;
  ;; The cost is not hypothetical. Reading 69880d84 back, aggressive-cap fires
  ;; while max(prompt_tokens) is 22,905 against a 128,000 window whose
  ;; aggressive rung is 76,800. Either one oversized result spiked the measure
  ;; and was clipped back down — the first cap freed 17,999 tokens in one go —
  ;; or the ladder is miscalibrated. The number that decides it is the one
  ;; that was not written down (karamazov-be8).
  (cells/load-cells!)
  (let [notes (atom [])
        big (apply str (repeat 40000 "x"))
        data {:compaction/tier :aggressive-cap
              :compaction/ratio 0.83
              :compaction/before 61000
              :branch {:messages (into [{:role "user" :content "go"}]
                                       (repeat 8 {:role "user" :content big}))}}]
    (with-redefs [samizdat.store.journal/note!
                  (fn [_ _ kind m] (swap! notes conj (assoc (:data m) ::kind kind)) nil)]
      ((:handler (cell/get-cell! :compaction/cap))
       {:conn ::conn :run-id "r1"
        :llm-config {:context-window 128000}}
       data))
    (let [n (first (filter #(= "cap" (:action %)) @notes))]
      (is (some? n) "the cap noted something")
      (is (= 61000 (:before n))
          "the pressure it measured, so the ladder's calibration is auditable
           from the journal instead of by inference")
      (is (= 0.83 (:ratio n))
          "and the fraction of the window that pressure was")
      (testing "without losing what it already recorded"
        (is (= "aggressive-cap" (:tier n)))
        (is (pos? (:freed n)))))))

;; --- the routing lives in the manifest, not in a cell ------------------------

(deftest the-ladder-routes-through-the-manifest
  ;; THE POINT OF THE WHOLE CHANGE. A single cell that measured the pressure
  ;; and then did all four things internally is the same hardcoding moved out
  ;; one level: the manifest shows one opaque box and nothing about the policy
  ;; is rewireable. The rule roles/supervisor.md states is that a dispatch
  ;; predicate READS a key a cell wrote and never computes one.
  (cells/load-cells!)
  (doseq [m ["worker" "loop" "supervisor" "reviewer"]]
    (let [d (manifests/read-definition (manifests/manifest-body! m))]
      (testing (str m " has each rung as its own node")
        (is (= :compaction/measure (get-in d [:cells :measure])) m)
        (is (= :compaction/cap (get-in d [:cells :cap])) m)
        (is (= :compaction/prune (get-in d [:cells :prune])) m)
        (is (= :compaction/fold (get-in d [:cells :fold])) m))
      (testing (str m " routes on a key rather than an inline decision")
        (is (= {:cap :cap :none :infer} (get-in d [:edges :measure])) m)
        (is (= {:prune :prune :none :infer} (get-in d [:edges :cap])) m))
      (testing (str m " starts the turn by measuring, and ends the ladder at infer")
        (is (= :measure (get-in d [:edges :start])) m)
        (is (= :infer (get-in d [:edges :fold])) m)))))

(deftest the-dispatch-patterns-only-read-the-route
  ;; A predicate that computed the tier would put the routing back inside code
  ;; and leave the manifest describing something that is not what runs. The
  ;; tables are patterns now (karamazov-aqsr.4), which cannot compute at all;
  ;; what this pins is that they read the ROUTE key and nothing else.
  (cells/load-cells!)
  (let [d (manifests/read-definition (manifests/manifest-body! "worker"))
        entries (concat (get-in d [:dispatches :measure]) (get-in d [:dispatches :cap]))]
    (is (seq entries))
    (doseq [[_label pattern] entries]
      (is (or (= '_ pattern)
              (and (map? pattern)
                   (= [:compaction/route] (keys pattern))
                   (keyword? (:compaction/route pattern))))
          (str "a dispatch pattern must read :compaction/route, not compute: "
               (pr-str pattern))))))

(deftest measure-writes-the-route-and-touches-nothing
  (cells/load-cells!)
  (let [msgs [{:role "system" :content "sys"} {:role "user" :content (apply str (repeat 40000 "x"))}]
        run (fn [window]
              ((:handler (cell/get-cell! :compaction/measure))
               {:llm-config {:context-window window}}
               {:branch {:messages msgs}}))]
    (testing "an unknown window routes :none — every rung is a fraction of it"
      (is (= :none (:compaction/route (run nil))))
      (is (nil? (:compaction/tier (run nil)))))
    (testing "pressure past the first rung routes to the cheapest action"
      (let [out (run 12000)]
        (is (= :cap (:compaction/route out)))
        (is (some? (:compaction/tier out)))))
    (testing "and it rewrites no messages — measuring and acting are separate"
      (is (= msgs (get-in (run 12000) [:branch :messages]))))))

(deftest a-pruned-result-is-keyed-by-the-tool-that-produced-it
  ;; The loop stamps :tool on a tool-result message for exactly this. Without
  ;; it every pruned result falls to the generic template, which carries the
  ;; one thing worth nothing: a preview of output nobody will read.
  ;;
  ;; Enough messages that the protected tail does not claim them all — which
  ;; is itself worth pinning, since a short conversation must prune NOTHING.
  (cells/load-cells!)
  (let [long-out (apply str (repeat 900 "x"))
        head (fn [tool body] {:role "user" :tool tool :content body})
        msgs (into [(head "shell" (str "jolt -M:test\n" long-out))
                    (head "grep" long-out)
                    {:role "user" :content long-out}]
                   (repeat 6 {:role "user" :content "recent"}))
        out (get-in ((:handler (cell/get-cell! :compaction/prune))
                     {} {:branch {:messages msgs}})
                    [:branch :messages])]
    (is (str/starts-with? (:content (nth out 0)) "[shell] ran `jolt -M:test`")
        "a shell result's useful line is the command it ran")
    (is (str/starts-with? (:content (nth out 1)) "[grep]"))
    (is (str/includes? (:content (nth out 1)) "matches"))
    (is (str/starts-with? (:content (nth out 2)) "[]")
        "an unstamped message still prunes, on the generic line")
    (is (every? #(= "recent" (:content %)) (take-last 6 out))
        "the protected tail is untouched")
    (is (= :fold (:compaction/route
                  ((:handler (cell/get-cell! :compaction/prune))
                   {} {:branch {:messages msgs}}))))))

(deftest a-short-conversation-prunes-nothing
  ;; The protected tail claims every message, which is correct: there is no
  ;; older output to remove and clipping the live exchange would be the one
  ;; thing compaction must never do.
  (cells/load-cells!)
  (let [msgs (vec (repeat 4 {:role "user" :tool "shell" :content (apply str (repeat 900 "x"))}))
        out (get-in ((:handler (cell/get-cell! :compaction/prune))
                     {} {:branch {:messages msgs}})
                    [:branch :messages])]
    (is (= msgs out))))

(deftest the-ladder-has-a-window-to-measure-against
  ;; IT WAS BUILT INERT. Every rung is a fraction of the model's context
  ;; window and nothing in the config set one, so `measure` routed :none on
  ;; every turn and no fold could ever happen — the whole ladder ran and did
  ;; nothing, which is the shape of every bug this project keeps finding.
  (doseq [p [:glm :deepseek :openai :local :ollama]]
    (let [w (:context-window (get config/providers-for-test p))]
      (is (and w (pos? w)) (str "provider " p " must declare a context window"))))
  (testing "and it reaches the :llm config the loop hands to compaction"
    (is (pos? (:context-window (:llm (config/load-config {:llm {:provider :glm}})))))))

(deftest a-compaction-note-names-the-branch-and-the-turn
  ;; Every rung noted WHAT it did and none noted WHERE: {:data …} with no
  ;; :branch-id and no :turn, so a fold could not be joined to the cache miss
  ;; it caused. On endless-flight (GLM-5.3, 2026-09-13) 249 caps and 11
  ;; folds were unjoinable to any of the 33 low-hit turns (karamazov-o4wm.1).
  (cells/load-cells!)
  (let [notes (atom [])
        big (apply str (repeat 40000 "x"))
        data {:compaction/tier :aggressive-cap
              :compaction/ratio 0.83
              :compaction/before 61000
              :turn 4
              :branch {:id "B7"
                       :messages (into [{:role "user" :content "go"}]
                                       (repeat 8 {:role "user" :content big}))}}]
    (with-redefs [samizdat.store.journal/note!
                  (fn [_ _ kind m] (swap! notes conj (assoc m ::kind kind)) nil)]
      ((:handler (cell/get-cell! :compaction/cap))
       {:conn ::conn :run-id "r1" :llm-config {:context-window 128000}}
       data))
    (let [n (first (filter #(= "cap" (get-in % [:data :action])) @notes))]
      (is (some? n))
      (is (= "B7" (:branch-id n)))
      (is (= 4 (:turn n))))))

(deftest a-framed-shell-result-still-prunes-to-its-command
  ;; karamazov-o4wm.3: the frame's opening tag is the first line of every
  ;; tool result now, and the shell template's `first-line` is the command.
  ;; The summariser reads the body, not the frame.
  (cells/load-cells!)
  (let [long-out (apply str (repeat 900 "x"))
        msgs (into [{:role "user" :tool "shell"
                     :content (samizdat.llm.message/frame-result
                               "shell" (str "jolt -M:test\n" long-out))}]
                   (repeat 6 {:role "user" :content "recent"}))
        out (get-in ((:handler (cell/get-cell! :compaction/prune))
                     {} {:branch {:messages msgs}})
                    [:branch :messages])]
    (is (str/starts-with? (:content (nth out 0)) "[shell] ran `jolt -M:test`")
        "the command, not the frame's opening tag")))

;; --- the fold's role (karamazov-fp21.2) --------------------------------------

(deftest a-fold-can-stand-as-a-user-turn-where-the-template-refuses-a-system-one
  ;; Qwen3.5's chat template enforces "System message must be at the
  ;; beginning" and 500s the next call on any system role mid-conversation.
  ;; The fold marker IS one. The role is a parameter, and a later fold finds
  ;; the marker whichever role carried it.
  (let [msgs (convo 12)
        out (cmp/apply-summary msgs [2 8] "MARKER:" "the summary" "user")]
    (is (= "user" (:role (nth out 2))))
    (is (= "MARKER:the summary" (:content (nth out 2))))
    (is (:compaction? (nth out 2))))
  (testing "the default is still a system message"
    (is (= "system" (:role (nth (cmp/apply-summary (convo 12) [2 8] "M:" "s") 2)))))
  (testing "a later fold finds a user-role marker as it finds a system one"
    (let [msgs [{:role "user" :content "a"}
                {:role "user" :content "MARKER:first"}
                {:role "user" :content "b"}]]
      (is (= [1 "first"] (cmp/find-previous-summary msgs "MARKER:")))))
  (testing "an assistant turn is never mistaken for a marker"
    (is (nil? (cmp/find-previous-summary
               [{:role "assistant" :content "MARKER:no"}] "MARKER:")))))

(deftest the-fold-cell-takes-its-role-from-the-probe-unless-policy-overrides
  ;; gates.edn :fold-role :auto reads what the startup probe learned about
  ;; the endpoint's template (llm-config :fold-role); an explicit "system" or
  ;; "user" is the operator's word over the probe's.
  (cells/load-cells!)
  (let [big (apply str (repeat 3000 "x"))
        msgs (into [{:role "system" :content "sys"} {:role "user" :content "go"}]
                   (mapcat (fn [i] [{:role "assistant" :content (str "call " i)}
                                    {:role "user" :content big}])
                           (range 8)))
        data {:compaction/tier :fold :compaction/before 999999
              :branch {:messages msgs}}
        fold (fn [llm-config]
               (with-redefs [samizdat.store.journal/note! (fn [& _] nil)
                             samizdat.llm.client/chat
                             (fn [& _] {:content "## Active Task\nFinish it.\n## Goal\nShip.\n## Completed Actions\nRead a.\n## Active State\nGreen."})
                             samizdat.agent.compaction/validate-summary (fn [& _] true)
                             samizdat.store.knowledge/distil-session! (fn [& _] 0)]
                 (let [out ((:handler (cell/get-cell! :compaction/fold))
                            {:conn ::conn :run-id "r1"
                             :llm-adapter ::adapter :llm-config llm-config}
                            data)]
                   (some #(when (:compaction? %) (:role %))
                         (get-in out [:branch :messages])))))]
    (testing "no probe result and :auto — the system role, as before"
      (is (= "system" (fold {:context-window 128000}))))
    (testing "the probe said the template refuses a mid-conversation system role"
      (is (= "user" (fold {:context-window 128000 :fold-role "user"}))))
    (testing "a pinned message inside the window survives the fold and is not summarised"
      (let [task {:role "user" :content "PINNED TASK STATEMENT" :pinned? true}
            msgs' (assoc msgs 7 task)
            sent (atom nil)
            out (with-redefs [samizdat.store.journal/note! (fn [& _] nil)
                              samizdat.llm.client/chat
                              (fn [& args]
                                (reset! sent (pr-str args))
                                {:content "## Active Task\nFinish it.\n## Goal\nShip.\n## Completed Actions\nRead a.\n## Active State\nGreen."})
                              samizdat.agent.compaction/validate-summary (fn [& _] true)
                              samizdat.store.knowledge/distil-session! (fn [& _] 0)]
                  ((:handler (cell/get-cell! :compaction/fold))
                   {:conn ::conn :run-id "r1"
                    :llm-adapter ::adapter :llm-config {:context-window 128000}}
                   (assoc-in data [:branch :messages] msgs')))
            out-msgs (get-in out [:branch :messages])]
        (is (some :compaction? out-msgs) "the fold happened")
        (is (some #(= task %) out-msgs) "the pinned message is still there, whole")
        (is (not (str/includes? @sent "PINNED TASK STATEMENT"))
            "and the summarizer never saw it, so it is not duplicated in the summary")))
    (testing "an explicit policy value wins over the probe"
      (with-redefs [gates/threshold (let [orig gates/threshold]
                                      (fn [k] (if (= k :fold-role) "system" (orig k))))]
        (is (= "system" (fold {:context-window 128000 :fold-role "user"})))))))
