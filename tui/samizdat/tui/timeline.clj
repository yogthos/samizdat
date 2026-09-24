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

(ns samizdat.tui.timeline
  "A branch's conversation as one timeline of who said what.

  A run is not a chat between two parties. The person states the problem
  and steers; the agent speaks and calls tools; the critic scores it; the
  supervisor watches the whole run and intervenes; the harness notes what
  it did. Each is a ROLE, and the conversation pane draws each in its own
  voice, dirge-style (`<you>`, `<agent>`, `<critic>` …), in the order it
  happened.

  Pure: the entries are built from the view state and the settings in
  tui.edn `:conversation` — which issuer is which role, which journal notes
  are shown and as whom, and where in a note's data its words are."
  (:require ;; the java.time.* host shim, before data.json — see samizdat.store.journal
            [jolt.time]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [samizdat.llm.fence :as fence]))

(defn- parse [s]
  (if (string? s)
    (try (json/read-str s :key-fn keyword) (catch Throwable _ s))
    s))

(defn- banner-arg
  "The one argument a tool call is known by — its path, command, pattern —
  named per tool in the settings, else the first string argument."
  [tool args by-tool]
  (let [a (parse args)]
    (when (map? a)
      (let [k (get by-tool tool)
            v (if k (get a (keyword k)) (some #(when (string? %) %) (vals a)))]
        (some-> v str not-empty)))))

(defn- failed? [t]
  (contains? #{"failure" "mechanics"} (str (:category t))))

(defn- words
  "`s` split into what is said and what was thought: the prose with call
  markup and reasoning blocks removed (fence/prose), and the reasoning those
  blocks held joined onto `reasoning`.

  Joined only when they differ: a provider that returns reasoning_text AND
  leaves the same words in a <think> block drew every paragraph of the fold
  twice (karamazov-mu66)."
  [s reasoning]
  {:say (fence/prose s)
   :thinking (let [parts (remove str/blank? [(str reasoning) (fence/reasoning-of s)])]
               (str/join "\n" (if (and (= 2 (count parts)) (apply = (map str/trim parts))) (take 1 parts) parts)))})

(defn- turn-entries
  "One turn row as its entries. `tid` is the row's name in keys: its turn
  number, suffixed `.2`, `.3` … when the branch has used that number before."
  [t tid text banner]
  (let [n (:turn t)
        at (str (:created_at t))
        {:keys [say thinking]} (words (:assistant_text text) (:reasoning_text text))
        base {:role :agent :turn n :turn-key tid :at at}]
    (cond-> []
      (not (str/blank? thinking))
      (conj (assoc base :key (str "t" tid "/thinking") :kind :thinking :text thinking))

      (not (str/blank? say))
      (conj (assoc base :key (str "t" tid "/say") :kind :say :text say))

      :always
      (conj (assoc base :key (str "t" tid "/tool") :kind :tool
             :tool (str (:tool_name t))
             :arg (banner-arg (str (:tool_name t)) (:args t) banner)
             :result (str (:result t))
             :failed? (failed? t)
             :turn-row t)))))

(defn- all-turn-entries
  "Every turn row's entries, in the order the branch wrote them.

  A turn NUMBER is not a turn: runs before karamazov-pefk numbered each
  supervisor pass from 1 on the one SUP branch, 3067 rows under 233 numbers.
  The first row with a number keeps the plain key, so a branch that never
  repeats one is keyed as it always was; a later row with that number is
  `n.2`, `n.3` — stable as the branch grows, because rows only append. Its
  prose is the first row's alone: the per-turn endpoint answers a number
  with the first row that has it, and that text is not the others'."
  [turns text banner]
  (loop [[t & more] turns seen {} out (transient [])]
    (if-not t
      (persistent! out)
      (let [n (:turn t)
            k (inc (get seen n 0))
            tid (if (= 1 k) (str n) (str n "." k))]
        (recur more (assoc seen n k)
               (reduce conj! out (turn-entries t tid (when (= 1 k) (get text n)) banner)))))))

(defn- steer-text [{:keys [kind payload]}]
  (let [p (parse payload)
        words (cond (map? p) (or (:text p) (:message p) (pr-str p))
                    :else (str p))]
    (if (= "message" (str kind)) words (str "[" kind "] " words))))

(defn- steer-entries [interventions branch-id issued-by]
  (for [i interventions
        :when (or (nil? (:branch_id i)) (= branch-id (:branch_id i)))]
    {:key (str "i" (:id i)) :role (get issued-by (str (:issued_by i)) :user) :kind :say
     :at (str (:created_at i)) :text (steer-text i)
     :pending? (= "pending" (str (:status i)))}))

(defn- note-text [data say]
  (let [v (when (seq say) (get-in data (mapv keyword say)))]
    (if (and v (not (str/blank? (str v))))
      (str v)
      (pr-str data))))

(defn- note-entries [notes by-kind]
  (for [n notes
        :let [{:keys [role say]} (get by-kind (str (:kind n)))]
        :when role
        :let [w (words (note-text (parse (:data n)) say) nil)
              base {:role role :at (str (:created_at n)) :note (str (:kind n))}]
        e [(when-not (str/blank? (:thinking w))
             (assoc base :key (str "n" (:id n) "/thinking") :kind :thinking :text (:thinking w)))
           (when-not (str/blank? (:say w))
             (assoc base :key (str "n" (:id n)) :kind :say :text (:say w)))]
        :when e]
    e))

(defn- live-entries
  "The reply the branch is writing right now, as it streams in: after
  everything else, marked :live?, and gone when its turn row lands."
  [{:keys [text reasoning]}]
  (let [{:keys [say thinking]} (words text reasoning)]
    (cond-> []
      (not (str/blank? thinking))
      (conj {:key "live/thinking" :role :agent :kind :thinking :text thinking :live? true})
      (not (str/blank? say))
      (conj {:key "live/say" :role :agent :kind :say :text say :live? true}))))

(defn- history
  "Everything but the reply being streamed, oldest first — ending with the
  run's answer when it has one, because that is what the person asked for.
  A finished run's answer used to be only a truncated claim in a side panel
  (karamazov-ttrn)."
  [problem turns text interventions branch-id notes local-notes answer
   {:keys [issued-by banner] by-kind :notes}]
  (let [later (sort-by :at
                       (concat (all-turn-entries turns text banner)
                               (steer-entries interventions branch-id issued-by)
                               (note-entries notes by-kind)
                               ;; What this TUI printed: command output, /help.
                               (for [n local-notes]
                                 {:key (:key n) :role :system :kind :say
                                  :at (:at n) :text (:text n)})))]
    (cond-> (vec (cond->> later
                   (not (str/blank? (str problem)))
                   (cons {:key "problem" :role :user :kind :say :at "" :text (str problem)})))
      (not (str/blank? (str answer)))
      (conj {:key "answer" :role :agent :kind :say :final? true :text (str answer)}))))

;; The last answer, and what it was made from. Every frame asks — a keystroke
;; is a frame — and rebuilding it from every turn row was 108ms of a 170ms
;; frame on a 3067-turn branch, for a vector that had not changed: typing
;; only moves :input (karamazov-iimi). One slot is enough, since one
;; conversation is drawn. Compared with `=`, which is `identical?` first and
;; so costs nothing while the state holds the same values.
(defonce ^:private last-history (atom nil))

(defn- history-of [state settings]
  (let [in [(get-in state [:detail :run :problem])
            (get-in state [:branch :turns])
            (:turn-text state)
            (get-in state [:detail :interventions])
            (:branch-id state)
            (get-in state [:branch :notes])
            (:local-notes state)
            (get-in state [:detail :run :final_answer])
            settings]
        [held es] @last-history]
    (if (and held (= held in))
      es
      (let [es (apply history in)]
        (reset! last-history [in es])
        es))))

(defn entries
  "The branch on screen as a vector of entries, oldest first:
  {:key :role :kind :at …} with :kind one of :say (a line in a role's voice,
  :text), :thinking (:text) or :tool (:tool :arg :result :failed? :turn
  :turn-key). Keys are stable as the story grows, so a fold or a scroll
  anchor holds.

  The same vector, not an equal one, while nothing it is made from has
  changed; the reply being streamed is laid on top of it, which is cheap."
  [state settings]
  (let [es (history-of state settings)
        live (live-entries (get-in state [:live (:branch-id state)]))]
    (if (seq live) (into es live) es)))

(defn fold-id
  "The id of the fold an entry's body would sit behind."
  [e]
  (case (:kind e)
    :thinking (:key e)
    :tool (str (:key e) "/more")
    nil))

(defn fold-ids
  "The folds the conversation actually draws, in order: every thinking, and
  every result longer than `:result-lines`."
  [es {:keys [result-lines]}]
  (keep (fn [e]
          (case (:kind e)
            :thinking (fold-id e)
            :tool (when (> (count (re-seq #"\n" (:result e))) (dec (or result-lines 4)))
                    (fold-id e))
            nil))
        es))
