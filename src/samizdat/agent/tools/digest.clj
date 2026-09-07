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

(ns samizdat.agent.tools.digest
  "read_digest — a read that returns an ANSWER instead of a page.

  Most of what a branch does with a large file is not reasoning about it: it
  pages through five screens to answer one question, and every page is
  billed at the branch's rate and resent on every following turn. This tool
  sends the files to a reader — the :reader role's model when config :run
  :role-models assigns one, the branch's own otherwise — with the question,
  and hands back only the bullets. The files never enter the branch's
  context, so a follow-up question costs the reader again and the branch
  nothing (karamazov-b76m, after Spotify's shunt).

  Mechanism only. WHEN a read should be a digest is phases.edn's rule
  (files/large-untargeted-read?), the numbers are gates.edn :digest, and
  every word the reader is told is prompts/digest.md.

  Two things stay with the branch, on purpose. Editing: a digest is not text
  to patch against — unless the branch asks for anchors, in which case every
  line the reader sees carries its `line:hash`, and a bullet that cites one
  is a valid `patch` address. And judgement: the reader finds what it is
  asked for and stops; it is not asked to debug, and what it answers enters
  the run as a claim like any other."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [jolt.fs :as fs]
            [samizdat.agent.files :as files]
            [samizdat.agent.gates :as gates]
            [samizdat.agent.tools.base :as base]
            [samizdat.config :as config]
            [samizdat.hashline :as hashline]
            [samizdat.llm.client :as llm]
            [samizdat.llm.registry :as registry]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]))

(defn- msg [vars] (prompt/render "digest-tool" vars))

(defn reader
  "The adapter and config a digest runs on: the :reader role's when one is
  assigned, else the branch's own — still a saving, of context if not of
  price."
  [{:keys [config llm-adapter llm-config]}]
  (if-let [llm (config/role-llm config llm-config :reader)]
    {:adapter (registry/adapter-for (:provider llm)) :config llm :role :reader}
    {:adapter llm-adapter :config llm-config :role :branch}))

(defn- paths-of [ctx]
  (let [p (base/arg ctx :paths)]
    (cond (nil? p) []
          (string? p) [p]
          (sequential? p) (mapv str p)
          :else [(str p)])))

(defn material
  "The files as the reader sees them: each wrapped in a <file path=…> element
  so the boundaries are unambiguous, anchored when asked, and the whole
  clipped at `cap` characters with a note saying how much was left out — the
  reader must not guess at what it did not see. {:text :chars :clipped}."
  [files anchors? cap]
  (let [render (fn [{:keys [path content]}]
                 (str "<file path=\"" path "\">\n"
                      (if anchors?
                        (hashline/render content 1 (count (hashline/content-lines content)))
                        content)
                      "\n</file>"))
        full (str/join "\n\n" (map render files))
        chars (count full)]
    (if (<= chars cap)
      {:text full :chars chars :clipped 0}
      {:text (str (subs full 0 cap) "\n" (msg {:clipped (- chars cap) :cap cap}))
       :chars chars :clipped (- chars cap)})))

(defn read-digest
  "Answer `question` about `paths` through the reader. The tool result is the
  reader's bullets, :neutral — a digest establishes nothing, like a read —
  and a :digest note on the run's journal records what the branch did not
  have to read and what the answer cost."
  [{:keys [branch root conn run-id] :as ctx}]
  (let [paths (paths-of ctx)
        question (str (base/arg ctx :question))
        refs (files/ctx-reference-roots ctx)
        policy (gates/digest-policy)]
    (cond
      (empty? paths) (base/malformed branch (msg {:needs-paths true}))
      (str/blank? question) (base/malformed branch (msg {:needs-question true}))
      :else
      (let [resolved (for [p paths]
                       {:path p :abs (files/resolve-for-read (or root ".") refs p)})
            outside (first (remove :abs resolved))
            missing (first (filter #(and (:abs %) (not (fs/exists? (:abs %)))) resolved))]
        (cond
          outside (base/malformed branch (msg {:outside-root true :path (:path outside)}))
          missing (base/malformed branch (msg {:missing true :path (:path missing)}))
          :else
          (let [anchors? (boolean (base/arg ctx :anchors))
                files (for [{:keys [path abs]} resolved] {:path path :content (slurp abs)})
                {:keys [text chars clipped]} (material files anchors? (:input-chars policy))
                {:keys [adapter config role]} (reader ctx)
                messages [{:role "system"
                           :content (prompt/render "digest" {:budget-chars (:budget-chars policy)
                                                             :anchors anchors?})}
                          {:role "user" :content (str question "\n\n" text)}]]
            (try
              (let [reply (llm/chat adapter config messages)
                    answer (str/trim (str (:content reply)))]
                (when (and conn run-id)
                  (journal/note! conn run-id :digest
                                 {:branch-id (:id branch)
                                  :data {:paths paths :chars-in chars :clipped clipped
                                         :role role :model (:model config)
                                         :usage (:usage reply)}}))
                (base/ok branch (if (str/blank? answer) (msg {:empty true}) answer)
                         :digest {:paths paths :chars-in chars :role role}))
              (catch Throwable e
                (log/warn "read_digest: the reader failed:" (ex-message e))
                (base/unavailable branch "the reader" e)))))))))

(defmethod base/run-tool "read_digest" [ctx] (read-digest ctx))
