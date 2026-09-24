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

(ns samizdat.layers
  "The one place a LAYERED settings file is assembled.

  Two kinds of file configure samizdat, and they are deliberately different.
  The workflow — manifests, cells, prompts, policy tables — belongs to one
  project: it is copied into `.samizdat/` on the first run and read only from
  there (samizdat.userspace). What lives HERE is the other kind: the files
  that say how a person likes to work rather than how a project works —
  `config.edn` and each front end's own (`tui.edn`, `gui.edn`, `webui.edn`).
  Those follow a person from project to project, so they layer:

    1. env        the file an environment variable names:
                  SAMIZDAT_<NAME>_FILE (plus SAMIZDAT_TUI_LAYOUT, kept)
    2. project    <root>/.samizdat/<name>.edn
    3. served     a copy handed in by the caller — the TUI's name for the
                  file the harness holds, when the harness is elsewhere
    4. global     <config-home>/samizdat/<name>.edn (~/.config by default)
    5. shipped    resources/<name>.edn, handed in by the caller

  highest first, each DEEP-MERGED over the ones below it. Maps merge key by
  key, so a global file can set one colour and a project another; anything
  else — a vector especially — is replaced whole, because a hiccup `:layout`
  merged position by position would be a tree nobody wrote.

  Any name resolves. A new front end is a new name, not a new loader.

  DEGRADE, NEVER THROW. A layer that does not read, or is not a map, is
  dropped and named in `:errors`; the rest still merge. These files are
  edited while the thing reading them runs, and a half-saved file must cost
  its own layer, not the process.

  Mechanism only: nothing here knows what any key means."
  (:refer-clojure :exclude [resolve])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- the merge ---------------------------------------------------------------

(defn deep-merge
  "Merge maps left to right, recursing when BOTH values are maps; any other
  collision is won by the later value, nil included. nil maps are skipped."
  [& ms]
  (apply merge-with (fn [a b] (if (and (map? a) (map? b))
                                (deep-merge a b)
                                b))
         ms))

;; --- where the files are -----------------------------------------------------

(defn- getenv [k]
  (let [v (jolt.host/getenv k)] (when-not (str/blank? v) v)))

(defn config-home
  "The user's config directory: $XDG_CONFIG_HOME, else ~/.config, else nil
  when neither is known. A var so a test can point it at a temp dir."
  []
  (or (getenv "XDG_CONFIG_HOME")
      (some-> (getenv "HOME") (str "/.config"))))

(defn global-dir
  "Where a person's own layer lives: <config-home>/samizdat, or nil."
  []
  (some-> (config-home) (str "/samizdat")))

(def ^:private legacy-vars
  "Variables that named a file before the general rule existed. Kept, below
  the general one, so a setup that works keeps working."
  {"tui" ["SAMIZDAT_TUI_LAYOUT"]})

(defn env-vars
  "The environment variables that may name `name`'s file, in precedence
  order: SAMIZDAT_<NAME>_FILE, then any legacy spelling."
  [name]
  (into [(str "SAMIZDAT_"
              (-> (str name) str/upper-case (str/replace #"[^A-Z0-9]+" "_"))
              "_FILE")]
        (get legacy-vars (str name))))

(defn candidates
  "The FILE layers for `name`, highest first, as {:layer :path} (plus :var
  for the env layer). Only what `opts` names is consulted — `:root`,
  `:global-dir`, `:getenv` — so a caller that has no project or no config
  home simply gets fewer layers."
  [name {:keys [root global-dir] :as opts}]
  (let [getenv (or (:getenv opts) getenv)
        file (str name ".edn")]
    (vec (concat
          (when-let [[v p] (some (fn [v] (when-let [p (not-empty (str (getenv v)))]
                                           [v p]))
                                 (env-vars name))]
            [{:layer :env :path p :var v}])
          (when root [{:layer :project :path (str root "/.samizdat/" file)}])
          (when global-dir [{:layer :global :path (str global-dir "/" file)}])))))

;; --- reading one layer -------------------------------------------------------

(defn- parse
  "`body` as {:value map}, {:error why}, or nil for a blank body — an empty
  file is somebody who has not written anything yet, not a mistake."
  [body]
  (when-not (str/blank? (str body))
    (try
      (let [v (edn/read-string (str body))]
        (if (map? v) {:value v} {:error "is not a map"}))
      (catch Throwable e
        {:error (str "did not parse: " (ex-message e))}))))

;; {path {:stamp [mtime length] :body :value}}. Stamped by mtime AND length:
;; lastModified is milliseconds and an edit saved inside one would otherwise
;; not be seen. A layer that failed to parse is NOT cached — a torn read of a
;; file mid-save must not be remembered as the answer when the finished write
;; need not move the stamp again.
(defonce ^:private file-cache (atom {}))

(defn forget-files!
  "Drop the file cache, so the next resolve goes to disk. For tests."
  []
  (reset! file-cache {})
  nil)

(defn- read-file
  "The file at `path` as {:body :value} / {:body :error}, nil when it is not
  there."
  [path]
  (let [f (io/file (str path))]
    (when (.isFile f)
      (let [stamp [(.lastModified f) (.length f)]
            c (get @file-cache path)]
        (if (= stamp (:stamp c))
          (select-keys c [:body :value])
          (try
            (let [body (slurp f)
                  p (parse body)]
              (when (and p (not (:error p)))
                (swap! file-cache assoc path (assoc p :stamp stamp :body body)))
              (assoc p :body body))
            (catch Throwable e
              {:error (str "could not be read: " (ex-message e))})))))))

;; --- the whole ---------------------------------------------------------------

(defn- layer
  "One layer read: the candidate or text layer with :body/:value, :error, or
  nil when there is nothing there."
  [{:keys [layer path text] :as src}]
  (let [src (dissoc src :text)]
    (if path
      (if-let [r (read-file path)]
        (merge src r)
        ;; An env variable is somebody meaning something: pointing it at a
        ;; file that is not there is a mistake worth a line, where a missing
        ;; project or global file is the ordinary case.
        (when (= :env layer)
          (assoc src :error (str "named by " (:var src) " but not there"))))
      (when-let [p (parse text)]
        (merge src p {:body text})))))

(defn resolve
  "The settings file `name`, assembled from every layer that has it.

  `opts`: `:root` (the project), `:global-dir`, `:getenv` (for tests),
  `:served` and `:shipped` (texts the caller holds — see the ns doc).

  Returns {:name :body :value :sources :errors}:
    :value    the merged map, nil when no layer had anything
    :body     its text — VERBATIM when one layer answered, so the comments a
              person reads survive; the printed merged value otherwise
    :sources  the layers that contributed, highest first, {:layer :path}
    :errors   the layers dropped, {:layer :path :error}"
  [name {:keys [served shipped] :as opts}]
  (let [[above below] (split-with #(not= :global (:layer %)) (candidates name opts))
        read (keep layer (concat above
                                 [{:layer :served :text served}]
                                 below
                                 [{:layer :shipped :text shipped}]))
        good (filter :value read)
        value (when (seq good) (apply deep-merge (reverse (map :value good))))]
    {:name (str name)
     :value value
     :body (cond (empty? good) nil
                 (= 1 (count good)) (:body (first good))
                 :else (pr-str value))
     :sources (mapv #(select-keys % [:layer :path :var]) good)
     :errors (vec (for [r read :when (:error r)]
                    (select-keys r [:layer :path :var :error])))}))
