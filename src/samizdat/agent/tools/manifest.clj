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

(ns samizdat.agent.tools.manifest
  "Manifest management. The agentic loop is a named, versioned workflow in the
  workflows table, and there can be many of them — the factory `loop` beside a
  more sophisticated one. This tool lists them, shows one, and saves a tuned or
  brand-new manifest. A save must COMPILE the way the loader will before it is
  stored, so a manifest that cannot run cannot be saved.

  Which manifest a run drives is chosen by config (:run :loop / HARNESS_LOOP /
  a project's .samizdat/config.edn). Tuning the active manifest is picked up on
  the next run, because the loader loads the latest stored version. Validation
  goes through mycelium + the cell registry directly rather than the workflow
  loader, to keep this tool out of the loop-driver's require graph.

  `patch` is the edit that names only what changes (karamazov-rnnc, mycelium
  #57/#58): a list of structural ops — rename a node, add one, rewire an edge
  — applied to the stored text, compiled the way a save is, and written back
  OVER the original text so its comments survive. A save re-emits the whole
  file, and a local model re-emitting a 128-line manifest it read a few turns
  earlier drops the prose and the odd edge on the way. `refs` shows where a
  node is named before such an edit, `diff` what one did after."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [mycelium.patch :as patch]
            [samizdat.agent.tools.base :as base]
            [samizdat.manifests :as manifests]
            [samizdat.prompt :as prompt]
            [samizdat.store.userspace :as us]
            [samizdat.symbolic.dispatch :as dispatch]))

(defn- validate!
  "Compile the definition EXACTLY the way load-loop! will: cells loaded,
  composed sub-loops registered through the userspace seam, ctx-key requires
  checked, and the :constraints derived from the enforced invariants. Throws
  on any error. The tool used to run a bare pre-compile that skipped the
  last two, so a manifest that could not run could still be saved — and then
  threw out of load-loop! at the next run start (karamazov-blt.6)."
  [edn-text]
  (manifests/compile-loop (manifests/read-definition edn-text))
  true)

(defn- refused
  "The complaint for a throwable out of validate!.

  A pattern refusal — a shadowed branch, a pattern that is not a map, a
  guard the engine does not know — renders through its template, which
  carries the pattern rules: the author here is the model, and the engine's
  message alone names a language it has never been shown. Anything else is
  its message as before.

  `(or (ex-message e) (str e))`, not `(ex-message e)`: a bare
  NullPointerException or RuntimeException has a nil message, and a nil
  complaint would take the else branch and STORE the broken manifest — the
  invariant this tool exists for, inverted by the one throwable that says
  nothing. mutation.clj uses the same idiom in both its rollback paths."
  [e]
  (let [message (or (ex-message e) (str e))]
    (if-let [d (dispatch/refusal e)]
      (prompt/render "dispatch-refused"
                     {:complaint message
                      :instead (some-> (:instead d) pr-str)})
      message)))

(defn- form-head
  "A form as the reader will recognise it without its body: `(fn [d] …)`.
  A function object has no text; it is named as what it is."
  [form]
  (cond
    (fn? form) "a compiled function"
    (seq? form) (str "(" (str/join " " (map pr-str (take 2 form))) " …)")
    :else (pr-str form)))

(defn- dispatch-report
  "What the analysis found and did not refuse, rendered for the author
  looking at the table — or nil when there is nothing to say. Two
  paragraphs, each present only when it applies:

  Where the order is the only thing deciding. Two branches that overlap
  with neither more specific are legal (the loop's own :parse table has
  them), so this is a report and not a refusal.

  Which entries were never analysed. A (fn [d] ...) form is legal and
  opaque — checked for neither shadowing nor order — and a table with three
  of them and no overlapping patterns used to show clean and read as checked
  through. The escape hatch is disclosed every time it is used, so that a
  report with no such paragraph means every entry was analysed
  (karamazov-viht.2).

  The moment to say either is when the manifest is saved or shown, not on
  every run that compiles it."
  [edn-text]
  (let [{:keys [order-dependent opaque]}
        (dispatch/report (:dispatches (manifests/read-definition edn-text)))]
    (when (or (seq order-dependent) (seq opaque))
      (str (when (seq order-dependent)
             (str "\n\n"
                  (prompt/render
                   "dispatch-order"
                   {:pairs (str/join "\n"
                                     (for [{:keys [cell labels patterns]} order-dependent]
                                       (str "  " cell " — " (first labels) " "
                                            (pr-str (first patterns)) " before "
                                            (second labels) " " (pr-str (second patterns)))))})))
           (when (seq opaque)
             (str "\n\n"
                  (prompt/render
                   "dispatch-opaque"
                   {:entries (str/join "\n"
                                       (for [{:keys [cell label form]} opaque]
                                         (str "  " cell " — " label " " (form-head form))))})))))))

(defn- cycle-report
  "Every cycle of the manifest that cannot change its own exit — no
  dispatch on it with an edge out reads a key a cell on it promises
  (manifests/unguarded-cycles) — rendered for the author, or nil. A warning
  and not a refusal: the compile carried it as :unguarded-cycle and stored
  the manifest anyway. Rendered on save and patch, after the compile that
  loaded the cells whose :output it reads; the log line the compile writes
  is not where the author of an edit is looking (karamazov-viht.4)."
  [edn-text]
  (when-let [cycles (seq (manifests/unguarded-cycles (manifests/read-definition edn-text)))]
    (str "\n\n"
         (prompt/render
          "cycle-unguarded"
          {:cycles (str/join "\n"
                             (for [{:keys [cycle reads produces]} cycles]
                               (str "  " (str/join " → " (map str cycle)) " → " (first cycle)
                                    " — its exits read " (pr-str (vec (sort reads)))
                                    ", its cells promise " (pr-str (vec (sort produces))))))}))))

(def ^:private usage
  "Actions: list, show {name, version?}, save {name, edn | file, rationale}, patch {name, ops, rationale, expect-version?}, refs {name, cell}, diff {name, from?, to?}. A manifest is the loop as data — a :cells map, :edges, and dispatch patterns. Save and patch validate by compiling before they store; the run that uses it is chosen by config :run :loop. Show, save and patch report where only branch order decides and name every dispatch entry written as a form, which the analysis cannot read. Prefer patch to save for an edit: it names the change and keeps the rest of the file, comments included. rationale: one sentence on why — the history shows it to the next supervisor deciding whether your change stays.")

(defn- deflag
  "The vendored op refusals speak the CLI's `--rewire`; here an argument is a
  map key, so say `:rewire`."
  [s]
  (str/replace (str s) #"--([a-z-]+)" ":$1"))

(defn- say
  "A sentence of this tool's from its template, keyed by reason."
  [reason & {:as vars}]
  (prompt/render "manifest-tool" (assoc vars reason true)))

(defn- ops-help
  "The patch ops for the retry to read: the words are the template's, the
  argument list of each op is the registry's (optional ones marked ?), so
  the vocabulary cannot drift from what coerce-op accepts."
  []
  (say :ops-help
       :args (into {}
                   (for [[op-name {:keys [args]}] (patch/ops)]
                     [(keyword op-name)
                      (str "{" (str/join ", " (for [{:keys [name required?]} args]
                                                (str (clojure.core/name name)
                                                     (when-not required? "?"))))
                           "}")]))))

(defn- saved-line
  "What a successful store tells the model, for save and patch alike."
  [name v]
  (say :saved :name name :version v))

(defn- current
  "The manifest an edit or a read starts from: the newest stored row, else
  the factory template through the userspace seam (which seeds it as v1 when
  a project is bound — karamazov-blt.4, the same fallback `show` makes), else
  nil. The template alone has no version."
  [conn name]
  (or (us/load-latest conn :manifest name)
      (when-let [body (manifests/manifest-body name)]
        (or (us/load-latest conn :manifest name)
            {:body body :version nil}))))

(defn- read-ops
  "The `ops` argument as op maps ready for patch/apply-ops: a JSON list of
  objects (keys keywordized on the way in, values strings) or one EDN string.
  Each op's string arguments are read the way the op's registry entry
  declares — a cell name to a keyword, an edge map from EDN. Returns
  {:ops [...]} or {:error complaint}."
  [raw]
  (try
    (let [ops (if (string? raw) (edn/read-string raw) raw)
          ;; one op sent bare is one op, not a shape error
          ops (if (map? ops) [ops] ops)]
      (cond
        (not (sequential? ops))
        {:error (say :ops-not-a-list)}

        (not (every? map? ops))
        {:error (say :ops-not-maps)}

        :else
        {:ops (mapv (fn [op] (patch/coerce-op (update op :op #(some-> % name)))) ops)}))
    (catch Throwable e
      {:error (say :ops-unreadable :reason (or (ex-message e) (str e)))})))

(defn- cell-id
  "A manifest cell's handler id, whichever way it is written."
  [d]
  (if (map? d) (:id d) d))

(defn- format-refs
  [cell refs]
  (str cell " — " (count refs) " reference" (when (not= 1 (count refs)) "s")
       (when (seq refs) "\n")
       (str/join "\n" (map (fn [{:keys [role path]}]
                             (format "  %-20s %s" (name role) (str/join " " (map pr-str path))))
                           refs))))

(defn- format-diff
  "patch/diff-manifests rendered for the reader: + added, - removed, ~ changed,
  per section."
  [{:keys [same? cells edges dispatches sections]} a b]
  (if same?
    "no differences"
    (let [section (fn [title {:keys [added removed changed]} fmt-added fmt-removed fmt-changed]
                    (when (or (seq added) (seq removed) (seq changed))
                      (str title ":\n"
                           (str/join "\n" (concat (map #(str "  + " (fmt-added %)) added)
                                                   (map #(str "  - " (fmt-removed %)) removed)
                                                   (mapcat fmt-changed changed)))
                           "\n")))]
      (str/trimr
       (str (section "cells" cells
                     (fn [k] (str k " (" (pr-str (cell-id (get-in b [:cells k]))) ")"))
                     (fn [k] (str k " (" (pr-str (cell-id (get-in a [:cells k]))) ")"))
                     (fn [[k fields]] (map (fn [[f [o n]]] (str "  ~ " k " " f " " (pr-str o) " → " (pr-str n))) fields)))
            (section "edges" edges
                     (fn [k] (str k " → " (pr-str (get-in b [:edges k]))))
                     (fn [k] (str k " → " (pr-str (get-in a [:edges k]))))
                     (fn [[k [o n]]] [(str "  ~ " k " " (pr-str o) " → " (pr-str n))]))
            (section "dispatches" dispatches
                     pr-str pr-str
                     (fn [[k _]] [(str "  ~ " k)]))
            (when (seq sections)
              (str "other:\n"
                   (str/join "\n" (map (fn [[k [o n]]] (str "  ~ " k " " (pr-str o) " → " (pr-str n))) sections))
                   "\n")))))))

(defn- version-arg [ctx k]
  (some-> (base/arg ctx k) str str/trim not-empty parse-long))

(defn- render-list
  "Every manifest this project can run: the stored rows PLUS the shipped
  templates not yet seeded. Store-only listing hid a factory manifest until
  its first run seeded it — `manifest show worker` before any worker run
  answered \"No manifest worker\", so the agent could not read the thing it
  is invited to tune (karamazov-blt.4)."
  [conn]
  (let [rows (us/names conn :manifest)
        stored (set (map :name rows))
        unseeded (for [nm manifests/shipped-manifests
                       :when (and (not (stored nm))
                                  (io/resource (manifests/manifest-resource nm)))]
                   (str nm "  [factory template, unseeded]"))]
    (str/join "\n"
              (concat
               (for [{:keys [name version versions]} rows]
                 (str name "  v" version " (" versions
                      (if (= 1 versions) " version)" " versions)")
                      (when (io/resource (manifests/manifest-resource name))
                        "  [factory]")))
               unseeded))))

(defmethod base/run-tool "manifest" [{:keys [branch conn] :as ctx}]
  (let [action (some-> (base/arg ctx :action) str str/trim str/lower-case not-empty)]
    (try
      (case action
        nil
        (base/malformed branch (str "`manifest` needs an `action`. " usage))

        "list"
        (base/ok branch (render-list conn))

        "show"
        (let [name (base/arg ctx :name)
              v (some-> (base/arg ctx :version) str str/trim not-empty parse-long)]
          (cond
            ;; base/missing yields a complaint STRING for malformed to wrap —
            ;; bare, it dropped :category/:branch from the result map
            ;; (provenance CR1-1).
            (str/blank? (str name)) (base/malformed branch (base/missing ctx :name))
            :else
            (if-let [row (if v
                           (us/load-version conn :manifest name v)
                           (us/load-latest conn :manifest name))]
              (base/ok branch (str name " v" (:version row) ":\n\n" (:body row)
                                   (dispatch-report (:body row))))
              ;; Not stored — fall back to the userspace seam, which serves
              ;; the factory template (and seeds it as v1 when a project is
              ;; bound), so a shipped manifest is readable before any run
              ;; drives it (karamazov-blt.4). Version pinning has nothing to
              ;; pin to here, so only the latest form takes this path.
              (if-let [body (when-not v (manifests/manifest-body name))]
                (base/ok branch (str name " (factory template):\n\n" body
                                     (dispatch-report body)))
                (base/malformed branch (str "No manifest " name
                                            (when v (str " v" v)) "."))))))

        "save"
        (let [name (base/arg ctx :name)
              {edn-text :body err :error} (base/save-body ctx :edn)
              why (base/rationale ctx)]
          (cond
            (str/blank? (str name)) (base/malformed branch (base/missing ctx :name))
            err (base/malformed branch err)
            (str/blank? (str edn-text)) (base/malformed branch (base/missing ctx :edn))
            (nil? why) (base/malformed branch (base/missing ctx :rationale))
            :else
            ;; The refusal is caught HERE rather than by the outer handler, so
            ;; a manifest that does not compile — a correctable edit, and the
            ;; loop this tool exists to invite — is not billed to the branch's
            ;; failure counter. The outer catch is left for what happens AFTER
            ;; this point, notably the store write.
            (if-let [complaint (try (validate! edn-text) nil
                                    (catch Throwable e (refused e)))]
              ;; The complaint plus `usage`, which already says a save
              ;; validates before it stores — no new sentence in src/.
              (base/rejected branch
                             (str "`manifest save` refused: " complaint
                                  "\n\n" usage))
              (let [v (us/save! conn :manifest name edn-text "project" why)]
                (base/ok branch
                         (str (saved-line name v) (dispatch-report edn-text)
                              (cycle-report edn-text))
                         :progress? true)))))

        "patch"
        (let [name (base/arg ctx :name)
              raw-ops (base/arg ctx :ops)
              why (base/rationale ctx)
              expect (version-arg ctx :expect-version)
              {:keys [ops error]} (when raw-ops (read-ops raw-ops))]
          (cond
            (str/blank? (str name)) (base/malformed branch (base/missing ctx :name))
            (nil? raw-ops) (base/malformed branch (base/missing ctx :ops))
            (nil? why) (base/malformed branch (base/missing ctx :rationale))
            error (base/malformed branch (str error "\n\n" (ops-help)))
            :else
            (if-let [{:keys [body version]} (current conn name)]
              (if (and expect (not= expect version))
                ;; The store's version is the concurrency token: an edit made
                ;; against a version that is no longer the newest is told so,
                ;; not applied to whatever is there now.
                (base/rejected branch (say :stale :name name :version version :expect expect))
                (let [old (manifests/read-definition body)
                      ;; Caught HERE for the reason save's is: a refused op or
                      ;; a result that does not compile is a correctable
                      ;; edit, not evidence about the branch (karamazov-gn64).
                      outcome (try {:new (patch/apply-ops
                                          old {:ops ops
                                               :validator #(manifests/compile-loop %)})}
                                   (catch Throwable e {:complaint (refused e)}))]
                  (if-let [complaint (:complaint outcome)]
                    (base/rejected branch
                                   (str "`manifest patch` refused: " (deflag complaint)
                                        "\n\n" (ops-help)))
                    (let [new (:new outcome)
                          text (patch/render body old new)
                          v (us/save! conn :manifest name text "project" why)]
                      (base/ok branch
                               (str (saved-line name v)
                                    "\n\n" (format-diff (patch/diff-manifests old new) old new)
                                    (dispatch-report text)
                                    (cycle-report text))
                               :progress? true)))))
              (base/malformed branch (str "No manifest " name ".")))))

        "refs"
        (let [name (base/arg ctx :name)
              cell (some-> (base/arg ctx :cell) str str/trim not-empty)]
          (cond
            (str/blank? (str name)) (base/malformed branch (base/missing ctx :name))
            (nil? cell) (base/malformed branch (base/missing ctx :cell))
            :else
            (if-let [{:keys [body]} (current conn name)]
              (let [cell (patch/cell-kw cell)]
                (base/ok branch
                         (format-refs cell (patch/cell-refs (manifests/read-definition body) cell))))
              (base/malformed branch (str "No manifest " name ".")))))

        "diff"
        (let [name (base/arg ctx :name)
              from (version-arg ctx :from)
              to (version-arg ctx :to)]
          (cond
            (str/blank? (str name)) (base/malformed branch (base/missing ctx :name))
            :else
            (let [latest (us/load-latest conn :manifest name)
                  to (or to (:version latest))
                  from (or from (some-> to dec))]
              (cond
                (nil? latest)
                (base/malformed branch (say :diff-nothing-stored :name name))

                (< from 1)
                (base/malformed branch (say :diff-one-version :name name :version (:version latest)))

                :else
                (let [a (us/load-version conn :manifest name from)
                      b (us/load-version conn :manifest name to)]
                  (cond
                    (nil? a) (base/malformed branch (str "No manifest " name " v" from "."))
                    (nil? b) (base/malformed branch (str "No manifest " name " v" to "."))
                    :else
                    (let [old (manifests/read-definition (:body a))
                          new (manifests/read-definition (:body b))]
                      (base/ok branch
                               (str name " v" from " → v" to ":\n"
                                    (format-diff (patch/diff-manifests old new) old new))))))))))

        (base/malformed branch (str "Unknown manifest action `" action "`. " usage)))
      (catch Throwable e
        (base/fail branch (str "`manifest " action "` refused: " (ex-message e)
                               "\n\n" usage))))))
