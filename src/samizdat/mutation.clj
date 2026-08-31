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

(ns samizdat.mutation
  "The self-modification protocol: the safety around the agent editing its own
  cells.

  The agent edits a cell file, then this runs
  checkpoint -> reload -> validate -> soak -> commit or rollback. A good edit
  commits and is live on the next turn; a bad one — a syntax error, a wiring
  break, or a cell that throws on valid input — rolls the registry AND the file
  back to the last known-good state and is journaled as a negative constraint,
  so a bad edit can never brick the loop.

  This is the plan's 'edit the executor, hot-reload, revert on fault' loop, and
  autolith's journaled mutation state machine (journal -> install -> check ->
  commit -> select -> durable) with a two-tier explore-then-commit: the reload
  installs into the live image, validate + soak are the checks, and commit is
  the point of no return. Once dolt lands (karamazov-ioo.17) a commit becomes a
  dolt commit and rollback a dolt revert; today the durable record is the cell
  file on disk plus the journal."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [mycelium.cell :as cell]
            [mycelium.core :as myc]
            [samizdat.agent.gates :as gates]
            [samizdat.cells :as cells]
            [samizdat.manifests :as manifests]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]
            [samizdat.store.userspace :as store]
            [samizdat.userspace :as userspace]))

(def ^:private soak-timeout-ms 10000)

(defn- restore-files!
  "Write the checkpoint's file contents back to disk — undo the agent's edit."
  [files]
  (doseq [[path content] files]
    (spit path content)))

(defn changed-span
  "The lines `before` and `after` actually differ by, as
  `{:removed [...] :added [...] :at n}`, or nil when they are the same.

  Common prefix and suffix are dropped, so a one-line change inside a
  three-hundred-line cell records as one line and not as the file. Both spans
  are capped at `cap` lines: the record has to be small enough to always keep,
  and a diff nobody keeps is the bug this exists to fix. `:truncated` says so
  rather than letting a clipped span read as the whole change."
  [before after cap]
  (let [a (vec (str/split-lines (str before)))
        b (vec (str/split-lines (str after)))]
    (when (not= a b)
      (let [n (min (count a) (count b))
            pre (or (first (for [i (range n) :when (not= (a i) (b i))] i)) n)
            suf (or (first (for [i (range (- n pre))
                                 :let [x (- (count a) 1 i) y (- (count b) 1 i)]
                                 :when (not= (a x) (b y))]
                             i))
                    (- n pre))
            rm (subvec a pre (- (count a) suf))
            ad (subvec b pre (- (count b) suf))]
        (cond-> {:at (inc pre)
                 :removed (vec (take cap rm))
                 :added (vec (take cap ad))}
          (or (> (count rm) cap) (> (count ad) cap))
          (assoc :truncated true))))))

(defn- attempt-of
  "What the agent actually wrote, read off disk BEFORE the checkpoint is
  restored over it. One entry per changed file.

  Best effort: this runs on the failure path, and a rollback that threw while
  describing itself would trade a working recovery for a record of it."
  [files]
  (vec (keep (fn [[path original]]
               (try
                 (let [now (slurp path)]
                   (when-let [d (changed-span original now
                                              (gates/threshold :mutation-diff-lines))]
                     (assoc d :path (str path))))
                 (catch Throwable _ nil)))
             files)))

(defn- soak
  "Dry-run the loop with the edited cells to catch a cell that compiles and
  wires but throws when actually run. Effectful cells are stubbed to identity
  (the :pure/:effects marks earn their keep here) so the run does no IO, and it
  is bounded by a timeout so a cell that loops cannot hang the protocol. The
  registry is restored to its pre-soak state afterward, so the stubs never
  leak. Returns nil on success or a reason string on failure."
  [loop-def soak-input]
  (let [snap (cell/registry-snapshot)]
    (try
      ;; Stub every effectful cell the loop references.
      (doseq [[_ cell-ref] (:cells loop-def)
              :let [cell-id (if (map? cell-ref) (:id cell-ref) cell-ref)
                    spec (cell/get-cell cell-id)]
              :when (and spec (not (cell/pure? spec)))]
        (cell/register-spec! cell-id (assoc spec :handler (fn [_ d] d))))
      ;; Under the SAME :validate mode production runs under. The soak asks
      ;; "does this cell throw when actually run"; holding it to a stricter
      ;; standard than the run it is standing in for would reject an edit that
      ;; works, over a declaration the rollout has not finished tightening.
      (let [compiled (myc/pre-compile loop-def {:validate (manifests/validate-mode)})
            fut (future
                  (try {:result (myc/run-compiled compiled {:max-turns 1}
                                                  (or soak-input {}))}
                       (catch Throwable e {:error (or (ex-message e) (str e))})))
            outcome (deref fut soak-timeout-ms ::timeout)]
        (when (= ::timeout outcome)
          ;; Best effort: a looping candidate otherwise burns a thread forever
          ;; per rejected edit (blt.38). Cancellation may not interrupt a
          ;; tight loop, but a cancellable wait dies here instead of never.
          (try (future-cancel fut) (catch Throwable _ nil)))
        (cond
          (= ::timeout outcome)
          "soak did not terminate within the time budget — the edited cell may loop"

          (:error outcome)
          (str "soak run threw: " (:error outcome))

          (myc/error? (:result outcome))
          (str "soak run produced an error: "
               (:message (myc/workflow-error (:result outcome))))

          :else nil))
      (catch Throwable e
        (str "soak could not run: " (or (ex-message e) (str e))))
      (finally
        (cell/registry-restore! snap)))))

(defn- validate
  "Compile the loop definition through mycelium's static checks (structure,
  reachability, dispatch coverage, constraints). Also rejects a cell that did
  not declare its effects: the soak stubs effectful cells by their :pure/
  :effects marks, so an undeclared cell would run its side effects during the
  soak — the safety the marks exist for is void. Returns nil on success or a
  reason string."
  [compile-fn loop-def]
  (try
    (let [compiled (compile-fn loop-def)
          warnings (:mycelium/compile-warnings
                    (or (:compiled-fsm compiled) compiled))]
      (when-let [undeclared (seq (filter #(= :undeclared-effects (:type %)) warnings))]
        (str "validate: these cells do not declare :pure or :effects, so the "
             "soak cannot safely stub them: "
             (str/join ", " (map :cell-id undeclared)))))
    (catch Throwable e
      (str "validate: the loop no longer compiles — " (or (ex-message e) (str e))))))

(defn- rollback!
  [{:keys [conn run-id dirs]} {:keys [registry files]} reason]
  ;; Undo the edit on disk, restore the registry, then reload from the restored
  ;; (good) files so the loader's known-good content re-syncs. The reload of
  ;; good files cannot fail; if it somehow does, the registry restore above
  ;; already left the running system working.
  (let [attempt (attempt-of files)]
  (restore-files! files)
  (cell/registry-restore! registry)
  (try (cells/load-cells! (or dirs cells/default-dirs)) (catch Throwable _ nil))
  ;; WHAT WAS TRIED, not only that something was. Read before restore-files!
  ;; above overwrites it — the reason alone lets the next run re-derive the
  ;; same edit, which is the failure WikiSkill's skill-impact.md exists to
  ;; prevent (karamazov-mpd).
  (when (and conn run-id)
    (journal/note! conn run-id :mutation-rolled-back
                   {:data {:reason reason :attempt attempt}}))
  (log/warn "cell mutation rolled back:" reason)
  {:status :rolled-back :reason reason}))

(defn apply-cell-edit!
  "Run the mutation protocol after the agent has edited cell files on disk.

  opts:
    :dirs       — the cell dirs to load (default cells/default-dirs)
    :loop-def   — the loop workflow definition to validate + soak against
    :soak-input — the initial data map the soak dry-run starts from
    :compile-fn — how to compile+validate (default mycelium pre-compile)
    :conn :run-id — to journal the outcome (optional)

  Returns {:status :committed} on success (the edit is live), or
  {:status :rolled-back :reason \"...\"} with the registry and files restored."
  [{:keys [dirs loop-def soak-input compile-fn conn run-id] :as opts}]
  (let [dirs (or dirs cells/default-dirs)
        compile-fn (or compile-fn myc/pre-compile)
        ;; CHECKPOINT — the last known-good state to roll back to: the registry
        ;; as loaded, and the on-disk content the loader last loaded (the
        ;; pre-edit content, since nothing has reloaded the agent's edit yet).
        checkpoint {:registry (cell/registry-snapshot)
                    :files (cells/loaded-file-content)}]
    ;; REFUSE when the last load was store-mode. Its content is keyed by store
    ;; NAMES ("loop", "beam"), not paths — a rollback would spit those names as
    ;; files into the cwd, and the (cells/load-cells! dirs) below would replace
    ;; the project's store-evolved cells with templates/disk for the rest of
    ;; the run (karamazov-blt.7). The dir protocol is for a dir-mode image; a
    ;; store-mode image edits through propose-cell!.
    (if (and (seq (:files checkpoint))
             (not-any? #(str/includes? (str %) "/") (keys (:files checkpoint))))
      ;; The sentence is the tool's to render (prompts/cell-tool.md); here the
      ;; reason is data, like propose-cell!'s :unbound.
      {:status :rolled-back :reason :store-mode-image}
    (try
      ;; RELOAD — install the edit into the live image. Transactional: a syntax
      ;; error throws here and the loader has already restored the registry;
      ;; we still restore the file below.
      (cells/load-cells! dirs)
      ;; VALIDATE — does the loop still compile with the edited cells?
      (if-let [reason (validate compile-fn loop-def)]
        (rollback! opts checkpoint reason)
        ;; SOAK — does the edited cell actually run without throwing?
        (if-let [reason (soak loop-def soak-input)]
          (rollback! opts checkpoint reason)
          ;; COMMIT — the edit stands; it is already live in the registry.
          (do (when (and conn run-id)
                (journal/note! conn run-id :mutation-committed
                               {:data {:cells (keys (cells/loaded))}}))
              (log/info "cell mutation committed")
              {:status :committed})))
      (catch Throwable e
        ;; RELOAD failed (syntax error): the loader restored the registry;
        ;; restore the file and report.
        (rollback! opts checkpoint
                   (str "reload: the edited cell file did not load — "
                        (or (ex-message e) (str e)))))))))

;; --- the store-backed edit (per-project userspace) ---------------------------
;;
;; The file-based protocol above edits resources/cells — the harness's own
;; template, shared by every project. That was never userspace: a supervisor
;; "changing its cells" was changing the harness for everybody. `propose-cell!`
;; is the same protocol against the PROJECT's copy in the userspace store.
;;
;; And it inverts the order. The file path had to save first (the agent had
;; already written the file) and undo on failure; here nothing is written until
;; the candidate has survived, so a bad edit never enters the project's
;; version history at all. The ATTEMPT is still recorded — in the journal,
;; with the reason — which is the right split: the store holds versions that
;; were live, the journal holds every attempt and its verdict.

(defn shadowed-cells
  "Cell ids in `body` that ANOTHER stored cell file already defines, as
  `[[id owning-name] ...]`. Empty when the save is honest.

  WHY THIS IS A REFUSAL AND NOT A WARNING. `load-cells!` loads the stored
  files name-sorted, so two files defining the same id both register and the
  later name wins. Nothing about that is visible: the body compiles, the cells
  register, the soak passes, and the mutation reports success. The damage
  appears later, when somebody edits the CANONICAL file and their version is
  silently shadowed by the stale copy.

  Live in run e1491f04 — the first fully validated agent self-edit, and a real
  fix. The supervisor guarded a prompt sentence that had been sending it to
  chase a phantom crash, then saved the WHOLE feature cell file under the new
  name `feature/supervise`. Every `:feature/*` cell was then defined twice,
  with the copy sorting later and winning. The edit was right and only the
  name was wrong, which is exactly the mistake a check can catch and a soak
  cannot (karamazov-990).

  Best effort: a body that will not read is not this function's complaint to
  make — `load-string` below reports that far better."
  [name body]
  (try
    (let [ids (set (cells/defcell-ids body))]
      (when (seq ids)
        (vec (for [[other b] (some-> (userspace/conn) (store/latest-bodies :cell))
                   :when (not= (str other) (str name))
                   id (set (cells/defcell-ids b))
                   :when (contains? ids id)]
               [id other]))))
    (catch Throwable _ nil)))

(defn propose-cell!
  "Validate a candidate cell body, and commit it as a new version of this
  project's cell only if it survives.

  opts:
    :name       — the cell's userspace name (the template basename, e.g. loop)
    :body       — the candidate Clojure source
    :loop-def   — the workflow definition to validate and soak against
    :extra-defs — {manifest-name definition} of OTHER manifests to validate
                  (compile only, no soak). A cell wired into the beam or a
                  team loop is not referenced by the active loop at all, so
                  validating that one definition let an edit that broke every
                  other workflow commit untouched (karamazov-blt.2)
    :soak-input — the initial data map the soak dry-run starts from
    :compile-fn — how to compile+validate (default mycelium pre-compile)
    :rationale  — why, stored with the committed version (karamazov-c58)
    :conn :run-id — to journal the outcome (optional)

  Returns {:status :committed :version n} or
  {:status :rolled-back :reason ...} with the registry restored and nothing
  written to the store."
  [{:keys [name body loop-def extra-defs soak-input compile-fn rationale conn run-id]}]
  (let [compile-fn (or compile-fn myc/pre-compile)
        shadowing (shadowed-cells name body)
        snapshot (cell/registry-snapshot)
        fail (fn [reason]
               (cell/registry-restore! snapshot)
               (when (and conn run-id)
                 (journal/note! conn run-id :mutation-rolled-back
                                {:data {:cell name :reason reason}}))
               (log/warn "cell proposal rejected:" name reason)
               {:status :rolled-back :reason reason})]
    (if (seq shadowing)
      ;; REFUSED BEFORE INSTALLING. A save under the wrong name is not a bad
      ;; edit that a soak can catch — the body compiles, the cells register,
      ;; the soak passes, and the damage only appears later when the canonical
      ;; file is edited and silently loses. Nothing to roll back either,
      ;; because nothing went wrong.
      (fail (prompt/render "cell-shadowed"
                           {:ids (str/join ", " (map first shadowing))
                            :one (= 1 (count shadowing))
                            :owners (str/join ", " (distinct (map second shadowing)))
                            :name name}))
    (try
      ;; INSTALL the candidate into the live image, on top of the project's
      ;; other cells. Syntax errors surface here.
      (binding [*ns* *ns*]
        (load-string body))
      (if-let [reason (or (validate compile-fn loop-def)
                          (some (fn [[nm d]]
                                  (when-let [r (validate compile-fn d)]
                                    (str "manifest '" nm "': " r)))
                                extra-defs))]
        (fail reason)
        (if-let [reason (soak loop-def soak-input)]
          (fail reason)
          ;; COMMIT. The candidate is already live; this is what makes it
          ;; survive a restart and what another run will load.
          ;;
          ;; save! returns nil when no project store is bound (RFC-001), and
          ;; that is NOT a commit: reporting :committed with a nil version had
          ;; the tool telling the model "saved as v … live on your next turn"
          ;; about an edit that vanishes on restart (karamazov-blt.8). It is
          ;; not a rollback either — validate and soak passed, the candidate
          ;; stays live in this image — so the caller hears exactly that.
          (let [v (userspace/save! :cell name body rationale)]
            (if (nil? v)
              ;; :reason is a KEYWORD, not prose: the sentence the model reads
              ;; is the tool's to render (prompts/cell-tool.md), same as every
              ;; other model-facing word.
              (do (log/warn "cell" name "is live but UNSAVED — no project store is bound")
                  {:status :live-unsaved :reason :unbound})
              (do (when (and conn run-id)
                    (journal/note! conn run-id :mutation-committed
                                   {:data {:cell name :version v}}))
                  (log/info "cell" name "committed as version" v)
                  {:status :committed :version v})))))
      (catch Throwable e
        (fail (str "the candidate did not load — " (or (ex-message e) (str e)))))))))
