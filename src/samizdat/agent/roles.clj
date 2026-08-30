;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.agent.roles
  "WHICH WORLD EACH ROLE SEES. Mechanism only — the table is resources/roles.edn.

  Every role used to be built the same way: the implementer's system prompt
  plus a role suffix. That made role differentiation SUBTRACTIVE — a
  supervisor was handed 31 tools written for somebody building the project,
  and its own prompt then spent a paragraph arguing it back out of them. The
  argument does not always work: one supervisor spent 108 of a run's 211 turns
  hunting a source tree it was never going to be allowed to open
  (karamazov-i1u).

  Here a role is CONSTRUCTED. `surface` is what it may call, `scope-catalogue`
  is the tool documentation filtered to that surface, and a call outside it is
  refused rather than discouraged — so the prompt no longer has to talk the
  model out of a capability the prompt itself advertised.

  What this does NOT do is seal roles off from each other. Each already runs
  on its own branch with its own message stream and that is unchanged; the
  supervisor investigates across the boundary on purpose, and what it learns
  arrives as a tool result in its own stream rather than as inherited
  context."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [samizdat.config :as config]
            [samizdat.userspace :as userspace]))

(defn table
  "The role table from roles.edn, through the userspace seam so a project can
  retune its own roles without a rebuild."
  []
  (userspace/edn-body! :policy "roles"))

(defn names [] (sort (keys (table))))

(defn spec [role] (get (table) (keyword role)))

(defn doc [role] (str (:doc (spec role))))

(defn sees
  "What this role's opening context is assembled from. Documentation for the
  reader and for a supervisor deciding what to change."
  [role]
  (:sees (spec role) #{}))

(defn all-tool-names
  "Every registered tool name. Resolved late so this namespace does not depend
  on the tool tree at load — roles.edn is read by prompt assembly, which runs
  earlier than the tool registry in some entry points."
  []
  (when-let [v (resolve 'samizdat.agent.tools/tool-names)]
    ;; tool-names is a FN, not a var holding a set: deref the var to get it,
    ;; then call it.
    (set (@v))))

(def repl-tools
  "The tools that ARE the REPL. `doc` and `complete` belong here with `eval`:
  all three answer out of an evaluation image, and leaving the introspection
  pair behind would advertise a REPL the role cannot evaluate in."
  #{"eval" "doc" "complete"})

(defn- declared-surface
  "The tool names roles.edn gives `role`: a set, or the `:all` sentinel."
  [role]
  (let [t (:tools (spec role) :all)]
    (cond
      (= :all t) :all
      (set? t) t
      (coll? t) (set (map str t))
      :else #{})))

(defn confine
  "`surface` with the REPL tools removed when the operator has turned the REPL
  off. Pure — the mode is passed in, because the thing that decides it is the
  operator's file and this namespace's job is only to apply it.

  THE SUBTRACTION HAPPENS HERE RATHER THAN IN roles.edn, and that is the whole
  point of the function. roles.edn is agent-editable userspace: it lists
  \"eval\" on the implementor and supervisor surfaces, and a run that could
  turn its own REPL back on by editing it would have a toggle in name only.
  Same rule repl/guard.clj and policy/protected-paths already state, both
  citing karamazov-zrq.

  `:all` STAYS `:all`, and the REPL subtraction for it lives at the two call
  sites instead. Collapsing it here was tried and is wrong for the reason
  `surface` gives: `all-tool-names` resolves the registry late and answers nil
  during prompt assembly, so the collapse handed a role the empty set — no
  tools at all, the exact failure the sentinel exists to prevent. Withholding
  everything is not a safe direction to fail in; it is a different outage.
  `may-use?` refuses the REPL tools under `:off` on its own, and
  `scope-catalogue` filters them with a predicate, so neither needs the set."
  [surface mode]
  (if (or (not= :off mode) (= :all surface))
    surface
    (set/difference surface repl-tools)))

(defn surface
  "The tool names `role` may call: a set, or `:all`.

  `:all` stays a SENTINEL rather than expanding to the registry. Expanding it
  needs samizdat.agent.tools loaded, and roles are read during prompt
  assembly, which in some entry points runs first — so an expansion would
  quietly return the empty set and hand the implementor NO tools. A sentinel
  cannot fail that way: the two callers below both treat it as unrestricted
  without ever asking what the registry holds.

  The `:off` case is the one exception, and it is safe: `confine` only needs
  the registry when it is about to take tools AWAY, so an empty registry there
  withholds rather than grants."
  ([role]
   (surface role (config/eval-mode (userspace/project-root))))
  ([role mode]
   (confine (declared-surface role) mode)))

(defn may-use?
  "Whether `role` may call `tool`. A role the table does not name is
  unrestricted — roles are opt-in, so adding one cannot silently disarm a
  workflow that never had one."
  ([role tool]
   (may-use? role tool (config/eval-mode (userspace/project-root))))
  ([role tool mode]
   (let [s (surface role mode)
         denied (set (map str (:denied (spec role))))]
     (and (not (contains? denied (str tool)))
          ;; The REPL toggle outranks "a role the table does not name is
          ;; unrestricted": an unnamed role is an unwritten policy, not a
          ;; licence to hold a tool the operator switched off.
          (not (and (= :off mode) (contains? repl-tools (str tool))))
          (or (nil? (spec role)) (= :all s) (contains? s (str tool)))))))

;; --- the tool catalogue, filtered -------------------------------------------
;;
;; system.md documents each tool as a `name({args})` line at column zero
;; followed by indented prose. The prose stays HAND WRITTEN — a generated
;; prompt reads like one — and only WHICH entries appear is computed.

(def ^:private entry-head
  "A tool catalogue entry's opening line: `name({…})` hard against the margin."
  #"^([a-z_][a-z0-9_]*)\(\{")

(def ^:private section-head
  "A catalogue section: `### Doing work`. Sections are what group entries, and
  a section's PROSE is written about the entries under it."
  #"^###\s+(.*)$")

(defn- sections
  "The text split into `{:head line-or-nil :lines [...]}` at every `###`,
  in order. Anything before the first heading is one headless section."
  [lines]
  (reduce (fn [acc line]
            (if (re-find section-head line)
              (conj acc {:head line :lines []})
              (if (seq acc)
                (update-in acc [(dec (count acc)) :lines] conj line)
                [{:head nil :lines [line]}])))
          []
          lines))

(defn- filter-entries
  "One section's lines with every entry the role may not call removed, along
  with the indented prose that belongs to it.

  `allowed` is called, not looked up, so it may be a set (the usual case) or a
  predicate — which is what an unrestricted surface minus the REPL tools is."
  [lines allowed]
  (:out (reduce (fn [{:keys [keep?] :as acc} line]
                  (if-let [[_ nm] (re-find entry-head line)]
                    ;; A new entry decides for itself and for the indented
                    ;; prose that follows it.
                    (let [k (boolean (allowed nm))]
                      (assoc acc :keep? k :out (cond-> (:out acc) k (conj line))))
                    ;; Anything else belongs to the entry above it, unless we
                    ;; are not inside one at all.
                    (if (and (not keep?) (re-find #"^\s+\S" line))
                      acc
                      (assoc acc :out (conj (:out acc) line) :keep? true))))
                {:out [] :keep? true}
                lines)))

(defn scope-catalogue
  "`text` with every tool entry the role may not use removed, prose and all —
  and any catalogue SECTION whose entries were all removed dropped whole.

  The section rule is most of the win, and filtering entries alone missed it.
  The harness-mutation section is nine tool entries wrapped in four paragraphs
  explaining what cells and manifests are, why the loop belongs to the
  project, and where the line between base and userspace falls. Strip the
  entries and every word of that prose stays — thousands of characters telling
  a role about a capability it has just been shown it does not have, which is
  worse than telling it nothing.

  HAD entries and lost them all, not merely `has none`: a section that never
  documented a tool is prose standing on its own — the breadcrumb index, the
  turn format — and belongs to every role.

  A nil role, or one the table does not name, gets the text unchanged — unless
  the REPL is off, which is a fact about the HARNESS rather than about the
  role, and so has to reach the catalogue even for a role the table never
  mentioned."
  ([text role]
   (scope-catalogue text role (config/eval-mode (userspace/project-root))))
  ([text role mode]
   (if (and (or (nil? (spec role)) (= :all (surface role mode)))
            (not= :off mode))
     text
     (let [allowed (surface role mode)
           allowed (if (= :all allowed)
                     ;; A role with no declared surface still loses the REPL
                     ;; entries; everything else in the catalogue stays.
                     (complement repl-tools)
                     allowed)]
      (->> (sections (str/split-lines (str text)))
           (mapcat (fn [{:keys [head lines]}]
                     (let [entries (keep #(second (re-find entry-head %)) lines)]
                       (if (and head (seq entries) (not-any? allowed entries))
                         nil
                         (cond->> (filter-entries lines allowed)
                           head (cons head))))))
           (str/join "\n"))))))
