;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.repl.guard
  "EVAL MAY NOT KILL THE SERVER.

  `eval-code` runs in the LIVE HARNESS IMAGE — that is the whole point of it,
  and it is why the agent can rewrite itself while running. It also means the
  agent's code and the harness's code share one process, so `(System/exit 0)`
  in an eval is not an error the tool reports: it is the server ending, mid-run,
  with a success status.

  OBSERVED, run a3ba69bb: the serve process exited 0 while a branch was
  in-flight, printing a directory listing of the harness's own root just before
  it went. A parked server never exits on its own, and nothing in abort, beam
  teardown or process disposal calls exit — the trigger was in the eval
  (karamazov-1xx). Reproduced directly: an eval of `(System/exit 0)` ends the
  process, and the line after it never runs.

  IN `src/` AND NOT IN `gates.edn`, DELIBERATELY. This is the same shape as
  `the run config is not writable by the run it gates`: a liveness guard the
  guarded thing can edit is not a guard. The harness staying alive is mechanism.

  TWO LAYERS, BECAUSE NEITHER IS ENOUGH ALONE:

  - `terminating-form?` refuses the call before it runs. It is a STATIC read of
    the form, so it catches what was actually observed — a model reaching for
    exit — and not a determined adversary, who has `resolve` and a hundred other
    routes. Confinement is a different job (karamazov-zrq); this one is about
    the harness surviving its own agent's ordinary mistakes.
  - `record-exit!` cannot prevent anything, and does not try. It makes every
    exit VISIBLE — including the routes the static check misses. An exit that
    gets recorded is a bug someone can fix; the reason a3ba69bb took a whole
    investigation is that a 0 with no message looks exactly like a clean
    shutdown. `core/-main` hangs it off the shutdown hook the host already
    provides, registered before `system/stop!` so the store is still open
    enough to answer what was running."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [samizdat.symbolic :as sym]))

(def terminators
  "Calls that end the process rather than the evaluation.

  `System/exit` is the observed one. `.halt` is worse — it skips shutdown hooks,
  so it would defeat the second layer too. `Runtime/getRuntime` is here because
  the two interop routes to the same thing (`(.exit (Runtime/getRuntime) 0)`)
  read as an ordinary method call and only the receiver gives them away."
  '#{System/exit java.lang.System/exit Runtime/getRuntime java.lang.Runtime/getRuntime
     .halt .exit})

(def ^:private definition-heads
  "Forms whose body does not run when the form is evaluated."
  '#{defn defn- fn fn* defmacro definline})

(defn- executed-subforms
  "`form`'s subforms, minus the bodies of definitions.

  DEFINING A THING THAT EXITS IS NOT EXITING, and the difference is not
  academic here: every Clojure test runner ends `(System/exit 0)`, this
  project's deps.edn requires the agent to write one, and the eval tool's whole
  pitch is to iterate on a form before writing it to a file. A guard that
  refuses `(defn -main [] (System/exit 0))` refuses the work it exists to
  protect, and a guard that fires on legitimate work gets worked around."
  [form]
  (tree-seq (fn [x]
              (and (coll? x)
                   (not (and (seq? x)
                             (symbol? (first x))
                             (definition-heads (symbol (name (first x))))))))
            seq
            form))

(def kernel-writers
  "Calls that put bytes on disk. Paired with a `src/` path below, this is a
  kernel-source write."
  '#{spit clojure.java.io/copy io/copy write-lines fs/write-lines
     jolt.fs/spit fs/spit})

(def ^:private kernel-path-re
  "A string argument naming harness SOURCE rather than userspace.

  `src/` only — the vendored trees (mycelium, maestro, ring_chez) live under
  it now, so one arm covers the whole kernel. resources/ is deliberately
  absent: cells, manifests and prompts ARE the supervisor's editing surface,
  and refusing those would refuse the whole point of the role."
  #"(^|/)src/")

(defn- call?
  [x]
  (and (seq? x) (symbol? (first x))))

(defn facts
  "What the rules can see of `form`, as facts.

  Two walks. The EXECUTED subforms — definition bodies excluded, because
  defining a thing that exits is not exiting — give :executed-symbol for
  every symbol, and for every call an id with its :call head, :keyword-arg,
  :src-path-arg (a string argument naming harness source under src/) and
  :harness-ref (an argument that mentions samizdat). Every subform, bodies
  included, gives :any-call with the head's bare name, because calling a
  -main is refused wherever it sits. The terminator and kernel-writer sets
  ride along as facts, so a rule joins against them like anything else."
  [form]
  (let [executed (executed-subforms form)]
    (concat
     (map (fn [s] [:terminator s]) terminators)
     (map (fn [s] [:kernel-writer s]) kernel-writers)
     (for [x executed :when (symbol? x)] [:executed-symbol x])
     (apply concat
            (map-indexed
             (fn [i x]
               (when (call? x)
                 (concat [[:call i (first x)]]
                         (for [a (rest x) :when (keyword? a)]
                           [:keyword-arg i a])
                         (for [a (rest x)
                               :when (and (string? a) (re-find kernel-path-re a))]
                           [:src-path-arg i a])
                         (for [a (rest x) :when (re-find #"samizdat" (str a))]
                           [:harness-ref i a]))))
             executed))
     (keep-indexed (fn [i x]
                     (when (call? x) [:any-call i (first x) (name (first x))]))
                   (tree-seq coll? seq form)))))

(def ^:private rule-table
  "The guard as rules over those facts. :on names the var whose binding is
  what the rule fired on, for a refusal to quote."
  '[{:name :process-exit
     :doc "ends the process"
     :where [[:executed-symbol ?s] [:terminator ?s]]
     :on ?s}
    {:name :kernel-source-write
     :doc "writes harness source"
     :where [[:call ?f ?head] [:kernel-writer ?head] [:src-path-arg ?f ?path]]
     :on ?path}
    {:name :harness-reload
     :doc "reloads a harness namespace"
     :where [[:call ?f require] [:keyword-arg ?f ?flag] [:harness-ref ?f ?ns]]
     :if [:in ?flag #{:reload :reload-all}]
     :on ?ns}
    {:name :entry-point-call
     :doc "calls an entry point"
     :where [[:any-call ?f ?head "-main"]]
     :on ?head}])

(def ^:private compiled-rules (sym/fact-rules rule-table))

(defn rules
  "The guard's rules, enumerable: for each, its name, a label for what it
  catches, the clauses that decide it and the guard if any.

  IN src/ AND NOT IN resources/, on purpose, like everything else in this
  namespace: a guard the guarded thing can edit is not a guard. Being data
  makes the reach reviewable, not editable — which is the distinction this
  round exists to keep.

  What each catches, densely: :process-exit is a terminator symbol among
  the executed subforms — System/exit, .halt, and the Runtime interop routes
  that only the receiver gives away. :kernel-source-write is a kernel-writer
  call (spit and friends) with a string argument under src/; resources/ is
  deliberately not src/, being the supervisor's editing surface.
  :harness-reload is a require carrying :reload or :reload-all and naming a
  samizdat namespace — the second half of the observed escape, since a
  patched file is only dangerous once it is in the running image.
  :entry-point-call is a -main in head position anywhere, bodies included,
  because the exit is inside the callee where no symbol check can see it."
  []
  (mapv #(select-keys % [:name :doc :where :if]) rule-table))

(defn findings
  "Every rule that fires on `form`: [{:rule name :on subject :bindings b}]."
  [form]
  (let [subject (into {} (map (juxt :name :on)) rule-table)]
    (mapv (fn [f] (assoc f :on (get-in f [:bindings (subject (:rule f))])))
          (sym/fire (sym/facts (facts form)) compiled-rules))))

(defn- fired
  "The findings of the rules named in `names`, on `form`."
  [form names]
  (filter #(contains? names (:rule %)) (findings form)))

(defn terminating-form?
  "Whether evaluating `form` would end the process.

  Walks the READ form as data, so it sees through nesting, threading macros and
  quoting alike — but not into definition bodies, which do not run. What is
  left is what this eval actually executes.

  `.exit`/`.halt` are matched as bare symbols: a false positive costs the agent
  one refused eval and a message telling it exactly what to do, and a false
  negative costs the run."
  [form]
  (boolean (seq (fired form #{:process-exit}))))

(defn kernel-writes
  "The kernel-write findings on `form` — a write to harness source, or a
  hot-load of a harness namespace — for the refusal to name."
  [form]
  (fired form #{:kernel-source-write :harness-reload}))

(defn kernel-write?
  "Whether `form` writes to harness source, or hot-loads a harness namespace
  back into the running image.

  THE SUPERVISOR IS THE ONE ROLE STILL IN THE LIVE IMAGE (karamazov-zrq), so
  it is the one role that can still do what run a3ba69bb's S2 branch did: spit
  a patched `src/` file and `(require … :reload)` it, putting model-written
  kernel code into the running harness with no checkpoint, no validate, no
  soak, no userspace version, and nothing for rollback to see.

  A STATIC READ, and the same admission `terminating-form?` makes about
  itself: it catches the reach, not a determined adversary, who has `resolve`
  and a hundred other routes. What it buys is that the ordinary move — the one
  actually observed — stops being available by accident, and the supported
  route is named instead."
  [form]
  (boolean (seq (kernel-writes form))))

(defn entry-point-call?
  "Whether `form` calls a `-main`.

  THE ROUTE THE SYMBOL CHECK CANNOT SEE, and the harness's own guidance walks
  the model straight into it: the eval tool invites requiring and exercising
  the project's namespaces, and a project's test runner conventionally ends
  `(System/exit 0)`. `(flight.test-runner/-main)` contains no terminator to
  find — the exit is inside the callee, one file away, in code the agent wrote
  itself minutes earlier.

  `-main` BY NAME, because the name is the convention that means `process entry
  point` and process entry points end processes. A `-main` that does not exit
  is refused too, and that costs one eval and a message naming the two things
  that do work.

  IN HEAD POSITION ONLY, which is the difference between calling one and
  writing one. `(defn -main [] …)` must stay allowed: the eval tool's whole
  pitch is to iterate on a form before writing it to a file, and this project's
  deps.edn REQUIRES the agent to define a `-main`. Refusing that would refuse
  the recommended workflow — a guard that fires on the work it is meant to
  protect gets worked around, and then protects nothing."
  [form]
  (boolean (seq (fired form #{:entry-point-call}))))

(defn offending
  "The terminator symbols in `form`, sorted, for the refusal to name them."
  [form]
  (->> (fired form #{:process-exit}) (map (comp str :on)) distinct sort))

(defn main-calls
  "The `-main` symbols CALLED in `form`, qualified as written, for the refusal
  to name the one the agent actually reached for."
  [form]
  (->> (fired form #{:entry-point-call}) (map (comp str :on)) distinct sort))

(defn exit-note
  "What to record when the process ends, as DATA rather than a sentence.

  `active` is whatever the caller knows is still in flight — run ids, branch
  labels — and it is the whole value of the record: a server exiting with
  nothing running is somebody stopping it, and a server exiting with three
  branches mid-turn is this bug. `:bug?` is that distinction, decided here so
  it can be tested without capturing a log.

  Data and not a formatted string so the words live at the `log` call, where
  they are a developer's to read off a console — the prose ratchet strips
  strings under a log form, and a note assembled one function away from its
  logging is prose the ratchet cannot tell from a sentence aimed at the model."
  [active]
  (let [ids (mapv str active)]
    {:bug? (boolean (seq ids)) :active ids :count (count ids)}))

(defn record-exit!
  "Log the exit at the level its shape deserves: a warning when work was in
  flight, because that is the bug, and debug when it was idle. Returns the
  note."
  [active]
  (let [{:keys [bug? active] :as note} (exit-note active)]
    (if bug?
      (log/warn "harness process exiting with work still in flight:"
                (str/join ", " active)
                "- a parked server does not exit on its own (karamazov-1xx)")
      (log/debug "harness process exiting; nothing was in flight"))
    note))
