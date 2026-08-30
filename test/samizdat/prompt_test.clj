;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.prompt-test
  "The prompt seams, both halves.

  The prompt-to-dispatch contract: the two directions drift independently —
  a tool dispatched but undocumented is invisible to the model, and a tool
  documented but not dispatched burns turns on the :default method while the
  model reads the failure as its own mistake. Both are asserted so neither
  survives an edit.

  The renderer contract: selmer renders every prompt template (gates'
  message files and suffixes, the beam's steer prose, the loop's valve
  message, the domain prompts). Template files keep the {{...}} spelling the
  hand-rolled str/replace chains used, so the move changed the renderer,
  not the templates."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jolt.fs :as fs]
            [samizdat.agent.loop :as loop]
            [samizdat.agent.tools :as tools]
            [samizdat.prompt :as prompt]
            [samizdat.userspace :as userspace]))

(deftest every-tool-is-documented
  ;; Matched as a WORD at the start of a documentation line, not as a
  ;; substring. `str/includes?` counted a tool named `cell` as documented
  ;; because the prompt contains the word `cells` — so a whole tool could be
  ;; added, be invisible to the model, and this test would pass.
  (let [prompt (loop/system-prompt)
        documented? (fn [nm]
                      (re-find (re-pattern
                                (str "(?m)^\\s*"
                                     (java.util.regex.Pattern/quote (str nm))
                                     "\\b"))
                               prompt))
        undocumented (remove documented? (tools/tool-names))]
    (is (empty? undocumented)
        (str "these tools are dispatched by run-tool but are not documented on a"
             " line of their own in the prompt, so the model cannot call them: "
             (str/join ", " undocumented)))))

(deftest every-documented-tool-exists
  ;; The opposite drift, which is worse in one way: the model spends turns
  ;; calling something that lands on the :default method, and reads the failure
  ;; as its own mistake.
  (let [prompt (loop/system-prompt)
        known (set (tools/tool-names))
        ;; Names in the prompt are written as `name({args})`.
        mentioned (map second (re-seq #"(?m)^(\w+)\(\{?" prompt))
        phantom (remove known mentioned)]
    (is (empty? phantom)
        (str "the prompt documents tools that run-tool does not dispatch: "
             (str/join ", " phantom)))))

(deftest no-unsubstituted-placeholders
  ;; The prompt is assembled from a template; a substitution placeholder
  ;; reaching the model means an edit broke the seam. `{{env/NAME}}` is
  ;; excluded on purpose — it is documented runtime syntax the shell tool
  ;; resolves at spawn, not a template hole the loader should have filled.
  (let [prompt (loop/system-prompt)
        holes (->> (re-seq #"\{\{([^}]+)\}\}" prompt)
                   (map second)
                   (remove #(str/starts-with? % "env/")))]
    (is (empty? holes)
        (str "unfilled template placeholders: " (str/join ", " holes)))))

(deftest env-syntax-survives-selmer
  ;; selmer parses {{env/NAME}} as a nested lookup and renders it empty;
  ;; samizdat.prompt sentinel-wraps it so the documented shell-tool syntax
  ;; reaches the model verbatim. Without this, the secret-reference docs
  ;; silently vanish from the system prompt.
  (is (str/includes? (loop/system-prompt) "{{env/NAME}}"))
  (is (= "ref {{env/FOO}} end"
         (prompt/render-str "ref {{env/FOO}} end" {}))))

(deftest prompts-render-through-selmer
  ;; Placeholders interpolate: hyphenated keys (gates' {{turn-count}}),
  ;; numbers without pre-str-ing, values raw — no HTML escaping, because a
  ;; prompt full of code must not have < > & quoted into entities.
  (is (str/includes? (prompt/render "explore-cap" {:lead "L: " :cap 5})
                     "L: 5 turns"))
  (is (= "v=(->> xs (map inc)) & <plain>"
         (prompt/render-str "v={{v}}" {:v "(->> xs (map inc)) & <plain>"})))
  ;; A missing key renders empty rather than surfacing a literal {{...}} to
  ;; the model. The str/replace chains left the placeholder visible; the
  ;; content pins (what the message must contain) carry the load now.
  (is (= "a  b" (prompt/render-str "a {{absent}} b" {})))
  ;; Single braces are not template syntax — architect.md's JSON decision
  ;; block passes through untouched, and a value containing {{...}} is
  ;; inserted as text, never re-parsed.
  (is (= "{\"decision\": \"decompose\"} 1"
         (prompt/render-str "{\"decision\": \"decompose\"} {{n}}" {:n 1})))
  (is (= "a {{inner}} b"
         (prompt/render-str "a {{v}} b" {:v "{{inner}}"}))))

(deftest selmer-replaces-the-str-replace-seams
  ;; The renderer is selmer, not str/replace: the parser is selmer's (a
  ;; filter in the placeholder would fire, where a replace chain would
  ;; leave it verbatim).
  (is (= "Y" (prompt/render-str "{{v|upper}}" {:v "y"}))))

;; --- the prompt chain (LR-7) -------------------------------------------------

(deftest a-chain-takes-the-first-present-level
  ;; First-present-wins: a level REPLACES the text, it does not add to it.
  (is (= "top" (prompt/resolve-chain [{:text "top"} {:text "bottom"}])))
  (is (= "bottom" (prompt/resolve-chain [{:project "/nonexistent/nope.md"}
                                         {:text "bottom"}]))
      "an absent level inherits from the one below"))

(deftest a-blank-level-means-explicitly-none-and-stops-the-walk
  ;; This is the distinction the whole trichotomy rests on. Collapsing blank
  ;; into absent would make it impossible to suppress a layer at all.
  (is (nil? (prompt/resolve-chain [{:text "  "} {:text "bottom"}])))
  (is (nil? (prompt/resolve-chain [{:text ""} {:file "system"}]))))

(deftest an-exhausted-chain-is-nil-not-an-error
  (is (nil? (prompt/resolve-chain [])))
  (is (nil? (prompt/resolve-chain [{:project "/nonexistent/a"}
                                   {:project "/nonexistent/b"}]))))

(deftest a-file-level-resolves-through-io-resource
  ;; Not a cwd-relative path: it has to work inside a built binary, where
  ;; resources/ does not exist on disk.
  (is (str/includes? (prompt/resolve-chain [{:file "system"}]) "tool call"))
  (is (nil? (prompt/resolve-chain [{:file "no-such-prompt-anywhere"}]))))

(deftest an-unknown-entry-kind-fails-loud
  ;; The chain is runtime-editable, so a typo is a live possibility — and a
  ;; silently dropped layer would look exactly like a suppressed one.
  (is (thrown-with-msg? Exception #":project, :file or :text"
                        (prompt/resolve-chain [{:flie "typo"}]))))

(deftest a-project-file-overrides-the-shipped-prompt
  (let [dir (java.io.File. ".samizdat/prompts")
        f (java.io.File. dir "chain-test.md")]
    (try
      (.mkdirs dir)
      (spit f "the project's own text")
      (is (= "the project's own text"
             (prompt/resolve-chain [{:project (.getPath f)} {:file "system"}])))
      (testing "and an empty project file suppresses the layer entirely"
        (spit f "")
        (is (nil? (prompt/resolve-chain [{:project (.getPath f)} {:file "system"}]))))
      (finally (.delete f)))))

(deftest the-system-layer-is-declared-and-ends-at-the-shipped-file
  (let [entries (:system (prompt/chains))]
    (is (seq entries) "the system prompt goes through the chain")
    (is (= {:file "system"} (last entries))
        "the shipped prompt is the floor, so an unconfigured harness is unchanged")
    (is (str/includes? (prompt/layer :system) "tool call"))))

(deftest an-undeclared-layer-falls-back-to-its-own-prompt-file
  ;; Adding a layer to prompt-chain.edn is opt-in; a layer with no chain
  ;; behaves exactly as a plain prompt read.
  (is (= (str/trim (prompt/prompt "crossover"))
         (str/trim (prompt/layer :crossover)))))

(deftest shipped-prompts-match-what-ships
  ;; Enumerated rather than globbed, for the reason cells/shipped-cells is:
  ;; `jolt build` bakes resources/ into the binary and an embedded resource
  ;; has no filesystem path for a glob to walk, so a built binary run outside
  ;; the project root would report that the harness has no prompts. An
  ;; enumerated list cannot drift on its own — this is what pins it.
  (let [on-disk (->> (file-seq (java.io.File. "resources/prompts"))
                     (filter #(.isFile %))
                     (map #(-> (.getPath %)
                               (str/replace #"^resources/prompts/" "")
                               (str/replace #"\.md$" "")))
                     set)]
    (is (seq on-disk) "resources/prompts is readable from the test's cwd")
    (is (= on-disk (set prompt/shipped-prompts))
        (str "prompt/shipped-prompts and resources/prompts disagree; missing: "
             (sort (remove (set prompt/shipped-prompts) on-disk))
             ", listed but absent: "
             (sort (remove on-disk prompt/shipped-prompts))))))

(deftest every-shipped-prompt-renders
  ;; A template that cannot be parsed fails where it is USED — for a gate
  ;; message that is mid-run, and for the system prompt it is the top of every
  ;; branch. Cheap to check them all here instead.
  (doseq [n prompt/shipped-prompts]
    (is (string? (prompt/render-str (prompt/prompt n) {}))
        (str "prompts/" n ".md does not render"))))

;; --- the prompt is scoped to the PROJECT, not just the role ------------------

(deftest the-self-hosting-sections-only-appear-when-the-target-is-the-harness
  ;; karamazov-8zk. system.md spends several sections on samizdat's own
  ;; architecture — cells and manifests, src-is-mechanism vs
  ;; resources-are-behaviour, "you are building the very harness you run in".
  ;; All of it is load-bearing when the run's target IS samizdat and all of it
  ;; is standing instruction about the wrong codebase otherwise, in the
  ;; most-weighted part of the context. Live, for every run of the fps
  ;; campaign: a model writing a raylib renderer was being told to prefer
  ;; adding a reusable cell and to put its decisions in gates.edn.
  (let [prev (userspace/project-root)]
    (try
      (userspace/bind-root! (System/getProperty "user.dir"))
      (let [own (loop/system-prompt)]
        (is (str/includes? own "src is mechanism"))
        (is (str/includes? own "the very harness you run in")))
      (userspace/bind-root! "/tmp")
      (let [other (loop/system-prompt)]
        (is (not (str/includes? other "src is mechanism"))
            "a run on another project is told where changes go in SAMIZDAT")
        (is (not (str/includes? other "the very harness you run in")))
        (is (not (str/includes? other "prefer to add a reusable cell")))
        (testing "what survives is the project-agnostic half — the turn format,
                  the REPL-first loop, the honesty rules, one-namespace-one-
                  responsibility"
          (is (str/includes? other "One namespace, one responsibility"))
          (is (str/includes? other "REPL first"))
          (is (str/includes? other "```tool-call"))
          (is (str/includes? other "what you build, don't just test it"))))
      (testing "no unfilled placeholders or stray tags survive either branch"
        (doseq [p [(do (userspace/bind-root! (System/getProperty "user.dir"))
                       (loop/system-prompt))
                   (do (userspace/bind-root! "/tmp") (loop/system-prompt))]]
          (is (not (str/includes? p "{%")))
          (is (empty? (->> (re-seq #"\{\{([^}]+)\}\}" p)
                           (map second)
                           (remove #(str/starts-with? % "env/")))))))
      (finally (userspace/bind-root! prev)))))

(deftest an-unbound-root-keeps-the-whole-prompt
  ;; Unknown reads as TRUE. A test, a bare REPL, a driver that forgot to bind:
  ;; deleting whole instruction blocks on a missing binding is a silent
  ;; failure, and the shipped prompt is the harness's own.
  (let [prev (userspace/project-root)]
    (try
      (userspace/bind-root! nil)
      (is (str/includes? (loop/system-prompt) "the very harness you run in"))
      (finally (userspace/bind-root! prev)))))

(deftest declared-reference-paths-are-named-in-the-tool-catalogue
  ;; karamazov-1an, the other half: the read is now allowed, and the model has
  ;; to be told it is. A capability nothing announces is one only a model that
  ;; guesses at it will use.
  (let [prev (userspace/project-root)
        root (str "/tmp/samizdat-prompt-" (random-uuid))
        examples (str root "-examples")]
    (fs/create-dirs (str root "/.samizdat"))
    (fs/create-dirs examples)
    (spit (str root "/.samizdat/config.edn")
          (pr-str {:run {:reference-paths [examples]}}))
    (try
      (userspace/bind-root! root)
      (let [p (loop/system-prompt)]
        (is (str/includes? p (str (fs/canonicalize examples)))
            "the declared path is named in read_file's entry")
        (is (str/includes? p "READ-ONLY reference material")))
      (userspace/bind-root! "/tmp")
      (is (not (str/includes? (loop/system-prompt) "READ-ONLY reference material"))
          "a project that declared none is not told about a capability it has
           not got")
      (finally
        (userspace/bind-root! prev)
        (fs/delete-tree root)
        (fs/delete-tree examples)))))
