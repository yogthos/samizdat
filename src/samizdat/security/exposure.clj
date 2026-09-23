;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.security.exposure
  "WHICH PROVIDERS MAY SEE WHICH FILES (karamazov-d5wo.8).

  A project names path patterns per provider under config
  `:run :provider-deny`, e.g. `{:deepseek [\"secrets/**\" \"**/*.pem\"]}`, and
  a read tool whose result would reach that provider refuses a matching
  path; grep drops matching hits. The provider is the branch's own, plus the
  :reader role's for read_digest, whose model reads the file itself.

  IN THE PROJECT'S CONFIG AND NOT IN gates.edn, for the reason the shell
  table and protected-paths give (samizdat.security.policy): this confines
  the agent, gates.edn is agent-editable, and .samizdat/config.edn is the one
  file the file tools and the shell policy refuse to let a run write.

  NOT A SANDBOX. `shell` (cat, sed) and `eval` (slurp) read files without
  naming them as a path argument, and nothing here parses those. It keeps a
  denied file out of the tools the model reads with by default; a model set
  on reading it can still reach it through the shell."
  (:require [clojure.string :as str]
            [jolt.fs :as fs]
            [samizdat.agent.files :as files]
            [samizdat.prompt :as prompt]))

(defn- glob->re [glob]
  (re-pattern
   (str "^"
        (-> (str glob)
            (str/replace #"[.+^$(){}\[\]|\\]" #(str "\\" %))
            (str/replace "**/" "\u0000")
            (str/replace "**" "\u0001")
            (str/replace "*" "[^/]*")
            (str/replace "?" "[^/]")
            (str/replace "\u0000" "(?:.*/)?")
            (str/replace "\u0001" ".*"))
        "$")))

(defn glob-matches?
  "Whether root-relative `path` matches `glob`: `**` crosses directories
  (and `**/` matches none), `*` and `?` stay inside one."
  [glob path]
  (boolean (re-matches (glob->re glob) (str path))))

(defn- provider-key [p] (some-> p name str/lower-case keyword))

(defn denied
  "The first pattern `config` denies `provider` that matches `path`, or nil."
  [config provider path]
  (let [rules (get-in config [:run :provider-deny])
        pats (some (fn [[k v]] (when (= (provider-key k) (provider-key provider)) v)) rules)]
    (some #(when (glob-matches? % path) %) pats)))

(defn- relative
  "`path` relative to `root` when it lies under it, else as given."
  [root path]
  (if-let [abs (and root (files/resolve-under-root root path))]
    (str/replace (subs abs (count (str (fs/canonicalize root)))) #"^/" "")
    (str path)))

(def ^:private path-args
  "The arguments that name a file a tool will read: path (read_file, the
  writers, grep's scope), file (lsp), paths (read_digest, grep)."
  [:path :file :paths])

(defn- providers-for
  "Every provider the result of this call reaches."
  [{:keys [config llm-config tool-name]}]
  (distinct (remove nil? [(provider-key (:provider llm-config))
                          (when (= "read_digest" tool-name)
                            (provider-key (get-in config [:run :role-models :reader :provider])))])))

(defn refusal
  "A refusal message when this call names a path a provider its result
  reaches may not see, else nil."
  [{:keys [config root args] :as ctx}]
  (when (get-in config [:run :provider-deny])
    (first
     (for [k path-args
           :let [v (get args k)]
           p (if (sequential? v) v [v])
           :when (and (string? p) (not (str/blank? p)))
           provider (providers-for ctx)
           :let [pat (denied config provider (relative root p))]
           :when pat]
       (prompt/render "exposure-refused"
                      {:path (relative root p) :pattern pat :provider (name provider)})))))

(defn visible
  "`hits` ({:path ...}) without those a provider this call reaches may not see."
  [{:keys [config root] :as ctx} hits]
  (if-not (get-in config [:run :provider-deny])
    hits
    (let [ps (providers-for ctx)]
      (remove (fn [h] (some #(denied config % (relative root (:path h))) ps)) hits))))
