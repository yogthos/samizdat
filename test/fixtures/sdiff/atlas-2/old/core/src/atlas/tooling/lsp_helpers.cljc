(ns atlas.tooling.lsp-helpers
  "Helper functions for IDE navigation of semantic identities.

  Works with standard clj-lsp - no custom LSP server needed!

  Key functions:
  - find-dev-id-usages: Find all occurrences of a dev-id in source files
  - find-by-aspect: Find all identities with a specific aspect
  - hover-info: Generate hover text for IDE display
  - export-definitions: Export for external tool consumption"
  (:require [atlas.registry :as cid]
            [atlas.registry.lookup :as rt]
            [atlas.query :as q]
            [atlas.cljc.platform :as platform]
            [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])))

;; =============================================================================
;; KEYWORD SEARCH - Find all usages of a dev-id in source files
;; =============================================================================

(defn find-keyword-in-file
  "Find all occurrences of a keyword string in a file.
  Returns sequence of {:file :line :content} maps."
  [file keyword-str]
  #?(:clj
     (try
       (let [content (slurp file)
             lines (str/split-lines content)]
         (->> lines
              (keep-indexed
               (fn [idx line]
                 (when (str/includes? line keyword-str)
                   {:file (str file)
                    :line (inc idx)
                    :content (str/trim line)})))
              (into [])))
       (catch Exception _ []))
     :cljs []))

(defn find-dev-id-usages
  "Find all usages of a dev-id across the codebase.

  Searches src/ and test/ directories for .clj, .cljs, .cljc files.

  Example:
    (find-dev-id-usages :component/database)
    ;=> [{:file \"src/app/core.clj\" :line 42 :content \"...\"}]"
  [dev-id]
  #?(:clj
     (let [keyword-str (str dev-id)
           source-dirs ["src" "test"]
           clj-files (for [dir source-dirs
                           :let [dir-file (io/file dir)]
                           :when (.exists dir-file)
                           file (file-seq dir-file)
                           :when (and (.isFile file)
                                      (let [name (.getName file)]
                                        (or (.endsWith name ".clj")
                                            (.endsWith name ".cljs")
                                            (.endsWith name ".cljc"))))]
                       file)]
       (->> clj-files
            (mapcat #(find-keyword-in-file % keyword-str))
            (into [])))
     :cljs []))

;; =============================================================================
;; ASPECT SEARCH - Find all identities with a specific aspect
;; =============================================================================

(defn find-by-aspect
  "Find all identities containing an aspect, with their details.

  Example:
    (find-by-aspect :tier/foundation)
    ;=> [{:dev-id :component/db :identity #{...} :context [...] ...}]"
  [aspect]
  (for [identity-set (q/all-identities (cid/current-registry))
        :when (contains? identity-set aspect)
        :let [value (cid/fetch identity-set)
              dev-id (:atlas/dev-id value)]]
    {:dev-id dev-id
     :identity identity-set
     :context (:interface-endpoint/context value)
     :response (:interface-endpoint/response value)
     :deps (:execution-function/deps value)
     :docs (:docs/content value)}))

;; =============================================================================
;; HOVER INFO - Generate hover text for IDE
;; =============================================================================

(defn- extract-aspect-by-ns
  "Extract first aspect with given namespace from identity set."
  [identity-set ns-name]
  (first (filter #(= ns-name (namespace %)) identity-set)))

(defn hover-info
  "Generate hover information for a dev-id.

  Returns markdown-formatted string suitable for IDE hover display.

  Example:
    (println (hover-info :component/db))
    ;; **Semantic Identity**
    ;; **Dev-ID:** `:component/db`
    ;; **Aspects:** `:tier/foundation`, `:domain/users`, ...
    ;; **Tier:** `:tier/foundation`
    ;; ..."
  [dev-id]
  (when-let [identity-set (rt/identity-for dev-id)]
    (let [props (rt/props-for dev-id)
          aspects (sort identity-set)
          tier (extract-aspect-by-ns identity-set "tier")
          domain (extract-aspect-by-ns identity-set "domain")
          context (:interface-endpoint/context props)
          response (:interface-endpoint/response props)
          deps (:execution-function/deps props)
          docs (:docs/content props)]
      (->> ["**Semantic Identity**"
            ""
            (str "**Dev-ID:** `" dev-id "`")
            (str "**Aspects:** " (str/join ", " (map #(str "`" % "`") aspects)))
            ""
            (when tier (str "**Tier:** `" tier "`"))
            (when domain (str "**Domain:** `" domain "`"))
            ""
            (when (seq context)
              (str "**Context (inputs):** " (str/join ", " (map #(str "`" % "`") context))))
            (when (seq response)
              (str "**Response (outputs):** " (str/join ", " (map #(str "`" % "`") response))))
            (when (seq deps)
              (str "**Dependencies:** " (str/join ", " (map #(str "`" % "`") deps))))
            ""
            (when docs
              (str "**Documentation:**\n" docs))]
           (remove nil?)
           (str/join "\n")))))

;; =============================================================================
;; EXPORT FOR EXTERNAL TOOLS
;; =============================================================================

(defn export-definitions
  "Export definitions in a format external tools can use.

  Returns sequence of maps with searchable content."
  []
  (for [identity-set (q/all-identities (cid/current-registry))
        :let [value (cid/fetch identity-set)
              dev-id (:atlas/dev-id value)]]
    {:dev-id (str dev-id)
     :aspects (mapv str (sort identity-set))
     :context (mapv str (or (:interface-endpoint/context value) []))
     :response (mapv str (or (:interface-endpoint/response value) []))
     :deps (mapv str (or (:execution-function/deps value) []))
     :docs (:docs/content value)
     :searchable-string (str dev-id " " (str/join " " identity-set))}))

(defn write-search-index
  "Write a search index file for external tools.

  Creates .clj-kondo/.cache/semantic-search-index.edn"
  []
  #?(:clj
     (let [defs (vec (export-definitions))
           index-file ".clj-kondo/.cache/semantic-search-index.edn"]
       (io/make-parents index-file)
       (spit index-file (pr-str defs))
       {:file index-file
        :entries (count defs)})
     :cljs (platform/unsupported! :write-search-index)))

;; =============================================================================
;; COMPLETIONS - For editor autocomplete
;; =============================================================================

(defn complete-dev-id
  "Return dev-ids matching a prefix for autocomplete."
  [prefix]
  (let [prefix-str (if (keyword? prefix) (str prefix) prefix)]
    (->> (q/all-identities (cid/current-registry))
         (map #(:atlas/dev-id (cid/fetch %)))
         (filter #(str/starts-with? (str %) prefix-str))
         (sort)
         (into []))))

(defn complete-aspect
  "Return aspects matching a prefix for autocomplete."
  [prefix]
  (let [prefix-str (if (keyword? prefix) (str prefix) prefix)]
    (->> (q/aspect-frequency (cid/current-registry))
         (map first)
         (filter #(str/starts-with? (str %) prefix-str))
         (sort)
         (into []))))

;; =============================================================================
;; USAGE EXAMPLES
;; =============================================================================

(comment
  ;; Find all usages of a dev-id in source files
  (find-dev-id-usages :component/db)
  ;; => [{:file "src/app/core.clj" :line 42 :content "(init :component/db)"}
  ;;     {:file "test/app/test.clj" :line 10 :content "..."}]

  ;; Find all components in foundation tier
  (find-by-aspect :tier/foundation)
  ;; => [{:dev-id :component/db :identity #{...} ...}]

  ;; Generate hover text
  (println (hover-info :component/db))
  ;; **Semantic Identity**
  ;; **Dev-ID:** `:component/db`
  ;; **Aspects:** `:domain/users`, `:semantic-namespace/component`, `:tier/foundation`
  ;; ...

  ;; Autocomplete
  (complete-dev-id ":component/")
  ;; => [:component/db :component/cache :component/stripe]

  (complete-aspect ":tier/")
  ;; => [:tier/api :tier/foundation :tier/service]

  ;; Export for external search
  (write-search-index)
  ;; => {:file ".clj-kondo/.cache/semantic-search-index.edn" :entries 23}
  )
