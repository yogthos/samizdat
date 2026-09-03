;; Vendored from oakes/parinferish (src/parinferish/core.cljc) and NOT ours to
;; relicense, so this file carries no samizdat copyright notice. Upstream
;; dedicates every file that originates from the project to the PUBLIC DOMAIN
;; (README, "Licensing"), which is redistributable on any terms including this
;; tree's GPL-3.0-or-later.
;;
;; Taken verbatim but for ONE line. The changes are the extension (.cljc ->
;; .clj: nothing in samizdat's src/ is cljc, and the file has no reader
;; conditionals to strip), this header, and `count-last-line` passing "\n"
;; instead of \newline to clojure.string/last-index-of — marked inline as a
;; PORT CHANGE. Jolt's last-index-of takes a string needle where the JVM's
;; also accepts a char, and that line is reached by any multi-line string
;; token, so unported it threw out of `parse` on precisely the truncated
;; input this is here to repair. There is no other interop in the file: it
;; needs clojure.string, volatile! and regex literals, all of which Jolt has.
;;
;; WHY IT IS HERE. samizdat's own repair (samizdat.lisp/balance) works from
;; the DELIMITER STACK: it can append the closers a trailing truncation is
;; missing, and it refuses everything else, because closing a mid-file
;; imbalance from the stack alone re-parents the code around it. parinfer
;; works from INDENTATION, which is the information the stack does not have
;; and the one a model's output actually carries — given
;;
;;     (let [x 1
;;           y 2
;;       (+ x y))
;;
;; the stack says "mismatch at the last paren" and stops; indentation says the
;; binding vector ended before `(+`, and the `]` goes there. That is a repair
;; rather than a guess, and it is the failure samizdat measures most
;; (karamazov-5wb).
(ns parinferish.core
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [flatten]))

(def ^:private regexes
  [[:newline-and-indent #"^\n[ ]*"]
   [:whitespace         #"^[ \t\r,]+"]
   [:special-char       #"^['`~^@]"]
   [:delimiter          #"^[\[\]{}()]"]
   [:delimiter          #"^#\{"]
   ;; the next regex only matches the beginning of a string.
   ;; the rest is read by the parse-string function below.
   ;; parsing entire strings via regex was causing a stack
   ;; overflow error, so i'm doing it manually instead
   [:string             #"^\""]
   [:character          #"^\\\S"]
   [:backslash          #"^\\"]
   [:comment            #"^;.*"]
   [:number             #"^[+-]?\d+[/\.]?[a-zA-Z\d]*"]
   [:symbol             #"^[^\s\[\]{}('\"`,;)\\]+"]])

(def ^:private whitespace?
  #{:newline-and-indent :whitespace :comment
    ;; a lone backslash is not actually whitespace,
    ;; but it's here to prevent it from being immediately
    ;; followed by an end paren, since that would cause it
    ;; to be escaped and turned into a character
    :backslash})

(def ^:private open-delims #{"#{" "(" "[" "{"})
(def ^:private close-delims #{"}" ")" "]"})
(def ^:private delims {"#{" "}"
                       "(" ")"
                       "[" "]"
                       "{" "}"})

(defn- count-newlines [s]
  (let [*counter (volatile! 0)]
    (doseq [ch s]
      (when (= ch \newline)
        (vswap! *counter inc)))
    @*counter))

(defn- count-last-line [s]
  ;; PORT CHANGE: "\n" rather than \newline. Jolt's clojure.string/last-index-of
  ;; takes a string needle where the JVM's also accepts a char, and this is
  ;; reached for any multi-line string token — so on Jolt the unported line
  ;; threw "#\newline is not a string" out of parse for exactly the truncated,
  ;; unterminated-string input this is here to repair.
  (when-let [index (str/last-index-of s "\n")]
    (count (subs s (inc index)))))

(defn- read-token [group-name token *error *line *column *indent last-token]
  (let [token-data [(if (and (= group-name :symbol)
                             (str/starts-with? token ":"))
                      :keyword
                      group-name)
                    token]
        line (cond
               (= group-name :newline-and-indent) (vswap! *line inc)
               (= group-name :string) (vswap! *line + (count-newlines token))
               :else @*line)
        start-column (if (= group-name :newline-and-indent)
                       -1
                       @*column)
        end-column (cond
                     (= group-name :newline-and-indent)
                     (vreset! *column (dec (count token)))
                     (= group-name :string)
                     ;; multiline strings need to reset the column number correctly
                     (if-let [n (count-last-line token)]
                       (vreset! *column n)
                       (vswap! *column + (count token)))
                     :else
                     (vswap! *column + (count token)))
        indent (cond
                 (and (= :delimiter group-name)
                      (open-delims token))
                 (vreset! *indent end-column)
                 (= group-name :newline-and-indent)
                 (vreset! *indent (dec (count token)))
                 :else
                 @*indent)
        token-data (vary-meta token-data assoc
                     :line line
                     :column start-column
                     :indent indent)]
    (cond-> token-data
            (whitespace? group-name)
            (vary-meta assoc :whitespace? true)
            (and (= group-name :string)
                 (not (str/ends-with? token "\"")))
            (vary-meta assoc :error-message (vreset! *error "Unbalanced quote"))
            (and (= group-name :newline-and-indent)
                 (= (first last-token) :backslash))
            (vary-meta assoc :error-message (vreset! *error "Backslash at end of line")))))

(declare read-structured-token)

(defn- wrap-coll [data]
  (let [first-meta (-> data first meta)]
    (vary-meta (into [:collection] data)
      assoc :indent (:indent first-meta))))

(defn- insert-token [data token-data]
  (conj data (vary-meta token-data assoc :action :insert)))

(defn- remove-token [data token-data]
  (conj data (vary-meta token-data assoc :action :remove)))

(defn- insert-delim [data end-delim]
  (let [first-meta (-> data first meta)
        token-data (vary-meta [:delimiter end-delim] assoc
                     :indent (:indent first-meta)
                     :action :insert)
        [last-data first-data]
        (->> data
             reverse
             (split-with #(-> % first whitespace?))
             (mapv reverse)
             (mapv vec))]
    (into (conj first-data token-data)
          last-data)))

(defn- read-next-tokens-with-indent [flat-tokens {:keys [*index] :as opts}]
  (loop [data []
         ignore-data []
         last-index @*index
         new-line? false]
    (if-let [[group _ :as token-data] (read-structured-token flat-tokens opts)]
      (cond
        (= :delimiter group)
        (recur
          data
          (conj ignore-data (vary-meta token-data assoc :action :remove))
          last-index
          new-line?)
        (whitespace? group)
        (recur
          data
          (conj ignore-data token-data)
          last-index
          (or new-line? (= group :newline-and-indent)))
        :else
        (if new-line?
          (recur
            (-> data
                (into ignore-data)
                (conj token-data))
            []
            @*index
            new-line?)
          (recur
            data
            (conj ignore-data token-data)
            last-index
            new-line?)))
      (do
        (vreset! *index last-index)
        data))))

(defn- read-coll-indent-mode [flat-tokens [_ delim :as token-data] {:keys [*index] :as opts}]
  (let [end-delim (delims delim)
        indent (-> token-data meta :indent)
        opts (assoc opts :min-indent indent)]
    (loop [data [token-data]
           whitespace-data []
           last-index @*index]
      (if-let [[group token :as token-data] (read-structured-token flat-tokens opts)]
        (cond
          (whitespace? group)
          (recur data (conj whitespace-data token-data) last-index)
          (< (-> token-data meta :indent) indent)
          (do
            (vreset! *index last-index)
            (-> data
                (insert-delim end-delim)
                wrap-coll))
          (= :delimiter group)
          (if (= token end-delim)
            (let [tokens-to-move (read-next-tokens-with-indent flat-tokens opts)]
              (if (seq tokens-to-move)
                (-> data
                    (into whitespace-data)
                    (remove-token token-data)
                    (into tokens-to-move)
                    (insert-token token-data)
                    wrap-coll)
                (-> data
                    (into whitespace-data)
                    (conj token-data)
                    wrap-coll)))
            (recur
              (-> data
                  (into whitespace-data)
                  (remove-token token-data))
              []
              @*index))
          :else
          (recur
            (-> data
                (into whitespace-data)
                (conj token-data))
            []
            @*index))
        (do
          (vreset! *index last-index)
          (-> data
              (insert-delim end-delim)
              wrap-coll))))))

(defn- read-coll-paren-mode [flat-tokens [_ delim :as token-data] {:keys [indent-change *error] :as opts}]
  (let [end-delim (delims delim)
        min-indent (cond-> (-> token-data meta :indent)
                           indent-change
                           (+ indent-change))
        opts (dissoc opts :min-indent)]
    (loop [data [token-data]
           max-indent nil
           indent-change (or indent-change 0)]
      (if-let [[group token :as token-data] (read-structured-token flat-tokens
                                              (assoc opts :indent-change indent-change))]
        (cond
          (= :newline-and-indent group)
          (let [current-indent (-> token-data meta :indent)
                indent-change (cond
                                (< current-indent min-indent)
                                (- min-indent current-indent)
                                (and max-indent (> current-indent max-indent))
                                (- max-indent current-indent)
                                :else
                                0)]
            (recur
              (cond
                (pos? indent-change)
                (-> data
                    (conj token-data)
                    (conj (vary-meta [:whitespace (str/join (repeat indent-change " "))]
                            assoc :action :insert :whitespace? true)))
                (neg? indent-change)
                (-> data
                    (conj (update token-data 1 subs 0 (+ (count token) indent-change)))
                    (conj (vary-meta [:whitespace (str/join (repeat (* -1 indent-change) " "))]
                            assoc :action :remove :whitespace? true)))
                :else
                (conj data token-data))
              max-indent
              indent-change))
          (= :collection group)
          (recur
            (conj data token-data)
            (cond-> (-> token-data meta :indent dec)
                    max-indent
                    (min max-indent))
            indent-change)
          (= :delimiter group)
          (cond-> (wrap-coll (conj data token-data))
                  (not= token end-delim)
                  (vary-meta assoc :error-message (vreset! *error "Unmatched delimiter")))
          :else
          (recur (conj data token-data) max-indent indent-change))
        (vary-meta (wrap-coll data)
          assoc :error-message (vreset! *error "EOF while reading"))))))

(defn- read-coll [flat-tokens [_ delim :as token-data] {:keys [*index *error] :as opts}]
  (let [end-delim (delims delim)]
    (loop [data [token-data]]
      (if-let [[group token :as token-data] (read-structured-token flat-tokens opts)]
        (if (= :delimiter group)
          (cond-> (wrap-coll (conj data token-data))
                  (not= token end-delim)
                  (vary-meta assoc :error-message (vreset! *error "Unmatched delimiter")))
          (recur (conj data token-data)))
        (vary-meta (wrap-coll data)
          assoc :error-message (vreset! *error "EOF while reading"))))))

(defn- read-structured-token [flat-tokens {:keys [*index mode min-indent] :as opts}]
  (when-let [[group token :as token-data] (get flat-tokens (vswap! *index inc))]
    (when (or (nil? min-indent)
              (whitespace? group)
              (-> token-data meta :column (>= min-indent)))
      (if (and (= :delimiter group)
               (open-delims token))
        (case mode
          :indent (read-coll-indent-mode flat-tokens token-data opts)
          :paren (read-coll-paren-mode flat-tokens token-data opts)
          :smart (let [{:keys [cursor-line cursor-column]} opts
                       {:keys [line column]} (meta token-data)]
                   (if (or (< line cursor-line)
                           (and (= line cursor-line)
                                (< column cursor-column)))
                     (read-coll-indent-mode flat-tokens token-data opts)
                     (read-coll-paren-mode flat-tokens token-data opts)))
          nil (read-coll flat-tokens token-data opts))
        token-data))))

(defn- read-useful-token [flat-tokens {:keys [mode *error] :as opts}]
  (when-let [[_ token :as token-data] (read-structured-token flat-tokens opts)]
    (cond
      (close-delims token)
      (if (#{:indent :smart} mode)
        (vary-meta token-data assoc :action :remove)
        (vary-meta token-data assoc :error-message (vreset! *error "Unmatched delimiter")))
      :else
      token-data)))

(defn- parse-string [token input-str]
  (loop [token token
         input-str (subs input-str (count token))]
    (if-let [ch (first input-str)]
      (case ch
        \\
        (if (> (count input-str) 1)
          (recur (str token (subs input-str 0 2)) (subs input-str 2))
          (str token ch))
        \"
        (str token ch)
        (recur (str token ch) (subs input-str 1)))
      token)))

(defn parse
  "Returns a hierarcichal (I can never spell that goddamn word) vector of tuples
  representing each distinct Clojure token from the input string. Takes an options
  map that allows you to apply parinferish via the :mode option (:indent, :paren, :smart).
  Note that if you use parinfer, the return value will include both the inserted and removed
  tokens. The tokens parinfer wants to remove will be removed by the `flatten` function."
  ([input]
   (parse input {}))
  ([input opts]
   (when (and (= :smart (:mode opts))
              (or (nil? (:cursor-line opts))
                  (nil? (:cursor-column opts))))
     (throw (ex-info "Smart mode requires :cursor-line and :cursor-column" {})))
   (let [*error (volatile! nil)
         *line (volatile! 0)
         *column (volatile! 0)
         *indent (volatile! 0)
         tokens (loop [input-str input
                       tokens (transient [])
                       last-token nil]
                  (if-let [[group-name token]
                           (some (fn [[group-name regex]]
                                   (when-let [match (re-find regex input-str)]
                                     [group-name match]))
                                 regexes)]
                    (let [token (if (= group-name :string)
                                  (parse-string token input-str)
                                  token)
                          token-data (read-token group-name token *error *line *column *indent last-token)]
                      (recur (subs input-str (count token)) (conj! tokens token-data) token-data))
                    (persistent! tokens)))
         opts (assoc opts
                :*error *error
                :*index (volatile! -1))
         opts (cond-> opts @*error (dissoc :mode))]
     (loop [structured-tokens []]
       (if-let [token-data (read-useful-token tokens opts)]
         (recur (conj structured-tokens token-data))
         (let [{:keys [mode]} opts]
           (vary-meta structured-tokens
             assoc :mode mode :error? (some? @*error))))))))

(defn- node-iter [node-fn nodes node disable-parinfer?]
  (if (= (first node) :collection)
    (->> (reduce
           (fn [v child]
             (node-iter node-fn v child disable-parinfer?))
           []
           (rest node))
         (into (with-meta [:collection] (meta node)))
         node-fn
         (conj nodes))
    (if (if disable-parinfer?
          (-> node meta :action (= :insert))
          (-> node meta :action (= :remove)))
      nodes
      (conj nodes (node-fn node)))))

(defn flatten
  "Takes the result of `parse` and flattens it into a string. Optionally takes a
  `node-fn` which will receive each token and return whatever it wants, so you can
  have more control of what format it outputs to."
  ([parsed-code]
   (->> parsed-code
        (flatten #(-> % rest str/join))
        str/join))
  ([node-fn parsed-code]
   (let [m (meta parsed-code)
         disable-parinfer? (and (= :paren (:mode m))
                                (:error? m))]
     (reduce
       (fn [v code]
         (into v (node-iter node-fn [] code disable-parinfer?)))
       []
       parsed-code))))

(defn- diff-node [*line *column *diff parent-node node]
  (if (vector? node)
    (let [[type & children] node]
      (cond
        (= type :newline-and-indent)
        (do
          (vswap! *line inc)
          (vreset! *column -1))
        (= type :string)
        (vswap! *line + (count-newlines (first children))))
      (run! (partial diff-node *line *column *diff node) children))
    (let [line @*line
          column @*column
          parent-meta (meta parent-node)]
      (if (= :string (first parent-node))
        ;; multiline strings need to reset the column number correctly
        (if-let [n (count-last-line node)]
          (vreset! *column n)
          (vswap! *column + (count node)))
        (vswap! *column + (count node)))
      (when-let [action (:action parent-meta)]
        (let [last-diff (last @*diff)]
          ;; if we are removing the thing we just inserted,
          ;; the actions cancel each other out
          ;; so just remove both from the diff
          (if (and last-diff
                   (= action :remove)
                   (= (:action last-diff) :insert)
                   (= line (:line last-diff))
                   (= 1 (- column (:column last-diff)))
                   (= node (:content last-diff)))
            (vswap! *diff pop)
            (vswap! *diff conj {:line line
                                :column column
                                :content node
                                :action action
                                :type (first parent-node)})))
        (when (= action :remove)
          (vswap! *column - (count node)))))))

(defn diff
  "Takes the result of `parse` and returns a vector of maps describing
  each insertion or removal made by parinferish."
  [parsed-code]
  (let [*line (volatile! 0)
        *column (volatile! 0)
        *diff (volatile! [])
        m (meta parsed-code)
        disable-parinfer? (and (= :paren (:mode m))
                               (:error? m))]
    (when-not disable-parinfer?
      (run! (partial diff-node *line *column *diff nil) parsed-code))
    @*diff))

