;; THE FOLD, as NODES with the routing in the manifest.
;;
;; Compaction used to happen inside request assembly — a side effect of
;; building a call, invisible to every manifest, unreachable by `introspect`,
;; and tunable only by moving a character count. That made it the one part of
;; a turn the agent could not see or rewire, in a harness whose premise is that
;; the workflow is data the agent can rewrite.
;;
;; It is four nodes now, and the decisions between them are DISPATCHES rather
;; than `if`s inside one cell. That distinction is the whole point, and this
;; file got it wrong once: a single cell that measured the pressure and then
;; did all four things internally is the same hardcoding moved out one level —
;; the manifest showed one opaque box. The rule the supervisor's own prompt
;; states is that a dispatch predicate READS a key a cell wrote and never
;; computes. So:
;;
;;   measure  writes :compaction/tier and :compaction/route, touches nothing
;;   cap      clips oversized tool results, then routes again
;;   prune    replaces old tool-result BODIES with one line each
;;   fold     summarises a window, banks what it established, replaces it
;;
;; Each is separately rewireable. A project that wants pruning without folding
;; edges :prune straight to :infer; one that wants no compaction at all drops
;; the nodes. That is what "tunable rather than hardcoded" has to mean.
(ns cells.compaction
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [mycelium.cell :as cell]
            [samizdat.agent.compaction :as cmp]
            [samizdat.agent.gates :as gates]
            [samizdat.llm.client :as llm]
            [samizdat.prompt :as prompt]
            [samizdat.store.journal :as journal]
            [samizdat.store.knowledge :as knowledge]))

(defn- policy [] (gates/threshold :compaction))

(defn- window-of
  "The model's context window. nil when nobody recorded one — and a model whose
  window is unknown gets no ladder rather than a guessed one, because every
  rung is a fraction of it."
  [llm-config]
  (or (:context-window llm-config) (:max-context llm-config)))

(defn- prune-templates
  "The one-line stand-ins a pruned tool result becomes, from policy. Keyed by
  TOOL because the useful line differs: a shell result's is its command, a
  grep's is its match count, a read's is its size."
  [p]
  {:preview-chars (:preview-chars p)
   :render (fn [tmpl ctx] (prompt/render-str (str tmpl) ctx))
   :by-tool (:prune-lines p)
   :default (:prune-line-default p)})

(defn- note!
  "Record what the pass did. Best effort — a journal that refuses must not
  stop a compaction the context pressure requires."
  [conn run-id data]
  (try (journal/note! conn run-id :compaction {:data data}) (catch Throwable _ nil)))

;; --- measure -----------------------------------------------------------------

(cell/defcell :compaction/measure
  {:doc "Read the branch's context pressure and write down what should happen.

        Writes :compaction/tier (the rung reached, or nil) and
        :compaction/route, the key the manifest dispatches on. Touches no
        messages: measuring and acting are different jobs, and a cell that did
        both is exactly why the routing was invisible.

        A model with no recorded context window routes :none. Every rung is a
        fraction of that number, so guessing it would be guessing the policy."
   :pure true
   :requires [:llm-config]
   :input  [:map [:branch :map]]
   ;; One output rather than per-transition: both edges leave with the same
   ;; keys and only :compaction/route differs in VALUE, which is the whole
   ;; design — measure writes the key, the manifest dispatches on it.
   ;; :compaction/tier is nil when no window is recorded, hence :any.
   :output [:map [:compaction/tier :any] [:compaction/ratio :any]
            [:compaction/before :any] [:compaction/route :keyword]]}
  (fn [{:keys [llm-config]} {:keys [branch] :as data}]
    (let [p (policy)
          window (window-of llm-config)
          msgs (vec (:messages branch))
          tokens (cmp/estimate-tokens msgs (:chars-per-token p))
          ratio (cmp/pressure tokens window)
          t (and window (cmp/tier ratio (:ladder p)))]
      (assoc data
             :compaction/tier t
             :compaction/ratio ratio
             :compaction/before tokens
             ;; Any tier reached at all caps first: it is the cheapest thing
             ;; that can help and it often makes the rest unnecessary.
             :compaction/route (if t :cap :none)))))

;; --- cap ---------------------------------------------------------------------

(cell/defcell :compaction/cap
  {:doc "Clip every oversized tool result outside the protected tail.

        The model that CALLED the tool saw the whole result on its dispatch
        turn; later turns see head and tail with the dropped count between
        them, and can reopen the middle with `fetch_turn`. Head AND tail
        because the head says what ran and the tail says how it ended — a
        plain head truncation drops every error message.

        Then routes: on to a prune-and-fold when the tier calls for one and
        the clip did not buy enough headroom, otherwise straight to the model.
        That check never excuses an aggressive tier, which is how a branch
        reaches 0.9 having never folded."
   :effects [:db]
   :requires [:conn :run-id :llm-config]
   ;; The three :compaction/* keys are REQUIRED, and that is the ladder's own
   ;; contract: this rung reads the pressure the previous one measured. A
   ;; manifest that edges straight to :cap without :measure is a cap sizing its
   ;; clip from nil, which is exactly the rewiring the file header invites and
   ;; the one shape of it that does not work.
   :input  [:map [:branch :map] [:compaction/tier :any]
            [:compaction/ratio :any] [:compaction/before :any]]
   :output [:map [:branch :map] [:compaction/freed :any]
            [:compaction/route :keyword]]}
  (fn [{:keys [conn run-id llm-config]} {:keys [branch] :as data}]
    (let [p (policy)
          window (window-of llm-config)
          t (:compaction/tier data)
          aggressive (cmp/aggressive? t)
          {:keys [messages freed]}
          (cmp/cap-oversized-results
           (vec (:messages branch))
           {:cap-tokens (cmp/result-cap (:compaction/ratio data) (:ladder p) (:result-cap p))
            :chars-per-token (:chars-per-token p)
            :protect-tail (:protect-tail p)
            :roles (:result-roles p)
            :note-fn (fn [n] (prompt/render-str (str (:clip-note p)) {:chars n}))})
          enough? (cmp/snip-bought-enough? freed window (:snip-sufficient p) aggressive)
          route (if (and (cmp/fold? t) (not enough?)) :prune :none)]
      (when (= route :none)
        ;; :before AND :ratio, not just what was freed. Cap is the rung that
        ;; fires most, so it is the one whose calibration most needs auditing —
        ;; and "freed 518 tokens at tier aggressive-cap" cannot answer whether
        ;; the tier was right, because the pressure that chose it is missing.
        ;; Fold recorded :before all along; this is cap catching up
        ;; (karamazov-be8).
        (note! conn run-id {:tier (some-> t name) :action "cap" :freed freed
                            :before (:compaction/before data)
                            :ratio (:compaction/ratio data)}))
      (assoc data
             :branch (assoc branch :messages messages)
             :compaction/freed freed
             :compaction/route route))))

;; --- prune -------------------------------------------------------------------

(cell/defcell :compaction/prune
  {:doc "Replace the BODY of older tool results with one line each, keeping the
        call itself and the protected tail whole.

        A different act from folding, and cheaper. A fold replaces a region of
        history with a summary OF it; this leaves every turn where it is and
        removes only the bulk — which is overwhelmingly tool output, the least
        re-read and most voluminous thing in a transcript. What the branch DID
        survives; what it SAW is one `fetch_turn` away.

        Its own node so a project can stop here: edge :prune to :infer and the
        loop prunes without ever paying for a summarizer call."
   :pure true
   :requires []
   ;; Reads the branch and policy only — no :compaction/* input, which is what
   ;; makes the "edge :prune to :infer" rewiring in the docstring legal.
   :input  [:map [:branch :map]]
   :output [:map [:branch :map] [:compaction/route :keyword]]}
  (fn [_ {:keys [branch] :as data}]
    (let [p (policy)]
      (assoc data
             :branch (assoc branch
                            :messages (cmp/prune-tool-outputs
                                       (vec (:messages branch))
                                       {:protect-tail (:protect-tail p)
                                        :min-chars (:prune-min-chars p)
                                        :roles (:result-roles p)
                                        :tool-of :tool
                                        :templates (prune-templates p)}))
             :compaction/route :fold))))

;; --- fold --------------------------------------------------------------------

(defn- summarize!
  "Ask the model for a structured summary of the folded region. Returns the
  text or nil, and nil is a legitimate answer the caller must handle by NOT
  dropping anything."
  [llm-adapter llm-config folded budget]
  (try
    (let [material (str/join "\n\n"
                             (for [m folded]
                               (str (str/upper-case (str (:role m))) ":\n" (str (:content m)))))
          reply (llm/chat llm-adapter llm-config
                          [{:role "system"
                            :content (prompt/render "compaction-summary" {:budget budget})}
                           {:role "user" :content material}])]
      (not-empty (str/trim (str (:content reply)))))
    (catch Throwable e
      (log/warn "compaction: summarizer failed:" (ex-message e))
      nil)))

(defn- template-sections
  "The section names the shipped template asks for — read from the prompt, so a
  project that rewords its template does not also have to edit a list."
  []
  (->> (str/split-lines (str (prompt/prompt "compaction-summary")))
       (keep #(second (re-find #"^##\s+(.*)$" (str/trim %))))
       (map str/trim)
       vec))

(defn- distil!
  "What the folded region ESTABLISHED, into the knowledge store, BEFORE the
  region goes.

  THIS IS WHAT MAKES FOLDING EARLY SAFE. A summary is one message and lossy by
  construction; a memory is durable, ranked, surfaced in the breadcrumb index
  on every later turn, and expandable with `recall {id}`. So a fold moves
  detail into a store the branch can query rather than destroying it — the
  difference between compacting at 75% and hoping, and compacting at 75% and
  knowing.

  Best effort: a knowledge store that refuses must never stop a fold the
  context pressure requires."
  [conn run-id summary {:keys [max-memories min-chars kind]}]
  (try
    (let [facts (->> (str/split-lines (str summary))
                     (map str/trim)
                     ;; The bullet lines, which is where the concrete material
                     ;; is — headings are structure, paragraphs are narrative.
                     (filter #(str/starts-with? % "- "))
                     (map #(str/replace % #"^-\s+" ""))
                     (remove #(< (count %) min-chars))
                     distinct
                     (take max-memories))]
      (doseq [f facts]
        (knowledge/remember! conn {:content f :kind kind :run-id run-id}))
      (count facts))
    (catch Throwable e
      (log/warn "compaction: distilling the fold failed:" (ex-message e))
      0)))

(cell/defcell :compaction/fold
  {:doc "Summarise a window of history, bank what it established, and replace
        it with the summary.

        THE ORDER IS LOAD-BEARING: distil BEFORE replacing. Those memories are
        what the breadcrumb index carries forward and what `recall` gets back;
        writing them after the region is gone would mean summarising a summary.

        A summarizer that returns a stub does NOT get to destroy history —
        validate-summary refuses, the pruned context is kept, and the refusal
        is recorded. Same for a fold that would free nothing: a pass that
        changes nothing must not rewrite anything, or a branch whose
        unfoldable overhead alone clears a threshold folds every turn forever."
   :effects [:net :db]
   :requires [:conn :run-id :llm-adapter :llm-config]
   ;; :compaction/before is required because the no-progress check compares
   ;; against it: a fold that cannot tell whether it shrank anything is the
   ;; every-turn-forever loop the docstring ends on.
   :input  [:map [:branch :map] [:compaction/before :any]
            [:compaction/tier :any]]
   ;; Every failure path returns `data` unchanged, so :branch is all this may
   ;; promise — and it promises it because the success path replaces the
   ;; messages in place.
   :output [:map [:branch :map]]}
  (fn [{:keys [conn run-id llm-adapter llm-config]} {:keys [branch] :as data}]
    (try
      (let [p (policy)
            msgs (vec (:messages branch))
            tail (if (cmp/aggressive? (:compaction/tier data))
                   (:aggressive-tail p) (:protect-tail p))
            [s e] (cmp/compress-window msgs (:protect-head p) tail)]
        (if (>= s e)
          (do (note! conn run-id {:tier (some-> (:compaction/tier data) name)
                                  :action "prune-only" :why "no window to fold"})
              data)
          (let [folded (subvec msgs s e)
                budget (cmp/summary-budget
                        (cmp/estimate-tokens folded (:chars-per-token p))
                        (:summary p))
                summary (summarize! llm-adapter llm-config folded budget)
                ok? (and summary
                         (cmp/validate-summary
                          summary
                          {:sections (template-sections)
                           :min-sections (:min-summary-sections p)
                           :empties (:empty-bodies p)}))]
            (if-not ok?
              (do (note! conn run-id
                         {:tier (some-> (:compaction/tier data) name)
                          :action "prune-only"
                          :why (if summary "summary rejected" "summarizer produced nothing")})
                  data)
              (let [kept (distil! conn run-id summary (:distil p))
                    out (cmp/apply-summary msgs [s e]
                                           (prompt/prompt "compaction-marker") summary)
                    after (cmp/estimate-tokens out (:chars-per-token p))]
                (if (>= after (:compaction/before data))
                  (do (note! conn run-id {:action "no-progress"
                                          :before (:compaction/before data) :after after})
                      data)
                  (do (note! conn run-id
                             {:tier (some-> (:compaction/tier data) name)
                              :action "fold" :folded (- e s)
                              :before (:compaction/before data) :after after
                              :memories kept})
                      (assoc data :branch (assoc branch :messages out)))))))))
      (catch Throwable ex
        ;; A branch that cannot be compacted runs on. The overflow it may hit
        ;; is worse, but it is the model's to report, not this pass's to cause.
        (log/warn "compaction fold failed:" (ex-message ex))
        data))))
