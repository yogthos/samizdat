;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.userspace-test
  "The base/userspace seam.

  What is under test is the property that makes userspace userspace: a project
  gets its OWN copy of the shipped template, evolves it, and neither the
  harness's files nor another project's copy is affected. A layer that is
  shared is not userspace no matter which directory it lives in."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [jolt.fs :as jfs]
            [samizdat.store.db :as db]
            [samizdat.store.runs :as runs]
            [samizdat.store.journal :as journal]
            [samizdat.store.userspace :as store]
            [samizdat.system :as system]
            [samizdat.userspace :as us]
            [samizdat.agent.state :as state]
            [samizdat.agent.tools :as tools]
            [samizdat.agent.gates :as gates]
            [samizdat.manual :as manual]
            [samizdat.prompt :as prompt]))

(def ^:dynamic *conn* nil)

(defn- with-project [f]
  (let [c (db/open! ":memory:")]
    (try (binding [*conn* c] (f))
         (finally (db/close c)))))

(use-fixtures :each with-project (fn [f] (try (f) (finally (us/unbind!)))))

;; --- the store ---------------------------------------------------------------

(deftest saves-are-appended-never-updated
  ;; The edit history of a system that rewrites itself is the most valuable
  ;; thing in its database.
  (is (= 1 (store/save! *conn* :cell "loop" "v1")))
  (is (= 2 (store/save! *conn* :cell "loop" "v2")))
  (is (= 3 (store/save! *conn* :cell "loop" "v3")))
  (is (= "v3" (:body (store/load-latest *conn* :cell "loop"))))
  (is (= "v1" (:body (store/load-version *conn* :cell "loop" 1)))
      "an older version stays readable — that is what makes rollback possible")
  (is (= [1 2 3] (mapv :version (store/versions *conn* :cell "loop")))))

(deftest an-edit-that-changed-nothing-is-still-recorded
  (store/save! *conn* :cell "loop" "same")
  (store/save! *conn* :cell "loop" "same")
  (is (= 2 (count (store/versions *conn* :cell "loop")))
      "what the supervisor TRIED is a fact; suppressing it makes the history lie"))

(deftest seed-installs-once-and-never-overwrites
  (store/seed! *conn* :cell "loop" "the template")
  (store/save! *conn* :cell "loop" "the project's own version")
  (store/seed! *conn* :cell "loop" "the template")
  (is (= "the project's own version" (:body (store/load-latest *conn* :cell "loop")))
      "seeding an evolved project must not drag it back to the template")
  (is (= 2 (count (store/versions *conn* :cell "loop")))))

(deftest revert-is-an-edit-not-a-deletion
  (store/save! *conn* :policy "gates" "good")
  (store/save! *conn* :policy "gates" "bad")
  (is (= 3 (store/revert! *conn* :policy "gates" 1)))
  (is (= "good" (:body (store/load-latest *conn* :policy "gates"))))
  (is (= "bad" (:body (store/load-version *conn* :policy "gates" 2)))
      "the failed edit stays where it can be read")
  (is (nil? (store/revert! *conn* :policy "gates" 99))))

(deftest kinds-are-separate-namespaces
  (store/save! *conn* :cell "loop" "a cell")
  (store/save! *conn* :manifest "loop" "a manifest")
  (is (= "a cell" (:body (store/load-latest *conn* :cell "loop"))))
  (is (= "a manifest" (:body (store/load-latest *conn* :manifest "loop")))))

(deftest an-unknown-kind-fails-loud
  ;; A row filed under a typo'd kind is a row nothing will ever read again.
  (is (thrown-with-msg? Exception #"unknown userspace kind"
                        (store/save! *conn* :celll "loop" "x"))))

(deftest latest-bodies-picks-the-newest-of-each
  (store/save! *conn* :cell "a" "a1")
  (store/save! *conn* :cell "a" "a2")
  (store/save! *conn* :cell "b" "b1")
  (store/save! *conn* :manifest "c" "c1")
  (is (= {"a" "a2" "b" "b1"} (store/latest-bodies *conn* :cell))
      "the newest version of each name, and nothing from another kind"))

;; --- the read seam -----------------------------------------------------------

(deftest unbound-reads-the-shipped-template
  (us/unbind!)
  (is (re-find #"defcell :loop/assemble" (us/body :cell "loop")))
  (is (re-find #"tool call" (us/body :prompt "system")))
  (is (map? (us/edn-body :manifest "loop")))
  (is (map? (us/edn-body :policy "gates")))
  (testing "and stores nothing, because there is no project to store it in"
    (is (= [] (us/versions :cell "loop")))))

(deftest a-bound-project-seeds-itself-on-first-read
  (us/bind! *conn*)
  (let [first-read (us/body :cell "loop")]
    (is (re-find #"defcell :loop/assemble" first-read))
    (is (= [1] (mapv :version (us/versions :cell "loop")))
        "reading is what gives the project its copy")
    (is (= first-read (us/template :cell "loop"))
        "and the copy starts identical to the template")))

(deftest the-project-evolves-and-the-template-does-not-follow
  ;; THE property. Two projects, one harness, divergent loops.
  (us/bind! *conn*)
  (us/body :cell "loop")
  (us/save! :cell "loop" "(ns cells.loop) ;; this project's own idea")
  (is (= "(ns cells.loop) ;; this project's own idea" (us/body :cell "loop")))
  (is (re-find #"defcell :loop/assemble" (us/template :cell "loop"))
      "the shipped template is untouched — another project still starts from it")
  (testing "a second project starts from the template, not from this one"
    (let [other (db/open! ":memory:")]
      (try (us/bind! other)
           (is (re-find #"defcell :loop/assemble" (us/body :cell "loop")))
           (finally (db/close other))))))

(deftest userspace-the-harness-never-shipped-is-first-class
  ;; A cell the supervisor wrote has no template by definition.
  (us/bind! *conn*)
  (is (nil? (us/body :cell "invented-by-the-agent")))
  (us/save! :cell "invented-by-the-agent" "(ns cells.invented)")
  (is (= "(ns cells.invented)" (us/body :cell "invented-by-the-agent")))
  (is (nil? (us/template :cell "invented-by-the-agent"))))

(deftest body-bang-fails-loud-and-says-where-it-looked
  (us/bind! *conn*)
  (is (thrown-with-msg? Exception #"cells/nope\.clj"
                        (us/body! :cell "nope"))))

(deftest an-unbound-save-says-so-rather-than-throwing
  (us/unbind!)
  (is (nil? (us/save! :cell "loop" "x"))
      "a REPL or a test editing userspace with no project is a real situation"))

(deftest seed-all-returns-the-projects-bodies-not-the-templates
  (us/bind! *conn*)
  (let [bodies (us/seed-all! :cell ["loop" "critic" "does-not-ship"])]
    (is (contains? bodies "loop"))
    (is (contains? bodies "critic"))
    (is (not (contains? bodies "does-not-ship"))
        "a name with no template and no project version is simply absent"))
  (testing "an evolved cell comes back evolved"
    (us/save! :cell "loop" "evolved")
    (is (= "evolved" (get (us/seed-all! :cell ["loop"]) "loop")))))

(deftest seed-all-unbound-is-the-template-itself
  (us/unbind!)
  (let [bodies (us/seed-all! :cell ["loop" "does-not-ship"])]
    (is (re-find #"defcell" (get bodies "loop")))
    (is (not (contains? bodies "does-not-ship")))))

;; --- manifests came across ---------------------------------------------------

(deftest manifests-live-in-the-one-userspace-store
  ;; store/workflows.clj was a shim that renamed :body to :edn for callers
  ;; written before the userspace table existed. It is gone: manifests are a
  ;; userspace kind like cells, policy and prompts, read the same way, with
  ;; the column called what the table calls it.
  (store/save! *conn* :manifest "loop" "{:description \"mine\"}")
  (is (= "{:description \"mine\"}"
         (:body (store/load-latest *conn* :manifest "loop"))))
  (is (= ["loop"] (mapv :name (store/names *conn* :manifest)))))

(deftest manifest-rows-from-before-the-migration-are-carried-across
  ;; v11 copies the workflows table in. A project that had already evolved its
  ;; loop must not silently lose that work on upgrade.
  (let [c (db/open! ":memory:")]
    ;; The old table still exists and still holds its rows; v11's copy is what
    ;; makes them readable through the new one.
    (db/execute! c ["INSERT OR IGNORE INTO workflows (name, version, edn, created_at)
                     VALUES (?, ?, ?, ?)" "legacy" 7 "{:description \"old\"}" (db/now)])
    (db/execute! c ["INSERT OR IGNORE INTO userspace (kind, name, version, body, created_at)
                     SELECT 'manifest', name, version, edn, created_at FROM workflows"])
    (is (= "{:description \"old\"}" (:body (store/load-latest c :manifest "legacy"))))
    (is (= 7 (:version (store/load-latest c :manifest "legacy"))))))

;; --- the `cell` tool: the supervisor's edge into userspace --------------------

(defn- run-cell [conn args]
  (tools/run-tool {:branch (state/new-branch {:id "B1" :problem "p"})
                   :conn conn
                   :tool-name "cell"
                   :args args}))

(deftest the-cell-tool-reports-a-project-with-no-versions-of-its-own
  (us/bind! *conn*)
  (let [r (run-cell *conn* {:action "list"})]
    (is (re-find #"shipped templates" (:result r))
        "a project running the template should be told that, not shown an empty list")))

(deftest the-cell-tool-shows-the-template-before-the-project-has-edited-it
  (us/bind! *conn*)
  (let [r (run-cell *conn* {:action "show" :name "loop"})]
    (is (re-find #"defcell :loop/assemble" (:result r)))))

(deftest the-cell-tool-lists-versions-and-says-when-there-are-none
  (us/bind! *conn*)
  (is (re-find #"still the shipped template"
               (:result (run-cell *conn* {:action "versions" :name "critic"}))))
  (us/save! :cell "critic" ";; mine")
  (is (re-find #"v1" (:result (run-cell *conn* {:action "versions" :name "critic"})))))

(deftest the-cell-tool-reverts-and-keeps-the-abandoned-version-readable
  (us/bind! *conn*)
  (us/save! :cell "critic" ";; v1")
  (us/save! :cell "critic" ";; v2 was a bad idea")
  (let [r (run-cell *conn* {:action "revert" :name "critic" :version "1"
                            :rationale "v2 was a bad idea"})]
    (is (= :neutral (:category r)))
    (is (re-find #"stored as v3" (:result r)))
    (is (= ";; v1" (us/body :cell "critic")))
    (is (= ";; v2 was a bad idea" (:body (store/load-version *conn* :cell "critic" 2)))
        "the version left behind stays readable — the next supervisor sees the attempt")))

(deftest the-cell-tool-refuses-a-revert-to-a-version-that-never-existed
  (us/bind! *conn*)
  (us/save! :cell "critic" ";; v1")
  (let [r (run-cell *conn* {:action "revert" :name "critic" :version "9"
                            :rationale "testing the miss"})]
    (is (= :mechanics (:category r)))
    (is (re-find #"No v9" (:result r)))
    (is (re-find #"v1" (:result r)) "and says what versions there are")))

(deftest a-save-can-read-its-body-from-a-file-under-the-root
  ;; karamazov-lf0: a live supervisor authored a cell fix, wrote it to a file
  ;; — the thing models do reliably — and never landed it, because save only
  ;; took the whole body inline in one JSON string. The write-a-file workflow
  ;; must END in the validated save.
  (us/bind! *conn*)
  (let [root (str (jfs/create-temp-dir))]
    (spit (str root "/fix.clj") ";; the authored fix")
    (testing "the file body reaches the same save the inline body does"
      ;; through the manifest tool (no soak needed): a valid manifest from file
      (spit (str root "/m.edn")
            (slurp (io/resource "manifests/critic.edn")))
      (let [r (tools/run-tool {:branch (state/new-branch {:id "B1" :problem "p"})
                               :conn *conn* :root root
                               :tool-name "manifest"
                               :args {:action "save" :name "from-file-check"
                                      :file "m.edn"
                                      :rationale "a from-file save"}})]
        (is (= :neutral (:category r)) (str (:result r)))
        (is (re-find #"Saved manifest" (:result r)))
        (is (= (slurp (str root "/m.edn")) (us/body :manifest "from-file-check"))
            "what was stored is byte-for-byte the file")))
    (testing "a path outside the root is refused, not read"
      (let [r (run-cell *conn* {:action "save" :name "critic"
                                :file "../../etc/passwd"})]
        (is (= :mechanics (:category r)))
        (is (re-find #"outside the project root" (:result r)))))
    (testing "a missing file is a complaint, not an empty save"
      (let [r (tools/run-tool {:branch (state/new-branch {:id "B1" :problem "p"})
                               :conn *conn* :root root
                               :tool-name "cell"
                               :args {:action "save" :name "critic"
                                      :file "nope.clj"}})]
        (is (= :mechanics (:category r)))
        (is (re-find #"No file nope.clj" (:result r)))))))

;; --- rationale: the commit message of self-modification (karamazov-c58) ------

(deftest a-save-records-why-and-a-revert-names-what-it-undid
  ;; Run c2260271: S0 landed campaign-derived prompt tuning as v3; thirteen
  ;; minutes later S1 — the next supervisor of the same run — reverted to v2,
  ;; because the history showed bodies and timestamps but never WHY, so a
  ;; successor confronted with an unfamiliar delta restored what it
  ;; recognized. Self-tuning without a rationale column is self-oscillation.
  (store/save! *conn* :prompt "p" "v1" "project" "first draft")
  (store/save! *conn* :prompt "p" "v2")
  (let [rows (store/versions *conn* :prompt "p")]
    (is (= "first draft" (:rationale (first rows))))
    (is (nil? (:rationale (second rows)))
        "an absent reason is recorded as absent, never invented"))
  (store/revert! *conn* :prompt "p" 1 "v2 dropped the turn-discipline section")
  (is (= "revert to v1: v2 dropped the turn-discipline section"
         (:rationale (last (store/versions *conn* :prompt "p"))))
      "a revert names what it restored and why — that is what the NEXT reader gets")
  (store/save! *conn* :cell "c" "x")
  (store/revert! *conn* :cell "c" 1)
  (is (= "revert to v1" (:rationale (last (store/versions *conn* :cell "c"))))
      "even a bare revert says it was one"))

(deftest the-tools-refuse-a-save-or-revert-with-no-rationale
  ;; The mutation tools are where the agent's hands touch the history, so
  ;; they are where the reason is demanded — the store stays flexible for
  ;; seeding and tests.
  (us/bind! *conn*)
  (doseq [[tool args]
          [["prompt" {:action "save" :name "p" :body "words"}]
           ["manifest" {:action "save" :name "m" :edn "{:cells {} :edges []}"}]
           ["policy" {:action "save" :name "gates" :edn "{}"}]
           ["cell" {:action "save" :name "c" :clj ";; x"}]
           ["prompt" {:action "revert" :name "p" :version "1"}]
           ["policy" {:action "revert" :name "gates" :version "1"}]
           ["cell" {:action "revert" :name "c" :version "1"}]]]
    (let [r (tools/run-tool {:tool-name tool :branch (state/new-branch {:id "B1" :problem "p"})
                             :conn *conn* :args args})]
      (is (= :mechanics (:category r)) (str tool " " (:action args)))
      (is (re-find #"rationale" (str (:result r))) (str tool " " (:action args))))))

(deftest a-rationale-rides-the-version-and-shows-in-the-history
  (us/bind! *conn*)
  (let [run-prompt (fn [args]
                     (tools/run-tool {:tool-name "prompt"
                                      :branch (state/new-branch {:id "B1" :problem "p"})
                                      :conn *conn* :args args}))]
    (let [r (run-prompt {:action "save" :name "my-note" :body "words"
                         :rationale "teach workers to stop re-reading files"})]
      (is (= :neutral (:category r)) (str (:result r))))
    (is (re-find #"teach workers to stop re-reading files"
                 (:result (run-prompt {:action "versions" :name "my-note"})))
        "the history shows the reason next to the version")
    (run-prompt {:action "save" :name "my-note" :body "v2 words"
                 :rationale "second thoughts"})
    (let [r (run-prompt {:action "revert" :name "my-note" :version "1"
                         :rationale "v2 lost the point"})]
      (is (= :neutral (:category r)) (str (:result r))))
    (is (re-find #"revert to v1: v2 lost the point"
                 (:result (run-prompt {:action "versions" :name "my-note"}))))))

(deftest green-runs-earn-standing-on-the-versions-that-ran-them
  ;; c58's third leg: a tuning that has survived green runs has EARNED
  ;; something a fresh supervisor should weigh before reverting it, and the
  ;; history is where that standing has to show.
  (us/bind! *conn*)
  (us/body :prompt "system")
  (us/save! :prompt "mine" "project tuning" "because")
  (us/save! :prompt "mine" "newer tuning" "more")
  (us/record-run-outcome! true)
  (us/record-run-outcome! true)
  (us/record-run-outcome! false)
  (let [rows (store/versions *conn* :prompt "mine")]
    (is (= [0 2] (mapv :success_count rows))
        "only the version that was CURRENT is credited, not its ancestors")
    (is (= [0 1] (mapv :failure_count rows))))
  (is (= 0 (:success_count (store/load-latest *conn* :prompt "system")))
      "factory rows carry no standing — they are the baseline, not a tuning")
  (testing "the versions listing shows it"
    (let [r (tools/run-tool {:tool-name "prompt"
                             :branch (state/new-branch {:id "B1" :problem "p"})
                             :conn *conn*
                             :args {:action "versions" :name "mine"}})]
      (is (re-find #"2 green" (:result r)) (str (:result r)))))
  (testing "unbound it is a quiet no-op, like every other unbound write"
    (us/unbind!)
    (is (nil? (us/record-run-outcome! true)))))

(deftest the-cell-tool-complains-usefully-about-a-missing-argument
  (us/bind! *conn*)
  (doseq [args [{} {:action "show"} {:action "save" :name "critic"}]]
    (let [r (run-cell *conn* args)]
      (is (= :mechanics (:category r)) (str "for " (pr-str args)))
      (is (seq (:result r))))))

(deftest an-unknown-cell-action-lists-the-real-ones
  (us/bind! *conn*)
  (let [r (run-cell *conn* {:action "frobnicate"})]
    (is (re-find #"Unknown cell action" (:result r)))
    (is (re-find #"revert" (:result r)))))

;; --- every layer is per-project ----------------------------------------------

(deftest all-four-kinds-resolve-to-the-project-and-fall-back-to-the-template
  ;; The acceptance criterion for the seam: the supervisor's prompt tells it
  ;; that cells, manifests, thresholds and prompts all belong to this project.
  ;; This is what makes that true rather than aspirational.
  (us/bind! *conn*)
  (testing "a policy threshold"
    (is (= 3 (gates/threshold :cull-threshold)) "the template's value")
    (us/save! :policy "gates"
              (pr-str (assoc-in (us/edn-body :policy "gates")
                                [:cull-threshold :value] 99)))
    (gates/reload-config!)
    (is (= 99 (gates/threshold :cull-threshold))
        "this project decided its branches get more rope"))
  (testing "a prompt"
    (us/save! :prompt "cull-reprieve" "this project's own reprieve wording")
    (is (= "this project's own reprieve wording" (prompt/prompt "cull-reprieve"))))
  (testing "the manual — which capabilities the agent is told it has"
    (us/save! :policy "manual"
              (pr-str [{:group "Mine"
                        :entries [{:name 'samizdat.tape/depth
                                   :summary "how long the tape is"}]}]))
    (is (= ["Mine"] (mapv :group (manual/groups)))))
  (testing "and none of it wrote the harness's own files"
    (is (re-find #":cull-threshold" (us/template :policy "gates")))
    ;; The VALUE, not the digits. A bare "99" anywhere in a 2000-line
    ;; gates.edn — another threshold, a doc citing a run id — collided with
    ;; this sentinel and failed a test about something else entirely. Assert
    ;; the thing actually at stake: the template still holds its own value.
    (is (= 3 (get-in (edn/read-string (us/template :policy "gates"))
                     [:cull-threshold :value]))
        "the project's 99 did not write through to the shipped template")
    (is (not= "this project's own reprieve wording" (us/template :prompt "cull-reprieve")))
    (is (re-find #"The tape" (us/template :policy "manual")))))

(deftest a-second-project-is-unaffected-by-the-first
  (us/bind! *conn*)
  (us/save! :policy "gates"
            (pr-str (assoc-in (us/edn-body :policy "gates")
                              [:cull-threshold :value] 99)))
  (gates/reload-config!)
  (is (= 99 (gates/threshold :cull-threshold)))
  (let [other (db/open! ":memory:")]
    (try
      (us/bind! other)
      (gates/reload-config!)
      (is (= 3 (gates/threshold :cull-threshold))
          "two projects on one binary, two cull thresholds — the whole point")
      (finally (db/close other)))))

(deftest unbinding-restores-the-template-everywhere
  (us/bind! *conn*)
  (us/save! :prompt "cull-reprieve" "project wording")
  (us/unbind!)
  (is (not= "project wording" (prompt/prompt "cull-reprieve"))
      "a test or a bare REPL sees the harness as shipped")
  (gates/reload-config!)
  (is (= 3 (gates/threshold :cull-threshold))))

(deftest a-harness-upgrade-reaches-an-untouched-project-entry
  ;; A project seeds its own copy of every template on first read, and that
  ;; copy used to be authoritative forever. Live: a project seeded gates.edn
  ;; on its first read, a threshold added to the harness afterwards was
  ;; missing from that project's table, and the rule reading it threw rather
  ;; than being absent. Entries seed lazily at first USE, so the project ended
  ;; up on a sediment of whatever harness version touched each one first.
  (let [conn (db/open! ":memory:")]
    (testing "an untouched entry follows the shipped template"
      (store/seed! conn :prompt "p" "v1 body")
      (store/seed! conn :prompt "p" "v2 body")
      (is (= "v2 body" (:body (store/load-latest conn :prompt "p"))))
      (is (= 1 (:version (store/load-latest conn :prompt "p")))
          "refreshed in place — appending would make it look edited and stop it following the next upgrade"))
    (testing "a save of a name nothing ever seeded is the project's own, version 1 or not"
      (store/save! conn :prompt "s" "written, never seeded")
      (store/seed! conn :prompt "s" "a template that appeared later")
      (is (= "written, never seeded" (:body (store/load-latest conn :prompt "s")))
          "the version number cannot tell these apart; the source column can"))
    (testing "an entry the project edited is never overwritten"
      (store/seed! conn :prompt "q" "factory")
      (store/save! conn :prompt "q" "the supervisor's version")
      (store/seed! conn :prompt "q" "a newer factory")
      (is (= "the supervisor's version" (:body (store/load-latest conn :prompt "q")))))
    (testing "and its history still reverts to what the project started from"
      (is (= "factory" (:body (store/load-version conn :prompt "q" 1)))))
    (testing "an identical template is not a write"
      (store/seed! conn :prompt "r" "same")
      (let [before (:created_at (store/load-latest conn :prompt "r"))]
        (store/seed! conn :prompt "r" "same")
        (is (= before (:created_at (store/load-latest conn :prompt "r"))))
        (is (= 1 (count (store/versions conn :prompt "r"))))))))

(deftest binding-a-project-serves-its-policy-not-the-templates
  ;; system/start! used to run the three policy reloads ~35 lines BEFORE
  ;; userspace/bind!, so gates/lexicon/phases cached the shipped templates and
  ;; nothing re-read them after the project bound — a project whose policy had
  ;; diverged silently ran factory numbers for the whole process lifetime
  ;; (karamazov-blt.1). bind-project! is the one seam that carries the order.
  (let [path (str "/tmp/samizdat-bind-order-" (random-uuid) ".sqlite3")
        c (db/open! path)]
    (try
      (us/bind! c)
      (let [g (us/edn-body! :policy "gates")]
        (us/save! :policy "gates" (pr-str (assoc-in g [:cull-threshold :value] 999))))
      (us/unbind!)
      (db/close c)
      (gates/reload-config!)
      (is (not= 999 (gates/threshold :cull-threshold))
          "unbound, the template's number serves — the project has not leaked")
      (let [c2 (db/open! path)]
        (try
          (system/bind-project! c2)
          (is (= 999 (gates/threshold :cull-threshold))
              "the bind seam reloads policy AFTER binding, so the project's number wins")
          (finally
            (us/unbind!)
            (gates/reload-config!)
            (db/close c2))))
      (finally
        (when (us/bound?) (us/unbind!))
        (gates/reload-config!)
        (doseq [suffix ["" "-wal" "-shm"]]
          (.delete (java.io.File. (str path suffix))))))))

(deftest a-write-during-a-read-is-not-clobbered-by-the-stale-fill
  ;; The cache fill was compute-then-swap!: a save! + invalidate! landing
  ;; between the two re-installed the pre-edit body, which then served until
  ;; the NEXT write — the supervisor's edit silently not taking, the exact
  ;; failure the cache docstring warns about (karamazov-blt.8). The fill now
  ;; carries the generation it read under and refuses to cache across an
  ;; invalidation.
  (let [c (db/open! ":memory:")]
    (try
      (us/bind! c)
      (us/save! :cell "race-target" "v1")
      (with-redefs [store/load-latest
                    (let [orig store/load-latest]
                      (fn [conn kind nm]
                        (let [r (orig conn kind nm)]
                          ;; a writer lands between the read and the cache fill
                          (when (and (= :cell kind) (= "race-target" nm))
                            (store/save! conn kind nm "v2")
                            (us/invalidate!))
                          r)))]
        (is (= "v1" (us/body :cell "race-target"))
            "the read that raced returns what it read — stale once is fine"))
      (is (= "v2" (us/body :cell "race-target"))
          "the next read serves the write; the stale fill did not stick")
      (finally (us/unbind!) (db/close c)))))

(deftest the-policy-tool-moves-a-threshold-live-and-rolls-back-a-broken-table
  ;; RFC-010 names "move a threshold" as a supervisor instrument and the
  ;; supervisor prompt says so, but no tool wrote the :policy kind — the only
  ;; route was raw eval plus knowing to call reload-config!, undiscoverable
  ;; (karamazov-blt.5). The tool saves, recompiles, and rolls back a save the
  ;; recompile rejects, so a typo in gates.edn cannot take the harness down.
  (let [c (db/open! ":memory:")]
    (try
      (us/bind! c)
      (gates/reload-config!)
      (let [before (gates/threshold :cull-threshold)
            g (us/edn-body! :policy "gates")
            r (tools/run-tool {:tool-name "policy" :branch {:id "B1"}
                               :args {:action "save" :name "gates"
                                      :edn (pr-str (assoc-in g [:cull-threshold :value] 42))
                                      :rationale "branches deserve more rope"}})]
        (is (= :neutral (:category r)) (str (:result r)))
        (is (= 42 (gates/threshold :cull-threshold))
            "the saved threshold is live immediately — no restart, no new run")
        (is (not= before 42) "and the test genuinely moved it")
        ;; parseable EDN whose steer table cannot compile: a gate whose :when
        ;; references a symbol that resolves to nothing (:gates is a VECTOR,
        ;; ordered by priority)
        (let [bad (update g :gates conj
                          {:gate :broken-gate
                           :priority 1
                           :when '(no-such-fn-xyz branch)
                           :message-suffix "x"
                           :prediction {:kind :tool-called :window 1}})
              r2 (tools/run-tool {:tool-name "policy" :branch {:id "B1"}
                                  :args {:action "save" :name "gates"
                                         :edn (pr-str bad)
                                         :rationale "a broken gate on purpose"}})]
          ;; :mechanics since karamazov-gn64. The table was rolled back and the
          ;; harness is where it started, so this is a correctable edit rather
          ;; than evidence about the branch's work — what the test is about is
          ;; the rollback below.
          (is (= :mechanics (:category r2)))
          (is (= 42 (gates/threshold :cull-threshold))
              "the broken save rolled back; the previous policy is live again"))
        (let [lst (tools/run-tool {:tool-name "policy" :branch {:id "B1"}
                                   :args {:action "list"}})]
          (is (str/includes? (str (:result lst)) "gates"))
          (is (str/includes? (str (:result lst)) "phases")
              "unedited tables list too — the whole surface is discoverable")))
      (finally (us/unbind!) (gates/reload-config!) (db/close c)))))

(deftest seeding-the-same-entry-from-parallel-branches-writes-one-version
  ;; karamazov-cuv. save! takes the writer lock for its insert, but the
  ;; load-latest that decides whether to seed at all sat outside it — so two
  ;; branches reaching an unseeded entry in the same instant both read nil,
  ;; both seeded, and the second appended a version whose body was
  ;; byte-identical to the first.
  ;;
  ;; Live in run a3ba69bb: roles/implementor v1 and v2, 1ms apart at run
  ;; start, when four fan-out workers each triggered the first read. Harmless
  ;; in content and not harmless in the history: a version that changed
  ;; nothing is the one thing an append-only record must not contain, and it
  ;; breaks first-write-wins for anything reading it.
  (let [conn (db/open! ":memory:")
        _ (doseq [f (mapv (fn [_] (future (store/seed! conn :prompt "roles/implementor" "BODY")))
                          (range 8))]
            @f)
        rows (db/fetch conn ["SELECT version, body FROM userspace WHERE name = ?"
                             "roles/implementor"])]
    (is (= [1] (mapv :version rows)) "one seed, however many branches raced for it")
    (is (= ["BODY"] (mapv :body rows))))
  (testing "and a project edit still appends, identical body or not — an edit
            that turned out to be a no-op is a fact about what was tried, and
            suppressing it would make the history lie by omission"
    (let [conn (db/open! ":memory:")]
      (store/seed! conn :prompt "p" "BODY")
      (store/save! conn :prompt "p" "BODY")
      (is (= [1 2] (sort (mapv :version (db/fetch conn ["SELECT version FROM userspace WHERE name = ?" "p"]))))))))

;; --- a run that regrades itself leaves a record (karamazov-7mo M10) ---------

(defn- save-gates [conn run-id body]
  (tools/run-tool {:branch (state/new-branch {:id "B1" :problem "p"})
                   :conn conn :run-id run-id
                   :tool-name "policy"
                   :args {:action "save" :name "gates" :edn body
                          :rationale "tuning"}}))

(deftest editing-the-runs-own-scoring-is-journalled
  ;; Prevention was rejected: a run may still rewrite the gates it is judged
  ;; by, because policy being runtime-editable data is the whole premise. The
  ;; edit is NAMED instead, so a reader of the run can tell a real correction
  ;; from a run grading itself green.
  (us/bind! *conn*)
  (let [rid (runs/start-run! *conn* {:problem "p"})
        original (us/edn-body :policy "gates")]
    (testing "reweighting fitness is on the record"
      (save-gates *conn* rid (pr-str (assoc-in original [:fitness :value :weights :tool-success] 99.0)))
      (let [notes (journal/notes *conn* rid :self-graded)]
        (is (= 1 (count notes)))
        (is (= ["fitness"] (:keys (first notes))))
        (is (str/includes? (str (:rationale (first notes))) "tuning"))))))

(deftest an-ordinary-gate-edit-is-not-a-regrade
  (us/bind! *conn*)
  (let [rid (runs/start-run! *conn* {:problem "p"})
        original (us/edn-body :policy "gates")]
    (save-gates *conn* rid (pr-str (assoc-in original [:run-health :value :thrash-min-turns] 7)))
    (is (empty? (journal/notes *conn* rid :self-graded))
        "changing a behaviour gate says nothing; only the scoring gates do")))

;; --- accumulated prescription (karamazov-7mo M9) ----------------------------

(deftest a-project-on-the-shipped-template-has-prescribed-nothing
  (us/bind! *conn*)
  (us/body :prompt "system")
  (us/body :policy "gates")
  (is (empty? (us/prescription-mass))
      "seeding the factory template is not the project prescribing anything"))

(deftest prescription-mass-counts-what-the-project-made-its-own
  (us/bind! *conn*)
  (let [factory (us/body :prompt "no-edits")]
    (us/save! :prompt "no-edits" (str factory "\nAnd another rule.") "tightening")
    (us/save! :prompt "no-edits" (str factory "\nAnd two more rules here.") "tightening again")
    (us/body :prompt "stuck")
    (let [m (us/prescription-mass)]
      (is (= 1 (get-in m [:prompt :names])) "one prompt overridden, not both")
      (is (= 2 (get-in m [:prompt :versions])) "both edits counted")
      (testing "and the growth against the factory body is visible"
        (is (> (get-in m [:prompt :chars]) (get-in m [:prompt :factory-chars]))))
      (testing "kinds the project never touched do not appear"
        (is (nil? (:policy m)))
        (is (nil? (:cell m)))))))

(deftest prescription-mass-separates-the-kinds
  (us/bind! *conn*)
  (us/body :prompt "stuck")
  (us/body :policy "gates")
  (us/save! :prompt "stuck" "a rewritten stuck prompt" "why")
  (us/save! :policy "gates" (pr-str (us/edn-body :policy "gates")) "why")
  (let [m (us/prescription-mass)]
    (is (= 1 (get-in m [:prompt :names])))
    (is (= 1 (get-in m [:policy :names])))))

;; --- prompt FILES under .samizdat/prompts (karamazov-1g6b.3) -----------------
;;
;; A project carries prompt overrides as files a human and the agent can both
;; edit in place: .samizdat/prompts/<name>.md for any model, and
;; .samizdat/prompts/<provider>/<model>/<name>.md for one provider+model
;; family. Most specific wins; a file beats a stored row; no file means
;; exactly what the seam did before.

(defn- temp-root-with-prompts
  "A temp project root with these prompt files written, {relative-path body}."
  [files]
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "samizdat-prompt-files"
                   (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [[rel body] files]
      (let [f (java.io.File. root (str ".samizdat/prompts/" rel))]
        (.mkdirs (.getParentFile f))
        (spit f body)))
    root))

(defn- rm-rf [^java.io.File f]
  (when (.isDirectory f) (doseq [c (.listFiles f)] (rm-rf c)))
  (.delete f))

(defmacro with-prompt-root
  "Bind `root` and `model` for the body, restoring both after."
  [[root model] & body]
  `(let [prev-root# (us/project-root)
         prev-model# (us/model-context)]
     (try
       (us/bind-root! ~root)
       (us/bind-model! ~model)
       ~@body
       (finally
         (us/bind-root! prev-root#)
         (us/bind-model! prev-model#)))))

(deftest a-prompt-file-resolves-most-specific-first
  (let [root (temp-root-with-prompts
              {"local/qwen3/split-decision.md" "MODEL FILE"
               "local/foo.md"                  "PROVIDER FILE"
               "bar.md"                        "PROJECT FILE"})]
    (try
      (with-prompt-root [root {:provider :local :model "Qwen3.8-27B-Q8_0"}]
        (is (= "MODEL FILE" (us/body :prompt "split-decision"))
            "the provider/model directory wins for the running model")
        (is (= "PROVIDER FILE" (us/body :prompt "foo"))
            "a provider-wide file serves every model on that provider")
        (is (= "PROJECT FILE" (us/body :prompt "bar"))
            "a plain project file serves every provider")
        (is (re-find #"\{\{problem\}\}" (us/body :prompt "problem"))
            "a name with no file is the shipped template, as before"))
      (with-prompt-root [root {:provider :deepseek :model "deepseek-v4-flash"}]
        (is (= "PROJECT FILE" (us/body :prompt "bar"))
            "the plain project file still applies on another provider")
        (is (nil? (us/body :prompt "foo"))
            "a local/ file is ignored on deepseek — and nothing ships under that name")
        (is (not= "MODEL FILE" (us/body :prompt "split-decision"))
            "the qwen3 file is ignored on deepseek"))
      (with-prompt-root [root nil]
        (is (= "PROJECT FILE" (us/body :prompt "bar"))
            "with no model bound the plain project file still resolves")
        (is (nil? (us/body :prompt "foo"))
            "and the provider/model directories are not consulted"))
      (finally (rm-rf (java.io.File. root))))))

(deftest the-model-directory-matches-as-a-family-prefix
  ;; The trait being accommodated is a family's training, so the directory
  ;; is a prefix — qwen3 covers Qwen3.8-27B-Q8_0 — and a point release does
  ;; not need re-authoring. The longest matching directory wins, so a project
  ;; can special-case one sub-family beneath a general one.
  (is (true? (us/model-dir-matches? "qwen3" "Qwen3.8-27B-Q8_0")))
  (is (true? (us/model-dir-matches? "glm" "glm-5.3")))
  (is (true? (us/model-dir-matches? "GLM-5" "glm-5.3")) "case-insensitive both ways")
  (is (false? (us/model-dir-matches? "qwen" "deepseek-v4-flash")))
  (is (false? (us/model-dir-matches? "qwen3" nil)))
  (let [root (temp-root-with-prompts
              {"local/qwen3/x.md"   "GENERAL"
               "local/qwen3.8/x.md" "SPECIFIC"})]
    (try
      (with-prompt-root [root {:provider :local :model "Qwen3.8-27B-Q8_0"}]
        (is (= "SPECIFIC" (us/body :prompt "x"))))
      (with-prompt-root [root {:provider :local :model "Qwen3.5-9B"}]
        (is (= "GENERAL" (us/body :prompt "x"))))
      (finally (rm-rf (java.io.File. root))))))

(deftest a-file-beats-a-stored-row-and-says-so
  ;; Two sources of truth is the drift this project keeps finding, so the
  ;; rule is stated and the tool reports it: a file, when present, IS the
  ;; newest version, and prompt-source names where the text came from.
  (us/bind! *conn*)
  (let [root (temp-root-with-prompts {"local/qwen3/bar.md" "FILE"})]
    (try
      (with-prompt-root [root {:provider :local :model "qwen3-27b"}]
        (us/save! :prompt "bar" "ROW" "a stored edit")
        (is (= "FILE" (us/body :prompt "bar")))
        (is (= {:source :file :layer :model
                :path (str root "/.samizdat/prompts/local/qwen3/bar.md")}
               (us/prompt-source "bar")))
        (testing "a name with a row and no file reports the row"
          (us/save! :prompt "baz" "ROW" "another")
          (is (= {:source :project :version 1} (us/prompt-source "baz"))))
        (testing "a shipped name with neither reports the template"
          (is (= {:source :template} (us/prompt-source "problem"))))
        (testing "an unknown name reports nothing"
          (is (nil? (us/prompt-source "no-such-prompt-anywhere")))))
      (finally (rm-rf (java.io.File. root))))))

(deftest editing-a-prompt-file-takes-effect-on-the-next-read
  ;; The point of files is that a human edits them in place, so the read
  ;; cache must not pin the first content it saw.
  (let [root (temp-root-with-prompts {"bar.md" "FIRST"})]
    (try
      (with-prompt-root [root nil]
        (is (= "FIRST" (us/body :prompt "bar")))
        (spit (java.io.File. root ".samizdat/prompts/bar.md") "SECOND")
        (is (= "SECOND" (us/body :prompt "bar"))))
      (finally (rm-rf (java.io.File. root))))))

(deftest prompt-variants-are-discoverable
  ;; A capability the supervisor cannot enumerate does not exist for it: the
  ;; prompt tool lists which names carry files, and for which provider/model.
  (let [root (temp-root-with-prompts
              {"local/qwen3/split-decision.md" "a"
               "deepseek/split-decision.md"    "b"
               "bar.md"                        "c"})]
    (try
      (with-prompt-root [root nil]
        (is (= {"split-decision" [{:layer :provider :provider "deepseek"
                                   :path (str root "/.samizdat/prompts/deepseek/split-decision.md")}
                                  {:layer :model :provider "local" :model-dir "qwen3"
                                   :path (str root "/.samizdat/prompts/local/qwen3/split-decision.md")}]
                "bar" [{:layer :project
                        :path (str root "/.samizdat/prompts/bar.md")}]}
               (us/prompt-variants))))
      (with-prompt-root [nil nil]
        (is (= {} (us/prompt-variants)) "no root, no files, no error"))
      (finally (rm-rf (java.io.File. root))))))

(deftest the-prompt-tool-shows-where-every-wording-comes-from
  ;; Discoverability is the whole point of files over hidden shadows: `list`
  ;; names each file and its layer, `show` says which layer answered, so the
  ;; next edit lands in the right place — a file with the file tools, a row
  ;; with `save`.
  (us/bind! *conn*)
  (let [root (temp-root-with-prompts {"local/qwen3/split-decision.md" "QWEN BLOCK"
                                      "bar.md" "PROJECT BAR"})
        run (fn [args] (tools/run-tool {:tool-name "prompt"
                                        :branch (state/new-branch {:id "B1" :problem "p"})
                                        :conn *conn* :args args}))]
    (try
      (with-prompt-root [root {:provider :local :model "Qwen3.8-27B-Q8_0"}]
        (us/save! :prompt "bar" "ROW BAR" "a stored edit the file now shadows")
        (let [listing (:result (run {:action "list"}))]
          (is (re-find #"(?m)^split-decision  \[template\]  \[file: model wins\]$" listing)
              (str listing))
          (is (re-find #"local/qwen3  .*local/qwen3/split-decision\.md" listing))
          (is (re-find #"(?m)^bar  v1 \(1 version\)  \[file: project wins\]$" listing)
              "a stored row that a file shadows says so, next to the version it shadows")
          (is (re-find #"project  .*/\.samizdat/prompts/bar\.md" listing)))
        (let [shown (:result (run {:action "show" :name "split-decision"}))]
          (is (re-find #"\[from model file .*local/qwen3/split-decision\.md\]" shown))
          (is (re-find #"QWEN BLOCK" shown)))
        (let [shown (:result (run {:action "show" :name "bar"}))]
          (is (re-find #"\[from project file" shown))
          (is (re-find #"PROJECT BAR" shown)))
        (is (re-find #"ROW BAR"
                     (:result (run {:action "show" :name "bar" :version "1"})))
            "asking for a version by number still reads the row the file shadows")
        (is (re-find #"\[from the shipped template\]"
                     (:result (run {:action "show" :name "problem"})))))
      (finally (rm-rf (java.io.File. root))))))
