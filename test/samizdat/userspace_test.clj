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
