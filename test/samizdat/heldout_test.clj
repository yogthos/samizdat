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

(ns samizdat.heldout-test
  "The held-out gate, stage 1 (karamazov-7mo.4 / ylte.4): the battery as the
  project stores it, the verdict rule, and the tools that consult it. The
  child process itself is exercised against a real project, not here; these
  inject the replay."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.agent.tools.base :as base]
            [samizdat.agent.tools]
            [samizdat.engine.proc :as proc]
            [samizdat.heldout :as heldout]
            [samizdat.store.db :as db]
            [samizdat.store.interventions :as interventions]
            [samizdat.store.runs :as runs]
            [samizdat.store.userspace :as store]
            [samizdat.userspace :as us]
            [samizdat.manifests]
            [samizdat.prompt]
            [samizdat.cells]
            [samizdat.agent.tools.policy]))

(defn- temp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "samizdat-heldout" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-recursively [^java.io.File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)] (delete-recursively c)))
  (.delete f))

(defmacro ^:private with-project
  [[root conn] & body]
  `(let [~root (temp-dir)
         ~conn (db/open! ":memory:")
         prev-root# (us/bind-root! ~root)]
     (us/bind! ~conn)
     (try (us/seed-project!)
          ~@body
          (finally (us/unbind!)
                   (us/bind-root! prev-root#)
                   (db/close ~conn)
                   (delete-recursively (io/file ~root))))))

(defn- write-case! [root id c]
  (let [f (io/file root ".samizdat" "battery" "subj" (str id ".edn"))]
    (.mkdirs (.getParentFile f))
    (spit f (pr-str c))
    f))

(def ^:private a-case
  {:id "c1"
   :fixture {:sha "abc123"}
   :model "m"
   :replay {:problem "p" :replies {"T0" ["x"]}}
   :expect [{:name "run reaches completed" :assert [:status :completed]}
            {:name "calls done" :assert [:tool-called "done"]}]})

(defn- result [& oks]
  {:run-id "r" :status :completed
   :result {:ok? (every? true? oks) :passed (count (filter true? oks)) :total (count oks)
            :targets (mapv (fn [n ok] {:name n :ok? ok})
                           ["run reaches completed" "calls done"] oks)}})

(defn- replay-by-text
  "A stand-in for the child: the candidate text decides which targets pass."
  [_case candidate]
  (let [t (str (:text candidate))]
    (cond
      (str/includes? t "CRASH") {:error :no-result}
      (str/includes? t "BREAK") (result true false)
      :else (result true true))))

(deftest the-stored-battery-is-the-authority
  (with-project [root conn]
    (let [f (write-case! root "c1" a-case)]
      (testing "a new case file is added"
        (let [{:keys [cases altered]} (heldout/cases conn)]
          (is (= ["c1"] (map :id cases)))
          (is (= "subj" (:subject (first cases))))
          (is (empty? altered))))
      (testing "a case whose file was weakened runs as stored, and the edit is named"
        (spit f (pr-str (assoc a-case :expect [])))
        (let [{:keys [cases altered]} (heldout/cases conn)]
          (is (= 2 (count (:expect (first cases)))))
          (is (= [(str f)] altered))))
      (testing "a case whose file was deleted still runs"
        (.delete f)
        (is (= ["c1"] (map :id (:cases (heldout/cases conn)))))))))

(deftest the-verdict-is-per-target-and-non-compensatory
  (let [cases [{:id "c1"} {:id "c2"}]
        ok (result true true)]
    (testing "nothing that passed fails: accepted, ties included"
      (is (:ok? (heldout/compare-results cases {"c1" ok "c2" ok} {"c1" ok "c2" ok}))))
    (testing "a target that passed and now fails is named with its case"
      (let [v (heldout/compare-results cases {"c1" ok "c2" ok}
                                       {"c1" ok "c2" (result true false)})]
        (is (false? (:ok? v)))
        (is (= ["c2 — calls done"] (:regressions v)))))
    (testing "fixing one target does not pay for breaking another"
      (let [v (heldout/compare-results [{:id "c1"}] {"c1" (result false true)}
                                       {"c1" (result true false)})]
        (is (false? (:ok? v)))
        (is (= ["c1 — calls done"] (:regressions v)))))
    (testing "a case that does not run at baseline is set aside, not blamed"
      (let [v (heldout/compare-results cases {"c1" {:error :no-result} "c2" ok}
                                       {"c1" {:error :no-result} "c2" ok})]
        (is (:ok? v))
        (is (= ["c1"] (:unmeasured v)))))
    (testing "a case that ran and no longer runs is a regression"
      (let [v (heldout/compare-results cases {"c1" ok "c2" ok}
                                       {"c1" {:error :timeout} "c2" ok})]
        (is (false? (:ok? v)))
        (is (re-find #"c1 — no longer runs" (first (:regressions v))))))))

(deftest the-gate-measures-a-candidate-and-records-what-it-measured-on
  (with-project [root conn]
    (testing "no battery, no gate: a project without cases tunes itself as before"
      (is (nil? (heldout/gate! conn {:kind :manifest :name "loop" :text "x"}
                               {:run-case replay-by-text}))))
    (write-case! root "c1" a-case)
    (let [bad (heldout/gate! conn {:kind :manifest :name "loop" :text "BREAK"}
                             {:run-case replay-by-text})]
      (is (false? (:ok? bad)))
      (is (= ["c1 — calls done"] (:regressions bad)))
      (is (str/includes? (heldout/refusal bad) "c1 — calls done") "the refusal names what broke"))
    (is (:ok? (heldout/gate! conn {:kind :manifest :name "loop" :text "fine"}
                             {:run-case replay-by-text})))
    (testing "each target is recorded with the fixture, revision, model and scorer"
      (let [rows (heldout/checks conn :manifest "loop")]
        (is (= 4 (count rows)))
        (is (every? #(= "abc123" (:fixture %)) rows))
        (is (every? #(= "m" (:model %)) rows))
        (is (every? #(= "battery/check" (:scorer %)) rows))
        (is (some #(and (= "c1 — calls done" (:target %)) (= 1 (:ok_before %)) (= 0 (:ok_after %))) rows))))))

(deftest a-flip-that-does-not-reproduce-does-not-refuse
  (with-project [root conn]
    (write-case! root "c1" a-case)
    (let [n (atom 0)
          ;; The candidate's first reading breaks a target; its second does not.
          run (fn [_ cand]
                (if (and cand (= 1 (swap! n inc)))
                  (result true false)
                  (result true true)))
          v (heldout/gate! conn {:kind :manifest :name "loop" :text "flaky"} {:run-case run})]
      (is (:ok? v) "noise is not a regression")
      (is (= ["c1 — calls done"] (:unconfirmed v)) "and it is named as unconfirmed"))))

(deftest a-baseline-is-measured-once-per-userspace
  (with-project [root conn]
    (write-case! root "c1" a-case)
    (let [calls (atom [])
          run (fn [c cand] (swap! calls conj (:text cand)) (result true true))]
      (heldout/gate! conn {:kind :manifest :name "loop" :text "one"} {:run-case run})
      (heldout/gate! conn {:kind :manifest :name "loop" :text "two"} {:run-case run})
      (is (= [nil "one" "two"] @calls) "the second edit reuses the baseline"))))

(deftest a-case-is-staged-as-its-fixture-with-the-candidate-written-in
  (with-project [root conn]
    (let [git (fn [& args] (apply proc/run {:timeout-ms 15000} "git" "-C" root args))
          _ (git "init" "-q")
          _ (spit (io/file root "game.clj") "(ns game)")
          _ (git "add" "game.clj")
          _ (git "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "fixture")
          sha (str/trim (:out (git "rev-parse" "HEAD")))
          _ (spit (io/file root ".samizdat" "samizdat.sqlite3") "not copied")
          dest (str (temp-dir) "/root")]
      (try
        (is (nil? (heldout/stage! {:fixture {:sha sha}} dest
                                  {:kind :manifest :name "loop" :text "{:candidate true}"})))
        (is (= "(ns game)" (slurp (io/file dest "game.clj"))) "the fixture's tree")
        (is (= "{:candidate true}" (slurp (io/file dest ".samizdat" "manifests" "loop.edn")))
            "the candidate in place of the project's file")
        (is (.exists (io/file dest ".samizdat" "manifests" "worker.edn")) "the rest of the userspace")
        (is (not (.exists (io/file dest ".samizdat" "samizdat.sqlite3"))) "and not the database")
        (is (= {:error :no-fixture} (heldout/stage! {} (str dest "2") nil)))
        (finally (delete-recursively (.getParentFile (io/file dest))))))))

(defn- messages-to [conn rid bid]
  (mapv interventions/text-of (interventions/pending conn rid bid)))

(deftest an-edit-is-measured-off-the-turn-and-its-author-is-told
  ;; karamazov-nha1: the battery costs minutes, a refusal a second measurement,
  ;; and a turn has 900 s. The tool call answers at once that the edit is
  ;; pending; the verdict arrives as a message on the author's next turn.
  (with-project [root conn]
    (write-case! root "c1" a-case)
    (let [rid (runs/start-run! conn {:problem "p"})
          worker (io/file root ".samizdat" "manifests" "worker.edn")
          before (slurp worker)
          save (fn [text]
                 (base/run-tool {:branch {:id "S1"} :conn conn :run-id rid :tool-name "manifest"
                                 :args {:action "save" :name "worker" :edn text
                                        :rationale "try it"}}))]
      (with-redefs [heldout/run-case! replay-by-text]
        (testing "a bad edit: pending now, refused later, never saved"
          (let [r (save (str before "\n;; BREAK\n"))]
            (is (= :neutral (:category r)) (:result r))
            (is (str/includes? (:result r) "NOT live yet"))
            (heldout/await-pending!)
            (is (= before (slurp worker)) "the file that runs is unchanged")
            (is (some #(str/includes? (str %) "c1 — calls done") (messages-to conn rid "S1"))
                "the refusal reaches the author, naming what broke")))
        (testing "a good edit: pending now, live once it passes"
          (let [text (str before "\n;; fine\n")
                r (save text)]
            (is (str/includes? (:result r) "NOT live yet"))
            (is (= before (slurp worker)) "not live before the verdict")
            (heldout/await-pending!)
            (is (= text (slurp worker)) "saved once the battery passed it")
            (is (some #(str/includes? (str %) "is now live") (messages-to conn rid "S1")))))))))

(deftest a-policy-edit-is-deferred-the-same-way
  (with-project [root conn]
    (write-case! root "c1" a-case)
    (let [rid (runs/start-run! conn {:problem "p"})
          gates (slurp (io/file root ".samizdat" "gates.edn"))]
      (with-redefs [heldout/run-case! replay-by-text]
        (let [r (base/run-tool {:branch {:id "S1"} :conn conn :run-id rid :tool-name "policy"
                                :args {:action "save" :name "gates"
                                       :edn (str gates "\n;; BREAK\n") :rationale "try it"}})]
          (is (str/includes? (:result r) "NOT live yet") (:result r))
          (heldout/await-pending!)
          ;; gates.edn's own prose says CRASH, which the stand-in reads as a
          ;; case that no longer runs: refused either way, and by name.
          (is (= gates (slurp (io/file root ".samizdat" "gates.edn"))))
          (is (some #(str/includes? (str %) "c1 — ") (messages-to conn rid "S1"))))))))

(deftest a-policy-table-that-will-not-load-is-refused-in-the-turn
  (with-project [root conn]
    (write-case! root "c1" a-case)
    (let [r (base/run-tool {:branch {:id "S1"} :conn conn :tool-name "policy"
                            :args {:action "save" :name "gates" :edn "[:not :a-map]"
                                   :rationale "break it"}})]
      (is (= :mechanics (:category r)) (:result r))
      (is (not (str/includes? (:result r) "NOT live yet")) "no replay for a table that cannot load"))))

(deftest the-battery-tool-lists-and-cannot-remove
  (with-project [root conn]
    (write-case! root "c1" a-case)
    (let [r (base/run-tool {:branch {:id "S1"} :conn conn :tool-name "battery"
                            :args {:action "list"}})]
      (is (str/includes? (:result r) "c1"))
      (is (str/includes? (:result r) "2 targets")))
    (let [r (base/run-tool {:branch {:id "S1"} :conn conn :tool-name "battery"
                            :args {:action "remove" :run_id "c1"}})]
      (is (= :mechanics (:category r)) "there is no remove"))))

(defn- row [arm fit tok & {:keys [green? crit] :or {green? true}}]
  {:arm arm :task :t :fitness fit :tokens tok :green? green?
   :acceptance (when (some? crit) {:results [{:name "horizon fades" :passed? crit}]})})

(def ^:private rules {:baseline :base :candidate :cand
                      :cost-rule {:base 0.10 :per-fitness 1.0}
                      :noise {:z 2.0 :min-band 0.02}})

(deftest stage-two-reads-the-live-arms-in-rrsi-order
  (let [base [(row :base 0.50 1000) (row :base 0.52 1000) (row :base 0.48 1000)]
        verdict (fn [cand & [opts]] (heldout/live-verdict (concat base cand) (merge rules opts)))]
    (testing "a real gain that pays for its tokens is accepted"
      (is (:accept? (verdict [(row :cand 0.70 1100) (row :cand 0.72 1100)]))))
    (testing "floor: under the best a kept version reached, by more than the noise"
      (let [v (verdict [(row :cand 0.50 800)] {:best {:t 0.80}})]
        (is (false? (:accept? v)))
        (is (= [:floor] (get-in v [:tasks :t :refused])))))
    (testing "cost: a gain that more than doubles the tokens does not pay"
      (is (= [:cost] (get-in (verdict [(row :cand 0.70 2500)]) [:tasks :t :refused]))))
    (testing "within the band only a cost cut, or a new component, is admissible"
      (is (= [:cost] (get-in (verdict [(row :cand 0.50 1000)]) [:tasks :t :refused])))
      (is (:accept? (verdict [(row :cand 0.50 800)])) "cheaper")
      (is (:accept? (verdict [(row :cand 0.50 1000)] {:structural? true})) "structural"))
    (testing "guards: a criterion the baseline always met, and the suite, may not be lost"
      (let [base [(row :base 0.5 1000 :crit true) (row :base 0.5 1000 :crit true)]
            v (heldout/live-verdict (concat base [(row :cand 0.9 1000 :crit false :green? false)]) rules)]
        (is (= [:criterion :suite] (get-in v [:tasks :t :refused])))
        (is (= ["horizon fades"] (get-in v [:tasks :t :lost-criteria])))))
    (testing "no rows on one side judges nothing, which is not an accept"
      (is (false? (:accept? (heldout/live-verdict base rules)))))))
