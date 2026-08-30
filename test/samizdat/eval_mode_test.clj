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

(ns samizdat.eval-mode-test
  "The `:eval` toggle: which image the REPL runs in, and whether it exists.

  THE TOGGLE IS A SECURITY CONTROL, so it is read from `.samizdat/config.edn`
  — the operator's file, which `policy/protected-paths` and `files/run-config?`
  already keep the agent out of — and NOT from roles.edn, gates.edn or
  phases.edn, all of which are agent-editable userspace. roles.edn:57,82 lists
  \"eval\" in the role surfaces and phases.edn:118 gates it per phase; a toggle
  living there is one the model can turn back on for itself.

  Same rule `repl/guard.clj` states (\"a liveness guard the guarded thing can
  edit is not a guard\") and `policy/protected-paths` states again (\"gates.edn
  is agent-editable userspace, so a protected list living there could be
  unprotected by the party it protects against\"). Both cite karamazov-zrq.

  The subtraction is a pure function taking the mode, so the table below is a
  table and not a filesystem fixture; `surface` reading the operator's file is
  covered once, at the bottom."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [jolt.fs :as fs]
            [samizdat.agent.roles :as roles]
            [samizdat.config :as config]
            [samizdat.prompt :as prompt]
            [samizdat.userspace :as userspace]))

;; --- what the operator's file says ------------------------------------------

(deftest eval-settings-defaults-to-the-sandboxed-project-image
  (testing "a project that says nothing gets the safe image, not the harness one"
    (is (= {:mode :project :sandbox :auto} (config/eval-settings {}))
        "the P0 must be closed out of the box, not on opt-in"))
  (testing "nil is the same as absent"
    (is (= {:mode :project :sandbox :auto} (config/eval-settings nil)))))

(deftest eval-settings-reads-the-operator-file
  (is (= {:mode :off :sandbox :auto}
         (config/eval-settings {:eval {:mode :off}})))
  (is (= {:mode :harness :sandbox :none}
         (config/eval-settings {:eval {:mode :harness :sandbox :none}}))))

(deftest a-broken-eval-setting-falls-back-rather-than-stopping-the-harness
  ;; project-config's own rule: "a broken project file must never stop the
  ;; harness". An unreadable mode is not a licence to open the image.
  (testing "an unknown mode falls back to the default"
    (is (= :project (:mode (config/eval-settings {:eval {:mode :yolo}}))))
    (is (= :project (:mode (config/eval-settings {:eval {:mode "project"}})))
        "a string is not a keyword — do not guess at the operator's intent"))
  (testing "an unknown sandbox backend falls back to :auto"
    (is (= :auto (:sandbox (config/eval-settings {:eval {:sandbox :bwrap-ish}})))))
  (testing "a non-map :eval is ignored whole"
    (is (= {:mode :project :sandbox :auto} (config/eval-settings {:eval "off"})))))

;; --- what the roles may call ------------------------------------------------

(deftest off-removes-the-repl-tools-from-every-role
  ;; Regardless of roles.edn: the surface is agent-editable, the toggle is not.
  (doseq [role (roles/names)]
    (let [s (roles/confine (roles/surface role) :off)]
      (when (set? s)
        (doseq [t roles/repl-tools]
          (is (not (contains? s t))
              (str role " kept " t " with the REPL turned off")))))))

(deftest an-unrestricted-surface-keeps-its-sentinel
  ;; Collapsing `:all` to (registry - repl-tools) was tried and reverted:
  ;; all-tool-names resolves the registry late and answers nil during prompt
  ;; assembly, so the collapse handed the role NO tools — the failure the
  ;; sentinel exists to prevent. The REPL subtraction for `:all` belongs to the
  ;; call sites, which is what the next two tests pin.
  (is (= :all (roles/confine :all :off))))

(deftest an-unrestricted-role-still-cannot-call-eval-with-the-repl-off
  ;; The enforcement half of the above. A role the table does not name is
  ;; unrestricted, and that must not survive the operator's switch.
  (is (not (roles/may-use? :no-such-role "eval" :off)))
  (is (roles/may-use? :no-such-role "write_file" :off))
  (is (roles/may-use? :no-such-role "eval" :project)))

(deftest project-and-harness-modes-keep-the-repl-tools
  (doseq [mode [:project :harness]]
    (is (= :all (roles/confine :all mode))
        (str "mode " mode " should not subtract anything"))
    (is (contains? (roles/confine #{"eval" "shell"} mode) "eval"))))

(deftest may-use-refuses-eval-with-the-repl-off
  (is (not (roles/may-use? :implementor "eval" :off))
      "the surface was filtered but the call was still allowed")
  (is (roles/may-use? :implementor "eval" :project))
  (is (roles/may-use? :implementor "write_file" :off)
      "turning the REPL off disarmed a tool that is not the REPL"))

;; --- what the prompt says ---------------------------------------------------

(deftest the-catalogue-loses-the-repl-entries-with-the-repl-off
  ;; scope-catalogue drops an entry AND the indented prose under it, so a role
  ;; is never told about a tool it cannot call.
  (let [text (str "### Doing work\n"
                  "eval({code, timeout_ms?})\n"
                  "    Evaluate Clojure in the live image.\n"
                  "write_file({path, content})\n"
                  "    Write a file.\n")
        out (roles/scope-catalogue text :implementor :off)]
    (is (not (str/includes? out "eval({"))
        "the prompt still advertises eval with the REPL turned off")
    (is (not (str/includes? out "Evaluate Clojure in the live image"))
        "the entry went but its prose stayed")
    (is (str/includes? out "write_file({"))))

;; --- which image a role lands in --------------------------------------------

(deftest the-supervisor-keeps-the-harness-image-under-project-mode
  ;; `:project` is a posture for the run, not one answer for every role: the
  ;; supervisor's job IS the harness, and a project image rooted at somebody
  ;; else's repo cannot see manifests, cells, prompts or policy.
  (is (= :harness (config/eval-image :project :supervisor)))
  (is (= :project (config/eval-image :project :implementor)))
  (is (= :project (config/eval-image :project :reviewer))))

(deftest a-role-nobody-declared-gets-the-project-image
  ;; The safe direction, and what makes adding a role harmless.
  (is (= :project (config/eval-image :project :some-new-role))))

(deftest off-and-harness-modes-apply-to-every-role
  (doseq [role [:supervisor :implementor :some-new-role]]
    (is (= :off (config/eval-image :off role))
        "turning the REPL off left a role holding one")
    (is (= :harness (config/eval-image :harness role)))))

;; --- what the prompt claims -------------------------------------------------

(defn- system-text
  "The shipped system prompt rendered for a mode, the way loop/system-prompt-for
  renders it. Templates only — the catalogue filtering is tested above."
  [mode]
  (prompt/render-str (or (prompt/layer :system) "")
                     {:repl (not= :off mode)
                      :harness-image (= :harness mode)}))

(deftest the-prompt-does-not-promise-a-repl-the-harness-cannot-deliver
  ;; repl.clj's docstring: "THE SYSTEM PROMPT PROMISES THIS AND THE HARNESS DID
  ;; NOT DELIVER IT … Every word of that instruction was unreachable for the
  ;; case the harness exists to serve." Dropping the tool without dropping the
  ;; prose recreates exactly that.
  (let [off (system-text :off)]
    (is (not (str/includes? off "REPL first"))
        "the REPL-first section survived with the REPL turned off")
    (is (not (str/includes? off "Prototype with `eval`")))
    (is (str/includes? off "no REPL")
        "the model was left to infer the absence rather than being told")
    (is (str/includes? off "the file is the deliverable")
        "the replacement workflow went missing with the section it replaces")))

(deftest the-prompt-names-the-image-the-repl-actually-runs-in
  ;; :project is a SEPARATE process rooted at the project. Telling the model it
  ;; is in "the same image the harness runs in" is the same class of false
  ;; claim as promising a REPL that is not there.
  (let [harness (system-text :harness)
        project (system-text :project)]
    (is (str/includes? harness "the same image the harness runs in"))
    (is (not (str/includes? project "the same image the harness runs in"))
        "a project-image run was told it is inside the harness image")
    (is (str/includes? project "SEPARATE process")
        "nothing told the model which image it is in")
    (testing "and it is told what the sandbox will refuse, before it tries"
      (is (str/includes? project "cannot run shell commands")))))

;; --- the operator's file, end to end ----------------------------------------

(deftest surface-reads-the-mode-from-the-project-config
  (let [root (str (fs/create-temp-dir))
        prior (userspace/project-root)]
    (fs/create-dirs (str root "/.samizdat"))
    (spit (str root "/.samizdat/config.edn") (pr-str {:eval {:mode :off}}))
    (try
      (userspace/bind-root! root)
      (let [s (roles/surface :implementor)]
        (is (set? s))
        (is (not (contains? s "eval"))
            "surface ignored the operator's .samizdat/config.edn"))
      (finally
        (userspace/bind-root! prior)))))
