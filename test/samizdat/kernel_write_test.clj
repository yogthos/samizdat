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

(ns samizdat.kernel-write-test
  "The supervisor keeps the live harness image, so it keeps the last route to
  the escape this bead is about: patch a `src/` file, reload it into the
  running process, and model-written kernel code is live with nothing for the
  mutation protocol to see.

  These pin BOTH directions. Over-refusing is a real cost here — the
  supervisor's whole job is editing the harness, and a guard that also refuses
  cells, manifests and prompts refuses the role."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.repl.guard :as guard]
            [samizdat.repl.route :as route]))

(defn- f [s] (read-string (str "(do " s "\n)")))

(deftest patching-harness-source-is-a-kernel-write
  (is (guard/kernel-write? (f "(spit \"src/samizdat/agent/tools/introspect.clj\" patched)")))
  (is (guard/kernel-write? (f "(spit \"src/ring_chez/adapter.clj\" x)")))
  (testing "including through a nested or threaded form"
    (is (guard/kernel-write? (f "(when true (spit \"src/samizdat/repl.clj\" x))")))))

(deftest hot-loading-a-harness-namespace-is-a-kernel-write
  ;; The second half of the observed escape: the write is only dangerous once
  ;; it is IN the running image.
  (is (guard/kernel-write? (f "(require 'samizdat.agent.tools.introspect :reload)")))
  (is (guard/kernel-write? (f "(require '[samizdat.repl :as r] :reload-all)"))))

(deftest the-supervisors-actual-job-is-not-refused
  ;; resources/ IS the editing surface. A guard that refuses it refuses the
  ;; role, which is a worse outcome than the one it prevents.
  (is (not (guard/kernel-write? (f "(spit \"resources/cells/feature.clj\" x)"))))
  (is (not (guard/kernel-write? (f "(spit \"resources/manifests/loop.edn\" x)"))))
  (is (not (guard/kernel-write? (f "(spit \"resources/prompts/system.md\" x)"))))
  (testing "reading harness source is not writing it"
    (is (not (guard/kernel-write? (f "(slurp \"src/samizdat/repl.clj\")")))))
  (testing "an ordinary require is not a hot-patch"
    (is (not (guard/kernel-write? (f "(require 'samizdat.repl)")))))
  (testing "reloading a NON-harness namespace is the project's business"
    (is (not (guard/kernel-write? (f "(require 'todo.core :reload)")))))
  (testing "defining a function that would write is not writing"
    ;; executed-subforms already excludes definition bodies; this pins that it
    ;; still holds for this guard.
    (is (not (guard/kernel-write? (f "(defn deploy [] (spit \"src/x.clj\" 1))"))))))

(deftest the-refusal-names-the-supported-route
  ;; A refusal the model cannot act on costs the same turn as no refusal.
  (let [r (route/eval-for {:root "/tmp/whatever" :role :supervisor}
                          "(spit \"src/samizdat/repl.clj\" \"x\")" nil 5000)]
    (is (not (:ok r)))
    (is (:policy-refusal? r))
    (is (str/includes? (str (:error r)) "mutation protocol"))
    (is (str/includes? (str (:error r)) "reload_cells")
        "the refusal did not point at the surface that IS editable")))

(deftest an-ordinary-supervisor-eval-still-runs-in-the-harness-image
  ;; The guard must not cost the supervisor the image its job depends on.
  (let [r (route/eval-for {:root "/tmp/whatever" :role :supervisor} "(+ 1 2)" nil 5000)]
    (is (:ok r))
    (is (= "3" (:value r)))))

(deftest a-kernel-write-finding-names-the-rule-and-what-it-fired-on
  (let [[hit] (guard/findings (f "(spit \"src/samizdat/repl.clj\" x)"))]
    (is (= :kernel-source-write (:rule hit)))
    (is (= 'spit (get-in hit [:bindings '?head])))
    (is (= "src/samizdat/repl.clj" (get-in hit [:bindings '?path]))))
  (let [[hit] (guard/findings (f "(require 'samizdat.repl :reload)"))]
    (is (= :harness-reload (:rule hit)))))

(deftest the-refusal-names-the-rule-that-fired
  (let [r (route/eval-for {:root "/tmp/whatever" :role :supervisor}
                          "(spit \"src/samizdat/repl.clj\" \"x\")" nil 5000)]
    (is (str/includes? (str (:error r)) "kernel-source-write"))
    (is (str/includes? (str (:error r)) "src/samizdat/repl.clj")
        "and what it fired on")))
