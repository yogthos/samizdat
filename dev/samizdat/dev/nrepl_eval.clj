;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.dev.nrepl-eval
  "Evaluate a form in a running nREPL from the command line and print what
  comes back: out, err, the value, and any exception. The client half of
  REPL-driven development for an agent or a script that has no editor.

      jolt -A:dev:test nrepl-server 7899       # a dev image, once (test adds gui)
      NREPL_PORT=7899 CODE='(+ 1 2)' jolt -A:dev -m samizdat.dev.nrepl-eval
      echo '(require (quote samizdat.foo-test) :reload)
            (clojure.test/run-tests (quote samizdat.foo-test))' \\
        | NREPL_PORT=7899 jolt -A:dev -m samizdat.dev.nrepl-eval

  Reads the code from CODE, else stdin. Uses the client-side bencode
  transport the jolt-lang/nrepl dependency already provides; nothing else.

  Start your OWN server for development. `jolt serve` writes the harness's
  nREPL port to .nrepl-port, and reloading edited namespaces into a harness
  that is mid-run changes the run."
  (:require [nrepl.transport :as t]))

(defn- done? [m] (some #{"done"} (get m "status")))

(defn- clone-session
  "Open a session so the eval has its own *ns* and bindings; nil if the
  server does not offer one."
  [tr]
  (t/send tr {"op" "clone"})
  (loop []
    (let [m (t/recv tr)]
      (cond (nil? m) nil
            (get m "new-session") (get m "new-session")
            (done? m) nil
            :else (recur)))))

(defn eval-and-print!
  "Send `code` for evaluation on `port` and stream the replies to stdout and
  stderr until the server says done. Returns nil."
  [port code]
  (let [tr (t/connect "127.0.0.1" port {:recv-timeout-secs 1800})
        session (clone-session tr)]
    (t/send tr (cond-> {"op" "eval" "code" code} session (assoc "session" session)))
    (loop []
      (when-let [m (t/recv tr)]
        (when-let [o (get m "out")] (print o) (flush))
        (when-let [e (get m "err")] (binding [*out* *err*] (print e) (flush)))
        (when (contains? m "value") (println "=>" (get m "value")))
        (when-let [ex (get m "ex")] (println "EX" ex (get m "root-ex")))
        (when-not (done? m) (recur))))
    (flush)))

(defn -main [& _]
  (eval-and-print! (Long/parseLong (or (System/getenv "NREPL_PORT") "7899"))
                   (or (System/getenv "CODE") (slurp *in*)))
  (System/exit 0))
