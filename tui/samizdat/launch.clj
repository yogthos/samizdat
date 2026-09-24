;; samizdat - a claim-first verification harness
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

(ns samizdat.launch
  "What the command line asks for, and the one effect a terminal UI sharing
  a process with the server needs: moving stderr out of the way.

  Three modes. With no arguments the process is the whole application — the
  server and the TUI in one image, the TUI talking to it over loopback like
  any other client. `--headless` is the server alone, for a machine with no
  terminal or a front end elsewhere. `--connect [URL]` is the TUI alone,
  against a server that is already running.

  Mechanism only: `samizdat.main` decides what each mode starts. Under tui/
  with it, not src/: this is how the application is launched, not what the
  harness does."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clojure.tools.logging.impl :as impl]
            [jolt.ffi :as ffi]))

(def usage
  (str/join
   "\n"
   ["usage: samizdat [--headless | --connect [URL]]"
    ""
    "  (no flags)       start the server and the terminal UI together"
    "  --headless       start only the HTTP server (and nREPL)"
    "  --connect [URL]  start only the terminal UI, against a running server"
    "                   (default: $SAMIZDAT_URL, else 127.0.0.1:$HARNESS_PORT)"
    "  -h, --help       print this message"]))

(defn parse-args
  "The launch mode `args` name, as `{:mode ...}`: `:tui` (the default),
  `:headless`, `:connect` with an optional `:url`, `:help`, or `:error` with
  a `:message`. An unknown flag is an error rather than ignored — a typo of
  `--headless` would otherwise take over the terminal it meant to leave
  alone."
  [args]
  (loop [[a & more] (seq args) opts {:mode :tui}]
    (let [clash (fn [m] (if (= :tui (:mode opts))
                          (assoc opts :mode m)
                          {:mode :error
                           :message (str (name (:mode opts)) " and " a
                                         " cannot be combined")}))]
      (cond
        (nil? a) opts
        (#{"-h" "--help"} a) {:mode :help}
        (= "--headless" a) (let [o (clash :headless)]
                             (if (= :error (:mode o)) o (recur more o)))
        (= "--connect" a) (let [o (clash :connect)
                                [url more] (if (and (first more)
                                                    (not (str/starts-with? (first more) "-")))
                                             [(first more) (rest more)]
                                             [nil more])]
                            (if (= :error (:mode o))
                              o
                              (recur more (assoc o :url url))))
        :else {:mode :error :message (str "unknown argument: " a)}))))

(defn log-path
  "Where the server's log goes while the TUI owns the terminal."
  [root]
  (str root "/.samizdat/samizdat.log"))

;; --- stderr -------------------------------------------------------------------

(ffi/defcfn ^:private c-open "open" [:string :int] :int)
(ffi/defcfn ^:private c-dup "dup" [:int] :int)
(ffi/defcfn ^:private c-dup2 "dup2" [:int :int] :int)
(ffi/defcfn ^:private c-close "close" [:int] :int)

(defn- o-wronly-append
  "O_WRONLY | O_APPEND: the one flag that differs is O_APPEND's value."
  []
  (bit-or 1 (if (str/starts-with? (str (System/getProperty "os.name")) "Mac")
              0x8 0x400)))

(defn redirect-stderr!
  "Point file descriptor 2 at `path`, appending, and return the descriptor
  the old stderr was saved to (for `restore-stderr!`).

  At the descriptor rather than by rebinding `*err*`: log lines, the
  runtime's own warnings and anything a native library prints all go
  through fd 2, and any of them landing on a fullscreen TUI scribbles over
  the frame. Throws, leaving stderr as it was, when the file cannot be
  opened."
  [path]
  (let [f (io/file path)]
    (some-> (.getParentFile f) .mkdirs)
    (spit f "" :append true)
    (let [fd (c-open path (o-wronly-append))]
      (when (neg? fd)
        (throw (ex-info (str "cannot open " path " for the log") {:path path})))
      (let [saved (c-dup 2)]
        (c-dup2 fd 2)
        (c-close fd)
        saved))))

(defn restore-stderr!
  "Undo `redirect-stderr!`, given what it returned."
  [saved]
  (when (and saved (not (neg? saved)))
    (c-dup2 saved 2)
    (c-close saved))
  nil)

;; --- the logger ---------------------------------------------------------------

(def ^:private level-order {:trace 0 :debug 1 :info 2 :warn 3 :error 4 :fatal 5})

(defn- file-logger-factory
  "A tools.logging factory appending to `path`, one line per call, with the
  same shape and threshold as the stock stderr logger."
  [path]
  (let [lock (Object.)
        line (fn [level logger-ns throwable message]
               (str (str/upper-case (name level)) " " logger-ns " - " message
                    (when throwable (str " " throwable)) "\n"))]
    (reify impl/LoggerFactory
      (name [_] "samizdat/file")
      (get-logger [_ logger-ns]
        (reify impl/Logger
          (enabled? [_ level]
            (>= (get level-order level 2) (get level-order impl/*level* 2)))
          (write! [_ level throwable message]
            (locking lock
              (spit path (line level logger-ns throwable message) :append true))))))))

(defn log-to-file!
  "Send tools.logging to `path` and return the factory it replaced, for
  `restore-logging!`.

  Needed on top of `redirect-stderr!` because when stdout and stderr are the
  same terminal the runtime's error port IS its output port — a log line
  would still reach the screen through fd 1."
  [path]
  (let [prev log/*logger-factory*]
    (some-> (io/file path) .getParentFile .mkdirs)
    (alter-var-root #'log/*logger-factory* (constantly (file-logger-factory path)))
    prev))

(defn restore-logging!
  "Undo `log-to-file!`, given what it returned."
  [prev]
  (alter-var-root #'log/*logger-factory* (constantly prev))
  nil)
