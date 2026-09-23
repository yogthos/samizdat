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

(ns samizdat.net
  "Starting the HTTP server: ring-chez-adapter, plus the one thing it does
  not do that this process needs — its listening socket marked close-on-exec.

  Every process the harness spawns — the Lean repl via `lake env`, prolog,
  octave, a shell tool — forks from this one, and without the flag each
  child holds a duplicate of the listening socket. lsof showed jolt, lake and
  repl sharing fd 4. The port then stays bound while ANY of them lives, so
  killing the server with a Lean session still up leaves the next start
  failing with address-in-use against a server that no longer exists. The
  vendored copy of the adapter carried the fix; upstream does not yet."
  (:require [clojure.tools.logging :as log]
            [jolt.ffi :as ffi]
            [ring-chez.adapter :as adapter]))

;; fcntl is variadic, and on Apple arm64 a fixed-arity binding silently
;; corrupts the stack-passed argument — hence :varargs on the setter.
(ffi/defcfn ^:private c-fcntl-set "fcntl" [:int :int :varargs :int] :int)
(ffi/defcfn ^:private c-fcntl-get "fcntl" [:int :int] :int)

;; F_GETFD / F_SETFD / FD_CLOEXEC are 1 / 2 / 1 on both macOS and Linux.
(def ^:private f-getfd 1)
(def ^:private f-setfd 2)
(def ^:private fd-cloexec 1)

(defn cloexec?
  "Whether `fd` is marked close-on-exec."
  [fd]
  (try (pos? (bit-and (c-fcntl-get fd f-getfd) fd-cloexec))
       (catch Throwable _ false)))

(defn close-on-exec!
  "Mark `fd` close-on-exec, and say whether it took — read back, because a
  version that trusted the call passed on macOS and did nothing on CI."
  [fd]
  (try (c-fcntl-set fd f-setfd fd-cloexec) (catch Throwable _ nil))
  (cloexec? fd))

(defn- free-port
  "A port the OS says is free right now."
  []
  (with-open [s (java.net.ServerSocket. 0)]
    (.getLocalPort s)))

(defn run-server
  "adapter/run-server, with the listening socket marked close-on-exec before
  anything can fork with it open.

  Port 0 means any free port, as it does to bind(2) — the adapter refuses 0,
  so one is chosen here, and the handle's :port says which."
  [handler opts]
  (let [opts (cond-> opts (contains? #{0 nil} (:port opts)) (assoc :port (free-port)))
        server (adapter/run-server handler opts)]
    (when-not (close-on-exec! (:socket server))
      (log/warn "http: could not mark the listening socket close-on-exec;"
                "a child process may keep port" (:port server) "bound"))
    server))

(defn stop-server [server]
  (adapter/stop-server server))
