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

(ns samizdat.security.token
  "THE HTTP API'S BEARER TOKEN (karamazov-3vu1.11).

  On Linux the sandbox cannot keep a shell command or the eval image off
  loopback — bwrap shares the host network, and seccomp cannot tell one
  connect from another — so what stands between a process the agent runs and
  the harness's API is something that process cannot read. A server issues a
  fresh token when it starts and writes it, owner-only, under
  ~/.config/samizdat/http/<port>.token: inside the secret regions
  (samizdat.security.confine), which neither the shell nor the image can
  read. samizdat.api.client reads it from there, so a front end in this
  process or another finds it without being told; a person's curl reads it
  the same way:

      curl -H \"Authorization: Bearer $(cat ~/.config/samizdat/http/3985.token)\" …

  The server drops it when it stops."
  (:require [clojure.string :as str]
            [jolt.fs :as fs]))

(defn home [] (System/getenv "HOME"))

(defn- default-dir [] (str (home) "/.config/samizdat/http"))

(defn dir
  "Where the tokens are written."
  []
  (default-dir))

(defn path-for [port] (str (dir) "/" port ".token"))

(defonce ^:private issued
  ;; port -> token, for this process's servers.
  (atom {}))

(defn- fresh []
  (let [b (byte-array 32)]
    (.nextBytes (java.security.SecureRandom.) b)
    (apply str (map #(format "%02x" (bit-and % 0xff)) b))))

(defn issue!
  "A new token for the server on `port`, written owner-only. Returns it."
  [port]
  (let [t (fresh)
        p (path-for port)]
    (fs/create-dirs (dir))
    ;; Created empty and narrowed BEFORE the token is written, so there is no
    ;; moment it sits in a file others may read. Appended, not spat over: a
    ;; plain spit replaces the file, and the replacement has the default mode.
    (fs/delete-if-exists p)
    (spit p "")
    (fs/set-posix-file-permissions p "rw-------")
    (spit p t :append true)
    (swap! issued assoc port t)
    t))

(defn read-for
  "The token a server on `port` issued, read from its file, or nil."
  [port]
  (some-> (try (slurp (path-for port)) (catch Throwable _ nil)) str/trim not-empty))

(defn current
  "The token this process's server holds, or nil when it has none."
  []
  (first (vals @issued)))

(defn revoke!
  "Drop the token for `port` and its file."
  [port]
  (swap! issued dissoc port)
  (try (fs/delete-if-exists (path-for port)) (catch Throwable _ nil))
  nil)

(defn revoke-all! []
  (doseq [p (keys @issued)] (revoke! p)))

(defn valid?
  "Whether `header` (an Authorization value) carries the token this process
  issued. Compared in constant time."
  [header]
  (let [t (current)
        given (some->> (str header) (re-matches #"(?i)Bearer\s+(\S+)") second)]
    (boolean
     (and t given (= (count t) (count given))
          (zero? (reduce bit-or 0 (map #(bit-xor (int %1) (int %2)) t given)))))))
