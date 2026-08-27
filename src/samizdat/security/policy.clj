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

(ns samizdat.security.policy
  "The shell command permission engine, ported from dirge src/permission/.

  Every command a tool would run faces a decision first: allow, ask, or deny.
  The rules are ordered and last-match-wins; a complex command (one carrying a
  substitution, subshell, compound operator, or unquoted redirection) can
  never ride an allow, because the rest of what the shell would run is
  invisible to the head an allow matched; a deny is head-anchored through
  env/wrapper prefixes AND evaluated per statement segment, so `nohup rm -rf
  /` and `ls; sudo rm -rf /` both still hit `rm -rf /**`; and an allow matches
  the command RAW so a `PATH=/tmp/evil git status` cannot ride a `git *`
  allow.

  Session grants (human-only, from the grants table) are consulted ahead of the
  base rules, so an approved `ask` becomes an allow for the rest of the run —
  but a hard deny always wins. This is the `perm` node of the security model
  (docs/RFCS/RFC-003-security-model.md), and `run-shell` is where it, the env scrub, and the
  redaction boundary meet on the shell tool path."
  (:require [clojure.string :as str]
            [samizdat.engine.proc :as proc]
            [samizdat.lexicon :as lexicon]
            [samizdat.prompt :as prompt]
            [samizdat.security.secrets :as secrets]
            [samizdat.store.grants :as grants]
            [samizdat.util :as util]))

;; --- glob → matcher ---------------------------------------------------------

(defn- glob->regex
  "A shell-style glob to a regex string. `*` and `**` both match any run of
  characters (including `/`), which is dirge's command-glob semantic — unlike
  a path glob, a `*` is not stopped by a slash. Everything else is literal.
  A trailing ` *` makes the args optional (`ls *` matches bare `ls`)."
  [pattern]
  (let [;; A trailing ` *`/` **` (space then stars) becomes an optional args
        ;; group, so the bare command with no args also matches.
        [head optional?] (if-let [m (re-matches #"(.*?)\s+\*+" pattern)]
                           [(nth m 1) true]
                           [pattern false])
        rx (->> head
                (partition-by #(= \* %))
                (map (fn [chs]
                       (if (= \* (first chs))
                         ".*"
                         (java.util.regex.Pattern/quote (apply str chs)))))
                (apply str))]
    (str "^" rx (when optional? "(?:\\s.*)?") "$")))

(defn matches?
  "Whether `input` matches the shell glob `pattern`."
  [pattern input]
  (boolean (re-matches (re-pattern (glob->regex pattern)) input)))

;; --- command classification -------------------------------------------------

(def ^:private complex-markers
  "A command carrying any of these was not decomposed: the shell would run an
  inner command an allow rule never sees. Treated as ask-regardless. (Compound
  operators and redirection are caught separately, by shell-split, because a
  regex cannot tell a quoted `;` from an operator.)"
  [#"\$\(" #"`" #"<\(" #">\(" #"\$\[" #"\(\("])

(def ^:private benign-redirect-re
  "Redirections that neither create a file nor run anything: stderr folded into
  another stream (`2>&1`, `>&2`) and either stream discarded to /dev/null.
  Anchored at the `>` itself, so the stream digit in front of it is already
  consumed."
  #"(?s)^(>&\d+|>>?[ \t]*/dev/null)")

(defn- benign-redirect-len
  "The length of the benign redirection starting at index `i` of `raw`, or nil
  when the redirection there is anything else — a write, an append, an input
  read — which stays opaque.

  A length rather than a flag because the scanner has to CONSUME the whole
  token: the `&` in `2>&1` is a redirection operator, and letting the
  statement splitter see it cut `ls -la 2>&1; cat x` into an `ls -la 2>`, a
  bare `1`, and a `cat x`, so the command no longer decomposed into statements
  any rule could match."
  [raw i]
  (let [m (re-find benign-redirect-re (subs raw i))
        tok (if (vector? m) (first m) m)
        end (when tok (+ i (count tok)))]
    (when (and tok
               ;; `> /dev/nullx` is an ordinary file write wearing a familiar
               ;; prefix; the token must end at the end of a word.
               (or (>= end (count raw))
                   (not (re-matches #"[A-Za-z0-9_./-]" (str (nth raw end))))))
      (count tok))))

(defn- shell-split
  "One quote-aware pass over a command string, yielding its shell STRUCTURE:
  the statement segments (split at unquoted `;`, `|`, `&`, and newline), WHICH
  of those separators it actually saw, and whether an unquoted redirection
  (`<` or `>`) appears anywhere.

  The separator set is reported because they are not equally dangerous. A `|`
  feeds one allowed command into the next and runs nothing an allow rule
  cannot see; a `;` or `&` starts a statement that has nothing to do with the
  first. `decide` uses the distinction to stop refusing `grep x | head`.

  A redirection that only discards or folds stderr does not count as one: it
  writes nothing and runs nothing, and treating `find … 2>/dev/null` as
  opaque cost a live run a turn every time it looked around (karamazov-7es).

  Quote semantics follow bash: single quotes are literal (nothing inside is
  an operator, not even backslash), double quotes honor backslash escapes, and
  an unquoted backslash escapes the next character. Operators inside quotes
  are string literals — `git commit -m \"a; b\"` is one statement.

  Redirection does not split: it does not start a new command, and a deny glob
  (`.*` spans the rest of the string) already covers the tail of its segment.
  This is the lexer provenance A-1 asked for — the old regex-only
  classification let `echo pwned; rm -rf ~` ride `echo **` because `.*`
  matches `;` too."
  [raw]
  (let [n (count raw)
        sep? #{\; \| \& \newline}]
    (loop [i 0, state :code, cur [], segs [], redirect? false, seps #{}]
      (if (>= i n)
        {:segments (->> (conj segs (apply str cur))
                        (map str/trim)
                        (remove str/blank?)
                        vec)
         :separators seps
         :redirection? redirect?}
        (let [c (nth raw i)]
          (case state
            :code (cond
                    (= c \\) (if (< (inc i) n)
                                (recur (+ i 2) :code (into cur [c (nth raw (inc i))]) segs redirect? seps)
                                (recur (inc i) :code (conj cur c) segs redirect? seps))
                    (= c \') (recur (inc i) :single (conj cur c) segs redirect? seps)
                    (= c \") (recur (inc i) :double (conj cur c) segs redirect? seps)
                    (sep? c) (recur (inc i) :code [] (conj segs (apply str cur)) redirect? (conj seps c))
                    (or (= c \<) (= c \>))
                    (if-let [len (and (= c \>) (benign-redirect-len raw i))]
                      ;; consumed whole, so its own `&` never reaches the
                      ;; statement splitter
                      (recur (+ i len) :code (into cur (subs raw i (+ i len)))
                             segs redirect? seps)
                      (recur (inc i) :code (conj cur c) segs true seps))
                    :else (recur (inc i) :code (conj cur c) segs redirect? seps))
            :single (if (= c \')
                      (recur (inc i) :code (conj cur c) segs redirect? seps)
                      (recur (inc i) :single (conj cur c) segs redirect? seps))
            :double (cond
                      (and (= c \\) (< (inc i) n))
                      (recur (+ i 2) :double (into cur [c (nth raw (inc i))]) segs redirect? seps)

                      (= c \") (recur (inc i) :code (conj cur c) segs redirect? seps)
                      :else (recur (inc i) :double (conj cur c) segs redirect? seps))))))))

(defn- exec-prefix-stripped
  "The command with leading `VAR=val` assignments and exec wrappers
  (env/nohup/nice/…) removed, so a head-anchored deny still sees the real
  command. Deny-side only: widening here can only over-deny."
  [raw]
  (let [wrappers #{"env" "nohup" "nice" "ionice" "setsid" "stdbuf" "time"
                   "timeout" "xargs" "sudo" "doas"}]
    (loop [s (str/trim raw)]
      (let [tok (first (str/split s #"\s+"))
            rest (str/trim (subs s (min (count s) (count tok))))]
        (cond
          ;; a VAR=value assignment prefix — strip and keep going
          (re-matches #"[A-Za-z_][A-Za-z0-9_]*=.*" (str tok))
          (if (str/blank? rest) s (recur rest))
          ;; an exec wrapper — strip and keep going, but a bare wrapper with
          ;; nothing after it IS the command (e.g. `env` alone), so stop there
          (and (contains? wrappers tok) (not (str/blank? rest)))
          (recur rest)
          :else s)))))

(defn- command-head
  "The leading executable token of the real command — env/wrapper prefixes
  stripped — for display and rule matching."
  [raw]
  (or (first (str/split (exec-prefix-stripped raw) #"\s+")) ""))

(defn classify
  "A shell command string into {:raw :head :complex?}. A complex command is
  one the shell would expand or compound — substitution, subshell, arithmetic,
  a `;`/`|`/`&`/newline separator, or an unquoted redirection — because in
  every one of those cases an allow rule matched on the head never saw the
  rest of what would run."
  [command]
  (let [raw (str/trim (str command))
        {:keys [segments separators redirection?]} (shell-split raw)]
    {:raw raw
     :head (command-head raw)
     :segments segments
     ;; DECOMPOSABLE: the command is exactly a list of statements, with
     ;; nothing the shell would expand into a command a rule cannot see — no
     ;; substitution, no subshell, no redirection that writes. Every segment
     ;; is then a command an allow rule can read in full, which is what lets
     ;; `decide` allow a compound whose every part is independently allowed.
     ;; The separator does not enter into it: `|`, `;`, `&&` and a newline
     ;; all run exactly the statements shell-split just handed back.
     :decomposable? (boolean (and (> (count segments) 1)
                                  (not redirection?)
                                  (not (some #(re-find % raw) complex-markers))))
     :complex? (boolean (or (some #(re-find % raw) complex-markers)
                            redirection?
                            (> (count segments) 1)))}))

;; --- the rules --------------------------------------------------------------

(def base-rules
  "The curated allow/ask/deny table, ported from dirge permission/mod.rs
  base_bash_rules. Ordered; last match wins. Interpreters (python/node/npx),
  git push, destructive git, package installs, sudo, and curl/wget are
  deliberately absent — they fall through to the default `ask`. Hard denies
  come last so they win over any allow."
  [;; read-only inspection
   ["ls **" :allow] ["cd **" :allow] ["pwd" :allow] ["echo **" :allow]
   ["which **" :allow] ["type **" :allow] ["cat **" :allow] ["head **" :allow]
   ["tail **" :allow] ["wc **" :allow] ["sort **" :allow] ["uniq **" :allow]
   ["cut **" :allow] ["diff **" :allow] ["grep **" :allow] ["rg **" :allow]
   ;; sed and awk sit with the other text tools rather than with the mutators:
   ;; `sed -i` does write, but so do `mv`, `cp`, `touch` and `chmod` below it,
   ;; and the agent already has an unrestricted `write_file`. Refusing them
   ;; protected nothing and cost a turn every time a run reached for the most
   ;; ordinary way to read part of a file.
   ["sed **" :allow] ["awk **" :allow]
   ["find **" :allow] ["file **" :allow] ["stat **" :allow] ["env" :allow]
   ["date **" :allow] ["whoami" :allow] ["hostname" :allow]
   ;; benign shell builtins
   ["export *" :allow] ["set *" :allow] ["unset *" :allow]
   ["pushd *" :allow] ["popd *" :allow]
   ;; git — local read/write inside the repo (push/reset/checkout/clean omitted)
   ["git status **" :allow] ["git log **" :allow] ["git diff **" :allow]
   ["git show **" :allow] ["git branch **" :allow] ["git add **" :allow]
   ["git commit **" :allow] ["git pull **" :allow] ["git fetch **" :allow]
   ["git remote **" :allow] ["git tag **" :allow] ["git blame **" :allow]
   ["git rev-parse **" :allow] ["git rev-list **" :allow] ["git ls-files **" :allow]
   ;; filesystem mutators
   ["mkdir **" :allow] ["touch **" :allow] ["mv **" :allow] ["cp **" :allow]
   ["ln **" :allow] ["chmod **" :allow]
   ;; project-scoped runners — jolt/clojure toolchain for THIS project, plus
   ;; the common ecosystems dirge trusts. Bare interpreters stay excluded.
   ;; The project's own toolchain — running its tests and evaluating Clojure
   ;; in the project image is the core self-modification workflow, and jolt
   ;; runs THIS project's code (same trust as editing it). The colon-alias
   ;; forms (`-A:test`, `-M:test`, `-A:dev`) need their own patterns: a
   ;; trailing ` **` makes args optional only after a space, so `jolt -A **`
   ;; does not match `jolt -A:test …`. Surfaced by the first dogfood run,
   ;; which blocked on exactly this and needed a manual grant to proceed.
   ["jolt test **" :allow] ["jolt build **" :allow]
   ["jolt -e **" :allow] ["jolt -A **" :allow] ["jolt -M **" :allow]
   ["jolt -A:test **" :allow] ["jolt -M:test **" :allow] ["jolt -A:dev **" :allow]
   ["jolt -A:test -e **" :allow] ["jolt -M:test -e **" :allow]
   ["clj -M **" :allow] ["clojure -M **" :allow] ["lein test **" :allow]
   ["cargo check **" :allow] ["cargo build **" :allow] ["cargo test **" :allow]
   ["cargo fmt **" :allow] ["cargo clippy **" :allow] ["cargo run **" :allow]
   ["pytest **" :allow] ["ruff **" :allow] ["black **" :allow] ["mypy **" :allow]
   ["go build **" :allow] ["go test **" :allow] ["go run **" :allow]
   ["make **" :allow] ["just **" :allow] ["bd **" :allow]
   ;; hard denies — destructive system-level operations, last so they win
   ["rm -rf /**" :deny] ["sudo rm -rf /**" :deny] ["dd **" :deny]
   ["mkfs **" :deny] ["mkfs.* **" :deny] ["fdisk **" :deny] ["mkswap **" :deny]])

(def ^:private default-effect :ask)

;; --- protected paths ---------------------------------------------------------

(def ^:private protected-paths
  "Paths no shell statement may reach under a head that can write.
  .samizdat/config.edn defines the ship gates (:run :verify-cmd,
  :require-test?): run 671e8a99 replaced its verify command mid-run with one
  that ran 0 tests and exited 0 — a Gate 2 that always passes
  (karamazov-kvw). The file tools refuse it too (files/run-config?); this
  closes the mv/cp/sed side doors those allows would otherwise leave open.

  Hardcoded rather than policy data on purpose: gates.edn is agent-editable
  userspace, so a protected list living there could be unprotected by the
  party it protects against. Not airtight — a segment can name the file
  without its full path — but the failure it exists to stop is a run drifting
  into rewriting its own gate, not a jailbreak; `eval` containment is its own
  open problem (karamazov-zrq)."
  [".samizdat/config.edn"])

(def ^:private read-only-heads
  "Heads that cannot write their file arguments, so a protected path in their
  arguments is a READ — which stays open: a run may always inspect its own
  gates. Anything else touching a protected path is denied, and over-denying
  (an unlisted read like `sed -n`) is the correct direction; `cat` is the
  road."
  #{"cat" "head" "tail" "wc" "grep" "rg" "less" "more" "diff" "ls" "stat"
    "file" "find" "sort" "uniq" "cut" "md5" "shasum" "echo"})

(defn- protected-path-hit
  "The first protected path some candidate statement mentions under a head
  that can write, or nil."
  [candidates]
  (some (fn [seg]
          (when-not (contains? read-only-heads (command-head seg))
            (some #(when (str/includes? seg %) %) protected-paths)))
        candidates))

(defn- last-match
  "The effect of the last rule whose pattern matches any of `candidates`, or
  nil when none match."
  [rules candidates]
  (reduce (fn [acc [pattern effect]]
            (if (some #(matches? pattern %) candidates)
              effect
              acc))
          nil
          rules))

(defn decide
  "The decision for a shell command: {:effect :allow|:ask|:deny :head :raw}.

  Order, most-authoritative last: a hard deny in the base rules always wins;
  otherwise a session grant (human-only) allows; otherwise the base rules
  (last match); otherwise the default `ask`. A complex command whose only
  support is an allow is downgraded to `ask` — its inner command is invisible.

  `session` is {:grants [pattern ...]} from the grants table (empty is fine)."
  [session command]
  (let [{:keys [raw head complex? decomposable? segments]} (classify command)
        ;; Allow matching sees the command RAW — a wrapper prefix changes what
        ;; runs and must not ride an allow. Deny matching sees EVERY statement
        ;; segment (each is a command the shell would run on its own) plus its
        ;; exec-prefix-stripped form, so a denied command hidden after a `;`, a
        ;; newline, or a pipe still denies — widening here can only over-deny.
        allow-candidates [raw]
        deny-candidates (->> (shell-split raw)
                             :segments
                             (cons raw)
                             (mapcat (fn [s] [s (exec-prefix-stripped s)]))
                             distinct
                             vec)
        deny-hit (last-match (filter #(= :deny (second %)) base-rules) deny-candidates)
        ;; A statement that can write a protected path is a hard deny like the
        ;; base deny rules — it wins over grants, and compound decomposition
        ;; cannot resurrect it.
        protected-hit (protected-path-hit deny-candidates)
        deny-hit (or deny-hit (when protected-hit :deny))
        grant-hit (when (some #(matches? % raw) (:grants session)) :allow)
        base-hit (last-match base-rules allow-candidates)
        effect (cond
                 deny-hit :deny
                 grant-hit :allow
                 :else (or base-hit default-effect))
        ;; The statements of a decomposable command, each judged on its own.
        ;; A compound was refused wholesale — `find . -type f | sort`,
        ;; `ls -la; cat deps.edn`, `git status && ls -la` — every part on the
        ;; allow list, nothing hidden from a rule, and a run spends a turn on
        ;; each refusal it walks into. What makes a compound opaque is a
        ;; command a rule never saw, not the punctuation between commands: if
        ;; shell-split enumerated every statement and each one matched an
        ;; allow, then every command the shell will run has been allowed.
        ;; Substitution, subshells and writing redirections still hide a
        ;; command and still downgrade.
        segment-effects (when decomposable?
                          (map #(last-match base-rules [%]) segments))
        compound-allow? (and decomposable?
                             (not deny-hit)
                             (seq segments)
                             (every? #(= :allow %) segment-effects))
        ;; The first statement that is not independently allowed — what the
        ;; refusal names, so the model fixes THAT rather than re-splitting a
        ;; command whose other parts were never the problem.
        blocked-segment (when (and decomposable? (not compound-allow?))
                          (->> (map vector segments segment-effects)
                               (some (fn [[s e]] (when-not (= :allow e) s)))))
        ;; A complex command cannot otherwise ride an allow: downgrade
        ;; allow → ask, but a deny still stands.
        promoted? (and complex? (not compound-allow?)
                       (= :allow (or base-hit default-effect))
                       (not deny-hit) (not grant-hit))
        effect (cond
                 (and compound-allow? (not= :deny effect)) :allow
                 (and complex? (= :allow effect)) :ask
                 :else effect)]
    ;; `:promoted?` — this command WOULD have been allowed on its head and was
    ;; downgraded for being compound. Returned because the refusal has to be
    ;; able to say so: without it the message reads "`ls` is not on the allow
    ;; list", which is false and sent a live run round the same wall twice.
    {:effect effect :head head :raw raw :complex? complex? :promoted? promoted?
     :blocked-segment blocked-segment
     ;; Which protected path forced the deny, so the refusal can name it.
     :protected-path protected-hit}))

;; --- the shell tool ---------------------------------------------------------

(defn- max-output-chars
  "How much of a command's output the model sees. gates.edn
  `:context-budget :shell-output-chars`.

  How much the model gets to see is one table, and this was a constant
  outside it — larger than `:tool-result-chars` for a good reason (a build
  log's useful part is at the end of a great deal of noise) that nobody
  reading either number could see, because they were not in the same place.

  A truncation limit, not a security control: the redaction boundary is
  applied to what remains, not to what is cut, so raising this cannot expose
  anything redaction would have caught."
  []
  (lexicon/budget :shell-output-chars))

(defn- complex-markers-in
  "Which compound-command constructs `raw` actually contains, so a refusal can
  name the one the model used instead of listing every possibility."
  [raw]
  (->> [["&&" "&&"] ["||" "||"] ["|" "|"] [";" ";"] ["$(" "$(...)"]
        ["`" "backticks"] ["<(" "<(...)"] [">" ">"]]
       (keep (fn [[needle label]] (when (str/includes? raw needle) label)))
       distinct))

(defn run-shell
  "Run a shell command through the full gate: decide, then (on allow) resolve
  symbolic refs, spawn with a scrubbed environment, and redact the output
  before it returns. Returns a tool-result map.

  `ctx` carries :conn :run-id :root and :args {:command …}; :env defaults to
  the process environment. This is the one place the perm, scrub, and redact
  nodes of the security model meet.

  The command runs in the run's `:root`, like every other tool. It used to run
  wherever the harness process happened to be: the file tools resolve paths
  under the root and the ship gate's verify `cd`s into it, so a run targeting
  another checkout had `shell` reading and building a different tree than
  `read_file` and `done` — with the same relative paths naming different
  files. Absent root, the process cwd, which is what it always did."
  [{:keys [conn run-id args root] :as ctx}]
  (let [command (str (:command args))
        env (or (:env ctx) (into {} (System/getenv)))
        session (if (and conn run-id) (grants/for-run conn run-id) {:grants []})
        {:keys [effect head complex? promoted? blocked-segment protected-path]}
        (decide session command)
        known (secrets/known-values env command)]
    (case effect
      :deny
      ;; :mechanics, not :failure: a deny is the harness declining a
      ;; well-formed call, not evidence about the branch's line of inquiry —
      ;; charging it to the cull counter was karamazov-blt.15. The shell tool
      ;; stamps :policy-refusal? on top, which is what routes it to the
      ;; refusal counter.
      {:category :mechanics :progress? false
       :result (if protected-path
                 (prompt/render "shell-refused"
                                {:protected true :path protected-path})
                 (str "Command denied by policy: `" head "` is on the deny list."
                      " This cannot be overridden."))
       :policy {:effect :deny}}

      :ask
      ;; The refusal has to teach the fix, or it is just a wall. Observed live
      ;; twice in one run: the model opened with `ls -la && cat README.md`,
      ;; was refused, and four turns later tried
      ;; `find . -type f | head -50 && echo --- && …` — the same shape, because
      ;; nothing in the first refusal said that being COMPOUND was the reason.
      ;; It reads as "the shell is closed" rather than "issue these separately".
      {:category :neutral :progress? false :needs-approval true
       :result (prompt/render "shell-refused"
                              {:command command :head head :complex? complex?
                               :promoted promoted?
                               ;; Named when the command DID decompose and one
                               ;; statement is the whole reason: pointing at
                               ;; that part beats telling a model to re-split a
                               ;; command whose other parts were never refused.
                               :blocked blocked-segment
                               :blockedhead (some-> blocked-segment command-head)
                               :markers (when complex?
                                          (str/join " or "
                                                    (map #(str "`" % "`")
                                                         (complex-markers-in command))))})
       :policy {:effect :ask
                ;; No grant unlocks a COMPLEX command (invariant 5 downgrades
                ;; it to :ask even over a grant), so suggesting `head *` for
                ;; one taught a fix that could not work — the observed
                ;; same-wall-twice loop through the grant path (blt.38). The
                ;; refusal text already teaches "issue these separately".
                :suggest (when-not complex? (str head " *"))}}

      :allow
      (let [resolved (secrets/resolve-refs command env)
            ;; The child sees ONLY the scrubbed environment — name-sensitive
            ;; vars removed, value-shaped credentials redacted — so a
            ;; subprocess cannot read a secret the parent holds even by
            ;; expanding $VAR itself. env -i semantics (see proc/run :env).
            child-env (secrets/scrub-env env)
            ;; Prefixed rather than passed as a :dir, because proc/run has no
            ;; working-directory option and `bash -c` is already the shell we
            ;; are handing the command to. The root is single-quoted through
            ;; the shared helper; `resolved` is the model's own command and is
            ;; deliberately NOT quoted — running it as written is the tool.
            r (proc/run {:timeout-ms (or (:timeout-ms ctx) 120000)
                         :env child-env}
                        "bash" "-c"
                        (str "cd " (util/sh-quote (or root ".")) " && " resolved))
            out (if (:timeout r)
                  (str "[timed out after " (:ms r) "ms]")
                  (str (:out r)
                       (when (seq (:err r)) (str "\n" (:err r)))))
            ;; Redact the WHOLE output first, then truncate — so a secret that
            ;; would straddle the truncation boundary is caught before the cut.
            ;; truncate-middle keeps the head AND tail, because the end of a
            ;; command's output (a test summary, an exit line) is as load-
            ;; bearing as the start.
            redacted (util/truncate-middle (secrets/redact out known)
                                           (max-output-chars))]
        ;; A missing exit code is a spawn that did not report one, which is
        ;; not evidence the command succeeded. `(or (:exit r) 0)` read it as
        ;; success, the opposite of what run-verify does with the same shape;
        ;; both now fail closed.
        {:category (if (and (not (:timeout r)) (zero? (or (:exit r) 1)))
                     :success :failure)
         :progress? true
         :result redacted
         ;; Carried as a flag rather than left to string-matching the output:
         ;; the loop weights a timeout heavier on the failure streak and drops
         ;; the storm window's retry allowance for the call that hung.
         :timeout? (boolean (:timeout r))
         :policy {:effect :allow}}))))
