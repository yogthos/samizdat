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
            [instaparse.combinators :as c]
            [instaparse.core :as insta]
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

(def ^:private shell-grammar
  "The shell STRUCTURE a decision needs, as a grammar rather than as state
  flags in a scanner: statements, the separators between them, quoting, and
  redirection. A POSIX subset — exactly the subset the old char-by-char pass
  tracked — so a rule reads structure the shell would agree with, and a
  string the shell would refuse (an unclosed quote, a dangling escape) is a
  parse FAILURE with a position instead of a silently wrong split.

  Built with the combinator API so the regexes are regex literals; the same
  grammar in EBNF would carry three levels of string escaping.

    command  = seg (sep seg)*            ; && is two seps and an empty seg
    seg      = item*
    item     = squote / dquote / escape / benign / redirect / plain
    squote   = '...'                     ; literal, nothing inside is an operator
    dquote   = \"...\"                   ; backslash escapes honoured
    escape   = \\ + any char, or a trailing backslash
    benign   = >&N, >/dev/null, >>/dev/null — a redirection that writes
               nothing and runs nothing; must end at a word boundary, so
               `> /dev/nullx` is an ordinary write
    redirect = < or >                    ; anything else, and it is opaque
    sep      = ; | & or a newline
    plain    = a run of anything else

  Ordered choice (/), so benign is tried before redirect. Tags are hidden on
  everything but seg, sep and redirect: a segment's children come back as
  the strings that spell it, quotes and all, and the two tagged items are
  the two facts the decision reads."
  {:command  (c/cat (c/nt :seg) (c/star (c/cat (c/nt :sep) (c/nt :seg))))
   :seg      (c/star (c/nt :item))
   :item     (c/hide-tag (c/ord (c/nt :squote) (c/nt :dquote) (c/nt :escape)
                                (c/nt :benign) (c/nt :redirect) (c/nt :plain)))
   :squote   (c/hide-tag (c/regexp #"'[^']*'"))
   :dquote   (c/hide-tag (c/regexp #"(?s)\"(?:[^\"\\]|\\.)*\""))
   :escape   (c/hide-tag (c/regexp #"(?s)\\.|\\$"))
   :benign   (c/hide-tag (c/regexp #"(?:>&[0-9]+|>>?[ \t]*/dev/null)(?![A-Za-z0-9_./-])"))
   :redirect (c/regexp #"[<>]")
   :sep      (c/regexp #"[;|&\n]")
   :plain    (c/hide-tag (c/regexp #"[^;|&\n<>'\"\\]+"))})

(def ^:private shell-parser
  (insta/parser shell-grammar :start :command))

(defn- segment-text
  "The text of one :seg node — its children, spelled back out."
  [[_ & items]]
  (apply str (map (fn [x] (if (vector? x) (second x) x)) items)))

(defn shell-split
  "A command string's shell STRUCTURE: the statement segments (split at an
  unquoted `;`, `|`, `&` or newline), WHICH of those separators it actually
  saw, and whether an unquoted redirection (`<` or `>`) appears anywhere.

  The separator set is reported because they are not equally dangerous. A
  `|` feeds one allowed command into the next and runs nothing an allow rule
  cannot see; a `;` or `&` starts a statement that has nothing to do with the
  first.

  A redirection that only discards or folds stderr does not count as one: it
  writes nothing and runs nothing, and treating `find … 2>/dev/null` as
  opaque cost a live run a turn every time it looked around (karamazov-7es).

  Quote semantics follow bash: single quotes are literal (nothing inside is
  an operator, not even backslash), double quotes honour backslash escapes,
  and an unquoted backslash escapes the next character. Operators inside
  quotes are string literals — `git commit -m \"a; b\"` is one statement.

  Redirection does not split: it does not start a new command, and a deny
  glob (`.*` spans the rest of the string) already covers the tail of its
  segment. This is the lexer provenance A-1 asked for — the old regex-only
  classification let `echo pwned; rm -rf ~` ride `echo **` because `.*`
  matches `;` too.

  A string the grammar refuses — an unclosed quote — comes back as its one
  raw segment plus `:malformed {:index i}`, the character the parse stopped
  at. The scanner used to swallow an unclosed quote to the end of the string
  and let the command through on its head, for bash to fail with `unexpected
  EOF`; `classify` now marks it opaque and the refusal names the position."
  [raw]
  (let [tree (shell-parser raw)]
    (if (insta/failure? tree)
      (let [{:keys [index]} (insta/get-failure tree)]
        {:segments [raw] :separators #{} :redirection? false
         :malformed {:index index}})
      (let [nodes (rest tree)]
        {:segments (->> nodes
                        (filter #(= :seg (first %)))
                        (map segment-text)
                        (map str/trim)
                        (remove str/blank?)
                        vec)
         :separators (into #{} (comp (filter #(= :sep (first %)))
                                     (map #(first (second %))))
                           nodes)
         :redirection? (boolean
                        (some (fn [[tag & items]]
                                (and (= :seg tag)
                                     (some #(and (vector? %) (= :redirect (first %)))
                                           items)))
                              nodes))}))))

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

(def ^:private hijacking-vars
  "Environment variables that change WHICH program runs, or what it loads
  before running. An assignment of one of these is an exec wrapper wearing a
  different syntax, and must not be stripped when matching an allow.

  `PATH=/tmp/evil git status` is the canonical case (dirge-8zem) and it was
  already pinned by a test — the loader and interpreter variables below are
  the same trick through a different door, and the git ones make git itself
  exec an arbitrary program. Matched by prefix for the LD_/DYLD_/GIT_
  families, because their members are numerous and keep being added.

  Anything NOT here falls through to today's behaviour when in doubt: the
  command is matched raw, no allow rule fires, and a human is asked."
  {:names #{"IFS" "ENV" "BASH_ENV" "SHELLOPTS" "PYTHONPATH" "PYTHONSTARTUP"
            "PYTHONHOME" "NODE_OPTIONS" "NODE_PATH" "PERL5LIB" "PERL5OPT"
            "RUBYOPT" "RUBYLIB" "GEM_PATH" "CLASSPATH" "JAVA_TOOL_OPTIONS"
            "JDK_JAVA_OPTIONS" "_JAVA_OPTIONS" "PATH"}
   :prefixes ["LD_" "DYLD_" "GIT_"]})

(defn- hijacking-var? [nm]
  (let [nm (str nm)]
    (boolean (or (contains? (:names hijacking-vars) nm)
                 (some #(str/starts-with? nm %) (:prefixes hijacking-vars))))))

(defn- assignments-stripped
  "The command with leading `VAR=val` assignments removed — and nothing else,
  and only for variables that cannot change what runs.

  An ordinary assignment prefix and an exec wrapper are not the same act, and
  treating them as one made a documented workflow unreachable. `sudo cmd`,
  `xargs cmd` and `timeout cmd` all change what runs — `sudo` escalates,
  `xargs` runs the command once per input line — so an allow rule matched past
  them would be approving something it never read. `FOO=1 cmd` runs exactly
  `cmd` with one more variable in its environment, and the program may read
  its environment either way (what it may SEE is scrub-env's question).

  The exception is the variables in `hijacking-vars`, which do choose the
  program or its libraries. Hitting one stops the walk, so the command is
  matched raw and falls through to `ask`.

  Live consequence (run a3566c73): the brief said to verify with
  `RAYLIB_APP_AUTO_QUIT_MS=1500 jolt -M:run`, which is how the examples repo
  documents a headless smoke run — and the assignment in head position meant
  no allow rule could match it, not even the one for `jolt -M:test`. The
  branch asked four times across the run and was refused every time.

  Anything a substitution could hide (`FOO=$(…)`) is a complex-marker and is
  caught by `classify` before this matters."
  [raw]
  (loop [s (str/trim (str raw))]
    (let [tok (str (first (str/split s #"\s+")))
          rest (str/trim (subs s (min (count s) (count tok))))]
      (if (and (re-matches #"[A-Za-z_][A-Za-z0-9_]*=.*" tok)
               (not (hijacking-var? (first (str/split tok #"=" 2))))
               (not (str/blank? rest)))
        (recur rest)
        s))))

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
        {:keys [segments redirection? malformed]} (shell-split raw)]
    {:raw raw
     :head (command-head raw)
     :segments segments
     ;; The shell would refuse this string (an unclosed quote); nothing a
     ;; rule reads off it is trustworthy, so it is opaque like a substitution.
     :malformed malformed
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
     :complex? (boolean (or malformed
                            (some #(re-find % raw) complex-markers)
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
   ;; sed and awk sit with the other text tools rather than with the mutators
   ;; because refusing them cost a turn every time a run reached for the most
   ;; ordinary way to read part of a file, and `sed -n` is a read.
   ;;
   ;; THE JUSTIFICATION THAT USED TO BE HERE WAS FALSE, and it is worth saying
   ;; so rather than replacing it quietly. It read "the agent already has an
   ;; unrestricted write_file", which was never true: write_file is confined to
   ;; the project root (files/resolve-under-root), and eval — the other thing
   ;; that could once write anywhere — is confined too now (karamazov-zrq). An
   ;; argument of the form "this is open anyway" outlived the two things that
   ;; made it true, which is exactly how a hole survives a review.
   ;;
   ;; What is actually true: `sed -i` here CAN write outside the project root,
   ;; and nothing above catches it unless the path is in protected-paths. That
   ;; is a real gap, accepted for the read case's sake and written down instead
   ;; of dressed up. Narrowing it means splitting the read and write forms of
   ;; these heads, which the classifier cannot do today.
   ["sed **" :allow] ["awk **" :allow]
   ["find **" :allow] ["file **" :allow] ["stat **" :allow] ["env" :allow]
   ;; `magick` reads an image and reports on it — a histogram, the dimensions,
   ;; the mean colour. It sits with the read-only inspectors because that is
   ;; what a graphical project needs it for: run 69880d84 rendered a frame to
   ;; shot.png, had no way to find out whether anything was IN it, and burned
   ;; four turns being refused (135, 136, 140, and again after a grant). The
   ;; model cannot see an image, so a histogram is the only evidence available
   ;; to it that a frame is not blank — and "the process exited 0" is not
   ;; evidence that anything was drawn.
   ;;
   ;; ImageMagick can also WRITE, and this allows that — the same accepted gap
   ;; as sed/awk above, and on the same terms: it is allowed for the read case,
   ;; the write half is not separately gated, and that is a cost rather than a
   ;; non-issue. It is NOT justified by write_file being unrestricted, which it
   ;; never was.
   ["magick **" :allow] ["identify **" :allow]
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
   ;; The project's RUN alias, beside its test alias. `cargo run` and `go run`
   ;; were already here and this was not, purely because the colon-alias quirk
   ;; above needs one pattern per alias and nobody had needed this one. Live
   ;; consequence (run a3566c73): the brief's second acceptance criterion is
   ;; `RAYLIB_APP_AUTO_QUIT_MS=1500 jolt -M:run`, and the branch spent three
   ;; turns being refused a command it had been told to run. Running the
   ;; project's own entry point is the same trust as running its tests, and
   ;; the shell timeout still bounds a program that never exits.
   ["jolt -M:run **" :allow] ["jolt -A:run **" :allow] ["jolt run **" :allow]
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
  "The last rule — [pattern effect] — whose pattern matches any of
  `candidates`, or nil when none match. The rule and not just its effect, so
  a decision can say which one made it."
  [rules candidates]
  (reduce (fn [acc [pattern _ :as rule]]
            (if (some #(matches? pattern %) candidates)
              rule
              acc))
          nil
          rules))

(def ^:private structural-rules
  "The rules that are not table rows: how the table, the grants and the
  protected paths combine into one decision. Enumerable beside the table,
  and every decision names the one that made it."
  [{:name :deny :doc "hard deny; always wins"}
   {:name :protected-path :doc "writes the run config"}
   {:name :grant :doc "a human's allow"}
   {:name :allow :doc "an allow-table match"}
   {:name :compound-allow :doc "every statement allowed"}
   {:name :complex-downgrade :doc "allow demoted; command opaque"}
   {:name :blocked-segment :doc "one statement not allowed"}
   {:name :malformed :doc "unparseable by the shell"}
   {:name :default :doc "nothing matched; ask"}])

(defn rules
  "The whole policy as data: {:structural [...] :table base-rules}.

  :structural is the decision's own logic, most authoritative first — a deny
  from the table or a protected path beats a grant beats the table beats the
  default — with the two compound outcomes (every statement allowed, or the
  first statement that is not) and the downgrade an opaque command gets. A
  decision's :rule is one of these by name, with the pattern, path or
  statement that made it."
  []
  {:structural structural-rules :table base-rules})

(defn decide
  "The decision for a shell command: {:effect :allow|:ask|:deny :head :raw
  :rule}, where :rule names which rule made it — see `rules`.

  Order, most-authoritative last: a hard deny in the base rules always wins;
  otherwise a session grant (human-only) allows; otherwise the base rules
  (last match); otherwise the default `ask`. A complex command whose only
  support is an allow is downgraded to `ask` — its inner command is invisible.

  `session` is {:grants [pattern ...]} from the grants table (empty is fine)."
  [session command]
  (let [{:keys [raw head complex? decomposable? segments malformed]} (classify command)
        ;; Allow matching sees the command RAW — a wrapper prefix changes what
        ;; runs and must not ride an allow. Deny matching sees EVERY statement
        ;; segment (each is a command the shell would run on its own) plus its
        ;; exec-prefix-stripped form, so a denied command hidden after a `;`, a
        ;; newline, or a pipe still denies — widening here can only over-deny.
        ;; …with one exception, and only one: a leading `VAR=val` assignment,
        ;; which sets a variable for the very command a rule is about to read
        ;; rather than standing in front of a different one. See
        ;; `assignments-stripped` — exec wrappers are deliberately NOT stripped
        ;; here.
        allow-candidates (distinct [raw (assignments-stripped raw)])
        deny-candidates (->> (shell-split raw)
                             :segments
                             (cons raw)
                             (mapcat (fn [s] [s (exec-prefix-stripped s)]))
                             distinct
                             vec)
        deny-rule (last-match (filter #(= :deny (second %)) base-rules) deny-candidates)
        ;; A statement that can write a protected path is a hard deny like the
        ;; base deny rules — it wins over grants, and compound decomposition
        ;; cannot resurrect it.
        protected-hit (protected-path-hit deny-candidates)
        deny-hit (or (second deny-rule) (when protected-hit :deny))
        grant-pattern (some #(when (matches? % raw) %) (:grants session))
        grant-hit (when grant-pattern :allow)
        base-rule (last-match base-rules allow-candidates)
        base-hit (second base-rule)
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
        ;; Judged exactly as a whole command is, assignment prefix and all —
        ;; the two paths reading the same statement differently is how
        ;; `jolt -M:test | tail` was allowed while
        ;; `RAYLIB_APP_AUTO_QUIT_MS=1500 jolt -M:test | tail` was not.
        ;; GRANTS COUNT HERE TOO. The rule this path already states is that if
        ;; every statement matched an allow, every command the shell will run
        ;; has been allowed — and a grant IS an allow, a human's. Reading only
        ;; base-rules made a grant work alone and fail inside `cd X && granted`,
        ;; which is the shape agents actually use: run 69880d84 was granted
        ;; `magick **` to unblock a render gate and was still refused, told
        ;; "`magick` is not on the allow list" moments after a human put it
        ;; there. A deny still wins above; this only widens the allow side.
        granted? (fn [cands] (some (fn [p] (some #(matches? p %) cands))
                                   (:grants session)))
        segment-effects (when decomposable?
                          (map (fn [seg]
                                 (let [cands (distinct [seg (assignments-stripped seg)])]
                                   (if (granted? cands)
                                     :allow
                                     (second (last-match base-rules cands)))))
                               segments))
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
        downgraded? (and complex? (not compound-allow?) (= :allow effect))
        effect (cond
                 (and compound-allow? (not= :deny effect)) :allow
                 downgraded? :ask
                 :else effect)
        ;; WHICH RULE MADE IT, most authoritative first, mirroring the
        ;; cascade above — so the refusal names the rule instead of leaving
        ;; the model to guess at a table it has never seen.
        rule (cond
               deny-rule {:name :deny :pattern (first deny-rule)}
               protected-hit {:name :protected-path :path protected-hit}
               compound-allow? {:name :compound-allow}
               malformed {:name :malformed :index (:index malformed)}
               blocked-segment {:name :blocked-segment :segment blocked-segment}
               downgraded? {:name :complex-downgrade
                            :pattern (or grant-pattern (first base-rule))}
               grant-pattern {:name :grant :pattern grant-pattern}
               base-rule {:name :allow :pattern (first base-rule)}
               :else {:name :default})]
    ;; `:promoted?` — this command WOULD have been allowed on its head and was
    ;; downgraded for being compound. Returned because the refusal has to be
    ;; able to say so: without it the message reads "`ls` is not on the allow
    ;; list", which is false and sent a live run round the same wall twice.
    {:effect effect :head head :raw raw :complex? complex? :promoted? promoted?
     :blocked-segment blocked-segment :malformed malformed
     :rule rule
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
        {:keys [effect head complex? promoted? blocked-segment protected-path
                malformed rule]}
        (decide session command)
        rule-text (str/join " " (remove nil? [(name (:name rule)) (:pattern rule)
                                              (:path rule) (:segment rule)
                                              (:index rule)]))
        known (secrets/known-values env command)]
    (case effect
      :deny
      ;; :mechanics, not :failure: a deny is the harness declining a
      ;; well-formed call, not evidence about the branch's line of inquiry —
      ;; charging it to the cull counter was karamazov-blt.15. The shell tool
      ;; stamps :policy-refusal? on top, which is what routes it to the
      ;; refusal counter.
      {:category :mechanics :progress? false
       :result (prompt/render "shell-refused"
                              (if protected-path
                                {:protected true :path protected-path :rule rule-text}
                                {:denied true :head head :rule rule-text}))
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
                               :promoted promoted? :rule rule-text
                               :malformed (:index malformed)
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
