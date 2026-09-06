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

(ns samizdat.security.secrets-test
  "The secrets layer, ported from dirge src/sandbox/mod.rs. The unit tests pin
  the ported predicates against dirge's own assertions; the specification
  tests at the bottom assert the security-model property directly — a planted
  canary secret appears in NO model-bound payload, journal row, or event."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [samizdat.security.secrets :as secrets]))

;; --- sensitive names (dirge test_is_sensitive_env_name) ---------------------

(deftest sensitive-names
  (testing "credential-shaped names are sensitive"
    (is (secrets/sensitive-name? "OPENAI_API_KEY"))
    (is (secrets/sensitive-name? "ANTHROPIC_API_KEY"))
    (is (secrets/sensitive-name? "DEEPSEEK_API_KEY"))
    (is (secrets/sensitive-name? "MY_SECRET"))
    (is (secrets/sensitive-name? "DB_PASSWORD"))
    (is (secrets/sensitive-name? "SERVICE_AUTH")))
  (testing "the explicit cloud-credential names with no generic pattern"
    (is (secrets/sensitive-name? "AWS_SESSION_TOKEN"))
    (is (secrets/sensitive-name? "GITLAB_TOKEN"))
    (is (secrets/sensitive-name? "BITBUCKET_TOKEN")))
  (testing "SAFE_EXACT names pass despite matching a pattern — the tools need them"
    (is (not (secrets/sensitive-name? "GITHUB_TOKEN")))
    (is (not (secrets/sensitive-name? "GH_TOKEN")))
    (is (not (secrets/sensitive-name? "SSH_AUTH_SOCK"))))
  (testing "ordinary names pass"
    (is (not (secrets/sensitive-name? "PATH")))
    (is (not (secrets/sensitive-name? "HOME")))
    (is (not (secrets/sensitive-name? "LANG"))))
  (testing "case-insensitive"
    (is (secrets/sensitive-name? "openai_api_key"))
    (is (not (secrets/sensitive-name? "github_token")))))

;; --- sensitive values (dirge is_sensitive_env_value) ------------------------

(deftest sensitive-values
  (testing "vendor-prefix credential shapes — every prefix dirge catches"
    (is (secrets/sensitive-value? "sk-abcdefghijklmnopqrstuvwx"))
    (is (secrets/sensitive-value? "sk-proj-abcdefghijklmnopqrstuvwx"))
    (is (secrets/sensitive-value? "AKIAIOSFODNN7EXAMPLE"))
    (is (secrets/sensitive-value? "ghp_abcdefghijklmnopqrstuvwxyz0123456789"))
    (is (secrets/sensitive-value? "hf_abcdefghijklmnopqrstuvwxyz01234567"))
    (is (secrets/sensitive-value? "xai-abcdefghijklmnopqrstuvwx"))
    (is (secrets/sensitive-value? "AIzaSyDabcdefghijklmnopqrstuvwxyz0123456"))
    ;; Real shapes: github_pat is 22 then 59 alnum; a Slack token is
    ;; xox[bpras]- then three numeric groups then a 32+ alnum hash.
    (is (secrets/sensitive-value?
         (str "github_pat_" (apply str (repeat 22 "A")) "_" (apply str (repeat 59 "B")))))
    (is (secrets/sensitive-value?
         (str "xoxb-12-1234567890-1234567890-" (apply str (repeat 32 "a"))))))
  (testing "URL userinfo carries a credential in a benign-named var"
    (is (secrets/sensitive-value? "postgres://user:hunter2@db.internal/app")))
  (testing "opaque values without a vendor prefix do NOT trip it"
    (is (not (secrets/sensitive-value? "/nix/store/abc123-foo")))
    (is (not (secrets/sensitive-value? "just a sentence")))
    (is (not (secrets/sensitive-value? ""))))
  (testing "adversarial input terminates — irregex backtracks where Rust's DFA
            would not, so every quantifier is bounded and a substring gate runs
            first. Before the fix, github_pat_ x 10000 hung the process."
    ;; If the ReDoS regressed, this test would hang rather than fail — the
    ;; wall-clock is the assertion. A generous count check keeps it honest.
    (is (false? (secrets/sensitive-value? (apply str (repeat 10000 "github_pat_x")))))
    (is (false? (secrets/sensitive-value? (apply str (repeat 5000 "sk-")))))
    (is (= 120000 (count (secrets/redact (apply str (repeat 10000 "github_pat_x"))))))))

;; --- scrub-env --------------------------------------------------------------

(deftest scrub-env-strips-and-redacts
  (let [scrubbed (secrets/scrub-env
                  {"PATH" "/usr/bin"
                   "HOME" "/home/x"
                   "OPENAI_API_KEY" "sk-realkeyrealkeyrealkey12"
                   "GITHUB_TOKEN" "ghp_thisoneisallowedthrough000000000000"
                   "DATABASE_URL" "postgres://u:secretpw@h/db"
                   "ALIAS" "the key is sk-realkeyrealkeyrealkey12 embedded"})]
    (testing "name-sensitive vars are removed entirely"
      (is (not (contains? scrubbed "OPENAI_API_KEY"))))
    (testing "SAFE_EXACT survives so the tools that need it work"
      (is (= "ghp_thisoneisallowedthrough000000000000" (scrubbed "GITHUB_TOKEN"))))
    (testing "benign names pass through untouched"
      (is (= "/usr/bin" (scrubbed "PATH")))
      (is (= "/home/x" (scrubbed "HOME"))))
    (testing "a value-shaped credential in a benign name is redacted, not dropped"
      (is (= "[REDACTED]" (scrubbed "DATABASE_URL"))))
    (testing "a var carrying a KNOWN stripped value is caught even without a prefix"
      (is (= "[REDACTED]" (scrubbed "ALIAS"))))))

;; --- redact -----------------------------------------------------------------

(deftest redact-boundary
  (testing "vendor-prefix tokens by regex"
    (is (= "leaked [REDACTED] here"
           (secrets/redact "leaked sk-abcdefghijklmnopqrstuvwx here"))))
  (testing "URL userinfo password only — scheme and host stay readable"
    (is (= "postgres://u:[REDACTED]@h/db"
           (secrets/redact "postgres://u:hunter2@h/db"))))
  (testing "known opaque values by substring, even with no recognizable shape"
    (is (= "the token is [REDACTED] ok"
           (secrets/redact "the token is OPAQUE-BUILD-abc ok" ["OPAQUE-BUILD-abc"]))))
  (testing "clean text is returned unchanged"
    (is (= "nothing to see" (secrets/redact "nothing to see")))))

;; --- symbolic reference resolution ------------------------------------------

(deftest symbolic-refs
  (let [env {"OPENAI_API_KEY" "sk-symbolicvalue000000000" "PATH" "/usr/bin"}]
    (testing "a {{env/NAME}} ref resolves to the value at spawn time"
      (is (= "curl -H sk-symbolicvalue000000000 x"
             (secrets/resolve-refs "curl -H {{env/OPENAI_API_KEY}} x" env))))
    (testing "resolving reports which values it exposed, for the redaction set"
      (is (= #{"sk-symbolicvalue000000000"}
             (secrets/refs-used "curl -H {{env/OPENAI_API_KEY}} x" env))))
    (testing "an unknown ref resolves to empty and exposes nothing"
      (is (= "x  y" (secrets/resolve-refs "x {{env/NOPE}} y" env)))
      (is (empty? (secrets/refs-used "x {{env/NOPE}} y" env))))))

;; --- SPECIFICATION: the canary never crosses the boundary -------------------

(deftest spec-a-planted-canary-never-reaches-model-space
  ;; The security-model property, asserted directly. A secret enters at the
  ;; env, is referenced symbolically by a command, and the command echoes it;
  ;; the value must appear in NO model-bound payload the run produces.
  (let [canary "sk-CANARYcanarycanary00000"
        env {"SECRET_API_KEY" canary "PATH" "/usr/bin"}
        command "echo using {{env/SECRET_API_KEY}} now"
        ;; What actually reaches the subprocess: refs resolved, env scrubbed.
        resolved (secrets/resolve-refs command env)
        child-env (secrets/scrub-env env)
        ;; The subprocess echoes the resolved secret — the worst case.
        raw-output resolved
        ;; Everything model-bound goes through redact with the run's known
        ;; values (config keys + resolved refs + stripped env values).
        known (secrets/known-values env command)
        tool-result (secrets/redact raw-output known)
        journal-row (secrets/redact raw-output known)]
    (testing "the child env carries no name-sensitive var"
      (is (not (contains? child-env "SECRET_API_KEY"))))
    (testing "the canary appears in nothing the model or journal will read"
      (is (not (str/includes? tool-result canary)))
      (is (not (str/includes? journal-row canary)))
      (is (str/includes? tool-result "[REDACTED]")))
    (testing "the model still directed the call — the symbolic ref is intact upstream"
      (is (str/includes? command "{{env/SECRET_API_KEY}}"))
      (is (not (str/includes? command canary))))))

(deftest the-substring-pass-redacts-an-opaque-secret
  ;; RFC-003 F4. This pass exists for secrets with no recognizable shape — a
  ;; database password, a bearer token with no vendor prefix — and it was DEAD
  ;; on every real call path: `known-values` returns a SET, and `distinct` on a
  ;; set yields the first element as nil under this runtime, so the reduce
  ;; iterated over nothing.
  ;;
  ;; It looked tested. The canary below starts with `sk-`, so the vendor-prefix
  ;; REGEX caught it and the spec passed while asserting nothing about this
  ;; pass. So this test uses a value with NO recognizable shape, and passes the
  ;; known-values as the set the real caller passes.
  (testing "a set, which is what known-values returns"
    (is (= "value is [REDACTED] here"
           (secrets/redact "value is opaqueSECRETvalue here" #{"opaqueSECRETvalue"}))))
  (testing "and a seq, which is what a hand-written caller might pass"
    (is (= "value is [REDACTED] here"
           (secrets/redact "value is opaqueSECRETvalue here" ["opaqueSECRETvalue"]))))
  (testing "the regex is not what is doing the work here"
    (is (not (secrets/sensitive-value? "opaqueSECRETvalue"))
        "no vendor prefix, no URL userinfo — only the substring pass can catch it")))

(deftest spec-eval-output-is-inside-the-redaction-boundary
  ;; RFC-003 F1. `eval` runs in the harness process, so it can read the
  ;; environment and the resolved config directly — strictly more capability
  ;; than the shell path, which gets a scrubbed env AND a redacted result.
  ;; It had neither, and the security model asserted that no path from the
  ;; environment reaches model space unredacted.
  ;;
  ;; This closes the ACCIDENTAL leak, which is the realistic one: a model
  ;; prints a config map while debugging and a provider key lands in the branch
  ;; messages and the journal permanently. Deliberate exfiltration is out of
  ;; scope by design — in-process execution cannot be contained from inside the
  ;; process — and RFC-003 says so rather than leaving it unhandled.
  (let [canary "sk-CANARYcanarycanary00000"
        env {"SOME_API_KEY" canary}
        known (secrets/known-values env)
        ;; What the eval tool now does to a payload on its way to the model.
        payload (str "=> {:api-key \"" canary "\"}")]
    (is (not (str/includes? (secrets/redact payload known) canary))
        "a credential read in-process does not reach the transcript verbatim")))

(deftest the-whole-github-token-family-is-redactable
  ;; GITHUB_TOKEN/GH_TOKEN are deliberately SAFE_EXACT — gh must see them — so
  ;; their values are never in known-values and the regex rail is the only
  ;; thing between `echo $GITHUB_TOKEN` (which rides the echo allow) and the
  ;; journal. The rail covered ghp_/github_pat_ only, while `gh auth login`
  ;; issues gho_/ghu_/ghs_/ghr_ tokens (karamazov-blt.30).
  (doseq [prefix ["ghp_" "gho_" "ghu_" "ghs_" "ghr_"]]
    (let [tok (str prefix "AbCdEfGhIjKlMnOpQrStUvWxYz0123456789")]
      (is (secrets/sensitive-value? tok)
          (str prefix " is a high-confidence credential shape"))
      (is (not (str/includes? (secrets/redact (str "token: " tok)) tok))
          (str prefix " is caught by the regex rail with no known-values")))))

(deftest scrub-env-drops-the-parents-own-project-dir
  ;; JOLT_PWD is the variable jolt's dev wrapper (bin/jolt) exports to say
  ;; which directory is THE PROJECT, and the runtime resolves relative files
  ;; against it. It describes the parent and must never reach a child: a
  ;; project image or a shell tool spawned by a harness that was itself
  ;; started from a source-tree jolt inherited the HARNESS checkout as its
  ;; project dir, so `(slurp "README.md")` inside a run read the harness's
  ;; README and `jolt -M:test` would have run the harness's suite — exactly
  ;; what the project image exists to prevent. Dropping it lets the child's
  ;; own wrapper set it from the cwd `:dir` gave it.
  (let [scrubbed (secrets/scrub-env {"JOLT_PWD" "/Users/someone/src/samizdat"
                                     "PATH" "/usr/bin"
                                     "HOME" "/Users/someone"})]
    (is (not (contains? scrubbed "JOLT_PWD")))
    (is (= "/usr/bin" (get scrubbed "PATH")) "ordinary vars pass through")
    (is (= "/Users/someone" (get scrubbed "HOME")))))
