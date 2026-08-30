;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Ported from dirge's src/agent/agent_loop/tool_error_class.rs and
;; tool_retry.rs. The four-class taxonomy, the both-must-hold retry gate, the
;; positive allowlist and the short backoff are all that design.

(ns samizdat.agent.toolerr
  "WHY A TOOL CALL FAILED, and whether running it again is allowed to be tried.

  A tool result is success-or-error text, which answers `did the tool report a
  problem` — not the question the next turn needs answered. Four classes, and
  each wants a different response from the branch:

    :misuse        the call was malformed. The contract is already in context;
                   re-read it rather than retrying.
    :missing-info  the world is not shaped the way the branch thinks — file
                   absent, symbol unknown, pattern matched nothing. The fix is
                   to LOOK. This is the WANDERING signal: a run accumulating
                   these is operating on a wrong picture of the tree, which is
                   exactly the failure that reads as many varied calls, none of
                   them repeats, and that no repetition guard can see.
    :transient     the call did not complete for reasons unrelated to its
                   inputs. Retrying can legitimately work and the branch
                   changing its approach cannot help.
    :fatal         a wall — the filesystem or the OS saying no.

  THE ASYMMETRY THAT SHAPES THE RETRY. A provider request retries freely,
  because a request that failed in transport never reached the model. A tool
  call is not like that: **a timeout does not mean the work did not happen.** A
  shell command killed at its budget may have run to completion, or half of it.
  Re-issuing that is how one `git push` becomes two.

  So the gate is BOTH: the error class says retrying *could* work, and the
  tool's own nature says retrying is *allowed to be tried*. A positive
  allowlist of read-only tools, so anything new is non-retryable until somebody
  argues otherwise — the safe answer is the one that should have to be
  argued against."
  (:require [clojure.string :as str]))

(defn classify
  "Which class of failure this result is, from its text and its flags.

  `vocab` is the wordlists.edn table — the phrases are what a tool actually
  says, and a project whose tools speak differently must be able to retune the
  classification without a rebuild. A result nothing matches is `:misuse`,
  the class whose advice (re-read the contract) is harmless when wrong."
  [{:keys [result timeout? category]} vocab]
  (let [t (str/lower-case (str result))
        any? (fn [k] (some #(str/includes? t (str %)) (get vocab k)))]
    (cond
      ;; A timeout is transient BY FLAG, before any text matching: it is the
      ;; one failure the tool layer records structurally, and reading it out of
      ;; prose when the flag is right there would be guessing at a fact.
      timeout? :transient
      (any? :fatal) :fatal
      (any? :transient) :transient
      (any? :missing-info) :missing-info
      (= :mechanics category) :misuse
      :else :misuse)))

(defn retry-safe?
  "Whether re-running this tool can be assumed not to duplicate an effect.

  A POSITIVE ALLOWLIST of pure reads. Running one twice returns the same
  answer or a better one — the language server that timed out while indexing
  has had another moment to index, which is precisely the case this exists
  for and one the branch can do nothing useful about.

  Everything else is refused, including tools that merely look harmless.
  Borrowing a predicate that answers a neighbouring question — `is this
  side-effecting for the repetition guard` — is how a guard ends up protecting
  something it was never measured against."
  [tool read-only]
  (contains? read-only (str tool)))

(defn should-retry?
  "Whether to run this call again. BOTH conditions, never either alone."
  [{:keys [tool class attempt]} {:keys [max-attempts read-only]}]
  (boolean (and (= :transient class)
                (retry-safe? tool read-only)
                (< (or attempt 1) max-attempts))))

(defn backoff-ms
  "Wait before attempt `n`, doubling from `base`.

  A beat, not a rate-limit backoff. This sits INSIDE the branch's turn, and a
  minute-long pause mid-turn is indistinguishable from a hang; the condition it
  waits out is a warming language server, not a quota."
  [attempt base]
  (long (* base (Math/pow 2 (max 0 (dec (or attempt 1)))))))

(defn uncertain-effect?
  "Whether a failure leaves it UNKNOWN what the call landed.

  A timeout on a tool that changes something is the case: the command may have
  completed, or done half its work, and the result text cannot say which. The
  branch needs telling, because the reasonable next move — run it again — is
  the one thing that is unsafe."
  [{:keys [tool timeout? class]} read-only]
  (boolean (and (or timeout? (= :transient class))
                (not (retry-safe? tool read-only)))))
