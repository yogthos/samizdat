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

(ns samizdat.agent.source
  "THE gate every piece of model-authored Clojure passes through, whatever it
  is destined for — a file, an eval, a saved cell.

  There used to be three behaviours and no rule. `write_file` repaired,
  `edit_file` refused, and `eval` did nothing at all: a live run
  (5e8b5973) lost 2 of its first 17 turns to `Eval error: Unmatched
  delimiter: )` — no line, no column, no repair — while the identical text
  handed to `write_file` would have been closed for it. That was not a policy,
  it was an omission, and it stayed invisible because the seam that knew how
  to answer was a PRIVATE helper inside samizdat.agent.files.

  THE RULE, and it is one question:

      Is there pre-existing code that auto-closing could re-parent?

  Text WHOLLY AUTHORED IN THIS CALL — a whole-file write, an eval form, a cell
  body — has none. Appending the closer the model dropped can only finish what
  it was already writing, so repair is safe. Text landing INSIDE a file the
  model did not write this turn does have such code, and closing a truncation
  there silently re-parents the forms around it. That one is refused.

  DIAGNOSIS IS UNCONDITIONAL. It is pure and changes nothing, so no path has
  an excuse to answer with a bare reader message. Every refusal names a line
  and a column, and a mismatch names both ends.

  The shape enforces the caveats that produced karamazov-2d3, rather than
  merely avoiding them:

  - `:code` and `:problem` are MUTUALLY EXCLUSIVE. When there is a problem
    there is no `:code`, so a caller cannot write or eval broken text even by
    mistake. The bug wrote the file it had just broken.
  - The refusing path never CALLS `lisp/balance`. Not \"does not report a
    repair\" — never computes one, so there is no repair verdict in scope to
    leak. The bug handed back `auto-closed 1 unclosed delimiter(s) … appended
    `)`` about a file nothing had been appended to, because it asked balance's
    \"could this be repaired\" question at a site that refuses.
  - `:repaired` is present only when the text actually CHANGED, and `:code` is
    then the changed text. A note describing something that did not happen is
    how the original lie was told."
  (:require [samizdat.lisp :as lisp]
            [samizdat.prompt :as prompt]))

(defn explain
  "The one sentence the model reads about a syntax verdict, from
  prompts/clojure-syntax.md.

  `lisp` answers in DATA — a reason, a line and column, the reader's own words
  — and every sentence around that data lives in resources, keyed by reason,
  so a project working in another language can reword or drop them without a
  rebuild. The reason is passed as a BOOLEAN per key rather than compared in
  the template: selmer's `if` tests truthiness and has no equality operator."
  [{:keys [reason] :as verdict}]
  (when reason
    (prompt/render "clojure-syntax"
                   (-> verdict
                       (dissoc :status :content :reason)
                       (assoc (name reason) true)))))

(defn vet
  "Vet model-authored Clojure `text` before anything acts on it.

  Options:
    :whole?    the text is wholly authored in this call (a whole-file write,
               an eval form) so nothing pre-existing can be re-parented and a
               trailing truncation may be repaired. False for an edit landing
               inside a file, which is refused instead.
    :clojure?  whether to look at all; defaults true. A caller that knows it
               is holding a .txt or a shell script passes false and gets its
               text straight back — deciding WHAT is Clojure belongs to the
               caller, not here.

  Returns exactly one of:
    {:code text}                        it loads; use this text
    {:code repaired :repaired v :note s} it was repaired; use THIS text
    {:problem v :note s}                it does not load; act on the note

  Never both `:code` and `:problem` — that is the invariant a caller relies on
  to make writing broken text impossible."
  [text {:keys [whole? clojure?] :or {clojure? true}}]
  (let [text (str text)]
    (cond
      (not clojure?)
      {:code text}

      ;; REFUSING PATH. `diagnose` and nothing else: it answers \"is this
      ;; loadable now\", which is the question a caller that refuses actually
      ;; has. `balance` is not called here at all, so no repair verdict exists
      ;; to be reported by accident.
      (not whole?)
      (if-let [d (lisp/diagnose text)]
        {:problem d :note (explain d)}
        {:code text})

      ;; REPAIRING PATH. Nothing pre-existing to re-parent, so a trailing
      ;; truncation or over-close is closed — and `balance` only ever returns
      ;; :repaired when the result actually READS, so `:code` always loads.
      :else
      (let [r (lisp/balance text)]
        (case (:status r)
          :balanced {:code (:content r)}
          :repaired {:code (:content r)
                     :repaired (dissoc r :status :content)
                     :note (explain r)}
          ;; :unbalanced / :unreadable — a mid-file imbalance, a mismatch, an
          ;; unterminated string, or source that balances and still is not
          ;; Clojure. No :code, deliberately.
          {:problem (dissoc r :status) :note (explain r)})))))
