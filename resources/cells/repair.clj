;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; The tool-call REPAIR LADDER as cells. The rungs themselves are core
;; mechanism (samizdat.llm.fence) — string-aware scanners any workflow needs
;; to receive a call at all — but WHICH rungs run, and in what order, is a
;; composition, and composition is this layer's business. The `repair`
;; manifest wires these in order; the system installs the compiled manifest
;; into fence's repair seam at startup, so editing the manifest (or one of
;; these cells) through the ordinary mutation path changes how the very next
;; malformed call is repaired — no rebuild, no restart.
;;
;; Two rules every rung honors:
;;   - each rung is validated by the caller's re-parse, never by its own
;;     optimism (dirge's valid-but-wrong lesson: a repair that produces
;;     parseable output can still produce the WRONG output, so the ladder
;;     only ever makes a parse possible, and repairs that could fabricate
;;     content — closing an unterminated string — are refused in the rung).
;;   - the data map carries :body and nothing else is touched, so a rung a
;;     project adds composes without knowing its neighbours. Every rung
;;     declares exactly that shape, so the second rule is now checked at
;;     compile time rather than trusted: a rung that reached for another key
;;     would have to say so, and the manifest would refuse it.
(ns cells.repair
  (:require [mycelium.cell :as cell]
            [samizdat.llm.fence :as fence]))

(cell/defcell :repair/control-chars
  {:doc "Escape raw newlines, carriage returns and tabs inside string
        literals — the dominant malformation when a model hands over
        multi-line code. First, so every later rung sees intact string
        boundaries."
   :pure true
   :requires []
   :input  [:map [:body :string]]
   :output [:map [:body :string]]}
  (fn [_ data] (update data :body fence/repair-control-chars)))

(cell/defcell :repair/trailing-commas
  {:doc "Drop commas that sit directly before a } or ], outside strings —
        interior ones too, since a mid-body trailing comma survives any
        repair that only looks at the tail."
   :pure true
   :requires []
   :input  [:map [:body :string]]
   :output [:map [:body :string]]}
  (fn [_ data] (update data :body fence/strip-trailing-commas)))

(cell/defcell :repair/dangling-key
  {:doc "Complete a body that stops right after a key with null, so the
        closers can land and the tool's own missing-argument check names
        exactly which argument was lost. Refuses a body that stops inside a
        string — that is the truncation shape, and half a file written as a
        success costs the work."
   :pure true
   :requires []
   :input  [:map [:body :string]]
   :output [:map [:body :string]]}
  (fn [_ data] (update data :body fence/fill-dangling-key)))

(cell/defcell :repair/close-unbalanced
  {:doc "Append the } and ] the body is missing, counted outside strings.
        Last, after the dangling key is filled, or the closers would end an
        object mid-entry. Only ever adds; a body ending inside a string is
        left for the parse error to report."
   :pure true
   :requires []
   :input  [:map [:body :string]]
   :output [:map [:body :string]]}
  (fn [_ data] (update data :body fence/close-unbalanced)))
