;; samizdat - a self-hosting agentic harness
;; SPDX-License-Identifier: GPL-3.0-or-later

(ns samizdat.fake-done
  "What a scripted model sends with `done` so the ship checklist
  (karamazov-dsfx) does not refuse it: an entry for every id a test's branch
  can owe — its task's tests (t1) and the list items of what it was asked
  (p1..). An entry for an item that is not owed is ignored.")

(def checklist-json
  "The checklist argument as JSON, for a fenced tool call."
  (str "{"
       (apply str (interpose "," (map #(str "\"" % "\":{\"status\":\"met\",\"evidence\":\"handled\"}")
                                      (cons "t1" (map (partial str "p") (range 1 10))))))
       "}"))
