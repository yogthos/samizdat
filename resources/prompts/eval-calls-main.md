That eval calls `{{call}}`, a process entry point, so it was not run (rule
`{{rule}}`).

Eval shares the harness process. A `-main` conventionally ends with
`System/exit` — correct for a subprocess, fatal here: it would take down the
server mid-run along with every other branch. Your own test runner almost
certainly exits at the end.

Two things that do work:

  run the suite as a child process, where the exit code is the point:
      shell: `jolt -M:test`

  or call the test namespaces directly, skipping the runner that wraps them:
      eval: `(clojure.test/run-tests 'flight.mechanics-test)`
