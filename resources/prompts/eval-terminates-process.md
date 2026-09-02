That eval calls {{calls}}, which ends the harness process rather than the
evaluation — so it was not run (rule `{{rule}}`).

Eval executes in the LIVE harness image: your code and the harness share one
process. `(System/exit 0)` there is not an error you would get back, it is the
server going down mid-run, taking every other branch with it and exiting with a
success status so it looks like a clean shutdown.

To stop your own work, return an answer. To stop a RUN, use the abort tool. To
end a subprocess you started, use the shell tool — a child process is yours to
kill, and this one is not.
