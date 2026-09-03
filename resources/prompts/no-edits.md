You are working, but none of it is being kept. This run ends with its turn
budget, and only what is in a file survives it.

Check which of these you are doing:

- Prototyping in the REPL: a function that exists only as an `eval` is not
  part of the project, no test can reach it, and the ship gate cannot see it.
  Write it to its file NOW, rough is fine, and keep iterating against the file.
- Reading, planning, or re-verifying: if your last several turns were reads,
  greps, or re-running a suite that was already green, that is not progress —
  it is postponement. You know enough. Name the next file this task needs and
  write it NOW.
- Saving the writing for later: there is no later. A run's final turns are the
  worst place to land several files at once — that is exactly how a whole
  run's work is lost to one malformed call. Land each file the moment it
  exists in your head, then verify it, then move to the next.

If you cannot name the file you should write next, name the decision that
blocks it, make the smallest choice that unblocks it, and write that file.
