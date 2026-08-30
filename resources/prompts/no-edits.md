You are working, but none of it is being kept. The REPL session dies with this
run: a function that exists only as an `eval` is not part of the project, no
test can reach it, and the ship gate cannot see it.

Write what you have to its file NOW, even if it is rough and even if you are
mid-debug — a version on disk that you refine beats a better one that only
ever existed in a REPL. Then go back to iterating, against the file.

If you are stuck on one form, that is the strongest reason to write it: a file
you can re-read is easier to fix than a form you keep retyping.
