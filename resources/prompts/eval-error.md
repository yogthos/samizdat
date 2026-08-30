This ran in **the live image**, in the run's own eval session `{{where}}` — not from your files on disk. The two can disagree, and telling them apart is usually the whole diagnosis:

- **The session holds state your files do not.** A `require` that failed halfway leaves the namespace half-loaded, and every later call against it fails while the file is perfectly correct. Re-loading is the fix: `(require 'the.ns :reload)`.
- **Your files hold changes the session does not.** An edit is not live until the namespace is re-required, so an eval can keep exercising the version from before your last three edits.

If you have read the file and it looks right, believe the file. Check the session instead: `(require 'the.ns :reload)` and run the form again, or run it from a **fresh process** with `shell` — `jolt -e "(do (require 'the.ns) (println :ok))"` — which loads only from disk. If it passes there and fails here, the file was never the problem.

Do not re-issue the same form hoping for a different result; it has already told you what it has to say.
