---
name: repl-workflow
description: REPL-driven development in the live jolt image — prototype in eval, then WRITE the change to the file. The file on disk is the deliverable; an eval is scratch that vanishes.
---

# REPL-driven development

You work against a live jolt image: `eval` runs Clojure in the same process the
harness runs in, and defs persist across evals within your run. That is a
powerful way to develop — but it is a way to *develop*, not the deliverable. The
deliverable is the **edited file on disk**. An eval is scratch: it disappears
when the run ends, it does not show up in the diff, and a reviewer reading the
code will not see it. If your change lives only in eval, you have not made it.

## Work test-first — it is how you actually converge

Do not try to write the whole correct change in one shot and then hope. Pin the
goal with a test, then drive the code until that test is green. This is the loop
that lets you succeed one small step at a time instead of guessing:

1. **Look, briefly.** `read_file` the one function you need to change and `grep`
   for where it is used. Enough to act — not a tour of the codebase. Reading is
   not progress; a change on disk is.
2. **Write a failing test FIRST.** In the test namespace beside the code, add a
   focused test that states the exact behaviour you must produce — and run it so
   you *see it fail* for the right reason. Now "done" has a concrete meaning.
   Prototype tricky pieces in `eval` if you need to, but the test is the target.
3. **Make it pass, in small steps.** `edit_file` the smallest change you think
   moves the test toward green, `(require 'the.ns :reload)`, re-run the test,
   read the result. Wrong? Change ONE thing and run again. Each cycle is
   seconds. This edit→run→observe→fix loop is the whole job — keep going round it
   until the test is green.
4. **Confirm green, then ship.** When your test passes, call `done`.

## The hard gate: you are not done until the test is green

`done` runs your test. **If it is red, `done` is refused and the failure output
comes straight back to you** — that is your signal to keep iterating, not to stop.
`done` is also refused if you changed no files, or if you changed code but wrote
no test to pin it. So there is no shortcut: the only way out is a real change
with a passing test. That is not an obstacle — it is the loop working. Read the
returned failure, fix the code, and call `done` again.

The failure mode this prevents: prototyping in `eval`, watching it work, and
calling `done` with the file never touched (empty diff) or the behaviour never
tested. eval is where you *figure out* the change; `edit_file`/`write_file` is
where you *make* it; the test is what proves it.

## When you fixed it before you thought to test it

Step 2 has you watch the test fail first, which settles the question for free.
But the common real case is the other order: you were chasing a bug, you found
it, you fixed it, and only then did you write the test. **A test written after
the fix has never been seen to fail, so it is not yet evidence of anything** —
it may be asserting something that was already true.

Prove it, and it costs one turn:

```
cp src/ns/thing.clj /tmp/fixed.clj \
  && sed -i '' 's/<the fixed expression>/<the old broken one>/' src/ns/thing.clj \
  && jolt -M:test 2>&1 | tail -4 \
  ; cp /tmp/fixed.clj src/ns/thing.clj && jolt -M:test 2>&1 | tail -2
```

One command, both results in one output: the reverted tree failing, then the
restored tree green. The backup copy and the restore are in the same command as
the mutation, so there is no turn in which the fix is only in `/tmp`. Use `cp`
and `sed` rather than `git checkout` or `git stash` — the first pair is allowed
outright, the second needs a human.

Then **write down what you measured**, in the test namespace's docstring: which
assertions die without which fix, and — the part nobody can re-derive later —
why the *existing* tests stayed green. "weapon_test.clj keeps passing because it
calls tick directly, which is exactly the blind spot that shipped this bug" is
worth more than the test it annotates, because it names the shape of the hole
rather than one bug that fell through it.

## Practical notes

- Small, surgical edits: prototype the exact form, then `edit_file` just that
  region. Don't rewrite a whole file for a two-line change.
- After editing, `(require 'ns :reload)` picks up the file so your next eval
  runs the real thing, not your stale in-memory def.
- Keep the file compiling at each step — reload after each edit so a typo
  surfaces immediately, not three edits later.

## Working discipline

- **Plan as structure, not intention.** Before you touch files, name the exact
  namespaces, functions, and the order — "change the catch in `remember!`, then
  add its test" — not vague aims like "make it robust".
- **Never repeat a failed call.** If a call errors, returns nothing, or a test
  fails, read the message and do something *different* — inspect, narrow, or
  pick another tool. Re-issuing the same `eval`/`grep` hoping for a different
  result is the fastest way to burn your whole turn budget on nothing.
- **One tool per decision.** `read` to inspect, `grep` to locate, `edit_file` to
  change, `eval`/`shell` to run — pick the single right one instead of trying
  several.
- **Stay on your assigned part.** Re-anchor to what you were asked; do not
  "clean up" or refactor code you were not asked to touch. Success is the
  specific thing asked for, verified to work, with no unrequested changes — and
  the change is in the file.
