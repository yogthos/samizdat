The last few attempts have not moved you forward — they failed, errored, or retried something you already tried.

Step back rather than repairing again. Repairing a broken approach usually produces a differently broken approach, and the branch that keeps repairing is the branch that runs out of turns with nothing on disk.

What is worth trying instead:

- **A smaller piece.** One function, one file, one test that fails for the reason you think it fails. A sub-part of the same job is a different job and usually goes through normally — this is the cheapest way forward and the one most often skipped.
- **Delegate it.** `task({title, ...})` splits the work off to an owner with its own turns and its own context. If you have been on the same obstacle for many turns, it is not going to yield to one more attempt from you.
- **Suspect your own test.** If the code looks right every time you read it, the thing you have not re-read is the test. Check what it actually asserts against what you meant.
- **A different approach entirely.** Not the same design re-typed: a different way of getting the result. If you have been fighting an API, look at how a working example calls it.

Say what you are changing and why before you call anything. A branch that changes approach without being able to state what was wrong with the last one usually comes back to it two turns later.
