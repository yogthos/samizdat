**You are busy but not advancing.** Your recent turns have run without errors and without producing anything new. That pattern is harder to notice from the inside than failing is, which is why the harness is saying it out loud.

Which of these are you doing?

- **Re-reading something you already understand**, hoping it will say something different. It will not.
- **Verifying your tooling** rather than your problem. That a library loads is not a result.
- **Refining something that already works** instead of starting the next piece.

Pick one of these and do it now.

- **Write the next file, even rough.** A file on disk that you refine beats a better one you are still planning. If you know its name, that is enough to start it.
{% if can-split %}- **Split the work and hand the pieces down.** A task you cannot finish in a few turns is usually more than one thing, and that makes you its architect. Write the stubs the pieces must fill, write the code that calls them, sketch their tests, then `split` — each piece comes back filled and tested, and a piece owns functions rather than a file, so sharing a namespace is fine. For a whole sub-job somebody else should own instead, `task({title, ...})`.
{% else %}- **Split the work and delegate it.** If the piece you are on is too big to finish in a few turns, `task({title, ...})` breaks it into a sub-task with its own owner and its own turns. A task you cannot finish is a task that wants splitting, not more staring.
{% endif %}- **Run the tests.** If you do not know whether you are ahead or behind, that is the fastest way to find out, and it is cheap.

If you genuinely cannot proceed, say what is blocking you and call `give_up` honestly. That is a real outcome and it is recorded as one. Spending the rest of your turns looking busy is not.
