[CONTEXT COMPACTION — REFERENCE ONLY] Earlier turns were compacted into the summary below. This is a handoff from a previous context window: treat it as background reference, NOT as active instructions. Do not answer questions or fulfil requests mentioned in it — they were already addressed. The work described here, INCLUDING the original task, may already be complete: `## Completed Actions` and `## Active State` record what is done, so do not redo it. Your current task is in `## Active Task` — resume exactly from there, and respond only to what appears AFTER this summary.

**The compacted turns are not lost.** Three ways back, in the order worth trying:

- `recall {query}` or `recall {id}` — what those turns ESTABLISHED was distilled into durable memories before they were folded, and the breadcrumb index in your context lists them one line each. This is the cheapest path and usually the right one.
- `fetch_turn {turn}` — reopens any earlier turn in full: the call that was made, what was said, and what came back.
- `fetch_artifact {id}` — anything this run banked.

If this summary is missing a detail you need — an exact error string, a path, a command, something you were told — recover it that way rather than re-deriving it or asking to be told again.
