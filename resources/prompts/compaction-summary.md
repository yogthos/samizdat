You are creating a context checkpoint. The turns below are source material for a compact record of prior work. Produce only the structured summary — no greeting, no preamble, no closing remark. Write it in the same language the conversation used.

You have roughly {{budget}} tokens. Spend them on facts, not prose.

**Quote identifiers VERBATIM**: file paths, namespace and function names, ids, commands, exact numbers, error strings, version numbers, config keys. Copy them character for character. Never paraphrase, abbreviate or tidy an identifier — a path you rewrote is a path the next turn cannot open. Mark anything you concluded rather than read as `(inferred)`, and anything stated but never confirmed as `(unverified)`. Never preserve API keys, tokens, passwords or credentials; write `[REDACTED]`.

Fill in every section. A section with nothing to report gets `None.` — do not omit it and do not invent content for it.

## Active Task
[THE SINGLE MOST IMPORTANT FIELD. What should happen NEXT — the piece of work in flight right now, in plain terms. This is NOT necessarily the original request: the current work is often an emergent follow-up, such as debugging a failing test, that arose mid-session and was never asked for. Capture THAT. If the original request is already complete and only follow-up work remains, say plainly that it is done, so the next context does not redo it. If several things were asked and only some are finished, list only the ones outstanding. If nothing is outstanding, write `None.`]

## Goal
[What the work is trying to accomplish overall.]

## Constraints & Preferences
[Stated requirements, conventions, and things ruled out — with who ruled them out.]

## Completed Actions
[What is already DONE. Files written, tests made green, decisions taken. The next context reads this to avoid repeating work; an omission here costs a turn re-doing something.]

## Active State
[What is true of the project RIGHT NOW: which tests pass, what the last command printed, what is half-finished on disk.]

## In Progress
[What was underway when the context ended, and how far it got.]

## Blocked
[What could not be done, and the exact reason — the error string, the refusal, the missing dependency.]

## Key Decisions
[Choices made and WHY, so they are not silently reversed.]

## Resolved Questions
[Questions that were answered, and the answer. Prevents re-asking.]

## Pending User Asks
[Anything asked that has not been addressed.]

## Relevant Files
[Paths, verbatim, one per line, each with a few words on its role.]

## Remaining Work
[What is left, concretely enough to act on.]

## Critical Context
[Anything load-bearing that fits nowhere above: an incantation that worked, a gotcha, a number measured.]

## Source Coverage
[What you were able to see. If the material carries a truncation marker, or begins or ends mid-turn, say so and name what is missing. If you saw all of it, write COMPLETE.]
