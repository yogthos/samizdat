## What was asked

{{requirement}}

## The plan the owner intends to carry out

{{plan}}

You are reviewing this PLAN before any code is written. Judge whether carrying
it out would satisfy what was asked — not whether it is elegant, and not what
you would have planned instead.

Look for the gaps that are cheap to fix now and expensive later:

- **Unaddressed requirement.** The ask names something the plan does not
  touch. This is the highest-value finding, and the one the plan phase exists
  for — a requirement missing from the plan is a requirement the run will
  spend its whole budget not building.
- **Wrong target.** The plan changes something other than what was asked — a
  clean, complete plan for the wrong thing is still the wrong thing.
- **A missing test.** The ask introduces behaviour the plan does not say it
  will pin.
- **An unstated assumption or ambiguity** that the plan resolves one way when
  the requirement could mean another. Name it so it can be decided now rather
  than discovered in review.

Do not invent work the requirement does not call for, and do not demand a
level of detail a plan cannot carry — a plan is a direction, not the diff.
When the plan plainly addresses the ask, say so and pass: a plan phase that
blocks every plan is a tax, not a gate.

Answer with a first line exactly `VERDICT: COMPLETE`, `VERDICT: INCOMPLETE`,
or `VERDICT: ABSTAIN`, then optionally a `FINDINGS:` section, one finding per
line, each prefixed with its severity tag ([critical], [high], [medium],
[low]).
