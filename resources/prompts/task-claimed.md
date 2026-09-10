You are now working on **{{id}} — {{title}}**.
{% if body %}
{{body}}
{% endif %}{% if contract %}
CONTRACT — what the work must satisfy:
{{contract}}
{% endif %}{% if tests %}
TESTS — what defines delivery:
{{tests}}
{% endif %}
{% if surface %}{{surface}}

{% endif %}FIRST, before any code: is this ONE thing, or several wearing one title?

A task is one thing when a single change, pinned by a test, satisfies the whole
contract. It is several when it names parts that could each be built, tested
and reviewed on their own — usually visible as an "and", a list, or a layer
boundary (storage AND handlers AND templates).

ANSWER IT WITH A NUMBER, because "is this one thing" asked on its own is
reliably answered "yes" and then disproved 500 lines later. Estimate how many
lines of code this will take, added and deleted together. **Under {{budget}},
work it. Over {{budget}}, split it first.**

That number is not a style rule. It is the size at which a task stops being
reviewable in one piece: a change larger than that sends its good parts back
with its bad ones, because the review has to judge all of it at once.

The harness measures the same number as you work, from the tree rather than
from your estimate, and will say so if you cross it. Being told then is much
worse than deciding now — by then the budget you would have used on the parts
is spent on the whole.

If it is several, split it NOW, before starting: create a subtask per part with
`parentId` set to this task, then move your claim to the first one with
`task switch` (a plain `claim` is refused while you hold this task):

```tool-call
{"name": "task", "args": {"action": "create", "title": "<one part>", "parentId": "{{id}}", "contract": "<what that part must satisfy>", "tests": "<what proves it>"}}
```

```tool-call
{"name": "task", "args": {"action": "switch", "id": "<first subtask>", "reason": "split {{id}} into parts; working the first"}}
```

Splitting up front costs a few turns and gives every part its own review; not
splitting means one budget spent on a half-built whole, which lands nothing.
Do not split a task that is genuinely one change — a subtask you would close in
the same breath as its parent is overhead.

This is your task until you close it. Everything you do from here should serve
it; if you find work that does not, make it another task rather than widening
this one. Close it with `task close` when the contract is met, and say what you
did — then claim the next one.
