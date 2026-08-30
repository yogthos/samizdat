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
