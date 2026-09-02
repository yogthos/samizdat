A workflow stopped and is waiting for you. It is not lost: the state machine
is data, so the exact place it stopped was kept, and it can be re-entered once
the cause is fixed.

## What failed

**{{failure.type}}** at `{{failure.cell}}`{% if failure.node %} (node `{{failure.node}}`){% endif %}

{{failure.message}}

{% if failure.missing %}Keys the cell required and did not get: {{failure.missing|join:", "}}
{% endif %}{% if failure.extra %}Keys it had that its schema does not mention: {{failure.extra|join:", "}}
{% endif %}

## How the run got there

Each line is one cell that ran, with the edge it left by and the keys it ADDED
to the data map. A step that added nothing took a route that produced nothing,
which is usually where the answer is. This is the tail of the run — the trace
is capped, so earlier turns are not shown.

```
{{path}}
```

{% if available %}Keys present when it stopped: {{available|join:", "}}{% endif %}

## What to do

The workflow is `{{workflow}}`{% if version %} v{{version}}{% endif %} and it will
re-enter at `{{resume-state}}` — the cell that failed, so your fix is retried
rather than skipped.

1. Read the cell (`cell show {{failure.cell-name}}`) or the manifest
   (`manifest show {{workflow}}`) and decide which is wrong: the cell's code,
   the shape it declares, or the wiring that routes to it.
2. Fix it and save. The save validates and dry-runs before it stores, so a fix
   that does not hold is refused immediately and costs you nothing.
3. `parked resume {{branch-id}}` to re-enter.

You are not reading values here, only shapes. If you need to see what a key
actually held, `fetch_turn` has it.
