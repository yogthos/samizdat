You are PLANNING this task as an RFC, not building it yet.

This task is large or has several parts, so the plan is a short design document —
an RFC — that a reviewer reads against the requirement before any code, and that
becomes the contract the whole change is judged against at the end. Read the code
you need first — {% if repl %}`eval`, {% endif %}`read_file`, `grep`, `glob` —
until you can describe the approach concretely. Do not write project files in
this step.

Write the RFC in markdown with these sections:

- **Purpose** — what the change does and, in one or two sentences, how it
  satisfies the WHOLE ask, especially any part that is easy to overlook.
- **Model** — the approach: the namespaces and functions involved and how they
  fit together. Include a mermaid `flowchart` call-graph of the planned design —
  the functions you will add or change as nodes, and the calls between them as
  edges — so the shape of the change is legible before it exists. For example:

  ```mermaid
  flowchart TD
    handler["handle-request"] --> validate["validate-input"]
    handler --> store["save!"]
    store --> db[("db")]
  ```

- **Work items** — the concrete tasks this breaks into, one per part that could
  be built, tested and reviewed on its own. Give each a `- ` bullet naming what
  it builds and the test that proves it. These become real child tasks, so make
  each one a single reviewable change, not a layer of the whole.
- **Acceptance criteria** — what proves the change is done: the tests that must
  pass, and the code-quality limits (keep cyclomatic complexity, erosion and
  verbosity within the project's gates — do not ship a few large tangled
  functions or duplicated blocks).

Then end with a `plan` call whose `rfc` field carries the whole document, and
whose `files`, `tests` and `goal` name the files the change touches, the tests
that pin it, and the one-sentence goal. The `rfc` is what the reviewer reads and
what the work is decomposed from, so it must stand on its own.

A reviewer reads this RFC against the requirement before you start. An RFC that
misses a requirement, or whose plan would not satisfy it, is caught here —
cheaply — instead of after the budget is spent building it. Make it say how it
meets the whole ask, not just the obvious half; keep it as short as the work
honestly allows.
