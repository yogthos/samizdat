Refused (rule `{{rule}}`, on `{{on}}`): that would write harness source, or
reload it into the process you are running in.

Editing the kernel from `eval` puts model-written code into the live harness
with no checkpoint, no validation, no soak, and no version for rollback to
return to. A run did exactly this once and the change was invisible to every
safety the harness has.

The supported routes, in order of preference:

- **Cells, manifests and prompts** are the editing surface for behaviour, and
  they are not refused here — `write_file` under `resources/`, then
  `reload_cells` to validate and load the change.
- **`src/` itself** goes through the mutation protocol, which checkpoints
  first, validates the edit, soaks it, and can roll it back.

If you were reading rather than writing, `read_file` and `grep` reach harness
source without this refusal.
