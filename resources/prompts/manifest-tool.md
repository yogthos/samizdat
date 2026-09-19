{% if saved %}Saved manifest '{{name}}' v{{version}} — it compiles. A run configured for '{{name}}' (config :run :loop) will use it; tuning the active manifest is picked up on the next run.{% endif %}{% if stale %}`manifest patch` refused: '{{name}}' is at {% if version %}v{{version}}{% else %}the factory template (nothing stored yet){% endif %}, not v{{expect}} — show it again and re-apply.{% endif %}{% if ops-not-a-list %}`ops` must be a list of {:op "name" ...} maps, one per edit.{% endif %}{% if ops-not-maps %}Every entry in `ops` must be a map naming its :op.{% endif %}{% if ops-unreadable %}`ops` did not read as EDN: {{reason}}{% endif %}{% if diff-nothing-stored %}No stored versions of manifest {{name}} — diff compares stored versions, and this project has not saved one yet.{% endif %}{% if diff-one-version %}'{{name}}' has only one version (v{{version}}); nothing to compare it against.{% endif %}{% if ops-help %}Patch ops — `ops` is a list of {:op "name" ...} maps; an argument whose value is EDN (edges, dispatches, params, value, expect) is passed as a string:
  add-cell {{args.add-cell}}
    Add a node. With only `id` it names its registered handler, like every node here; `edges` and `dispatches` wire it, or `after` splices it into that node's unconditional edge. A `doc`, `input`, `output`, `on-error` or `requires` writes the definition out in full instead.
  delete-edge {{args.delete-edge}}
    Delete a node's outgoing edge, or one labelled transition and its dispatch entry.
  remove-cell {{args.remove-cell}}
    Remove a node and everything it owns. `rewire` retargets the edges and error paths that point at it; without it, they are listed and the op refuses. An invariant that names the node blocks either way — edit it first.
  rename-cell {{args.rename-cell}}
    Rename a node and rewrite every reference: edges, dispatches, invariants.
  set-cell-field {{args.set-cell-field}}
    Set one field of a node — `id` swaps its handler. `expect` refuses if the current value differs.
  set-dispatches {{args.set-dispatches}}
    Replace a node's dispatch table: "[[label pattern] ...]", first match wins.
  set-edge {{args.set-edge}}
    Set a node's outgoing edge, or one labelled transition of it. A label on a plain edge makes it a map — add the dispatch in the same batch.{% endif %}
