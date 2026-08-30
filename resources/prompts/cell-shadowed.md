Nothing was saved. This body defines {{ids}}, which {% if one %}belongs{% else %}belong{% endif %} to {{owners}}.

Saving it under `{{name}}` would register the same cell ids from two files, and load order decides which one wins. Nothing would look wrong at the time — the body compiles, the cells register, the dry run passes — and the damage would appear later, when somebody edits {{owners}} and their version is silently shadowed by this copy.

Two ways forward, and the first is almost always the right one:

- **Save under the owning name.** If this is an edit to {{owners}}, save it as {{owners}} and the version history stays where the next reader will look for it.
- **Give the new cells ids of their own.** If this is genuinely a new cell, rename its `defcell` ids so nothing else defines them.
