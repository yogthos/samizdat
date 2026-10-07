## Edits to this project's workflow that were REJECTED

Each of these files was changed — by a person, or by a command no branch was
told about — and the change did not pass its check. The newest earlier version
that passes is what runs, if there is one; the file still holds the broken
text.

{% for f in files %}- `{{f.path}}` ({{f.kind}} `{{f.role}}`): {{f.stage}} check failed{% if f.line %} at line {{f.line}}{% if f.column %}, column {{f.column}}{% endif %}{% endif %} — {{f.message}}{% if f.offered %}
  An updated `{{f.role}}` is on offer from the shipped workflow: `adopt show` it before repairing by hand — taking it may be the whole fix.{% endif %}
{% endfor %}
Fix each one in place: read the file, correct the problem named above, and
write it again — it is checked on the next read and runs the moment it passes.
If the edit was a mistake, restore the previous version with the matching
tool's `revert`.
