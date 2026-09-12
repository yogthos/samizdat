{% if no-matches %}No file matches `{{pattern}}`.

That is a fact about the tree, not an error. If you expected matches, the
pattern is probably anchored wrongly: `*.clj` matches only the root, `**/*.clj`
matches every depth. Hidden directories are never searched, so a path under
`.git` or a cache will not appear whatever you ask for.
{% endif %}{% if found %}{{total}} file(s) match `{{pattern}}`, showing {{from}}–{{to}}:
{% endif %}{% if more %}
{{remaining}} more. Continue with `glob({"pattern": {{pattern}}, "offset": {{next}}})`,
or narrow with `paths` — a pattern that matches half the tree is usually a
question that wanted a directory.
{% endif %}{% if bad-pattern %}`{{pattern}}` is not a usable glob: {{detail}}

Globs are path patterns, not regexes. `**` spans directories, `*` does not
cross a `/`, and `{a,b}` alternates. To search file CONTENTS by regex, that is
`grep`.
{% endif %}
