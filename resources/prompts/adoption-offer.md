## On offer to this project's workflow

This project's workflow is its own: nothing below has been applied. Each is
something the project has not answered yet — a role the shipped workflow has
and this project lacks (new), a shipped template that changed since this
project saw it (updated), or a version this project stored before its
workflow lived in files (pending).

{% for o in offers %}- {{o.kind}} `{{o.name}}` — {{o.offer}}{% if o.edited %}, and this project edited its own copy too{% endif %}{% if o.version %} (stored v{{o.version}}){% endif %}
{% endfor %}
Decide each one for THIS project. `adopt show` puts the offered text beside
the project's own; `adopt take` installs it, checked like any edit, and
`adopt decline` says no with the reason, so it is not asked again. An update
over a copy this project edited replaces that edit — read both first. A
manifest that names a cell this project lacks needs that cell adopted
first. You may leave an offer for later; it stays in `adopt list`.
