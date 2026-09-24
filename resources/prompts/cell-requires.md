{% for c in cells %}{% if c.missing-only %}Cell {{c.cell}} has no :requires. Declare the ctx keys its handler reads — `:requires {{c.requires}}` — even when that is none, so a manifest using it is checked against what its driver provides.
{% else %}Cell {{c.cell}} reads ctx keys its :requires does not declare: {{c.undeclared}}. Declare them — `:requires {{c.requires}}` — so a manifest using it is checked against what its driver provides, instead of the cell getting nil at run time.
{% endif %}{% endfor %}
