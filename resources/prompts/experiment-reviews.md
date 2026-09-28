## What the reviews said while your open changes were in force

The verdict on a change is one number. These are the reviews written since it
went in, which say what the work actually got wrong or right — read them
before deciding the number moved because of you.

{% for c in changes %}`{{c.name}}`:
{% for r in c.reviews %}- {{r.kind}}: {{r.verdict}}{% if r.findings %} — {{r.findings}}{% endif %}
{% endfor %}{% endfor %}