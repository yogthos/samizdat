

Checklist:
{% for r in rows %}- {{r.id}} [{{r.label}}] {{r.text}} — {{r.evidence}}
{% endfor %}
