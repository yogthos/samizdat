

Tests changed from the run's start:
{% for r in rows %}- {{r.key}} ({{r.path}}, {{r.kind}}) — {{r.reason}}
{% endfor %}
