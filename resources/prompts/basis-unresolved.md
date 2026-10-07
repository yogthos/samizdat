Not recorded: the basis names what the run's record does not have.
{% for r in refs %}- `{{r.ref}}`: {% if r.turn %}no such turn{% endif %}{% if r.artifact %}no such artifact{% endif %}{% if r.task %}no such task{% endif %}{% if r.memory %}no such memory{% endif %}{% if r.other %}not a reference (t12, B2:t7, a#3, s#2, sz-…, k-…){% endif %}
{% endfor %}Cite the turns, artifacts, tasks or memories the claim rests on — fetch_turn shows a turn — or leave out what you cannot point at.
