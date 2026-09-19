## What the last run learned about this project
{% if memories %}
Run {{run}} finished before you started and left these behind. They are what
the harness derived from what that run actually did — commands that worked,
commands the policy refused, patterns it measured — not somebody's summary.

{{memories}}

Treat them as a head start, not as gospel: a fact recorded once can be wrong,
and one that contradicts what you observe is worth saying so about rather than
working around. If you confirm one, `recall` it so the record shows it held.
{% endif %}{% if errors %}
Part of what run {{run}} learned was LOST: the harness threw while writing it
to memory, so the store is missing what that run would have told you. This is
not a clean slate — treat what you find in memory as incomplete, and orient
from the project rather than from its absence.
{% for e in errors %}- the {{e.half}} half: {{e.error}}
{% endfor %}{% endif %}
