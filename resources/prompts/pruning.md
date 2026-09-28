## Edits of this project's workflow that have not earned their place

Each is the version currently in force, and every run that ended while it was
current ended without shipping. A change that stops producing a gain is not
neutral: it costs context, turns and attention, and nothing makes it leave on
its own.

{% for e in edits %}- {{e.kind}} `{{e.name}}` v{{e.version}} — {{e.failed}} failed runs, none shipped{% if e.rationale %}; made because: "{{e.rationale}}"{% endif %}
{% endfor %}
If the reason it was made no longer holds, revert it and say what the record
showed. If it is guarding something the failures are not about, keep it and
say so — the record cannot tell those apart for you. Crashed runs are not
counted here; they are the harness's, not the edit's.
