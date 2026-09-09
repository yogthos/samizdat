## This session so far ({{turns}} turns){% if fitness %} — fitness {{fitness}}/turn{% endif %}
{% if tools %}Tools: {{tools}}
{% endif %}{% if signals %}Call mechanics: {{signals}}
{% endif %}{% if verify %}Ship verification: {{verify}}
{% endif %}{% if gates %}Gates: {{gates}}
{% endif %}{% if since %}
### Since your last look ({{since.turns}} turns){% if since.fitness %} — fitness {{since.fitness}}/turn{% endif %}
This is the interval your last change was in force for, and the only thing here that answers whether it helped. The tally above is the whole session and dilutes it.
{% if since.tools %}Tools: {{since.tools}}
{% endif %}{% if since.signals %}Call mechanics: {{since.signals}}
{% endif %}{% if since.verify %}Ship verification: {{since.verify}}
{% endif %}{% if since.gates %}Gates: {{since.gates}}
{% endif %}{% endif %}{% if quiet %}
### Since your last look
Nothing has happened since you looked: no turn has been taken. Whatever you changed has not been exercised yet, so there is nothing to judge it on — wait rather than changing something else.
{% endif %}{% if experiments %}
### Changes you have made, and what happened
{{experiments}}

{% if unsettled %}**{{unsettled}} change(s) above measured worse or unchanged and you have not acted on them.** Do that first, before anything else: revert them, or say why you are keeping one. A modification the evidence says is not helping, left in place because nobody got back to it, is exactly what this measurement exists to prevent — and the next supervisor inherits it with no sign it was ever questioned.

{% endif %}A change with verdict `better` earned its place — say so and leave it. `worse` means REVERT it; that is not a failure, it is the experiment working. `unchanged` means the change was not the fix, so revert it too rather than leaving a change nobody can justify. `too early` means wait — do not stack another change on an unmeasured one. `confounded` means the number moved but not because of you: the world moved under the measurement, so decide on the reasoning and say that is what you did. It still holds the slot until you settle it, the same as any other change in flight.
{% endif %}{% if findings %}
### Worth your attention
{{findings}}
{% endif %}