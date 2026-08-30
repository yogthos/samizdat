{% if needs-kind %}`intervene` needs a `kind` — what you want done to the run.

{% for k in kinds %}- `{{k.0}}` — {{k.1}}
{% endfor %}
Call it as `intervene({"kind": "message", "branch": "T0", "text": "..."})`.
{% endif %}{% if unknown %}`{{unknown}}` is not a directive this harness has. The ones it does:

{% for k in kinds %}- `{{k.0}}` — {{k.1}}
{% endfor %}
Pick the closest and call again. To tell a branch something, that is
`message`.
{% endif %}{% if submitted %}Directive queued: **{{kind}}**{% if target %} against `{{target}}`{% endif %}.

It is applied at the next round boundary, not this instant — a directive that
landed mid-turn would rewrite state under a branch that had already read it.
The branch sees it at the top of its next turn, above every machine gate.

{% if text %}What it will read: {{text}}

{% endif %}You are steering a run that is still going. Watch what it does with
this before sending another: a second directive on top of an unread first one
is noise, and a branch that gets a new instruction every turn never finishes
any of them.
{% endif %}{% if refused %}The harness declined that directive: {{refused}}

Your call was well formed — this is a policy refusal, not a mistake to repair.
`cull` is refused for the last running branch, because a run with no branches
cannot recover. If you want the run to end, let it end on its own terms or
raise the problem rather than removing the only worker.
{% endif %}
