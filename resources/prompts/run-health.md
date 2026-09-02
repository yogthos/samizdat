{{heading}}

Implementors: {{shipped}}/{{total}} shipped. Outcomes: {{outcomes}}
Reviewer: {{reviewer}}   Critic: {{critic}}

Per branch (turns / mechanics-thrash / shipped? / fitness per turn, the number the cull reads):
{{per-branch}}
{% if failures %}
Failures this run, newest last. Start HERE: read the failure's own words,
then `fetch_turn({turn: N, branch: "B"})` for the full record, then fix the
cause at the surface that governs it — a parse failure lives in the prompt
or call format, a provider failure at the endpoint or the context budget, a
tool failure in the work or the tool.
{% if failures.parse %}
Calls that did not parse ({{failures.parse.count}} total):
{{failures.parse.lines}}
{% endif %}{% if failures.provider %}
Provider failures ({{failures.provider.count}} total):
{{failures.provider.lines}}
{% endif %}{% if failures.tool %}
Tool failures ({{failures.tool.count}} total):
{{failures.tool.lines}}
{% endif %}{% if failures.wins %}
And what WORKED ({{failures.wins.count}} successful calls this run) — read
these before concluding the loop is broken, and before writing a rule that
would have stopped them:
{{failures.wins.lines}}
{% endif %}{% endif %}
{% if gates %}Steering that is NOT working — a gate this branch has been told
by repeatedly and has not once done what it asked. Either the advice is wrong,
it is aimed at the wrong branch, or this model does not respond to advice at
all; all three are yours to fix, and more nagging is not the fix:
{{gates}}

{% endif %}Signals:
{% if signals %}{{signals}}{% else %}- none flagged; the loop looks healthy{% endif %}
