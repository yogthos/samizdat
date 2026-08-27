{{heading}}

Implementors: {{shipped}}/{{total}} shipped. Outcomes: {{outcomes}}
Reviewer: {{reviewer}}   Critic: {{critic}}

Per branch (turns / mechanics-thrash / shipped?):
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
{% endif %}{% endif %}
Signals:
{% if signals %}{{signals}}{% else %}- none flagged; the loop looks healthy{% endif %}
