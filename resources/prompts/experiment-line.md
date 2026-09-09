- {{name}} [{{verdict}}]{% if reverted %} (reverted){% endif %}{% if kept %} (kept deliberately){% endif %}{% if before %} fitness {{before}} -> {{after}} over {{turns}} turn{% if many-turns %}s{% endif %}{% endif %}
    changed: {{change}}
    expected: {{hypothesis}}
{% if blind %}    NOT ATTRIBUTABLE. Discounting the provider's own failures, the same two stretches read {{blind.verdict}} ({{blind.before}} -> {{blind.after}}). From inside one process, a change that stopped empty replies and an endpoint that simply came back look identical, so no number here settles this one. Say which you believe and why, then revert or keep it deliberately — leaving it unsettled blocks the next change.
{% endif %}{% if regraded %}    SCORED UNDER WEIGHTS THIS RUN CHANGED. The before side was stamped at {{regraded.stamped}} and is {{regraded.now}} under the weights in force now; both numbers above are on the current scale, so they subtract honestly, but the scale is one this run chose. Say what was wrong with the old one.
{% endif %}{% if branches %}    {{branches.regressed}} of {{branches.measured}} measured branches went backwards. One aggregate cannot say that either way, and a change that lifted the session while breaking a branch is a different decision from one that lifted both.
{% endif %}
