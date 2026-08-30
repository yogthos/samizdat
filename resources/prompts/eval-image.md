eval runs in: {{image}}{% if harness %}  (the LIVE harness process — in-process, unconfined){% endif %}{% if project %}  (a sandboxed jolt subprocess rooted at {{root}}){% endif %}{% if off %}  (the REPL is switched off for this harness){% endif %}
role:         {{role}}
{% if project %}sandbox:      {{backend}}
it may:       read and write {{root}}, require and exercise its namespaces
it may not:   start processes, or read or write outside that tree{% endif %}
