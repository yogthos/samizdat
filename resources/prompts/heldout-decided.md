{% if accepted %}Your edit to {{kind}} `{{name}}` passed the held-out battery{% if total %} ({{passed}} of {{total}} targets, nothing that passed before fails){% endif %} and is now live as v{{version}}.{% endif %}{% if refused %}Your edit to {{kind}} `{{name}}` was NOT saved — the held-out battery refused it.

{{refusal}}{% endif %}{% if save-failed %}Your edit to {{kind}} `{{name}}` passed the held-out battery but did not go live: {{reason}}. What runs is unchanged.{% endif %}
