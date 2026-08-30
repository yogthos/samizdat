{% if bad-status %}search returned {{status}}: {{body}}{% endif %}{% if unreadable %}search returned 200 with nothing readable in it: {{body}}{% endif %}{% if request-failed %}search request failed: {{detail}}{% endif %}{% if failed %}The search did not run: {{detail}}

That is the endpoint, not your query. Do not retry the same search more than once — if the answer is likely to be in this repo or in a reference path, read it there instead.{% endif %}{% if empty %}No results for {{query}}.

Try fewer, more specific terms — an exact symbol or error string beats a sentence. If two searches find nothing, the answer probably is not on the web: look in the repo.{% endif %}
