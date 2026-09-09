{% if needs-questions %}
`ask_human` needs `questions`: a list, each entry with a `question` and
optionally `options` for the person to pick from.

    ask_human({"questions": [{"question": "which store?",
                              "options": ["sqlite", "postgres"]}]})
{% endif %}{% if nobody-configured %}
There is no human attached to this run, so `ask_human` cannot be answered.

This is the normal state: runs here are autonomous, and nobody is waiting at
a terminal to answer. Decide it yourself and say in your next message which
way you went and why — a stated assumption someone can correct later is worth
more than a question nobody will read.

If the choice genuinely cannot be made without a person, record it as a task
and carry on with the part that does not depend on it.
{% endif %}{% if unanswered %}
Nobody answered within {{seconds}} seconds, so the question has expired.

Do not ask again — the same silence will cost you another {{seconds}} seconds.
Decide it yourself, say which way you went and why, and continue.
{% endif %}
