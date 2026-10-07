## What to check

{% for d in dimensions %}- {{d.question}}{% if d.mandatory %} (mandatory: a failure is a [high] finding){% endif %}
{% for e in d.examples %}  {% if e.fail %}A failure looks like{% else %}A pass looks like{% endif %}: {{e.snippet}} ({{e.rationale}})
{% endfor %}{% endfor %}
