TASK:
{{problem}}

WORKFLOWS:
{% for w in workflows %}{{w.name}} — {{w.description}}
{% endfor %}{% if history %}
HOW THESE HAVE GONE ON THIS PROJECT BEFORE:
{% for h in history %}{{h}}
{% endfor %}
A workflow that keeps failing here is evidence against picking it again, however
well its description fits. A workflow with no line above has not been tried.
{% endif %}
Answer with the three lines: kind, size, and one workflow name from the list above.
