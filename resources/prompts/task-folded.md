Task {{id}}{% if title %} — {{title}}{% endif %} closed as {{status}}. Its work{% if from %} (turns {{from}}–{{to}}){% endif %} is folded to this line; fetch_turn reopens any of those turns if you need the detail.{% if kept %}

What the person answered during that work still stands — build on it, do not ask again:
{% for k in kept %}
{{k}}
{% endfor %}{% endif %}
