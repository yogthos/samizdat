Changed forms by the calls between them:
{% for g in groups %}{{forloop.counter}}. Around {{g.hub}}: {{g.count}} forms that call one another, in {{g.files}} file(s).
{% for m in g.members %}   {{m}}
{% endfor %}{% endfor %}{% if shared %}Shared by several groups (used from three or more other files, so they do not join their users into one group):
{% for m in shared %}   {{m}}
{% endfor %}{% endif %}{% if singles %}Calling no other changed form:
{% for m in singles %}   {{m}}
{% endfor %}{% endif %}
