This change alters tests that were there when the run started, and the answer does not say why. A test is the definition of done that existed before this work; changing it to get green and changing it because it was wrong look the same in a diff, and only you can say which it was.

{% for t in missing %}- **{{t.key}}** ({{t.path}}) — {{t.kind}}
{% endfor %}
If a change was not meant, put the test back. Otherwise call `done` again with a `changed_tests` entry for each: `"changed_tests": [{"test": "{{missing.0.key}}", "reason": "what was wrong with the old test, or where it went, and what now pins the behaviour"}, …]`.
