This answer does not account for every item on the checklist. The checklist is what this work was asked to deliver, fixed before it started, and an item the answer is silent on reads as done when nobody said so. Account for each one below by its id:

{% for i in missing %}- **{{i.id}}** {{i.text}}
{% endfor %}
Call `done` again with your answer and a `checklist` entry for every item: `"checklist": [{"item": "{{missing.0.id}}", "status": "met", "evidence": "what shows it: the test, the command and what it printed"}, …]`. The status is `met`, `not_met` or `n/a`. An item you could not finish is `not_met` with the reason — that ships, and it is the honest answer. Leaving it out does not.
