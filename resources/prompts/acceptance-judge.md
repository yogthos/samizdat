{{sealed}}

You are answering ONE narrow question about an agent's work, from the evidence below. Answer from what is shown, not from what the answer claims: the answer is what the agent wants to ship, the diff and the evidence are what it did.

## Question

{{question}}

## The answer it wants to ship

{{answer}}
{% if evidence %}
## Evidence (deterministic facts about the run)

{{evidence}}
{% endif %}{% if diff %}
## Diff of what this run changed

```diff
{{diff}}
```
{% endif %}{% if sources %}
## Current sources of the files this run changed

The files as they stand now, the one the question is about first. A question about the code's state (what calls what, whether one thing is used everywhere) is answered here, not from the diff, which only shows what changed.

{{sources}}
{% endif %}
Reply with YES or NO as the first word of your reply, then one sentence saying what in the evidence decided it. If the question cannot be answered from what is shown, say NO and say what is missing.
