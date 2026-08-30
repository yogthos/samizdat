## What was asked

{{requirement}}

## Evidence (deterministic facts about the run)

{{evidence}}
{% if diff %}
## Diff of what this run changed

```diff
{{diff}}
```
{% endif %}{% if transcript %}
## Transcript

{{transcript}}
{% endif %}
## The answer it wants to ship

{{answer}}

Judge the DIFF against WHAT WAS ASKED. Is this complete and correct? {{preamble}}
