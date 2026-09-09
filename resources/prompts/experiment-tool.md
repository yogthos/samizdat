{% if usage %}`experiment` binds a change you are making to what you expect it to do, so the next round can tell you whether it worked: {name, change, hypothesis}. Start one whenever you edit a cell, manifest, prompt or threshold.
`verdict {name}` reads it back: better / worse / unchanged / too early, with the fitness before and after.
`verdict {name, action, why}` settles it once you have acted — action is `reverted` or `kept`. A losing change you have not settled keeps being raised, because a modification the evidence says is not helping, left in place because nobody got back to it, is the thing this measurement exists to prevent.{% endif %}{% if started %}Experiment `{{name}}` started. Measuring from here.

  changed:  {{change}}
  expected: {{hypothesis}}

You will see the verdict on your next turn. Change nothing else until then — a second change measured alongside this one tells you nothing about either.{% endif %}{% if settled %}`{{name}}` settled as {{action}}.{% if why %} Reason: {{why}}{% endif %}

It will stop being raised. `remember` what it taught you if you have not already — a lever that does not move this problem is worth knowing next run, and this session's record dies with the process.{% endif %}{% if too-many %}Refused: `{{open}}` change is already in flight and unsettled, and the cap is `{{cap}}`.

Two changes measured over the same interval tell you nothing about either, so this is enforced rather than trusted to discipline. Settle what is open first — `verdict {name}` to read it, then `verdict {name, action: reverted|kept, why}` — and the slot frees.{% if unsettled %}

Waiting on: {{unsettled}}{% endif %}{% endif %}{% if no-experiment %}No experiment named `{{name}}`. `experiment` starts one.{% endif %}{% if reported %}`{{name}}` — {{verdict}}{% if numbers %} ({{numbers}}){% endif %}

  changed:  {{change}}
  expected: {{hypothesis}}
{% if better %}
It earned its place. Leave it, and `remember` what it fixed so the next run starts from it.{% endif %}{% if worse %}
Revert it. That is the experiment working, not a mistake — and `remember` that this lever made things worse, so nobody spends a round trying it again.{% endif %}{% if unchanged %}
The change was not the fix. Revert it rather than leaving a change nobody can justify, and look somewhere else.{% endif %}{% if too-early %}
Not enough has happened to tell yet. Wait — do not stack another change on top of an unmeasured one.{% endif %}{% if confounded %}
Not attributable. Discounting the provider's own failures, the same two stretches read {{confounded.verdict}} ({{confounded.numbers}}) — so the direction above belongs to the endpoint as much as to your change. From inside one run, a change that stopped empty replies and an endpoint that simply came back look identical, and no number here separates them. Decide on the reasoning, say which you believe, and settle it — `verdict {name, action: reverted|kept, why}`. It holds the slot until you do.{% endif %}{% if regraded %}
Scored under weights this run changed: the before side was stamped at {{regraded}} and both numbers above are on the scale in force now. They subtract honestly, but it is a scale you chose while being measured by it. Say what was wrong with the old one.{% endif %}{% if branches %}
{{branches.regressed}} of {{branches.measured}} measured branches went backwards under this change. The number above is the whole run and cannot say that either way — a change that lifted the run while breaking a branch is a different decision from one that lifted both.{% endif %}{% endif %}
