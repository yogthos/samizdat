{% if usage %}`experiment` binds a change you are making to what you expect it to do, so the next round can tell you whether it worked: {name, change, hypothesis, kind?, target?, predicts?}. Start one whenever you edit a cell, manifest, prompt or threshold. `kind` and `target` name what you edited (cell / manifest / prompt / policy and its name), and the ledger records the version. `predicts` is the one signal you expect to move and which way, e.g. "parse-error down" or "gate-unmet down"; the verdict says whether it did.
`verdict {name}` reads it back: better / worse / unchanged / costly / cheaper / below best / too early, with the fitness and the tokens per turn before and after.
`verdict {name, action, why}` settles it once you have acted — action is `reverted` or `kept`. A losing change you have not settled keeps being raised, because a modification the evidence says is not helping, left in place because nobody got back to it, is the thing this measurement exists to prevent.{% endif %}{% if started %}Experiment `{{name}}` started. Measuring from here.

  changed:  {{change}}
  expected: {{hypothesis}}{% if target %}
  edit:     {{target}}{% endif %}{% if predicts %}
  predicts: {{predicts}}{% endif %}

You will see the verdict on your next turn. Change nothing else until then — a second change measured alongside this one tells you nothing about either.{% endif %}{% if settled %}`{{name}}` settled as {{action}}.{% if why %} Reason: {{why}}{% endif %}

It will stop being raised. `remember` what it taught you if you have not already — a lever that does not move this problem is worth knowing next run, and this session's record dies with the process.{% endif %}{% if too-many %}Refused: `{{open}}` change is already in flight and unsettled, and the cap is `{{cap}}`.

Two changes measured over the same interval tell you nothing about either, so this is enforced rather than trusted to discipline. Settle what is open first — `verdict {name}` to read it, then `verdict {name, action: reverted|kept, why}` — and the slot frees.{% if unsettled %}

Waiting on: {{unsettled}}{% endif %}{% endif %}{% if bad-prediction %}`predicts` is one signal and a direction, e.g. "parse-error down". The signals: {{signals}}.{% endif %}{% if no-experiment %}No experiment named `{{name}}`. `experiment` starts one.{% endif %}{% if reported %}`{{name}}` — {{verdict}}{% if numbers %} ({{numbers}}){% endif %}

  changed:  {{change}}
  expected: {{hypothesis}}{% if prediction %}
  predicted {{prediction.said}}: {{prediction.moved}} — {% if prediction.hit %}it went that way{% else %}it did not{% endif %}{% endif %}
{% if better %}
It earned its place. Leave it, and `remember` what it fixed so the next run starts from it.{% endif %}{% if worse %}
Revert it. That is the experiment working, not a mistake — and `remember` that this lever made things worse, so nobody spends a round trying it again.{% endif %}{% if costly %}
It gained, but it added more tokens per turn than that gain pays for. Revert it, or say why the gain is worth the price — a harness that grows more expensive for small wins ends up slow for nothing.{% endif %}{% if below-best %}
It moved up from the stretch just before it, but it is still below the {{below-best}} a change you kept already reached. Beating the previous step while sitting under the best one is how a run walks downhill a small step at a time; revert it.{% endif %}{% if cheaper %}
Fitness did not move measurably, and the turns got clearly cheaper. That is worth keeping; say so and leave it.{% endif %}{% if unchanged %}
The change was not the fix. Revert it rather than leaving a change nobody can justify, and look somewhere else.{% endif %}{% if too-early %}
Not enough has happened to tell yet. Wait — do not stack another change on top of an unmeasured one.{% endif %}{% if confounded %}
Not attributable. Discounting the provider's own failures, the same two stretches read {{confounded.verdict}} ({{confounded.numbers}}) — so the direction above belongs to the endpoint as much as to your change. From inside one run, a change that stopped empty replies and an endpoint that simply came back look identical, and no number here separates them. Decide on the reasoning, say which you believe, and settle it — `verdict {name, action: reverted|kept, why}`. It holds the slot until you do.{% endif %}{% if regraded %}
Scored under weights this run changed: the before side was stamped at {{regraded}} and both numbers above are on the scale in force now. They subtract honestly, but it is a scale you chose while being measured by it. Say what was wrong with the old one.{% endif %}{% if branches %}
{{branches.regressed}} of {{branches.measured}} measured branches went backwards under this change. The number above is the whole run and cannot say that either way — a change that lifted the run while breaking a branch is a different decision from one that lifted both.{% endif %}{% endif %}
