## Gates that keep firing and are never met

Each of these fired in the number of distinct runs shown and its prediction was
never once met. A gate that fires and is ignored is not steering the loop — it
is spending a steer, a prompt's worth of context, and the branch's attention to
say something nobody acts on.

{% for g in gates %}- `{{g.gate}}` — fired {{g.fired}} times across {{g.runs}} runs, met 0, unmet {{g.unmet}}
{% endfor %}
If you agree it is dead, delete it: remove its entry from `gates.edn` and say
in the rationale what the evidence was. If you think its advice is right and it
is the WORDING that is being ignored, change the message instead and leave the
gate — those are different repairs and the record cannot tell them apart for
you.

One thing this list deliberately does not include: a gate whose predictions
settle late. That gate's advice is working and its window is too short, which
is a number to widen, not a gate to remove.

Left alone, these stay exactly as they are. Nothing here is retired for you.
