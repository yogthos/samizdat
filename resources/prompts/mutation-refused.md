## Changes you already tried that were REFUSED

The mutation protocol rejected each of these. It did not reach the running
harness, and nothing was charged to a branch for it.

{% for a in attempts %}- {{a.reason}}{% if a.targets %} (target: {{a.targets}}){% endif %}
{% endfor %}
Read these as ruled out, not as bad luck. A refusal is the protocol telling you
the edit was malformed, unreachable, non-terminating or unsafe to stub — so
proposing the same shape again spends a pass to be told the same thing. If you
still believe the change is right, change its SHAPE: a different seam, a
smaller edit, or the threshold rather than the cell.

If the refusal itself looks wrong — the edit was sound and the check
misjudged it — say so plainly in your answer rather than working around it.
A validator that rejects good edits is a harness defect, and it is exactly the
kind of thing you are here to find.
