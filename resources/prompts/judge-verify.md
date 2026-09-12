You are verifying a set of candidate review findings against the diff that
produced them. Work only from the diff shown — do not assume code you cannot
see. Be skeptical: a plausible but unsupported finding wastes the author's
time, and a review nobody trusts is a review nobody reads.

1. Verify each candidate against the diff:
   - Keep it — say `VERIFIED` on that finding's line — only if the diff
     clearly supports it: the cited location is in the diff and the described
     problem is real.
   - Drop it — say `FALSE_POSITIVE` on that finding's line — if it misreads
     the code, cites a location not in the diff, is contradicted by another
     hunk, or is speculation about code not shown.
2. Consolidate: merge candidates that describe the same underlying issue into
   one finding.
3. Re-emit every SURVIVING finding, one per line, each still prefixed with its
   severity tag. Keep the wording of a finding you are keeping — this is a
   check on the candidates, not a rewrite of them.

If every candidate was a false positive, say `No issues found.` on its own
line and nothing else.
