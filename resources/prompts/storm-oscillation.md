You are alternating between the same two calls: this exact `{{tool-name}}`
call would extend an A-B-A-B cycle the harness has watched repeat. Neither
call is converging, and the pair together is a loop — each one appears to
undo or re-require the other.

Stop the cycle. State in one sentence what each of the two calls was supposed
to achieve, and why doing one keeps sending you back to the other. Then take
ONE different action that breaks the dependency — a smaller edit, a different
file, a different check, or a re-read of the requirement both calls are
serving. If the two steps genuinely conflict, that conflict is the finding:
record it, or give up honestly with `give_up` and say what you learned.
