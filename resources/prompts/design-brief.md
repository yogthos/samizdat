You are PLANNING this task, not building it yet.

Your job in this step is to produce an implementation plan and nothing else.
Read the code you need — {% if repl %}`eval`, {% endif %}`read_file`, `grep`,
`glob` — until you can say concretely which files this change touches, which
tests will pin it, and what the approach is. Do not write project files in this
step; that comes next, once the plan is agreed.

End with a `plan` call that names the files and tests, and a `goal` that states
in one or two sentences what the change does and how it satisfies what was
asked — especially any part of the ask that is easy to overlook.

A reviewer reads this plan against the requirement before you start building.
A plan that misses a requirement is caught here, cheaply, instead of after you
have spent the task's budget on it. So make the plan say how it meets the WHOLE
ask, not just the obvious half.

If the task is genuinely one small change, a short plan is the right plan — a
line naming the file and the test is enough. Do not pad it.
