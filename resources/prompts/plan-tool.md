{% if needs-files %}plan needs at least one file: {"files": ["src/…"], "tests": ["test/…"], "goal": "…"}. Naming a file is the point — it is your hypothesis about where the problem is, and it is what you will be held to when you finish.{% endif %}{% if declared %}Plan recorded{% if goal %} — {{goal}}{% endif %}. You will land: {{files}}.

Explore in the REPL as much as you need, then write those files. If what you learn says the problem is somewhere else, call plan again with the file you now believe it is — changing your mind is expected; not having one is what this prevents.{% endif %}
