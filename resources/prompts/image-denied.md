The sandbox refused that, so it never ran. **Your code is not the problem** —
nothing here says the form is wrong.

The project REPL is confined to `{{root}}`. Inside it you can read and write the
project's own files, require and exercise its namespaces, and reach its database
and its assets. It cannot start processes, and it cannot read or write outside
the project.

{% if exec %}That call tried to start a process. `shell` is the tool for that —
it runs outside the REPL, under the command policy, and it is how you run the
project's tests.{% else %}That call tried to reach a path outside the project.
Use a path inside `{{root}}`, or `shell` if you genuinely need to reach further
and the policy allows it.{% endif %}

Refused: `{{detail}}`
