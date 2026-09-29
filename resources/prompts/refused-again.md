`{{head}}` has now been refused {{count}} times in this run ({{total}} refused commands in all). Rewording the command does not change the answer: the policy judges `{{head}}` itself, and in a run nobody is watching, a command that needs approval will not get it.

Stop reaching for `{{head}}`. Take a route that does not need it — a command that has already run in this run, a tool made for the job (`write_file`/`edit_file`/`patch` to change files, `read_file` to read them, `eval` to run code), or leave out the step if it was only tidying up. If the step truly cannot be done without it, say so in your answer and carry on with the rest.{% if goal %}

You are working on: {{goal}}{% endif %}
