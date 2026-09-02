{% if denied %}Command denied by policy: `{{head}}` is on the deny list. This cannot be overridden.{% else %}{% if protected %}Command denied: it could modify `{{path}}` — the operator's run config, which defines the verify gate this run ships against. The run a gate judges cannot rewrite the gate, and no grant unlocks this. Reading it is fine (`cat`, `grep`). If the gate itself is wrong, say so in your answer and let the operator change it.{% else %}Command needs approval: `{{command}}`.
{% if malformed %}
The shell would not parse it: an unclosed quote or a dangling escape, at
character {{malformed}}. Fix the quoting and send it again.
{% else %}{% if blocked %}
The parts of a compound command are judged one at a time, and every part but
this one is fine:

    {{blocked}}

`{{blockedhead}}` is not on the allow list. Reissue the command without that
part, or use a tool that does the same job: `read_file` and `grep` to look
around, `eval` to run Clojure.
{% else %}{% if promoted %}
`{{head}}` on its own IS allowed. This was refused because the command
{{markers}} — the shell would build or run something no rule ever saw, so it
is judged as one whole claim rather than by its first word (`echo $(rm -rf ~)`
must not ride `echo`). That rule is not going to be relaxed.

Issue the parts as separate calls. A plain list of allowed commands — joined
with `;`, `&&`, `||` or `|` — is allowed as it stands, so what needs splitting
out is the substitution or the redirection.
{% else %}{% if complex %}
This is a COMPOUND command — it contains {{markers}} — so it is judged as one
whole claim rather than by its first word, and `{{head}}` is not allowed on its
own either. Split it up and check the parts.
{% else %}
`{{head}}` is not on the allow list, so it needs a human to grant it — and if
this run has no human watching, it will not be granted. Prefer a tool that does
the same job without the shell: `read_file` and `grep` to look around, `eval`
to run Clojure, including this project's own tests once you have required the
namespace.
{% endif %}{% endif %}{% endif %}{% endif %}{% endif %}{% endif %}

Rule: `{{rule}}`
