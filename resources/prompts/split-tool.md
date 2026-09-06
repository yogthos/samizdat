{% if no-parts %}`split` needs `parts`: the pieces this task breaks into, 2 to {{max-parts}} of them.

Before you call it, write the work down in the tree:

1. Write the STUBS each piece must fill — the real `defn`, its argument
   vector, and a docstring saying what it owes. Give it a body that only
   throws, so it is unmistakably unfinished.
2. Write your own code that CALLS those stubs. That is your job, and it is
   what makes the pieces fit together.
3. Sketch the tests that will pin each piece.

Then declare the split:

```tool-call
{"name": "split", "args": {
  "reason": "one sentence: why this is more than one thing",
  "parts": [{"name": "a-short-kebab-name",
             "description": "what this piece must do",
             "file": "src/example/core.clj",
             "stubs": ["parse-line"],
             "tests": "test/example/core_test.clj"}]}}
```
{% endif %}{% if problems %}The split was not accepted, and nothing was created. Fix these and call `split` again:

{% for p in problems %}- {% if p.too-few %}Only {{p.detail}} piece(s). A split is 2 or more — if this is one thing, implement it instead.
{% endif %}{% if p.too-many %}{{p.detail}} pieces is more than the {{max-parts}} allowed. Group them.
{% endif %}{% if p.no-description %}`{{p.part}}`: no `description`.
{% endif %}{% if p.no-file %}`{{p.part}}`: no `file` — say which file its stubs live in.
{% endif %}{% if p.no-stubs %}`{{p.part}}`: no `stubs` — name the functions this piece must fill.
{% endif %}{% if p.no-tests %}`{{p.part}}`: no `tests` — name the test file that pins it.
{% endif %}{% if p.tests-absent %}`{{p.part}}`: the test file `{{p.detail}}` is not in the tree. Sketch the tests first, then split.
{% endif %}{% if p.file-absent %}`{{p.part}}`: the file `{{p.detail}}` is not in the tree.
{% endif %}{% if p.stub-absent %}`{{p.part}}`: nothing defines `{{p.detail}}` yet. Write the stub before you delegate it — the piece inherits what you wrote, and your own code calls that name.
{% endif %}{% if p.stub-already-filled %}`{{p.part}}`: `{{p.detail}}` is already implemented. A piece whose contract is already met has nothing to do; keep that one yourself.
{% endif %}{% if p.stub-owned-twice %}`{{p.part}}`: `{{p.detail}}` is already owned by another piece. Two owners on one function is two agents writing the same code.
{% endif %}{% endfor %}
{% endif %}{% if created %}Split into {{created|length}} pieces, each now an open task under {{parent}}:

{% for c in created %}- {{c.id}} `{{c.name}}` — fills {{c.stubs}}
{% endfor %}
The suite is red until they land, which is expected: the stubs are the failing
tests. Each piece is done when its stubs are implemented and its own tests
pass. When they come back, your job is to make YOUR task pass — and you may
adjust what they delivered if the pieces do not fit together the way you
planned.
{% endif %}
