The tests are green, but you have not implemented everything this piece owes.
Still stubs:

{% for n in unfilled %}- `{{n}}`
{% endfor %}
{% if file %}They live in `{{file}}`.
{% endif %}
This piece is not delivered until they have real bodies. The code that calls
them was written by whoever handed you this task, and it calls them by name —
a test that passes around a stub, or a stub deleted rather than filled, leaves
that caller broken.

Implement what is listed, keep the tests green, then call done.
