{{complaint}}
{% if instead %}
The shape that does what you meant: `{{instead}}`.{% endif %}

Dispatch entries are patterns over the data map, tried in order — first match wins, so put the specific pattern before the general one. A map matches any data map carrying those keys with those values; other keys are ignored, and a mentioned key must be present (`{:k nil}` needs `:k` there). A vector is closed and positional. `?x` binds a value and means the same value wherever it repeats. `_` matches anything. Sets and lists are literals. A guard is a third element, `[:big {:turn ?t} [:> ?t 10]]`, and `manual samizdat.symbolic/guard-catalog` lists every guard there is. A `(fn [d] ...)` form is still accepted where a pattern cannot say it.
