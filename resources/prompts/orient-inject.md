## Named in the task, found in the tree

The task names these, and this is where they are. A snippet is where to look, not what is there now — read the file before you change it.
{% for d in definitions %}
### `{{d.name}}` — {{d.path}}:{{d.line}}

```
{{d.snippet}}
```
{% endfor %}
