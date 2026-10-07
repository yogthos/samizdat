These RFC reviews passed only because some criteria were never decided; had those gone against them, they would not have passed. The work shipped, but nobody has shown it meets these criteria:
{% for r in reviews %}- task {{r.task}}: reward {{r.reward}}, undecided: {{r.undecided}}
{% endfor %}Check them yourself (fetch_turn, the diff), or ask why the judge could not answer: an RFC criterion a judge cannot decide is usually one worded too broadly.
