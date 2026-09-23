`{{args.command}}` was refused: it prints a whole file over the digest threshold (gates.edn :digest :min-lines) into your context at full price.

Two ways forward:
- read_digest({paths: [the file], question: "…"}) — ask what you need to know. A reader answers in bullets and the file stays out of your context. Add anchors: true when you mean to patch what it finds.
- read_file({path, outline: true}) — its definitions and the lines each spans, to find the section.
- read_file with an offset and limit, or `sed -n 'A,Bp'`, when you already know which section you need.
