`read_file` on {{args.path}} was refused: the file is over the digest threshold (gates.edn :digest :min-lines) and the call named no offset or limit, so it would page the whole file through your context at full price.

Two ways forward:
- read_digest({paths: ["{{args.path}}"], question: "…"}) — ask what you need to know. A reader answers in bullets and the file stays out of your context; asking again is free for you. Add anchors: true when you mean to patch what it finds.
- read_file({path: "{{args.path}}", offset, limit}) — when you already know which section you need.
