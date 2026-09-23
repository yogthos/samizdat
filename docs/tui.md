# The terminal UI

A full-screen terminal front end for a running harness. It shows what the
agent is doing, lets a person answer the questions a branch is parked on, and
takes an intervention when the run needs steering — laid out after dirge's.

It is a strict HTTP client, like the GUI: this process holds no engines, no
database handle and no run state of its own. Everything on screen arrived
through `samizdat.api.client`, and everything a key or a click does goes back
as a POST. That is why it can be pointed at a harness on another machine, and
why closing it does nothing to the run.

## Running it

```bash
jolt serve                      # the harness, in another terminal
jolt tui                        # the UI, against http://127.0.0.1:3985
jolt -M:tui http://host:3985    # or somewhere else
```

Where to connect, in order: an argument, then `SAMIZDAT_URL`, then
`HARNESS_PORT` (the port alone, when only the port moved), then the default
`http://127.0.0.1:3985` — the same port `jolt serve` binds, so the common case
needs no configuration at all.

The `:tui` alias carries `ftxui-jolt` as a `:local/root` dep, and that library
ships a C++ shim that has to be **compiled** (`jolt native` in its checkout,
needing cmake and a C++17 compiler). This is also why the TUI is not part of
`jolt test`: see [Tests](#tests).

## Driving it

| | |
|---|---|
| type + `Enter` | **with no run selected**, start a run on what you typed; **with one**, send it as a directive |
| `/` + a command | a slash command — see below; `Tab` completes the name, `/help` lists them |
| `Ctrl-P` / `Ctrl-N` | walk back and forth through what you sent |
| `PgUp` / `PgDn`, mouse wheel | scroll the conversation; it stops following the bottom |
| `End`, or `↓` while scrolled up | back to following the bottom |
| `Ctrl-O` | open the newest folded result or thinking; again to shut it |
| click a fold | open a thinking block or the rest of a result |
| `y` `a` `n` `d` `Esc` | the permission dialog: allow once, allow always (this session), deny, deny with a note, abort |
| `Esc` | over a questionnaire: reject it |
| `F5` | fetch everything now |
| `Ctrl-C`, `Ctrl-Q`, `/quit` | quit |
| arrows + `Enter` | pick a run in RUNS, a branch in BEAM, an option in a questionnaire |

The mouse is on, and folds are ftxui's own collapsibles — clicking one is how
it opens, with no hit-testing of our own.

The dialog's letter keys are consumed **only** while a permission dialog is up
and **only** with nothing typed: a `y` in the middle of a directive is a
letter, not a verdict (`samizdat.tui.state/dialog-action` decides). Everything
else belongs to whichever widget has the focus.

### Slash commands

Typed into the compose box. They are data — `tui.edn :commands` names each
one, what it does, its arguments and its help line, and `{:alias "/model"}`
makes a second name — so a command can be renamed or aliased without a
rebuild. What they print lands in the conversation as `<sys>`.

| | |
|---|---|
| `/model [id]` | list the provider's models, or switch — **live** for the run on screen (every branch's next request), otherwise for the next run started from here |
| `/effort <level>` | how hard the model thinks, the same way |
| `/mode [refuse\|block]` | what happens when a run needs you, for this server session: refuse, or block and ask. The footer shows it |
| `/run <problem>` `/abort` `/resume` | start a run; abort or resume the one on screen |
| `/runs [id]` `/branch <id>` | list the runs or open one by the start of its id; read another branch |
| `/steer` `/review` `/cull` `/fork` `/extend` `/pause` `/continue` `/switch` `/budget` `/stop` | the directive kinds the server takes, sent to the run on screen |
| `/follow` `/clear` `/help` `/quit` | |

A live `/model` or `/effort` is the `model` / `effort` intervention kind:
applied on arrival rather than queued (samizdat.agent.live), and noted in
the run's journal as `:llm-switch`, which the conversation shows.

### The dialogs

Both sit under the conversation, where the run is parked on them.

**Permission**: the tool, the whole command (never clipped — a person cannot
judge `rm -rf "$BUILD"/*` from its first characters), why it was stopped, and
`allow once (y)`, `allow always (a)`, `deny (n)`, `deny + note (d)`. *Allow
always* is offered only when the command has a pattern to allow (`cargo *`;
never a compound command), says which, and holds for **this session only**:
kept in the server's memory for that run, never written to the project, gone
with the process. *Deny + note* hands the compose box to the dialog; what you
type there is what the agent is told to do instead.

**Questionnaire** (`ask_human`): one question at a time — a menu, or
checkboxes and `confirm` when the question takes several — plus `your own
answer`, which hands the compose box over the same way, and `reject (Esc)`,
which the branch reads as *declined*, not as nobody being there.

### The compose box does two things

One box, and whether there is a run selected decides what Enter means.

**With no run selected** the words are a **problem statement**, and Enter
starts a run on them. Nothing else on screen can give a fresh harness its
first problem — before this the box answered "no run selected" and dropped
what you had typed.

**With a run selected** the words are an **intervention** for it, not a chat
message. Samizdat runs autonomously and a person steers it at a turn
boundary, so that is the same seam the supervisor uses.

The `start` button does it either way, which is how you start a second run
while reading the first. Only the problem statement is sent — the turn budget,
the beam width and the model are left to the server's own config, which is
what an omitted key means; the GUI has a form for those because it has room
for one. The request runs off the UI thread, because `POST /v1/runs` does not
answer until the beam has opened every branch, which can take tens of seconds.
The status line says `starting…` while it does, in cyan: that is a notice, not
an error, and it travels in its own field so the strip never claims both at
once.

`abort` and `resume` with no run selected say "no run selected" rather than
doing nothing silently — a button that quietly no-ops is indistinguishable
from one that is not wired up, which is exactly how it got reported.

## What is on screen

After dirge's: a top frame naming the three columns; the run's vitals on the
left; the conversation in the middle, where the room is; the work in
progress on the right; the avatar beside the compose box; the status line
under everything.

```
──[RUN STATUS]────────[AGENT LOG]───────────────────────────────[HARNESS]────
  S A M I Z D A T   ╭ BRANCH B1 ─────────────────────────────╮╭ RUNS ───────╮
╭ CONTEXT ────────╮ │<you> fix the parser                    ││ TASKS       │
│ tokens / turns  │ │<agent> Reading the lexer first.        ││             │
╰─────────────────╯ │▶ ◇ thinking (812 chars)                ││ MODIFIED    │
╭ ACTIVITY ───────╮ │╭──────────────────────────────────────╮││             │
│ · infer →call   │ ││READ_FILE ─ "src/lex.clj"      turn 1 │││             │
│                 │ ││(ns lex)                              │││             │
╰─────────────────╯ │╰──────────────────────────────────────╯││             │
╭ BEAM ───────────╮ │<critic> progress is slow               ││ GIT         │
╰─────────────────╯ ╰────────────────────────────────────────╯╰─────────────╯
                    ╭ CLAIMS ─────────╮╭ GATES ─────────╮
╭─────────────────╮╭───────────────────────────────────────────╮
│      (o .)      ││ a directive for the run — Enter sends    start  abort  resume │
╰─────────────────╯╰───────────────────────────────────────────╯
 ● connected  samizdat:main │ glm-5.3 │ 9k / 128k (7%) │ running │ mode:block │ live │ 61aba012
```

| widget | shows |
|---|---|
| `:widget/conversation` | the branch's story — see below |
| `:widget/activity` | the manifest states the agent is walking, newest last |
| `:widget/runs` | the run picker |
| `:widget/branches` | the beam — every branch on the run and what became of it |
| `:widget/tasks` | the board: open, in progress, blocked, done |
| `:widget/files` | files this run has written, newest first |
| `:widget/context` | the selected branch's context fill against the model's window, what the run has spent, and the cache |
| `:widget/gates` | gates that fired, and predictions still unsettled |
| `:widget/artifacts` | claims made, and how each was judged |
| `:widget/approvals` | the permission dialog or the questionnaire, when one is pending; nothing otherwise |
| `:widget/command-hints` | while a `/command` is being typed, what it could be and what each does |
| `:widget/git` | the working tree: branch, `+staged ~unstaged ?untracked`, the last commit |
| `:widget/input` | the compose box, plus start, abort and resume. `{:boxed true}` for a border, `{:title "STEER"}` for a titled panel |
| `:widget/rule` | a line with a title set into it, `──[ AGENT LOG ]──` |
| `:widget/avatar` | a face for what the agent is doing — faces and which tool makes which are `tui.edn :avatar` |
| `:widget/status` | the footer — see below |

### The conversation

One timeline per branch of who said what, in the order it happened
(samizdat.tui.timeline), each role in its own voice and colour:

- `<you>` — the problem, and every steer a person sent;
- `<agent>` — what the model said; its thinking folds under `◇ thinking`;
  each tool call is a **chamber**, headed by the tool and the argument it is
  known by, showing the first `:result-lines` of what came back with the rest
  a click (or `Ctrl-O`) away, diffs coloured, failures red;
- `<critic>` — the critic's scores and the feature loop's critique and review;
- `<supervisor>` — oversight passes and the watch's interventions;
- `<sys>` — compaction, model switches, and whatever this TUI printed.

Which journal notes appear, as whom, and where in each note's data its words
are, is `tui.edn :conversation :notes`; the branch fetch asks the server for
exactly those kinds (`?notes=…`).

It **follows the bottom**: the newest entry holds the frame's focus, so the
pane scrolls as the run speaks. Scrolling up anchors it on an entry, which
stays put however much arrives below; `End` follows again. It is bounded —
the newest `:turns` of them, 60 by default, since every entry is rebuilt on
every frame.

### The footer

Ported from dirge's status line, which reads
`project:branch | model | used/ctx (pct%) | Nmsgs | state`:

```
 ● connected  samizdat:tui-start-a-run │ glm-5.3 │ 9k / 128k (7%) │ 1 / 1 turns │ aborted │ 61aba012   127.0.0.1:3986
```

Left to right: the connection, the project and its git branch, the model
actually answering, the selected branch's last request against the model's
context window, turns against the run's ceiling, what the run is doing, the
run id, and which harness this is pointed at. Every segment is optional — the
first frame has none of them.

The connection dot is samizdat's own addition rather than dirge's. dirge's UI
*is* the process doing the work; this one is a client that can be pointed
anywhere, so it has to be able to say it has lost the thing it is watching.

Two details carried over deliberately:

- **The denominator is the window, not the fold-trigger budget.** So the
  percentage reads 0–100 instead of running past 100 once a fold became due,
  and an imminent fold is flagged with a `fold` / `fold!` marker at 75% and
  90% instead.
- **`project:branch` collapses to just the project** on a detached HEAD or
  outside a git tree — never a dangling separator.

And one that is samizdat's own: **the numerator is the branch's last
request**, the prompt tokens of the newest turn the provider measured, which
each branch row of `GET /v1/runs/:id` carries as `context`. It used to be the
run's cumulative total, which on a beam of five is every branch's every turn
summed, so the strip said `fold!` a handful of turns into any run and never
stopped. A branch nothing has measured yet draws no fill segment at all.

The CONTEXT panel draws the same fill as a gauge under the run's spend, and
below it the cache: the run's hit rate from `usage.cache-hit-rate`, and from
`usage.cache-misses` how many turns the cache failed and why — `forced` (a
native tool_choice), `rewritten` (a fold or a prune behind the tail), `tail`
(the provider dropped a byte-stable prefix), `first`; and from
`usage.context-block` what the harness's own per-turn block (the ledger, the
memories, the task) adds to each request on average. Each conversation turn
says what its request cost beside its tool name, `ctx 43k · hit 93%`, and
names the forced tool or the rewrite when one busted the cache — so a hit
rate that drops is readable at the turn it dropped.

### Where the project data comes from

`GET /v1/harness/project`, which the poller folds in beside the layout. The
footer and the GIT panel both read it:

```json
{"project": "samizdat", "root": "/Users/yogthos/src/samizdat",
 "branch": "tui-start-a-run", "staged": 0, "unstaged": 16, "untracked": 0,
 "last_commit": "Let the TUI start a run, and drop the caption over its input",
 "provider": "glm", "model": "glm-5.3", "context_window": 128000}
```

Served rather than read locally for the same reason the layout is: only the
harness process is bound to the project. A TUI that shelled out to git itself
would caption whichever directory it happened to be started from — silently,
and wrongly, whenever it is pointed at a harness on another machine.

The server caches the git side for `:git-snapshot-ttl-ms` (gates.edn, 3s by
default), because the snapshot is three `git` calls and the poller asks every
1.5s per connected front end. dirge learned the same thing the harder way: it
read `.git/HEAD` once per painted frame and froze its UI on large repos until
it cached the lookup.

## Rearranging it, while it runs

The core owns what a widget *is*. `resources/tui.edn` owns how the widgets are
*arranged*, and it is userspace — versioned per project alongside `gates.edn`
and the manifests, editable by a person as a file and by the agent as a stored
version, both of them without a restart.

The file is hiccup. Every tag is ftxui's own (`:vbox`, `:hbox`, `:border`,
`:separator`, `:flex`, `:width`, …) except `:widget/*`, each of which stands in
for one widget the core implements. Expansion replaces those and leaves the
rest alone, which is why two conversation panes side by side is a layout rather
than a feature somebody has to add.

`tui.edn` is a LAYERED settings file, like `config.edn`: it follows a person
from project to project, so it is assembled from several files, each merged
over the ones below it (samizdat.layers). Highest first:

1. **`$SAMIZDAT_TUI_FILE`** (or the older `$SAMIZDAT_TUI_LAYOUT`) — a file
   named outright;
2. **`.samizdat/tui.edn`** in the project the TUI was started in;
3. **what the harness serves** — `GET /v1/harness/layout`, the server's own
   project file (or a version the agent saved through
   `policy({action: "save", name: "tui", …})`). This is how the *agent*
   rearranges the UI of a front end running somewhere else;
4. **`~/.config/samizdat/tui.edn`** (`$XDG_CONFIG_HOME` honoured) — a
   person's own, across every project;
5. **the shipped `resources/tui.edn`**, which is what draws offline and on the
   first frame.

Maps merge key by key, so a global file can set one colour and a project file
another and both apply; anything else — the `:layout` vector especially — is
replaced whole by the highest file that sets it. A file only has to say what
it changes: `{:prose-turns 20}` alone is a complete `tui.edn`. A file that
does not read costs only itself, and the status line names it. Every file is
re-read when its mtime or length moves, so an edit shows up on the next frame
with the TUI still running.

Two settings live at the top level of the map, beside `:layout`:

- `:prose-turns` (12) — how many of the newest turns to fetch the model's
  prose for each poll. The branch listing deliberately leaves assistant and
  reasoning text out; on one real run that was 5.5MB against 62KB of results,
  enough that the panel exceeded its socket timeout and never drew. Raise it
  to scroll back further and pay a request per poll per turn.
- `:turns` (60), a prop on `:widget/conversation` — how far back it draws.

### Colours

Every colour on screen is `tui.edn :theme`: a map from a **class** to a
**style**, the way a stylesheet works. Widgets name classes (`:agent`,
`:critic`, `:tool-box`, `:perm-box`, …) and never colours; any node in
`:layout` may name classes too, `{:class :critic}`, or carry an inline
`{:style {:color "#ffb955" :bold true}}`, which wins over its classes. A
style takes ftxui's attributes — `:color :bg :bold :dim :italic :underlined
:inverted :strikethrough` — plus `:border` and `:border-color`. A colour is a
palette name, a 256-colour index, `"#rgb"`/`"#rrggbb"`, or `[:rgb r g b]`; one
ftxui cannot read is left out and named in the status line, never drawn (it
would otherwise throw at draw time and take the frame with it). The shipped
theme is dirge's phosphor palette, and because themes merge key by key a
`~/.config/samizdat/tui.edn` of `{:theme {:agent {:color "#ffffff"}}}` changes
exactly that one colour.

### Two things that will bite

**A bare `:height` is an equality constraint in ftxui**, not a maximum. A
column of pinned boxes cannot shrink to fit, so the ones at the bottom get
squeezed to nothing — five panels in the right gutter drew the last three as
empty rules. One flexed panel per column, the rest pinned.

**`:flex` takes a value, not just `true`.** `:grow` means *may take spare room,
may not be squeezed*; plain `true` means both.

### A bad edit costs a panel, never the screen

This file's whole reason for being userspace is that it gets edited while it
is being used, so every failure here is a rendering:

- a `:widget/*` tag nothing implements draws a named complaint in its own box,
  and its neighbours are untouched;
- a widget that throws is contained to its own box, with the message;
- a layout that is not hiccup at all falls back to the shipped template and
  says why in the status line;
- a file that does not parse — which is what a half-saved edit looks like for
  an instant — is not cached, so the finished write is still picked up.

## What is pushed, and what is polled

The run on screen is **pushed**. The TUI follows `GET /v1/runs/:id/events`, a
server-sent event stream (samizdat.api.stream): every journal event after the
cursor, then each one as it lands, plus the manifest steps and approval
changes that are never journalled. An event says what changed — a turn on the
branch being read, a question for a person, the run ending — and only that is
fetched, a burst of events coalesced into one fetch of each thing. The footer
says `live` while the stream is up. A dropped stream reconnects with
`Last-Event-ID` and resumes after the last event it saw; the server reads the
events back from the table by id, so a slow reader misses nothing.

What is not a run — the layout, the project, the run list — is **polled**,
every 5s while the stream is up. While it is down the run is polled too, as
it always was (every 1.5s connected, 30s not), and the footer says `polling`:
a server without the stream, or a dropped connection, costs freshness, not
function.

An outage costs nothing that is already drawn. Cursors, the trace and the
prose survive it. Switching runs drops all of it on purpose — turn numbers
restart per branch, so prose kept across a switch would caption the new
run's turn 3 with the old run's words.

## Tests

The state fold, the widgets and the layout are pure functions over data, so
the main suite covers them with no terminal and no server:

```bash
jolt test
```

What that cannot tell you is whether a **click** reaches a fold's `:on-change`
— `[:collapsible {…}]` being the right map is not the same claim as a person
being able to open a diff with the mouse. That half mounts real ftxui
components headlessly, sends a mouse press at real coordinates, and checks the
fold opened. It needs the compiled shim, which is why it is its own alias:

```bash
jolt tui-test
```

## Where the code is

| | |
|---|---|
| `tui/samizdat/tui/core.clj` | the loop, the feeds (stream, refresher, poller), the handlers, the slash commands — the only namespace that knows ftxui exists |
| `tui/samizdat/tui/state.clj` | the view state and every fold into it, pure |
| `tui/samizdat/tui/widgets.clj` | the widgets, each `(fn [state props] -> hiccup)`, pure |
| `tui/samizdat/tui/timeline.clj` | the conversation as entries in each role's voice, pure |
| `tui/samizdat/tui/commands.clj` | parsing, completion and help for slash commands, pure |
| `tui/samizdat/tui/theme.clj` | classes and inline styles resolved against the theme, colours checked, pure |
| `tui/samizdat/tui/layout.clj` | the layers, expansion, and the degradations |
| `src/samizdat/api/sse.clj` | following an event stream: the chunked body and the event lines, and reconnecting |
| `src/samizdat/api/stream.clj` | the server side of the stream |
| `resources/tui.edn` | the shipped arrangement, theme, commands, conversation settings and avatar |
