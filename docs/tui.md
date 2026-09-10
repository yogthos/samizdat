# The terminal UI

A full-screen terminal front end for a running harness. It shows what the
agent is doing, lets a person answer the questions a branch is parked on, and
takes an intervention when the run needs steering.

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
| click a fold header | open a thinking block, a tool's arguments, or a result — diffs are coloured |
| `y` / `n` | allow or deny the permission dialog |
| `F5` | poll now, without waiting out the interval |
| `Ctrl-C`, `Ctrl-Q` | quit |
| arrows + `Enter` | pick a run in RUNS, a branch in BEAM, an option in a questionnaire |
| type + `Enter` | **with no run selected**, start a run on what you typed; **with one**, send it as a directive |
| `start` / `abort` / `resume` | the three things done *to* a run rather than said to it |

The mouse is on, and folds are ftxui's own collapsibles — clicking one is how
it opens, with no hit-testing of our own.

`y` and `n` are consumed **only** while a yes/no permission dialog is up. Over
a questionnaire's answer box a `y` is a letter being typed and goes through
untouched (`samizdat.tui.state/pending-decision` decides which). Everything
else — arrows, text, clicks — belongs to whichever widget has the focus; the
global handler consumes as little as it can.

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

Every panel is a widget, and which ones exist is the layout's business, not
the core's. The shipped arrangement is a left gutter, the conversation, a
right gutter, a wide short row, and the bottom strip.

| widget | shows |
|---|---|
| `:widget/conversation` | the agent's turns: what it said, what it called, what came back. Thinking, arguments and results fold, shut by default |
| `:widget/activity` | the manifest states the agent is walking, newest last |
| `:widget/runs` | the run picker |
| `:widget/branches` | the beam — every branch on the run and what became of it. A run is several branches and the one being read is a choice |
| `:widget/tasks` | the board: open, in progress, blocked, done |
| `:widget/files` | files this run has written, newest first |
| `:widget/context` | context-window fill, and what the run has spent |
| `:widget/gates` | gates that fired, and predictions still unsettled |
| `:widget/artifacts` | claims made, and how each was judged |
| `:widget/approvals` | the one question a person is being asked, if any. Draws nothing when there is none |
| `:widget/git` | the working tree: branch, `+staged ~unstaged ?untracked`, and the last commit's subject |
| `:widget/input` | the compose box, plus start, abort and resume. Bare by default — pass `{:title "STEER"}` for a caption |
| `:widget/status` | the footer — see below |

Two details worth knowing because they look like bugs otherwise:

- **The approval dialog shows one question at a time**, and does not clip the
  command it is asking about. A wall of pending questions is how somebody
  answers the second one thinking it was the first, and with a branch parked
  on the answer that is not a cosmetic mistake.
- **The conversation is bounded** — the newest `:turns` of them, 60 by
  default. Every entry is rebuilt on every frame, so this is a frame-rate
  number as much as a history one.

### The footer

Ported from dirge's status line, which reads
`project:branch | model | used/ctx (pct%) | Nmsgs | state`:

```
 ● connected  samizdat:tui-start-a-run │ glm-5.3 │ 9k / 128k (7%) │ 1 / 1 turns │ aborted │ 61aba012   127.0.0.1:3986
```

Left to right: the connection, the project and its git branch, the model
actually answering, tokens against the model's context window, turns against
the run's ceiling, what the run is doing, the run id, and which harness this
is pointed at. Every segment is optional — the first frame has none of them.

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

To change it without restarting, drop an edited copy of `resources/tui.edn` at
`.samizdat/tui.edn` (or point `$SAMIZDAT_TUI_LAYOUT` at one). The layout is
re-read every frame; the file itself is only re-read when its mtime or length
moves, so this is cheap.

Three sources, most local first:

1. **the file** — `$SAMIZDAT_TUI_LAYOUT`, else `.samizdat/tui.edn`. What a
   person edits;
2. **what the harness serves** — `GET /v1/harness/layout`, the project's
   stored `tui` policy row, folded in by the poller. This is how the *agent*
   rearranges its own UI, through `policy({action: "save", name: "tui", …})`
   — the same seam that carries `gates.edn`. The TUI holds no database handle,
   so it asks the server, which does;
3. **the shipped template** off the classpath, which is what draws offline and
   on the first frame.

Two settings live at the top level of the map, beside `:layout`:

- `:prose-turns` (12) — how many of the newest turns to fetch the model's
  prose for each poll. The branch listing deliberately leaves assistant and
  reasoning text out; on one real run that was 5.5MB against 62KB of results,
  enough that the panel exceeded its socket timeout and never drew. Raise it
  to scroll back further and pay a request per poll per turn.
- `:turns` (60), a prop on `:widget/conversation` — how far back it draws.

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

## What it polls

One pass over every feed on a background thread, cheapest first, each fold
independent so a slow branch detail does not hold up the step trace: the
served layout, the run list, the step tail, pending approvals, the run detail,
the branch detail, then the prose for the turns on screen. Every 1.5s while
connected, every 30s while not — a TUI left open against a stopped server is
not hammering it. Answering a question polls immediately rather than waiting
out the interval, because the whole point of that dialog is that somebody is
watching it.

An outage costs nothing that is already drawn. Cursors, the trace and the
prose survive it; a panel that blanked on a dropped connection would lose the
history that says what happened before it. Switching runs drops all of it on
purpose — turn numbers restart per branch, so prose kept across a switch would
caption the new run's turn 3 with the old run's words.

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
| `tui/samizdat/tui/core.clj` | the loop, the pollers, the handlers — the only namespace that knows ftxui exists |
| `tui/samizdat/tui/state.clj` | the view state and every fold into it, pure |
| `tui/samizdat/tui/widgets.clj` | the widgets, each `(fn [state props] -> hiccup)`, pure |
| `tui/samizdat/tui/layout.clj` | the three sources, expansion, and the degradations |
| `resources/tui.edn` | the shipped arrangement |
