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
| type + `Enter` | send what is in the compose box |
| `abort` / `resume` | the two things done *to* a run rather than said to it |

The mouse is on, and folds are ftxui's own collapsibles — clicking one is how
it opens, with no hit-testing of our own.

`y` and `n` are consumed **only** while a yes/no permission dialog is up. Over
a questionnaire's answer box a `y` is a letter being typed and goes through
untouched (`samizdat.tui.state/pending-decision` decides which). Everything
else — arrows, text, clicks — belongs to whichever widget has the focus; the
global handler consumes as little as it can.

What the compose box sends is an **intervention**, not a chat message.
Samizdat runs autonomously and a person steers it at a turn boundary, so this
is the same seam the supervisor uses.

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
| `:widget/input` | the compose box, plus abort and resume |
| `:widget/status` | connected or offline, which run, its status, the last error |

Two details worth knowing because they look like bugs otherwise:

- **The approval dialog shows one question at a time**, and does not clip the
  command it is asking about. A wall of pending questions is how somebody
  answers the second one thinking it was the first, and with a branch parked
  on the answer that is not a cosmetic mistake.
- **The conversation is bounded** — the newest `:turns` of them, 60 by
  default. Every entry is rebuilt on every frame, so this is a frame-rate
  number as much as a history one.

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
