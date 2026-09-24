<p align="center">
  <img src="img/logo.svg" alt="samizdat logo" width="300">
</p>

A self-hosting agentic harness for Clojure development, written in
[Jolt](https://github.com/jolt-lang/jolt) (Clojure on Chez Scheme). The
defining idea: the harness is a live image, the agent's output space is the
harness's code space, and the agentic loop itself is data — a workflow
manifest the agent can inspect and, behind a validate/soak/rollback gate,
rewrite while it runs.

Three rings:

- **kernel** — the durable journal, event bus, store, secrets, and executor
  lifecycle. Agent-immutable.
- **executor** — the [mycelium](https://github.com/mycelium-clj/mycelium)
  workflow layer (vendored, running on a jolt port of
  [maestro](https://github.com/yogthos/maestro)). The loop is an EDN manifest;
  cells are the plugin unit and declare whether they are pure or effectful.
- **capabilities** — nREPL, clojure-lsp, and shell surface as plugins; every
  plugin is a cell in the workflow graph.

Multiple agents work each non-trivial task: work is grounded in a kanban
`tasks` table, decomposition hands subagents a contract plus tests as the
spec, and collaborators share a feature workspace (artifacts, failures,
messages) while keeping private working context.

Descended from veriframe, a claim-first verification harness for mathematics;
the proof engines (Lean, Z3, SWI-Prolog, Octave) left, the durable-journal
core, resume-by-replay, gate/arbiter loop, and beam scheduler stayed.

## Running

```
jolt tui        # the server and the terminal UI in one process
jolt serve      # the server alone: HTTP + nREPL, parks
jolt bin        # build ./samizdat (same entry point as `jolt tui`)
jolt test       # full suite
jolt smoke      # platform probes (sqlite, https, server)
```

The binary takes the same modes: `samizdat` starts the server and the TUI,
`samizdat --headless` the server alone, and `samizdat --connect [URL]` the TUI
alone against a server that is already running. With the TUI in the same
process the server's log goes to `.samizdat/samizdat.log`. The binary loads
the FTXUI shim and the system sqlite/OpenSSL at runtime, so it expects them
where they were at build time.

## Configuration

`config.edn` is layered: `~/.config/samizdat/config.edn`, then the project's
`.samizdat/config.edn`, then `$SAMIZDAT_CONFIG_FILE`, each merged over the one
before. Providers are declared by alias and assigned to roles:

```clojure
{:providers {:bonsai {:type :local                  ; the adapter + built-in preset
                      :base-url "http://127.0.0.1:8080/v1"
                      :thinking? true :max-tokens 8192}
             :glm    {:model "glm-5.3"}             ; alias names a built-in: no :type
             :flash  {:type :deepseek :model "deepseek-v4-flash"}
             :pro    {:type :deepseek :model "deepseek-v4-pro"}
             :gpu    {:type :openai
                      :base-url "https://gpu.lan:8000/v1"
                      :api-key-env "VLLM_API_KEY"    ; or :api-key "${VLLM_API_KEY}"
                      :headers {"X-Org" "${ORG_ID}"}
                      :context-window 65536}}
 :roles {:default :bonsai                           ; the run's own model
         :supervisor :glm
         :reader :flash
         :critic :pro}}
```

Built-in types are `:local`, `:openai`, `:deepseek`, `:glm` and `:ollama`. An
entry takes any `:llm` knob as well (`:temperature`, `:max-tokens`,
`:gen-floor-tps`, `:features`, ...); what it leaves out comes from its
type's preset. A role maps to one alias — for another model on the same
endpoint, declare another alias — and a role not in `:roles` runs on the
default. Only the providers
something selects are resolved, so a machine-wide file can declare endpoints
whose keys a given machine lacks; a role naming an unknown alias, or an unset
`${VAR}` on a selected provider, fails at startup. A running run's model can
be switched to a declared alias with a `model` intervention (`/model critic
flash`, or `flash:deepseek-v4-pro`). A top-level key a config file sets that
nothing reads is refused at startup, naming the file and the key.

State lives in `.samizdat/samizdat.sqlite3` (moving to [dolt](https://github.com/dolthub/dolt)
via [doltera](https://github.com/jolt-lang/doltera)); a run survives restart
and resumes from its journal.
