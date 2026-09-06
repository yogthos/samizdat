# RFC-005 — The provider layer

**Status:** implemented.

## Purpose

Specifies how samizdat talks to a model: one retry ladder, one message shape,
one tool-call convention, and a per-provider adapter carrying only the deltas
that genuinely exist.

## Scope

**This layer decides** how to make a call and what to do when it fails —
nothing about what to say or when to say it.

**It must not know** what a turn is, what a branch is, or that tools exist. It
sees messages in and a parsed reply out.

**It hands** `{:content :reasoning :finish-reason :usage :elapsed-ms}` to the
inference layer (RFC-004) and nothing to anyone else.

## Model

```
infer/render tape → messages
   │
llm/chat adapter config messages opts
   │   ├─ message/prepare        normalise roles, strip think blocks,
   │   │                         drop stale settled-state ledgers
   │   ├─ adapter/chat-body      the provider's wire shape
   │   ├─ http/post              bounded: socket + connect timeouts
   │   ├─ classify               :retry | :fatal
   │   └─ adapter/parse-chat     content · reasoning · finish-reason · usage
   ▼
{:content merged :finish-reason :usage :elapsed-ms}
   │
fence/parse-tool-call            ```tool-call fenced JSON → {:name :args}
```

**Why the adapter is a record and not a namespace per provider.** One adapter
covers OpenAI, DeepSeek, GLM, and any local llama-server or vLLM endpoint,
because they speak the same wire format. A subclass whose only content is a base
URL is not an abstraction. Real differences are record fields:
`provider-id`, `label`, `reasoning-key`, `max-tokens-key`.

**Why the reasoning stream is folded into the content.** DeepSeek and GLM return
`reasoning_content` beside `content`; others return nothing. `merge-reasoning`
wraps it in `<think>…</think>` so one fence parser works everywhere — a model
that emits its tool call inside its reasoning is handled identically to one that
emits it in the content.

## API

### `samizdat.llm.adapter` — the protocol

Deliberately small: every method an adapter may override is a place a provider
can quietly diverge from the retry and timeout discipline.

| method | contract |
|---|---|
| `(id this)` | Provider keyword. |
| `(display-name this)` | For errors and logs. |
| `(chat-url this config)` | Full completion URL. |
| `(models-url this config)` | Listing URL, or `nil`. **Not** simply base-url + `/models`: DeepSeek's `/beta` serves completions only. |
| `(auth-headers this config)` | String→string. |
| `(chat-body this config request)` | The wire body from `{:messages :max-tokens :temperature :prefill :force-tool :cache-key}`. Data; the client encodes. **An adapter MUST ignore a knob it cannot honour.** |
| `(prefill-support? this config)` | Whether this provider *and this endpoint* continue a flagged trailing assistant message. Takes config because it is not a property of the provider alone. |
| `(parse-chat this body)` | `{:content :reasoning :finish-reason :usage}`, or `nil` for a reply carrying no completion. |
| `(parse-models this body)` | Model ids. |
| `(error-message this body)` | Provider error text from a 200 body, or `nil`. |
| `(usage-cap? this status body)` | Whether a 429 is a wall rather than a window. |

### `samizdat.llm.client`

| fn | contract |
|---|---|
| `(chat adapter config messages [opts])` | The call. `opts`: `:max-tokens :temperature :max-retries :prefill :force-tool :cache-key`. **Throws** `ex-info` with `:provider` and `:attempts` when every attempt failed; the loop is bounded in attempts and each attempt in wall clock, so a stuck provider costs a known amount rather than the run. |
| `(list-models adapter config)` | Startup validation. |
| `(classify adapter status body)` | `:retry` or `:fatal`. Anything unrecognised is fatal — retrying an error nobody understands spends budget to learn nothing. |
| `(retry-after-ms headers)` | What the provider asked for, **unclamped**. |

Bounds: `default-max-retries` 2, `default-timeout-ms` 300000,
`default-conn-timeout-ms` 15000, `max-backoff-ms` 60000,
`max-in-run-retry-wait-ms` 300000.

### `samizdat.llm.message`

| fn | contract |
|---|---|
| `(prepare messages)` | Wire normalisation. Projects **role and content only**, so bookkeeping keys (`:turn`, `:pinned?`, `:compacted?`, `:original`) never reach a provider. |
| `(compact messages turns [opts])` | Compaction in place. See RFC-004. |
| `(strip-think-blocks content)` | Prior assistant turns lose their reasoning. Both DeepSeek and Zhipu document that `reasoning_content` must not be fed back. |
| `(merge-reasoning content reasoning)` | Fold a separate stream into `<think>` framing. |
| `(strip-stale-ledgers messages)` | All but the newest settled-state block. A ledger is **state**: only the newest is true, and an older copy is a strictly worse version. |

### `samizdat.llm.fence`

`(parse-tool-call content {:prefill p})` → `{:name :args}`, `{:name "__parse_error__"}`,
or `nil`. `(signals response parsed)` → `{:no-fence :truncated :parse-error
:auto-repaired :multiple-fences}`. `(reattach content prefill)` → the complete
assistant turn.

`:no-fence` and `:truncated` are separated deliberately: a reply that hit the
token cap mid-thought produced no fence because it never got that far, and
reading that as a model too weak to call a tool would be wrong — **the fix is
more tokens, not more steering.**

## Protocol

### Retry ladder

```
attempt → post-once → classify
  :ok        → return
  :fatal     → throw with the provider's name attached
  :retry     → if the provider's own reset is beyond max-in-run-retry-wait-ms,
               treat as fatal: a cap wearing a rate limit's headers cannot be
               waited out inside a run
             → else sleep backoff-ms and retry
```

Backoff is 2s, 8s, 32s with up to +25% jitter, or whatever the provider asked
for, clamped at 60s. **The jitter is what keeps a beam of branches that all hit
the same 429 from retrying in lockstep and re-colliding.**

A reply with neither content nor reasoning is an **error**, not an empty answer:
it usually means the model spent its whole budget thinking, and reporting it as
a successful empty turn would send the loop round again with nothing.

### Prefill and forced tools

Tool calls here are fenced JSON in free text, so the model can always answer in
prose instead — the harness's dominant mechanical failure. Two ways to remove
that option:

| mechanism | when | why not the other |
|---|---|---|
| assistant **prefill** with `:prefix true` | DeepSeek `/beta` only | preferred where available; `tool_choice` is rejected by some providers' thinking mode |
| native `tool_choice` | providers without prefill (GLM) | a fallback; only the forced tool is exposed |

`prefill-support?` gates both, and `chat-body` consults **the protocol method on
this adapter** rather than a private twin, so the answer a caller can query and
the answer acted on cannot drift (`provenance R3-14`).

**Prefill is preferred for a reason beyond thinking-mode: it is the only
cache-safe force.** Measured live 2026-09-06:

- A prefill appends a trailing assistant message, so the whole prior prefix
  stays cached (hit 10496 of 10514) — the force costs only the new tokens.
- A native `tool_choice` busts the cache once per forced episode, on **both**
  providers, and a constant tools block does not prevent it. GLM renders
  `tool_choice` into the prompt it hashes, so flipping it to a function is a new
  prefix (hit 0); DeepSeek rejects a forced `tool_choice` unless thinking is
  off, and toggling thinking off is itself a cache bust (hit 0). Forcing
  clusters at terminal gates, so the cost is ~one full-prompt miss per branch on
  GLM — accepted, since GLM cannot prefill and the alternative (a constant tools
  block) does not help.

deepseek-harness — the official DeepSeek reference — never prefills and never
forces: it uses native tool calling, whose structured output cannot be prose, so
it needs neither. samizdat keeps the fenced-prose convention, so it keeps prefill
(where the provider continues one) as the cache-safe force and the withholding
recovery, and falls back to native `tool_choice` only where it must.

**A no-call recovery is graduated, not always a prefill** (`agent/loop`
no-call step). A content prefix skips DeepSeek's reasoning phase entirely, so a
FIRST plain no-call gets a message-only steer — the model keeps its reasoning on
the turn it is struggling, which is how every provider but DeepSeek `/beta`
already recovers (the adapter drops a prefill it cannot continue). A repeat
no-call, a truncation, or a runaway ends the request mid-fence — the withholding
form the ladder was built on (message-only recovery went 0-for-42 on a weak local
model and could not lift a strong one out of a 24-turn no-call loop), kept as the
second rung.

### Local prefix cache

`:cache-key` (the branch id) reaches `chat-body`. For `provider-id :local` only,
it emits `cache_prompt true`, plus `id_slot` **iff** the provider config carries
a `:slots {cache-key → int}` table. Absent, the server picks by prefix
similarity and LRU — the right default, since a slot count is a property of how
the server was launched and a guessed index evicts another conversation's warm
prefix. A bad pin costs a re-prefill, never a wrong answer.

Every hosted provider's body is **byte-identical** with or without the key, and
a test asserts it.

## Invariants

| invariant | enforced by |
|---|---|
| One retry ladder for every provider. | It lives in `client`, not the adapters. |
| An adapter ignores knobs it cannot honour. | Protocol docstring; per-provider tests. |
| No bookkeeping key reaches a provider. | `prepare`'s `(select-keys m [])`. |
| Prior reasoning is never fed back. | `prepare` calls `strip-think-blocks` for assistant roles. |
| Only one settled-state ledger goes over the wire. | `strip-stale-ledgers`. |
| A hosted provider's body does not change when a local-only knob is present. | `llm-test/the-local-endpoint-gets-prefix-cache-reuse-and-nobody-else-does`. |

## Known gaps

- `usage` is `nil` when a provider does not report it, and the cache split is
  **absent rather than zero** when unreported: a fabricated zero would assert
  every token missed the cache, which is a different and false claim.
- The `:local` provider is identified by config key, not by probing the
  endpoint. A llama-server configured under `:openai` gets no prefix pinning.
