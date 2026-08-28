---
name: clojure-style
description: Load before writing or editing Clojure — the habits that decide whether the code is correct: immutability, pure cores with effects at the edges, small functions, laziness traps, and when a macro is the wrong tool.
---

# Writing Clojure that works

This is about semantics and quality, not formatting. Match the surrounding
file for anything cosmetic.

## Values, not mutation

The default is that nothing changes: you build a new value from an old one.
Before reaching for state, ask what the function would look like as a pure
transformation — usually it is shorter and it is testable without a fixture.

- Do not use an `atom` as a loop accumulator. `reduce`, `into`, `map`,
  `group-by` and friends express the same thing without a mutable cell and
  without a race.
- Where state is genuinely needed (a cache, a registry, a run's progress), one
  atom holding one map beats several atoms that can disagree. Every additional
  mutable reference is another invariant you now have to hold by hand.
- Never `def` inside a function body. It looks like a local binding and is a
  process-wide mutation that will surprise the next caller; use `let`.

## A pure core, effects at the edges

Push I/O, database calls and printing to the outside of a computation and keep
the middle a function of its arguments. This is what makes a thing testable at
all — and it is why the harness's own cells declare `:pure` or `:effects`.

- A function that both computes and writes is two functions. Split them and the
  computation becomes something you can check with a plain `=`.
- Pass what you need as arguments rather than reading it from a var. A function
  that reads a global cannot be reasoned about locally.

## Small functions, shallow nesting

Measured across harnesses: short functions land correct, long ones do not — a
20+ line function usually arrives broken, because every extra line is another
chance to lose a paren. It is also where readers get lost.

- One function at a time. Writing a new file: put the `ns` form down first,
  then add functions one by one, checking each.
- Use `->`, `->>`, `some->`, `cond->` instead of nesting calls. Threading turns
  a pyramid into steps you can read and edit one line at a time.
- `let` a subexpression into a named binding rather than inlining it three
  levels deep. The name is free documentation and the line gets shorter.

## Laziness is where the real bugs are

`map`, `filter`, `for`, `remove` and `take` return *recipes*, not results.
Nothing runs until something consumes them, and that gap is the single most
common source of Clojure bugs that pass review.

- **Never put a side effect in a lazy sequence.** `(map save! xs)` may run
  never, once, or partially. Use `run!`, `doseq`, or `mapv` when you mean it
  to happen.
- A lazy seq realized outside the scope that set it up is a classic failure:
  it escapes a `with-open` and reads from a closed handle, or escapes a
  `binding` and sees the wrong value. Force it with `doall`/`vec` *inside* the
  scope.
- To ask whether a sequence has anything in it, use `(seq xs)`, not
  `(> (count xs) 0)` — `count` walks the whole thing, which is wrong on a long
  seq and fatal on an infinite one.

## nil

`nil` is a legitimate value in Clojure, not an error: it is falsey, it seqs to
empty, and most core functions accept it. Lean on that rather than guarding
everywhere — but know the two places it bites: `nil` in arithmetic throws, and
a `nil` returned from a lookup is indistinguishable from a stored `nil` unless
you use `contains?` or a sentinel default.

## Reach for the simplest tool

- Plain maps and vectors are the default data. `defrecord` only when you need
  protocol dispatch or the performance; `deftype` almost never.
- **Prefer functions to macros.** A macro cannot be passed, composed or tested
  like a function, and it runs at a different time than the code around it
  reads as though it does. Write the function first; make it a macro only if
  the caller genuinely needs new syntax or lazy evaluation of its arguments.
- Multimethods when dispatch is open and data-driven; protocols when the set of
  operations is fixed. When neither is clearly right, a map of functions or a
  `case` usually is.

## Errors carry data

Throw `ex-info` with a map, not a bare exception with a sentence. The message
is for a human; the map is what a caller can actually branch on, and it costs
nothing to include the ids and values that explain the failure. Catching a
string-matched message is how error handling silently stops working when
someone rewords it.

Do not use exceptions for control flow — a function that returns `nil` or a
result map for the ordinary "not found" case is easier to compose than one that
throws it.

## Recursion

`recur` for loops, so the stack does not grow. But most manual recursion is
better as `reduce`, `into`, `iterate` or `tree-seq` — if you find yourself
threading an accumulator through a hand-rolled loop, one of those already does
it correctly.
