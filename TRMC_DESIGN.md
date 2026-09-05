# Tail Recursion Modulo Cons — design and status

Status: implemented, in two modes. This file records what was built and what
the design got wrong; the sections below are the original plan, annotated.

## What exists

`(op E1..Ek (self a1..an))` in tail position compiles to a loop instead of a
frame per level, for any `op` that is a pure primitive. Two strategies:

**HOLE** — one pass, mutating an open cdr, as the original plan describes.
Applies to `cons` and to `append` (which already copies every argument but the
last and shares that one, so a run of copied spines ending in an open cdr is
exactly its shape). Requires the body to be capture-free, for the call/cc
reason below.

**FOLD** — the pending operands are stacked by prepending and replayed at the
return. Costs a second pass and one cell per level more. In exchange it is
unrestricted, and that turned out to matter more than the original plan
assumed:

- It is not a reassociation and does not need one. In `(op E (self ...))` the
  operand is evaluated before the recursive call either way, and the replay
  runs innermost-first, which is the order the recursion would have applied
  them in as it unwound. Same calls, same values, same sequence; only the
  timing moves. So `(- k (loop ...))` transforms correctly despite subtraction
  being about as non-associative as it gets, and float `+` raises none of the
  questions it would for an accumulator that folds left.
- Prepending allocates but never mutates, so a continuation captured anywhere
  in the body restores its own accumulator value and rebuilds from it. The
  first run's result is untouched. That is precisely the hazard HOLE has to
  exclude, so FOLD needs no capture-free requirement at all.

`(cons (f x) (walk (cdr xs)))` — the shape the plan singled out as excluded —
therefore compiles. HOLE is preferred where it applies because one pass and
one cell per element beats two and two.

## What the plan got wrong

**Top-level `define` was never eligible.** The plan's own example,
`(define (build n) ...)`, does not qualify: the loop optimization is armed by
`compile_letrec`, and a global can be redefined between iterations, so its
self-call cannot legally become a jump. The shape that works is a named `let`,
which reaches `letrec` through the stdlib's own `let` macro. The C
`compile_named_let` never arms the loop optimization either, so `test_eval` —
which runs without the stdlib — has to spell the `letrec` out.

**Stage 2 was not about `env_depth`.** Following `cond` needed nothing but
walking it, since `cond` pushes no frame. `let` does push one, and
`emit_self_call_loop` now unwinds pending frames before the jump — but with
the stdlib loaded, `let` is a macro that expands to `((lambda (d) ...) val)`,
so the real blocker is elsewhere: `captured_by_inner_lambda` counts a
reference from inside that lambda as a capture even though it is inlined and
never becomes a closure, which costs the function its stack locals, which TRMC
needs for the accumulator's address. Teaching that walk about
immediately-applied lambdas is the actual stage 2, and it has to match the
inliner's acceptance conditions exactly.

**`max`, `min`, `gcd`, `lcm` get nothing.** They are stdlib procedures, not
primitives, so they are ordinary calls. That is the right answer, but it means
the arithmetic coverage is whatever the primitive table happens to hold.

## Still open

- The `captured_by_inner_lambda` change above, which is what makes `let`
  bodies work under the stdlib.
- More than one operator per body: the return can only finish one
  accumulator, so a body mixing `cons` and `append` sites transforms only the
  first operator's and leaves the rest as ordinary recursion (correct, just
  not optimized).
- Vectors, which would need a different accumulator and are probably not worth
  it.

---

# Original design

## The problem

```scheme
(define (build n) (if (= n 0) '() (cons n (build (- n 1)))))
```

`build` is not tail recursive: the recursive call sits inside a `cons`, so
each level needs a frame to hold the pending `cons`. Vesper's VM handles
about 500k frames after the ceiling was raised (`VM_MAX_FRAMES_SIZE` in
bytecode.h), and the CPS interpreter goes deeper, but both are bounded by
memory that a loop would not need at all.

TRMC turns the function into a loop by building the list forwards and
mutating the last cell's `cdr` in place. `map` in stdlib.scm (~line 1400)
already does exactly this by hand:

```scheme
(let ((head (cons '() '())))
  (let loop ((lst lst) (tail head))
    (if (null? lst)
        (cdr head)
        (let ((new-cell (cons (proc (car lst)) '())))
          (set-cdr! tail new-cell)
          (loop (cdr lst) new-cell)))))
```

The goal is to have the compiler do this for user code.

## Why this is cheaper than it looks

The compiler already turns self tail calls into loops. `compile_ctx`
(bytecode.h) carries `loop_var_id`, `loop_params`, `loop_arity` and
`loop_pending`; `compile_call` (compile.c, the "Loop Optimization" block near
line 4218) recognises a tail call to the letrec-bound loop variable and emits
argument evaluation, a `SET` per parameter, and `JUMP 0` instead of
`OP_TAILCALL`.

TRMC is the same transform with a `cons` wrapped around the call. The
detection conditions, the argument-evaluation code and the SET+JUMP tail are
all reusable. What is new is the accumulator and the return path.

## Shape

For a candidate function the compiler emits:

```
ip 0:      TRMC_INIT             ; head = nil, tail = nil
loop_start:
           ...body...

  at (cons E (self a1..an)) in tail position:
           <eval E>              ; stack: [E]
           TRMC_APPEND           ; cell = (E . nil); link into head/tail; pops E
           <eval a1..an>
           SET pn .. SET p1
           JUMP loop_start

  at any other tail position, value V:
           <eval V>
           RETURN                ; TRMC-aware, see below
```

`TRMC_APPEND` does the work `map` does by hand: allocate `(E . '())`; if
`tail` is nil set `head` to the new cell, otherwise `set-cdr!(tail, cell)`;
then `tail = cell`.

On return, the chain has one open hole at its end, which the returned value
fills: if `tail` is nil the function never appended anything and the value is
returned unchanged; otherwise `set-cdr!(tail, V)` and `head` is returned.

## Two decisions that matter

**Put `head` and `tail` in operand-stack slots, not in `vm_state`.**
The VM already traces the whole operand stack in `gc_update_vm_roots`
(vm.c), so stack slots are rooted for free and are correct across a
collection landing inside `TRMC_APPEND`. Fields on `vm_state` would need
their own entries in both `gc_update_vm_roots` and
`gc_update_vm_roots_minor`, and — worse — would be shared between an outer
and an inner activation of the same function. Stack slots are per-invocation,
which is what the accumulator has to be. `code_object` already supports stack
locals (`use_locals`, `num_locals`), so this is existing machinery.

**Make `OP_RETURN` TRMC-aware via a `code_object` flag, rather than emitting
a different return opcode.**
Otherwise every site in the compiler that ends a tail position has to know
about TRMC. A `code->trmc` flag read by `OP_RETURN` costs one predictable
branch next to the `vm->fp == 0` branch it already has, and the compiler's
return emission does not change at all.

## The call/cc problem

TRMC works because the half-built list is assumed unobservable until the
function returns. Vesper has full multi-shot continuations, so that
assumption is not free: if `E` or one of the argument expressions captures a
continuation and it is invoked a second time, the loop resumes with the
`head`/`tail` slots restored from the captured stack (capture_continuation
copies the operand stack), and starts writing into cells the first run
already returned to somebody.

OCaml's `[@tail_mod_cons]` never faces this — it has no call/cc. Gambit and
Bigloo restrict the transform.

**Recommendation: bail out conservatively.** Apply TRMC only when `E` and
every argument expression is *capture-free by construction*:

- self-evaluating literals and quoted data
- variable references
- applications of primitives known not to call back into Scheme
  (arithmetic, comparisons, `car`/`cdr`/`null?`/`cons`, …) whose own
  arguments are recursively capture-free

That covers `(cons n (build (- n 1)))`, which is the shape that actually
matters. It excludes `(cons (f x) (walk (cdr xs)))` because `f` is a general
call that might capture. Widening this is a later decision, not a stage-1
one; the check should live in one predicate so the policy has a single home.

Do not skip this and "document the deviation". A silently mutated list that
somebody already received is the kind of bug that takes a week to find, and
this codebase's own audit history is mostly hygiene and rooting bugs of
exactly that character.

## Detection

In `compile_call`, ahead of the existing loop-optimization block:

1. `cctx->tail_position` is true.
2. `fn_expr` is the atom `cons`, and `cons` still denotes the builtin at this
   point (same shadowing check the foldable-primitive path already does — a
   user is allowed to rebind `cons`).
3. `argc == 2`.
4. `cadr(args)` is `(self a1..an)` where `self` is `cctx->loop_var_id`,
   `n == cctx->loop_arity`, `loop_arity <= 16`, and `cctx->env_depth == 0`
   — i.e. exactly the conditions the existing block already enforces, for the
   same reasons (`JUMP` past a `POPENV` would corrupt the frame stack).
5. `car(args)` and `a1..an` are capture-free per the predicate above.
6. The body contains no *other* tail call. A plain `OP_TAILCALL` elsewhere
   would return the callee's value without closing the chain. Simplest
   stage-1 rule: scan the body for tail-position applications before
   committing, and fall back to the ordinary compilation if any is found.

`JUMP 0` becomes `JUMP loop_start`, since ip 0 now holds `TRMC_INIT` and
re-running it would discard the accumulator. That is a small change to the
existing loop block and worth making regardless — `JUMP 0` silently assumes
nothing is ever emitted before the loop body.

## Opcodes

Three, all with no operands:

| Opcode | Effect |
| --- | --- |
| `OP_TRMC_INIT` | set both accumulator slots to nil |
| `OP_TRMC_APPEND` | pop `v`; `cell = (v . nil)`; link into `head`/`tail`; push nothing |
| (none) | closing is done by `OP_RETURN` under `code->trmc` |

`OP_TRMC_APPEND` allocates, so it can collect; `head`, `tail` and `v` must
all be reachable across the `alloc_cons`. `v` is on the operand stack until
popped and the slots are stack slots, so this is satisfied as long as the
handler pops `v` *after* the allocation, not before.

Both new opcodes need rows in `optimize.c`'s `opcode_names[]` and correct
sizes in `instruction_size` — `compiled_pattern.c` has a regression test
asserting every opcode has a mnemonic, and `bytecode.h` should get the same
treatment.

## Staging

1. **Detection + opcodes + flag**, restricted to the single-site shape above
   with capture-free operands and no other tail call. No behaviour change
   except stack usage.
2. **`(cons E (self …))` under `if`/`cond`/`let` in tail position**, which
   means dealing with `env_depth > 0` — the loop jump has to run the pending
   `POPENV`s. This is the same constraint the existing loop optimization
   punts on, so it can be lifted for both at once.
3. **Other constructors**, and more than one TRMC site in a body. `list` is
   easy (it is `cons` chains); vectors need a different accumulator and are
   probably not worth it.

## Test plan

- Depth: 1e6 elements in constant frame space, VM mode. Compare against the
  same function compiled without TRMC.
- Equivalence: a matrix of shapes (empty result, single element, base case
  returning a non-list, dotted tail) must give identical results with and
  without the transform.
- Bail-out coverage: a capturing `E` must *not* be TRMC'd. Assert on the
  emitted opcodes, not just the result, or the test passes for the wrong
  reason.
- `call/cc` inside the loop: re-invoking a captured continuation must produce
  the same answer as the untransformed function.
- GC: run the depth tests under `VESPER_GC_STRESS=1` (see context.c). The
  accumulator rooting is the most likely thing to get wrong, and stress mode
  is what turns that from a rare flake into a deterministic failure.
- Differential: add the shapes to `tools/run_mit_differential.py`.

## What this does not fix

Non-tail recursion that is not modulo a constructor — `(+ 1 (f (- n 1)))`,
tree recursion, `append` over the left spine — still needs frames. TRMC is a
narrow transform that happens to cover the single most common shape.
