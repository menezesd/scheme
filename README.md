# Vesper

A compact R5RS-inspired Scheme interpreter written in C, featuring a bytecode
compiler, semispace garbage collector, hygienic macros, and full numeric tower.

## Features

- **Bytecode Compiler**: Compiles to bytecode for a stack-based VM with peephole optimization
- **Garbage Collection**: Generational collector with semispace nursery (Cheney's algorithm)
- **Tail Call Optimization**: Proper tail calls in both interpreter and VM
- **R5RS Compliance**: Passes standard compliance tests including tricky edge cases
- **Numeric Tower**: Integers, bignums, rationals, floats, and complex numbers
- **Hygienic Macros**: Full `syntax-rules` with referential transparency
- **First-Class Continuations**: Full `call/cc` with multi-shot continuation support
- **SRFI Support**: SRFI-1 (lists), SRFI-9 (records), SRFI-26 (cut/cute)
- **Ports**: File I/O and string ports with standard Scheme port operations

## Building

```bash
make          # Build the interpreter
make debug    # Build with debug symbols and sanitizers
make test     # Run Scheme test suite
make test-interpreter # Run Scheme test suite with CPS interpreter
make test-c   # Run C unit tests
make test-all # Run all tests
make clean    # Remove build artifacts
```

### Requirements

- GCC or Clang with C11 support
- GNU Make
- Standard C library with math support (`-lm`)

## Usage

### Interactive REPL

```bash
./vesper
]=> (+ 1 2 3)
;Value: 6
]=> (define (factorial n)
      (if (= n 0) 1 (* n (factorial (- n 1)))))
;Value: factorial
]=> (factorial 50)
;Value: 30414093201713378043612608166064768844377641568960512000000000000
```

### Running Scripts

```bash
./vesper script.scm
```

### Interpreter Mode

By default, Vesper uses its bytecode compiler. For the tree-walking CPS
interpreter (useful for debugging), use:

```bash
./vesper --interpreter script.scm
```

## Language Features

### Special Forms

```scheme
define    lambda    if        cond      case
let       let*      letrec    begin     and
or        quote     quasiquote set!     do
delay     define-syntax        syntax-rules
```

### Numeric Types

```scheme
; Integers (arbitrary precision)
(factorial 100)

; Rationals (exact)
(/ 1 3)           ; => 1/3
(+ 1/2 1/3)       ; => 5/6

; Floating point (inexact)
(sqrt 2)          ; => 1.4142135623730951
(sin 3.14159)     ; => 2.65358979335e-06

; Complex numbers
(sqrt -1)         ; => 0+1i
(+ 1+2i 3+4i)     ; => 4+6i
```

### Data Structures

```scheme
; Lists
(list 1 2 3)      ; => (1 2 3)
(cons 'a '(b c))  ; => (a b c)

; Vectors
#(1 2 3)          ; => #(1 2 3)
(vector-ref #(a b c) 1)  ; => b

; Strings
"hello world"
(string-append "foo" "bar")  ; => "foobar"

; Characters
#\a #\space #\newline
```

### Higher-Order Functions

```scheme
(map (lambda (x) (* x x)) '(1 2 3 4 5))
; => (1 4 9 16 25)

(filter odd? '(1 2 3 4 5))
; => (1 3 5)

(fold + 0 '(1 2 3 4 5))
; => 15
```

### Macros

Full hygienic macro support with `syntax-rules`:

```scheme
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if (not test) (begin body ...)))))
```

The macro system implements mark-based hygiene to handle complex cases like
nested macros with shadowing:

```scheme
;; Nested macro hygiene - x in bar's template refers to outer x, not inner
(let ((x 1))
  (let-syntax
      ((foo (syntax-rules ()
              ((_ y) (let-syntax
                           ((bar (syntax-rules ()
                                 ((_) (let ((x 2)) y)))))
                       (bar))))))
    (foo x)))  ; => 1 (correctly returns outer x)

;; Referential transparency - + refers to definition-time binding
(let-syntax ((foo (syntax-rules ()
                    ((_ expr) (+ expr 1)))))
  (let ((+ *))
    (foo 3)))  ; => 4 (uses + from definition, not shadowed *)
```

### Records (SRFI-9)

```scheme
(define-record-type point
  (make-point x y)
  point?
  (x point-x point-set-x!)
  (y point-y))

(define p (make-point 3 4))
(point-x p)        ; => 3
(point-set-x! p 5)
(point-x p)        ; => 5
```

### Partial Application (SRFI-26)

```scheme
;; cut - slots filled at call time
(define add1 (cut + <> 1))
(add1 5)  ; => 6

(define list-1-x-3 (cut list 1 <> 3))
(list-1-x-3 2)  ; => (1 2 3)

;; cute - non-slot expressions evaluated at definition time
(define counter 0)
(define cute-fn (cute cons (begin (set! counter (+ counter 1)) counter) <>))
counter     ; => 1 (evaluated once at definition)
(cute-fn 'a)  ; => (1 . a)
(cute-fn 'b)  ; => (1 . b) (still 1, not re-evaluated)
```

### Ports and I/O

```scheme
; File I/O
(call-with-input-file "data.txt"
  (lambda (port)
    (read port)))

; String ports
(call-with-output-string
  (lambda (port)
    (display "hello" port)
    (newline port)))
; => "hello\n"
```

### Continuations

```scheme
(call-with-current-continuation
  (lambda (k)
    (+ 1 (k 42))))
; => 42
```

## Architecture

### File Structure

| File | Description |
|------|-------------|
| `main.c` | Entry point, REPL, file loading |
| `compile.c/h` | Bytecode compiler with constant folding |
| `vm.c` | Stack-based virtual machine |
| `bytecode.h` | Opcode definitions and VM structures |
| `types.h` | Core type definitions, cell structure |
| `context.c/h` | Memory management, GC, cell allocation |
| `reader.c/h` | S-expression parser, tokenizer |
| `writer.c/h` | S-expression printer with cycle detection |
| `eval.c/h` | CPS interpreter (fallback mode) |
| `env.c/h` | Environment frames, variable binding |
| `primitives.c/h` | Built-in procedures (~150 primitives) |
| `macros.c/h` | Hygienic macro expander |
| `bignum.c/h` | Arbitrary precision integer arithmetic |
| `stdlib.scm` | Standard library (Scheme code) |

### Memory Model

The interpreter uses a generational garbage collector:

- **Nursery**: Semispace copying collector (Cheney's algorithm)
- **Old Generation**: Mark-and-sweep for long-lived objects
- **Cells**: 12-byte tagged unions containing type, car/cdr or numeric value
- **Reserved Space**: Cells 0-271 (nil, booleans, atoms, integer cache 0-255)

### Bytecode VM

The default execution mode compiles Scheme to bytecode:

```
┌──────────┐    ┌──────────┐    ┌──────────┐
│  Parse   │───>│ Compile  │───>│ Execute  │
│ (reader) │    │(bytecode)│    │   (VM)   │
└──────────┘    └──────────┘    └──────────┘
```

Key optimizations:
- Peephole optimization (fused compare-and-jump, etc.)
- Specialized opcodes for common operations (car, cdr, +, -, etc.)
- Constant folding for pure operations
- Tail call optimization via dedicated TAILCALL opcode

### Type System

Cell types are tagged with a 4-bit enum:

| Type | Description |
|------|-------------|
| `BT_ATOM` | Interned symbol |
| `BT_NUM` | Exact integer (64-bit) |
| `BT_BIGNUM` | Arbitrary precision integer |
| `BT_RATIONAL` | Exact rational (num/denom) |
| `BT_INEXACT` | IEEE 754 double |
| `BT_COMPLEX` | Complex number (real/imag) |
| `BT_CHAR` | Unicode character |
| `BT_STRING` | Mutable string |
| `BT_CONS` | Pair (car/cdr) |
| `BT_FUNCTION` | Lambda closure |
| `BT_PRIMOP` | Built-in procedure |
| `BT_VECTOR` | Fixed-size array |
| `BT_SYNTAX` | syntax-rules transformer |
| `BT_CONT` | First-class continuation |
| `BT_CLOSURE` | VM closure (bytecode) |
| `BT_VMCONT` | VM continuation |

## Testing

### Scheme Test Suite

```bash
make test
```

Runs `test.scm` which exercises all language features including arithmetic,
list operations, macros, continuations, and the standard library.

### C Unit Tests

```bash
make test-c
```

Runs four test suites:
- `test_bignum` - Arbitrary precision arithmetic
- `test_reader` - S-expression parsing
- `test_context` - Memory management and GC
- `test_macros` - Pattern matching and expansion

## Configuration

Key constants in `types.h`:

```c
#define SEMISPACE_SIZE (1 << 24)  // 16M cells per semispace
#define HEAP_RESERVED 272         // Reserved cells (atoms + integer cache)
#define INITIAL_STRING_CAP 32     // Initial string buffer size
```

## License

MIT

## References

- R5RS: [Revised^5 Report on Scheme](https://schemers.org/Documents/Standards/R5RS/)
- SICP: [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/index.html)
- Cheney GC: ["A Nonrecursive List Compacting Algorithm"](https://dl.acm.org/doi/10.1145/362790.362798)
