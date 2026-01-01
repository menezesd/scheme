# Scheme Interpreter

A compact R5RS-inspired Scheme interpreter written in C, featuring a semispace
garbage collector, trampoline-based CPS evaluator, hygienic macros, and full
numeric tower support.

## Features

- **Garbage Collection**: Semispace copying collector with Cheney's algorithm
- **Tail Call Optimization**: Trampoline-based continuation-passing style (CPS)
- **Numeric Tower**: Integers, bignums, rationals, floats, and complex numbers
- **Hygienic Macros**: Full `syntax-rules` with referential transparency and hygiene
- **SRFI Support**: SRFI-1 (list library), SRFI-9 (records), SRFI-26 (cut/cute)
- **Standard Library**: Comprehensive stdlib with list utilities, higher-order
  functions, and more
- **Ports**: File I/O and string ports with standard Scheme port operations
- **Vectors**: Mutable fixed-size arrays with standard vector operations

## Building

```bash
make          # Build the interpreter
make debug    # Build with debug symbols and sanitizers
make test     # Run Scheme test suite
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
./lisp
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
./lisp script.scm
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
| `types.h` | Core type definitions, cell structure, constants |
| `context.c/h` | Memory management, GC, cell allocation |
| `reader.c/h` | S-expression parser, tokenizer |
| `writer.c/h` | S-expression printer with cycle detection |
| `eval.c/h` | Trampoline evaluator, special forms |
| `env.c/h` | Environment frames, variable binding |
| `primitives.c/h` | Built-in procedures (~150 primitives) |
| `macros.c/h` | Hygienic macro expander (mark-based hygiene) |
| `bignum.c/h` | Arbitrary precision integer arithmetic |
| `stdlib.scm` | Standard library (Scheme code) |

### Memory Model

The interpreter uses a semispace copying garbage collector:

- **Heap**: Two semispaces of 16M cells each (configurable via `SEMISPACE_SIZE`)
- **Cells**: 12-byte tagged unions containing type, car/cdr or numeric value
- **Reserved Space**: Cells 0-14 are permanent atoms (nil, #t, quote, etc.)
- **Allocation**: Bump pointer within current semispace
- **Collection**: Cheney's algorithm copies live objects to other semispace

### Evaluation Model

The evaluator uses trampolined continuation-passing style to achieve proper
tail call optimization without growing the C stack:

```
┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│  TRAMP_EVAL  │───>│ TRAMP_APPLY  │───>│  TRAMP_DONE  │
│  (evaluate)  │    │ (apply cont) │    │  (finished)  │
└──────────────┘    └──────────────┘    └──────────────┘
       │                   │
       └───────────────────┘
         (tail calls loop back)
```

### Type System

Cell types are tagged with a 4-bit enum:

| Type | Description |
|------|-------------|
| `BT_NIL` | Empty list / false |
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
| `BT_MACRO` | Macro transformer |
| `BT_SYNTAX` | syntax-rules object |
| `BT_CONT` | First-class continuation |

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
#define HEAP_RESERVED 15          // Reserved permanent atoms
#define INITIAL_STRING_CAP 64     // Initial string buffer size
#define CHAR_NAME_BUF_SIZE 16     // Character name buffer
```

## Limitations

- No first-class environments
- No weak references
- Single-threaded only
- No module system

## License

MIT

## References

- R5RS: [Revised^5 Report on Scheme](https://schemers.org/Documents/Standards/R5RS/)
- SICP: [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/index.html)
- Cheney GC: ["A Nonrecursive List Compacting Algorithm"](https://dl.acm.org/doi/10.1145/362790.362798)
