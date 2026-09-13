#!/usr/bin/env python3
"""Compare vesper's semantics against MIT/GNU Scheme.

Each probe is evaluated three ways - vesper's bytecode VM, vesper's CPS
interpreter, and mit-scheme - and the three results must agree. Vesper models
MIT deliberately (see README), so a disagreement is either a vesper bug or a
place where the deviation should be recorded in EXPECTED_DEVIATIONS below.

Skips cleanly (exit 0) when mit-scheme is not installed, so `make test-diff`
stays usable on machines without it.
"""

import os
import math
import re
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
VESPER = os.path.join(ROOT, "vesper")

PRELUDE = """\
(define (%probe thunk)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler (lambda (e) (k 'error)) thunk))))
(define (%emit thunk) (write (%probe thunk)) (newline))
"""

# Probes are single expressions. Each is wrapped in %probe so that an error is
# reported as the symbol error in every implementation rather than dropping
# one of them into a REPL.
PROBES = [
    # Initial ports retain their identity and participate in parameterize.
    "(list (eq? (current-input-port) (current-input-port)) (eq? (current-output-port) (current-output-port)) (eq? (current-error-port) (current-error-port)))",
    '(parameterize ((current-output-port (open-output-string))) (display "hello") (get-output-string (current-output-port)))',
    '(parameterize ((current-input-port (open-input-string "42"))) (read))',
    '(parameterize ((current-error-port (open-output-string))) (display "oops" (current-error-port)) (get-output-string (current-error-port)))',
    '(let ((old (current-output-port)) (p (open-output-string))) (dynamic-wind (lambda () #f) (lambda () (current-output-port p) (eq? (current-output-port) p)) (lambda () (set-current-output-port! old))))',
    '(let ((p (open-output-string))) (close-output-port p) (parameterize ((current-output-port p)) (eq? p (current-output-port))))',
    '(let ((p (open-input-string "x"))) (close-input-port p) (parameterize ((current-input-port p)) (eq? p (current-input-port))))',
    '(parameterize ((current-output-port 42)) #t)',
    '(parameterize ((current-output-port (open-input-string "x"))) #t)',
    '(let ((p (open-output-bytevector))) (parameterize ((current-output-port p)) (write-u8 65)) (get-output-bytevector p))',
    '(let ((p (open-output-bytevector))) (parameterize ((current-error-port p)) (write-u8 65 (current-error-port))) (get-output-bytevector p))',
    '(let ((p (open-output-string))) (display "λ𝄞" p) (close-output-port p) (close-output-port p) (get-output-string p))',
    '(call-with-output-string (lambda (p) (display "text" p) (close-output-port p)))',
    "(list 1/2@0 #x10@0 .5@0 1@-0.0)",
    '(map string->number \'("2@1" "1@1/2" "#e1@0" "#i2@0" "#x10@1" "1@2@3" "1@+i"))',
    "(list (make-polar 1/2 0) (make-polar +inf.0 0) (make-polar 0 1) (make-polar 0.0 -1) (make-polar 2 -0.0))",
    "(let ((r (expt 10 400))) (= (make-polar r 0) r))",
    "(let ((x '#1=(1 #1#))) (eq? (cadr x) x))",
    "(let ((x '#1=#2=#(#1#))) (eq? (vector-ref x 0) x))",
    "(let-syntax ((identity (syntax-rules () ((_ x) x)))) (let ((x (identity '#1=(a b))) (y (identity '#1#))) (eq? x y)))",
    "(let-syntax ((m (syntax-rules () ((m) '(|##protected##| . x))))) (m))",
    "(list (list= = '(2) '(1 . tail)) (list= = '() '(1 . tail)) (list= = '(2) '(1) 'tail))",
    "(let ((x (cons 1 'tail))) (list= = x x))",
    "(let ((x (circular-list 1 2))) (list= = x x))",
    "(list= = '(1 2) (circular-list 1 2))",
    "(let ((tail (list 2 3)) (count 0)) (list= (lambda (a b) (set! count (+ count 1)) (= a b)) (cons 1 tail) (cons 1 tail)) count)",
    "(eval '(parameterize (((make-parameter 1) 2)) 3) (environment '(scheme base)))",
    '(eval \'(guard (e (#t 42)) (error "imported guard")) (environment \'(scheme base)))',
    # User identifiers resembling implementation names still obey hygiene.
    "(let-syntax ((m (syntax-rules () ((m) |##gensym##999999|)))) (let ((|##gensym##999999| 7)) (m)))",
    "(let ((|##gensym##999998| 42)) (let-syntax ((m (syntax-rules () ((m) |##gensym##999998|)))) (let ((|##gensym##999998| 7)) (m))))",
    # --- numeric tower ---
    "(+ 1 2)",
    "(exact->inexact 1/3)",
    "(- 1/2+3/4i)",
    "(/ 1/2+3/4i)",
    "(inexact->exact (make-rectangular 1/3 0.5))",
    "(inexact->exact (make-rectangular 0.5 1/3))",
    "(inexact->exact (make-rectangular 9007199254740993 0.5))",
    "(inexact->exact (make-rectangular +inf.0 1.0))",
    "(inexact->exact (make-rectangular 1.0 +inf.0))",
    "(expt 2 100)",
    "(expt 2 -1)",
    "(expt 1/2 3)",
    "(exact (/ 1 3))",
    "(round 2.5)",
    "(round 3.5)",
    "(round -2.5)",
    "(round 7/2)",
    "(floor -7/2)",
    "(ceiling -7/2)",
    "(truncate -7/2)",
    "(rationalize 1/1000 0)",
    "(rationalize -1/1000 -1/1000000)",
    "(rationalize 9007199254740993 0)",
    "(rationalize -3/2 1/2)",
    "(let ((z (make-rectangular (expt 10 400) 1))) (list (finite? z) (infinite? z) (nan? z)))",
    "(quotient -7 2)",
    "(remainder -7 2)",
    "(modulo -7 2)",
    "(gcd 0 0)",
    "(gcd 4 6)",
    "(lcm 4 6)",
    # inexactness contagion (R7RS 6.2.6)
    "(min 1 2.0)",
    "(max 1.0 2)",
    "(min 5 3 4.0)",
    "(min 1e300 1/2)",
    "(gcd 0.0 1)",
    "(gcd 4.0 6)",
    "(lcm 0.0 1)",
    "(lcm 4.0 6)",
    "(exact-integer? (round (max 1e-10 1.5 7/2)))",
    "(number->string 255 16)",
    "(number->string 1/3)",
    "(string->number \"#b101\")",
    "(string->number \"#o777\")",
    "(string->number \"#x-ff\")",
    "(string->number \"1/2\" 16)",
    "(= (real-part (string->number \"+inf.0+1.0i\")) +inf.0)",
    "(nan? (imag-part (string->number \"1+NaN.0i\")))",
    "(= (string->number \"+I\") 0+1i)",
    "(eqv? (string->number \"#e1.234567890123456789+1.0i\") 1234567890123456789/1000000000000000000+1i)",
    "(inexact? (real-part (string->number \"#i1+2i\")))",
    "(string->number \"#x1e-2i\")",
    "(string->number \"#e+inf.0+1i\")",
    "(read (open-input-string \"#e1.5+2.5i\"))",
    "(= (read (open-input-string \"#i+i\")) 0+1i)",
    "(read (open-input-string \"#d+inf.0\"))",
    "(read (open-input-string \".5+2i\"))",
    "(eqv? (string->number \"#i-0\") -0.0)",
    "(number->string 30-2i 16)",
    "(number->string 1/2+3/4i 2)",
    "(let* ((z (make-rectangular 1 (expt 10 400))) (s (number->string z))) (= z (string->number s)))",
    "(let ((s (number->string (make-rectangular 1.0 -0.0)))) (eqv? (imag-part (string->number s)) -0.0))",
    "(/ 1.0 0.0)",
    "(< 1 +inf.0)",
    "(sqrt 4)",
    "(sqrt 1/4)",
    "(sqrt -4)",
    "(log -1)",
    "(asin 2)",
    "(acos 2)",
    "(asin -2)",
    "(asin 0.5)",
    "(acos 0.5)",
    "(sin (asin 2))",
    "(atan 1 1)",
    "(atan 1+1i)",
    "(atan 0+2i)",
    "(imag-part (atan 0+1i))",
    "(imag-part (atan 0-1i))",
    "(imag-part (sqrt (make-rectangular -4.0 -0.0)))",
    "(sqrt 1.7e308+1.7e308i)",
    "(log 1.7e308+1.7e308i)",
    "(imag-part (asin 0.0+1e100i))",
    "(asin (make-rectangular 2.0 0.0))",
    "(acos (make-rectangular -2.0 -0.0))",
    "(sqrt1pm1 1.0+1e-20i)",
    "(sqrt1pm1 +inf.0)",
    "(log1p 1e-20+1e-20i)",
    "(log1p 0.0+1e-20i)",
    "(expm1 0.0+1e-20i)",
    "(log1p 1.7e308+1.7e308i)",
    "(= (expt 0.0+0.0i 2+1i) 0)",
    "(= (expt 0.0+0.0i 0) 1)",
    "(expt 0.0+0.0i -1)",
    "(imag-part (sqrt (exact->inexact (make-rectangular -4.0 -0.0))))",
    "(numerator 0.5)",
    "(denominator 0.5)",
    "(rationalize 3/10 1/10)",
    "(expt 2 1000)",
    "(* (expt 2 500) (expt 2 500))",
    "(gcd (expt 2 100) (expt 2 50))",
    "(quotient (expt 10 30) 7)",
    "(remainder (expt 10 30) 7)",
    "(eqv? 2 2.0)",
    "(eqv? 0.0 -0.0)",
    "(exact? 1/2)",
    "(integer? 2.0)",
    "(rational? 2.5)",

    # --- lists, strings, vectors, chars ---
    "(memq 2 '(1 2 . tail))",
    "(memv 2 '(1 2 . tail))",
    "(member 2 '(1 2 . tail))",
    "((member-procedure =) 2 '(1 2 . tail))",
    "(memq 2 '(1 . tail))",
    "(memv 2 '(1 . tail))",
    "(member 2 '(1 . tail))",
    "(let ((calls 0)) (let ((result (call-with-values (lambda () (span (lambda (x) (set! calls (+ calls 1)) (< calls 3)) '(a b c d))) list))) (list result calls)))",
    "(let ((calls 0)) (let ((result (call-with-values (lambda () (break (lambda (x) (set! calls (+ calls 1)) (>= calls 3)) '(a b c d))) list))) (list result calls)))",
    "(find even? (circular-list 1 2 3))",
    "(any even? (circular-list 1 2 3))",
    "(every odd? (circular-list 1 2 3))",
    "(list-index even? (circular-list 1 2 3))",
    "(take-while odd? (circular-list 1 3 2))",
    "(let ((c (circular-list 1 3 2))) (eq? (drop-while odd? c) (cddr c)))",
    "(let ((c (circular-list 1 3 2))) (eq? (find-tail even? c) (cddr c)))",
    "(let ((c (circular-list 1 3 2))) (eq? (member 3 c) (cdr c)))",
    "(reverse '(1 2 3))",
    "(list-copy '(1 2 . 3))",
    "(last-pair '(1 2 3))",
    "(append '(1) '(2) 3)",
    "(concatenate '((1) (2 . tail)))",
    "(concatenate '(tail))",
    "(concatenate! (list (cons 1 'discarded) (cons 2 'tail)))",
    "(call-with-values (lambda () (unzip1 '((1 2 3 4 5 . tail)))) list)",
    "(call-with-values (lambda () (unzip2 '((1 2 3 4 5 . tail)))) list)",
    "(call-with-values (lambda () (unzip3 '((1 2 3 4 5 . tail)))) list)",
    "(call-with-values (lambda () (unzip4 '((1 2 3 4 5 . tail)))) list)",
    "(call-with-values (lambda () (unzip5 '((1 2 3 4 5 . tail)))) list)",
    "(assoc 2.0 '((1 a) (2 b)))",
    "(member 2.0 '(1 2 3))",
    "(assoc \"a\" '((\"a\" . 1)))",
    "(substring \"hello\" 1 3)",
    "(string-copy \"hello\" 1)",
    "(string-append \"a\" \"b\" \"c\")",
    "(string-upcase \"straße\")",
    "(string<? \"a\" \"b\" \"c\")",
    "(char-ci=? #\\A #\\a)",
    "(char->integer #\\newline)",
    "(equal? (vector 1 2) (vector 1 2))",
    "(vector->list (vector 1 2 3) 1)",
    "(string->list \"abc\" 1)",
    "(let ((v (vector 1 2 3 4 5))) (vector-copy! v 0 v 2 5) v)",
    "(apply + 1 2 '(3 4))",
    "(map + '(1 2) '(3 4))",
    "(map + '(1) '(4 . tail))",
    "(map + '(4 5 . tail) '(1))",
    "(filter-map + '(1) '(4 . tail))",
    "(count < '(1) '(4 . tail))",
    "(append-map list '(1) '(4 . tail))",
    "(append-map! list '(1) '(4 . tail))",
    "(fold + 0 '(1) '(4 5 . tail))",
    "(fold-right + 0 '(1) '(4 5 . tail))",
    "(fold + 0 '(1) '(4 . tail))",
    "(fold-right + 0 '(1) '(4 . tail))",
    "(pair-fold (lambda (a b acc) (+ (car a) (car b) acc)) 0 '(1) '(4 . tail))",
    "(pair-fold-right (lambda (a b acc) (+ (car a) (car b) acc)) 0 '(1) '(4 . tail))",
    "(let ((sum 0)) (for-each (lambda (a b) (set! sum (+ sum a b))) '(1) '(4 . tail)) sum)",
    "(let ((sum 0)) (pair-for-each (lambda (a b) (set! sum (+ sum (car a) (car b)))) '(1) '(4 . tail)) sum)",
    "(zip '(1 2 3) (circular-list 'a 'b))",
    "(map list '(1) '() 'unused)",
    "(count < '(1) '() 'unused)",
    "(pair-fold #f 9 '(1) '() 'unused)",
    "(zip '(1) '() 'unused)",
    "(let ((saved #f) (first #f) (again? #f)) (let ((result (map (lambda (x) (if (= x 2) (call-with-current-continuation (lambda (k) (set! saved k) x)) x)) '(1 2 3)))) (if again? (list first result) (begin (set! first result) (set! again? #t) (saved 20)))))",
    "(let ((saved #f) (first #f) (again? #f)) (let ((result (map (lambda (x y) (if (= x 2) (call-with-current-continuation (lambda (k) (set! saved k) (+ x y))) (+ x y))) '(1 2 3) '(10 20 30)))) (if again? (list first result) (begin (set! first result) (set! again? #t) (saved 200)))))",
    "(symbol->string 'Abc)",
    "(symbol->string (string->symbol \"Abc\"))",
    "(eq? (string->symbol \"abc\") 'abc)",
    "(eq? (string->symbol \"123\") (read (open-input-string \"|123|\")))",
    "(let () (define-record-type thing (make-thing x) thing? (x thing-x)) (let ((ctor make-thing)) (set! make-thing (lambda (x) x)) (thing? (ctor 2))))",
    "(let () (define-record-type thing (make-thing x) thing? (x thing-x set-thing-x!)) (let ((obj (make-thing 1))) (set! thing? (lambda (x) #f)) (set-thing-x! obj 2) (thing-x obj)))",
    "(let () (define-record-type thing (make-thing make-thing) thing? (make-thing thing-x)) (let ((obj (make-thing 1))) (list (thing? obj) (thing-x obj))))",

    # --- control, macros, exceptions ---
    "(let loop ((i 0) (a 0)) (if (= i 10) a (loop (+ i 1) (+ a i))))",
    "(do ((i 0 (+ i 1)) (a '() (cons i a))) ((= i 3) a))",
    "(case 2 ((1) 'one) ((2 3) 'two) (else 'other))",
    "(case 'a ((a) => (lambda (x) (list x x))) (else 'no))",
    "(case 'z ((a) 'no) (else => list))",
    "(cond ((assv 1 '((1 2))) => cadr) (else 'no))",
    "(call-with-current-continuation (lambda (k) (+ 1 (k 42))))",
    "(call-with-values (lambda () (values 1 2)) list)",
    "(let-values (((a b) (values 1 2))) (list a b))",
    "(dynamic-wind (lambda () 1) (lambda () 2) (lambda () 3))",
    "(call-with-current-continuation (lambda (k) (dynamic-wind (lambda () 'in) (lambda () (k 7)) (lambda () 'out))))",
    "(guard (e (#t (list 'caught e))) (raise 'boom))",
    "(guard (e ((symbol? e) 'sym)) (raise 'x))",
    "(guard (e (#t 'outer)) (guard (e ((string? e) 'nope)) (raise 'sym)))",
    "(with-exception-handler (lambda (e) 99) (lambda () (raise-continuable 'c)))",
    "(with-exception-handler (lambda (e) 99) (lambda () (+ 1 (raise-continuable 1))))",
    # A handler returning from a non-continuable raise must raise a secondary
    # exception that an enclosing guard can catch (R7RS 6.11), not abort.
    "(guard (e (#t 'caught)) (with-exception-handler (lambda (e) 'ret) (lambda () (raise 'x))))",
    "(call/cc (lambda (k) (with-exception-handler (lambda (e) (k 'outer)) (lambda () (with-exception-handler (lambda (e) 'ret) (lambda () (raise 'x)))))))",
    "(let ((x 'outer)) (define-syntax m (syntax-rules () ((_) x))) (let ((x 'inner)) (m)))",
    "(let ((tmp 1) (other 2)) (define-syntax sw (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp))))) (sw tmp other) (list tmp other))",
    "(let-syntax ((m (syntax-rules () ((_ x) (+ x 1))))) (m 5))",
    "(let ((case-key 5)) (case 1 ((1) case-key)))",
    "(let ((p (delay 1))) (list (force p) (force p)))",
    "(promise? (force (delay (delay 42))))",
    "(force (force (delay (delay 42))))",
    "(let ((first #t) (p #f)) (set! p (delay (if first (begin (set! first #f) (+ 1 (force p))) 2))) (list (force p) (force p)))",
    "(let ((count 0)) (let* ((p (delay (begin (set! count (+ count 1)) 42))) (q (delay-force p))) (force q) (force p) count))",
    "(let ((p (make-parameter 1))) (list (p) (parameterize ((p 2)) (p)) (p)))",
    "(let ((p (make-parameter 1)) (q (make-parameter 2))) (parameterize ((p 10) (q (p))) (list (p) (q))))",
    "(letrec ((f (lambda (x) (begin (begin (define (g) x))) (g)))) (f 42))",
    "(let-syntax ((h (syntax-rules () ((_) 99)))) (letrec ((f (lambda (x) (define (g) (h)) (begin (begin (define (h) x))) (g)))) (f 42)))",
    # Ordinary non-tail recursion should not hit an engine limit at this depth.
    "(let ((f (lambda (f n) (if (= n 0) '() (cons n (f f (- n 1))))))) (length (f f 100000)))",
]

def list_boundary_probes():
    """Exercise traversal boundaries without following an endless cycle.

    A successful search may stop before a dotted tail; indexed operations
    need only the requested prefix. Mutation cases receive fresh list copies.
    """
    probes = []
    for shape in ("'()", "'tail", "'(1 . tail)", "'(1 2 3 . tail)", "'(1 2 3)"):
        source = f"(list-copy {shape})"
        for proc in ("list-ref", "list-tail", "take", "drop", "take-right",
                     "drop-right", "take!", "drop-right!"):
            for count in (0, 1, 3, 4):
                probes.append(f"({proc} {source} {count})")
        for proc in ("split-at", "split-at!"):
            for count in (0, 1, 3, 4):
                probes.append(
                    f"(call-with-values (lambda () ({proc} {source} {count})) list)")
        for proc in ("find", "find-tail", "take-while", "drop-while",
                     "take-while!", "list-index", "any", "every"):
            for predicate in ("even?", "odd?"):
                probes.append(f"({proc} {predicate} {source})")
        for proc in ("span", "break", "span!", "break!", "partition", "partition!"):
            probes.append(
                f"(call-with-values (lambda () ({proc} even? {source})) list)")
        for proc in ("filter", "remove", "filter!", "remove!"):
            probes.append(f"({proc} even? {source})")
        for proc in ("length", "length+", "last-pair", "last", "reverse",
                     "reverse!", "list-copy"):
            probes.append(f"({proc} {source})")
    probes.extend([
        "(let ((c (circular-list 1 2 3))) (list-ref c 8))",
        "(let ((c (circular-list 1 2 3))) (eq? (list-tail c 8) (cddr c)))",
        "(let ((c (circular-list 1 2 3))) (list-set! c 7 9) (take c 3))",
        "(let ((c (circular-list 1 2 3))) (take! c 5))",
        "(let ((calls 0)) (let ((result (call-with-values (lambda () (partition! (lambda (x) (set! calls (+ calls 1)) (odd? calls)) (list 1 2 3))) list))) (list result calls)))",
    ])
    return probes


PROBES.extend(list_boundary_probes())


# Probes where vesper deliberately differs from MIT. Keep this list short and
# justified; each entry is (probe, reason).
EXPECTED_DEVIATIONS = {
    # MIT's vector-copy! copies backwards, which corrupts this overlap. R7RS
    # requires the result to be as if the source were copied first.
    "(let ((v (vector 1 2 3 4 5))) (vector-copy! v 0 v 2 5) v)":
        "MIT copies overlapping ranges backwards; R7RS requires copy-first semantics",
}


DATUM_TOKEN = re.compile(
    r'#\\(?:[^\s()]+|.)|"(?:\\.|[^"\\])*"|\|(?:\\.|[^|\\])*\|'
    r'|[()]|\s+|[^()\s]+')
NUMERIC_START = re.compile(
    r'^[+-]?(?:[0-9]|\.[0-9])|^[+-](?:inf\.0|nan\.0|i$)', re.IGNORECASE)


def normalize_number(text):
    text = text.lower()
    # MIT writes .5 and 1. where vesper writes 0.5 and 1.0
    text = re.sub(r"(?<![0-9a-zA-Z_.])(-?)\.([0-9])", r"\g<1>0.\g<2>", text)
    text = re.sub(r"([0-9])\.(?![0-9])", r"\g<1>.0", text)
    # exponent shape: 1e+21 / 1e-07 vs 1e21 / 1e-7
    text = re.sub(r"e\+?(-?)0*(\d)", r"e\g<1>\g<2>", text)
    # MIT drops a zero real part from a complex number: +2i vs 0+2i
    text = re.sub(r"(?<![0-9a-zA-Z_.])0\.?0*\+", "+", text)
    return text


def normalize(text):
    """Normalize number spelling while preserving literal data and symbols."""
    return "".join(normalize_number(token) if NUMERIC_START.match(token) else token
                   for token in DATUM_TOKEN.findall(text.strip()))


NUMBER = re.compile(r"-?\d+\.\d+(?:e-?\d+)?")


def equivalent(a, b):
    """Equal after normalization, allowing last-ULP drift in inexact numbers.

    Vesper and MIT reach the same inexact value by different routes (asin/acos
    via the complex logarithm, say), so results can differ in the final digit.
    Compare the non-numeric skeleton exactly and the floats with a relative
    tolerance.
    """
    if a == b:
        return True
    left = DATUM_TOKEN.findall(a)
    right = DATUM_TOKEN.findall(b)
    if len(left) != len(right):
        return False
    for first, second in zip(left, right):
        if first == second:
            continue
        if not (NUMERIC_START.match(first) and NUMERIC_START.match(second)):
            return False
        if NUMBER.sub("#", first) != NUMBER.sub("#", second):
            return False
        for x, y in zip(NUMBER.findall(first), NUMBER.findall(second)):
            x, y = float(x), float(y)
            if x == y == 0 and math.copysign(1, x) != math.copysign(1, y):
                return False
            if not math.isclose(x, y, rel_tol=1e-12, abs_tol=0):
                return False
    return True


def build_source(probes):
    """One program that prints one line per probe.

    Running every probe in a single process keeps this usable inside
    `make test-all`: mit-scheme takes about a second to start, so a process
    per probe per implementation costs minutes. %probe contains errors, so one
    failing probe cannot swallow the ones after it.
    """
    body = "".join("(%%emit (lambda () %s))\n" % probe for probe in probes)
    return PRELUDE + body


def run_scheme(argv_for, source, timeout=600):
    handle, path = tempfile.mkstemp(suffix=".scm")
    try:
        with os.fdopen(handle, "w") as f:
            f.write(source)
        try:
            # stdin must be closed: mit-scheme drops into its REPL after
            # --load and would otherwise wait on an inherited stdin forever.
            done = subprocess.run(argv_for(path), capture_output=True,
                                  text=True, timeout=timeout,
                                  stdin=subprocess.DEVNULL)
        except subprocess.TimeoutExpired:
            raise RuntimeError(f"timed out after {timeout}s") from None
        if done.returncode != 0:
            detail = done.stderr.strip()
            raise RuntimeError(
                f"exited with status {done.returncode}"
                + (f"\n{detail}" if detail else ""))
        out = done.stdout
        # vesper's --interpreter banner is not part of the result
        return "\n".join(line for line in out.split("\n")
                         if not line.startswith("; CPS interpreter"))
    finally:
        os.unlink(path)


def main():
    mit = shutil.which("mit-scheme")
    if not mit:
        print("SKIP: mit-scheme not installed; differential probes not run")
        return 0
    if not os.path.exists(VESPER):
        print("FAIL: %s not built" % VESPER)
        return 1

    source = build_source(PROBES)
    engines = [
        ("vm", lambda path: [VESPER, path]),
        ("cps", lambda path: [VESPER, "--interpreter", path]),
        ("mit", lambda path: [mit, "--quiet", "--load", path, "--eval", "(exit)"]),
    ]

    results = {}
    for name, argv_for in engines:
        try:
            output = run_scheme(argv_for, source)
        except (OSError, RuntimeError) as error:
            print(f"FAIL: {name}: {error}")
            return 1
        lines = [normalize(line) for line in output.splitlines()]
        if len(lines) != len(PROBES):
            print("FAIL: %s produced %d result lines for %d probes"
                  % (name, len(lines), len(PROBES)))
            if len(lines) < len(PROBES):
                print("      stopped at probe: %s" % PROBES[len(lines)])
            return 1
        results[name] = lines

    failures = []
    for i, probe in enumerate(PROBES):
        vm, cps, ref = results["vm"][i], results["cps"][i], results["mit"][i]
        if not equivalent(vm, cps):
            failures.append((probe, "VM and CPS interpreter disagree", vm, cps, ref))
            continue
        if not equivalent(vm, ref) and probe not in EXPECTED_DEVIATIONS:
            failures.append((probe, "vesper and MIT disagree", vm, cps, ref))

    for probe, why, vm, cps, ref in failures:
        print("FAIL: %s" % probe)
        print("      %s" % why)
        print("      vm  : %s" % vm)
        print("      cps : %s" % cps)
        print("      mit : %s" % ref)

    deviations = len(EXPECTED_DEVIATIONS)
    print("%d probes, %d failures (%d expected deviations skipped)"
          % (len(PROBES), len(failures), deviations))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
