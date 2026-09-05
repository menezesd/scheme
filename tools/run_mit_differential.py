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
      (with-exception-handler (lambda (e) (k 'ERROR)) thunk))))
(define (%emit thunk) (write (%probe thunk)) (newline))
"""

# Probes are single expressions. Each is wrapped in %probe so that an error is
# reported as the symbol ERROR in every implementation rather than dropping
# one of them into a REPL.
PROBES = [
    # --- numeric tower ---
    "(+ 1 2)",
    "(exact->inexact 1/3)",
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
    "(reverse '(1 2 3))",
    "(list-copy '(1 2 . 3))",
    "(last-pair '(1 2 3))",
    "(append '(1) '(2) 3)",
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
    "(symbol->string 'Abc)",
    "(symbol->string (string->symbol \"Abc\"))",
    "(eq? (string->symbol \"abc\") 'abc)",

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
    "(let ((p (make-parameter 1))) (list (p) (parameterize ((p 2)) (p)) (p)))",
    # Ordinary non-tail recursion should not hit an engine limit at this depth.
    "(let ((f (lambda (f n) (if (= n 0) '() (cons n (f f (- n 1))))))) (length (f f 100000)))",
]

# Probes where vesper deliberately differs from MIT. Keep this list short and
# justified; each entry is (probe, reason).
EXPECTED_DEVIATIONS = {
    # MIT's vector-copy! copies backwards, which corrupts this overlap. R7RS
    # requires the result to be as if the source were copied first.
    "(let ((v (vector 1 2 3 4 5))) (vector-copy! v 0 v 2 5) v)":
        "MIT copies overlapping ranges backwards; R7RS requires copy-first semantics",
}


def normalize(text):
    """Fold away formatting-only differences between the two writers."""
    text = text.strip()
    # MIT writes .5 and 1. where vesper writes 0.5 and 1.0
    text = re.sub(r"(?<![0-9a-zA-Z_.])(-?)\.([0-9])", r"\g<1>0.\g<2>", text)
    text = re.sub(r"([0-9])\.(?![0-9])", r"\g<1>.0", text)
    # exponent shape: 1e+21 / 1e-07 vs 1e21 / 1e-7
    text = re.sub(r"e\+?(-?)0*(\d)", r"e\g<1>\g<2>", text)
    # MIT drops a zero real part from a complex number: +2i vs 0+2i
    text = re.sub(r"(?<![0-9a-zA-Z_.])0\.?0*\+", "+", text)
    # vesper writes non-ASCII chars as hex escapes
    text = text.lower()
    return text


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
    if NUMBER.sub("#", a) != NUMBER.sub("#", b):
        return False
    for x, y in zip(NUMBER.findall(a), NUMBER.findall(b)):
        fx, fy = float(x), float(y)
        if fx == fy:
            continue
        if abs(fx - fy) > 1e-12 * max(abs(fx), abs(fy)):
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
            return "<TIMEOUT>"
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
        lines = [normalize(line)
                 for line in run_scheme(argv_for, source).splitlines()]
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
