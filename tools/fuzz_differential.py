#!/usr/bin/env python3
"""Differential fuzzer: vesper's bytecode VM against its CPS interpreter.

Generates random, terminating Scheme programs and runs each under
`./vesper` and `./vesper --interpreter`; any difference in standard output
or exit status, or an engine that crashes, panics or hangs, is a
divergence. With --mit the programs are restricted to constructs whose
results MIT/GNU Scheme prints the same way (errors print as a bare <error>,
no flonums inside strings, none of the constructs MIT 12.1 handles
differently), and MIT becomes a third reference.

The generator is typed: every expression is produced for a result type
(exact integer, rational, flonum, boolean, char, string, symbol, list of
integers, vector of integers, or any printable datum), so programs mostly run
without errors and print deterministic values - never procedures, records,
hash tables or other objects whose printed form carries addresses or hash
numbers - and side effects are sequenced explicitly, never left to the
unspecified order of argument evaluation. Loops count down from small
literals, recursion is bounded by its argument, global procedures may only
call procedures defined before them, and multiplication is rationed, so
every program terminates and its numbers stay moderate. Covered: exact
arithmetic (fixnum boundaries, bignums, rationals), flonums, lists, vectors,
strings, chars, symbols, closures, let/let*/letrec/named let/do, internal and
top-level define, set!, call/cc escapes and re-entry (also into a
dynamic-wind), dynamic-wind, values, guard/raise/raise-continuable, error
objects and the engines' own error messages, parameterize, promises, string
ports, hash tables, records, top-level and local syntax-rules macros
(including hygiene probes), quasiquote (nested, vectors, splicing, dotted),
tail calls in every tail context over 10^5 iterations, and deep non-tail
recursion.

Programs run in batches: one process per engine executes many programs, each
statement wrapped so that an error prints a line instead of aborting the
file. A batch whose outputs differ is split into its programs, each confirmed
on its own and then shrunk - top-level forms deleted, subexpressions replaced
by their children or by simple atoms - for as long as the engines still part
the same way (same kind of divergence, same exit statuses, same pair of
messages on the first differing line).

Usage:
  tools/fuzz_differential.py [--seed N] [--count N] [--start N] [--depth N]
                             [--mit] [--jobs N] [--batch N] [--timeout S]
                             [--no-shrink] [--save DIR] [--stderr]
                             [--print-program N] [--no-avoid] [--known-bugs]

Known, not yet fixed divergences are listed in KNOWN_BUGS; the generator
steers clear of each one, or the comparison folds its two spellings together
(see --known-bugs), so a run reports only new ones. --no-avoid turns that
off. --stderr also compares standard error.

Exit status is 0 when every program agreed, 1 otherwise. The same seed
always generates the same programs (generation does not depend on run order
or --jobs).
"""

import argparse
import concurrent.futures
import math
import os
import random
import re
import shutil
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
VESPER = os.path.join(ROOT, "vesper")

# ---------------------------------------------------------------------------
# S-expression nodes: a str is an atom (already in source form); a list is a
# compound form. A few heads render specially (abbreviations, vectors and
# dotted lists).
# ---------------------------------------------------------------------------

ABBREV = {"quote": "'", "quasiquote": "`", "unquote": ",",
          "unquote-splicing": ",@"}


def render(node):
    if isinstance(node, str):
        return node
    if not node:
        return "()"
    head = node[0] if isinstance(node[0], str) else None
    if head in ABBREV and len(node) == 2:
        return ABBREV[head] + render(node[1])
    if head == "#vec":
        return "#(" + " ".join(render(x) for x in node[1:]) + ")"
    if head == "#dot":
        return ("(" + " ".join(render(x) for x in node[1:-1]) + " . "
                + render(node[-1]) + ")")
    return "(" + " ".join(render(x) for x in node) + ")"


# ---------------------------------------------------------------------------
# Generator
# ---------------------------------------------------------------------------

INT, RAT, REAL, BOOL, CHAR, STR, SYM, ILIST, VEC, ANY = (
    "int", "rat", "real", "bool", "char", "str", "sym", "ilist", "vec", "any")
PRINTABLE = [INT, RAT, REAL, BOOL, CHAR, STR, SYM, ILIST, VEC]

FIXNUM_EDGES = ["1073741823", "1073741824", "-1073741824", "2147483647",
                "2147483648", "-2147483649", "4294967296", "4611686018427387903",
                "4611686018427387904", "-4611686018427387905",
                "9223372036854775807", "9223372036854775808",
                "-9223372036854775808", "-9223372036854775809",
                "18446744073709551616"]
SYMBOLS = ["a", "b", "c", "foo", "bar", "baz", "x", "y", "hello-world",
           "list", "if", "lambda", "else", "tmp", "k1", "zz"]
ASCII_STRINGS = ['""', '"a"', '"abc"', '"hello"', '"Hello, World"',
                 '"a b c"', '"x\\ny"', '"tab\\there"', '"q\\"uote"',
                 '"back\\\\slash"', '"0123456789"', '"MiXeD"']
UNICODE_STRINGS = ['"λx.x"', '"naïve"', '"日本語"', '"𝄞clef"', '"Straße"']
ASCII_CHARS = ["#\\a", "#\\z", "#\\A", "#\\Q", "#\\0", "#\\9", "#\\space",
               "#\\newline", "#\\(", "#\\;", "#\\x41", "#\\-"]
UNICODE_CHARS = ["#\\λ", "#\\é", "#\\x3bb", "#\\tab", "#\\null", "#\\x7f",
                 "#\\delete", "#\\alarm", "#\\ß"]
FLOATS = ["1.5", "-0.25", "3.0", "0.1", "2.5", "-7.75", "1e10", "2.5e-3",
          "100.0", "-0.0", "0.0", "12345.678", "1e21", "6.02e23"]


# Divergences this fuzzer found that are not fixed yet. Each entry keeps the
# generator away from one bug, so that `make test-fuzz` guards against new
# divergences instead of failing on known ones. Delete an entry once its bug
# is fixed; --no-avoid generates the triggering shapes anyway.
KNOWN_BUGS = {}


class Budget:
    __slots__ = ("depth", "mul")

    def __init__(self, depth, mul):
        self.depth = depth
        self.mul = mul


class Gen:
    def __init__(self, rng, prefix, mit=False, avoid=frozenset(KNOWN_BUGS)):
        self.r = rng
        self.p = prefix
        self.mit = mit
        self.avoid = avoid
        self.counter = 0
        self.recent = []
        self.depth = 3
        # Program-level features: name -> info, filled while generating the
        # program's definitions so statements can use them.
        self.funcs = []      # (name, arity) for (int ...) -> int
        self.recs = []       # (name, arity) bounded recursive int -> int
        self.records = []    # dict(make, pred, getx, gety, setx)
        self.macros = {}     # kind -> name
        self.globals = {}    # type -> [names]
        self.tables = []     # names of global hash tables (int -> int)

    # -- helpers ----------------------------------------------------------
    def fresh(self, base="v"):
        self.counter += 1
        name = "%s%d" % (base, self.counter)
        if self.r.random() < 0.08:
            # Names that collide with macro internals or core keywords
            # exercise hygiene (a macro's tmp/t/lp must not capture them).
            bare = self.r.choice(["tmp", "t", "lp", "loop", "x", "e", "k"])
            # Not one handed out just before: two of them could land in
            # the same binding list, which is a syntax error.
            if self.r.random() < 0.5 and bare not in self.recent:
                name = bare
            else:
                name = bare + str(self.counter)
        self.recent = (self.recent + [name])[-6:]
        return name

    def qq(self, template):
        return ["quasiquote", template]

    def chance(self, p):
        return self.r.random() < p

    def pick(self, options):
        """options: list of (weight, thunk). Calls one thunk."""
        total = sum(w for w, _ in options if w > 0)
        x = self.r.random() * total
        for w, f in options:
            if w <= 0:
                continue
            x -= w
            if x <= 0:
                return f()
        return options[-1][1]()

    def var(self, env, ty):
        names = env.get(ty, ()) + tuple(self.globals.get(ty, ()))
        return self.r.choice(names) if names else None

    @staticmethod
    def bind(env, name, ty):
        new = {k: v for k, v in env.items()}
        # A rebinding shadows the name at every type.
        for k in list(new):
            if name in new[k]:
                new[k] = tuple(n for n in new[k] if n != name)
        new[ty] = new.get(ty, ()) + (name,)
        return new

    def sub(self, b, mul=None):
        return Budget(b.depth - 1, b.mul if mul is None else mul)

    def gen(self, ty, env, b):
        if ty == ANY:
            ty = self.r.choice(PRINTABLE + [ANY])
            if ty == ANY:
                return self.g_any(env, b)
        return getattr(self, "g_" + ty)(env, b)

    def nonzero(self, env, b):
        return self.pick([
            (2, lambda: str(self.r.choice([1, 2, 3, 7, -5, 10, 97, -1]))),
            (1, lambda: self.r.choice(FIXNUM_EDGES[3:])),
            (3, lambda: ["+", "1", ["abs", self.g_int(env, self.sub(b))]]),
        ])

    def small_count(self):
        return str(self.r.choice([0, 1, 2, 3, 4, 5, 7, 10, 12]))

    # -- integers ---------------------------------------------------------
    def int_lit(self):
        return self.pick([
            (6, lambda: str(self.r.randint(-20, 100))),
            (2, lambda: self.r.choice(FIXNUM_EDGES)),
            (2, lambda: ("-" if self.chance(0.3) else "")
             + str(self.r.randint(1, 9))
             + "".join(str(self.r.randint(0, 9))
                       for _ in range(self.r.randint(12, 40)))),
        ])

    def g_int(self, env, b):
        v = self.var(env, INT)
        if b.depth <= 0:
            return v if v and self.chance(0.6) else self.int_lit()
        d = self.sub(b)
        m = self.sub(b, max(0, b.mul - 1))
        I = lambda bb=d: self.g_int(env, bb)
        opts = [
            (4, lambda: v if v else self.int_lit()),
            (3, self.int_lit),
            (5, lambda: [self.r.choice(["+", "-"]), I(), I()]),
            (1, lambda: ["+", I(), I(), I()]),
            (1, lambda: ["-", I()]),
            (3 if b.mul > 0 else 0, lambda: ["*", I(m), I(m)]),
            (3, lambda: [self.r.choice(["quotient", "remainder", "modulo",
                                        "truncate-quotient", "floor-quotient",
                                        "floor-remainder", "truncate-remainder"]),
                         I(), self.nonzero(env, d)]),
            (1, lambda: ["abs", I()]),
            (1, lambda: [self.r.choice(["gcd", "max", "min"]), I(), I()]),
            (1 if b.mul > 0 else 0, lambda: ["lcm", I(m), I(m)]),
            (2 if b.mul > 0 else 0,
             lambda: ["expt", str(self.r.choice([-3, -2, 2, 3, 5, 7, 10])),
                      str(self.r.randint(0, 70))]),
            (1 if b.mul > 0 else 0, lambda: ["square", I(m)]),
            (2, lambda: ["length", self.g_ilist(env, d)]),
            (1, lambda: ["vector-length", self.g_vec(env, d)]),
            (1, lambda: ["string-length", self.g_str(env, d)]),
            (1, lambda: ["char->integer", self.g_char(env, d)]),
            (1, lambda: [self.r.choice(["numerator", "denominator"]),
                         self.g_rat(env, d)]),
            (2, lambda: [self.r.choice(["floor", "ceiling", "round", "truncate"]),
                         self.g_rat(env, d)]),
            (1, lambda: ["exact", [self.r.choice(["floor", "round", "truncate"]),
                                   self.real_bounded(env, d)]]),
            (4, lambda: ["if", self.g_bool(env, d), I(), I()]),
            (2, lambda: self.cond_form(env, d, INT)),
            (1, lambda: self.case_form(env, d, INT)),
            (4, lambda: self.let_form(env, d, INT)),
            (2, lambda: self.letstar_form(env, d, INT)),
            (2, lambda: self.lambda_app(env, d, INT)),
            (2, lambda: self.named_let_sum(env, b)),
            (2, lambda: self.do_sum(env, b)),
            (2, lambda: self.letrec_rec(env, b)),
            (1, lambda: self.mutual_rec(env, b)),
            (2, lambda: self.callcc_escape(env, d)),
            (1, lambda: self.callcc_reentry(env, d)),
            (2, lambda: self.values_form(env, d, INT)),
            (2, lambda: self.guard_raise(env, d)),
            (1, lambda: self.raise_continuable(env, d)),
            (1, lambda: self.error_irritants(env, d)),
            (1, lambda: self.wind_int(env, d)),
            (1, lambda: self.parameterize_form(env, d)),
            (1, lambda: self.promise_int(env, d)),
            (1, lambda: self.string_port_count(env, d)),
            (1, lambda: ["string->number", ["number->string", I(),
                                            self.r.choice(["2", "8", "10", "16"])],
                         "RADIX"]),
            (1, lambda: self.hash_count(env, d)),
            (2, lambda: [self.r.choice(["apply", "apply"]),
                         self.r.choice(["+", "max", "min"]), I(),
                         self.g_ilist(env, d)]),
            (1, lambda: ["fold", "+", I(), self.g_ilist(env, d)]),
            (1, lambda: ["fold-right", "-", I(), self.g_ilist(env, d)]),
            (1, lambda: ["reduce", self.r.choice(["+", "max"]), "0",
                         self.g_ilist(env, d)]),
            (1, lambda: ["count", self.r.choice(["even?", "odd?", "zero?"]),
                         self.g_ilist(env, d)]),
            (2, lambda: self.vector_ref(env, d)),
            (1, lambda: self.list_ref(env, d)),
            (1, lambda: self.isqrt_sum(env, d)),
            (1, lambda: [self.r.choice(["bitwise-and", "bitwise-ior",
                                        "bitwise-xor"]), I(), I()]),
            (1, lambda: ["arithmetic-shift", I(),
                         str(self.r.randint(-10, 40 if b.mul > 0 else 0))]),
            (1, lambda: ["char->digit", ["integer->char",
                                         ["+", "48", ["modulo", I(), "10"]]],
                         "10"]),
            (1, lambda: self.counter_closure(env, d)),
            (1, lambda: self.vector_accumulate(env, d)),
            (1, lambda: self.internal_define(env, d, INT)),
            (1, lambda: self.deep_tail_loop(env, d)),
            (1, lambda: self.deep_recursion(env, d)),
            (1, lambda: self.when_unless(env, d)),
            (1, lambda: self.symbol_case(env, d)),
        ]
        opts += self.feature_int_options(env, d, b)
        node = self.pick(opts)
        if isinstance(node, list) and node and node[0] == "string->number":
            node[2] = node[1][2]
        return node

    def feature_int_options(self, env, d, b):
        I = lambda: self.g_int(env, d)
        opts = []
        for name, arity in self.funcs:
            opts.append((2, lambda name=name, arity=arity:
                         [name] + [I() for _ in range(arity)]))
        for name, _ in self.recs:
            opts.append((2, lambda name=name:
                         [name, ["modulo", I(), str(self.r.randint(1, 25))]]))
        for rec in self.records:
            opts.append((2, lambda rec=rec: [rec["getx"], [rec["make"], I(), I()]]))
            opts.append((1, lambda rec=rec: self.record_mutate(env, d, rec)))
        if "swap" in self.macros:
            opts.append((1, lambda: self.swap_use(env, d)))
        if "while" in self.macros:
            opts.append((1, lambda: self.while_use(env, b)))
        if "sum" in self.macros:
            opts.append((1, lambda: [self.macros["sum"]] +
                         [I() for _ in range(self.r.randint(0, 4))]))
        if "mylet*" in self.macros:
            opts.append((1, lambda: self.mylet_use(env, d)))
        if "inc" in self.macros:
            opts.append((1, lambda: self.inc_use(env, d)))
        if self.tables:
            opts.append((1, lambda: ["hash-table-ref/default",
                                     self.r.choice(self.tables),
                                     ["modulo", I(), "7"], I()]))
        return opts

    # -- rationals and flonums ----------------------------------------------
    def rat_lit(self):
        return self.pick([
            (3, lambda: "%d/%d" % (self.r.randint(-50, 50), self.r.randint(1, 30))),
            (1, lambda: "%d/%d" % (self.r.randint(10**18, 10**25),
                                   self.r.randint(10**10, 10**20))),
            (1, lambda: str(self.r.randint(-9, 9))),
        ])

    def g_rat(self, env, b):
        v = self.var(env, RAT) or self.var(env, INT)
        if b.depth <= 0:
            return v if v and self.chance(0.5) else self.rat_lit()
        d = self.sub(b)
        m = self.sub(b, max(0, b.mul - 1))
        R = lambda bb=d: self.g_rat(env, bb)
        return self.pick([
            (3, lambda: v if v else self.rat_lit()),
            (2, self.rat_lit),
            (3, lambda: ["/", self.g_int(env, d), self.nonzero(env, d)]),
            (3, lambda: [self.r.choice(["+", "-"]), R(), R()]),
            (2 if b.mul > 0 else 0, lambda: ["*", R(m), R(m)]),
            (2, lambda: ["/", R(), ["+", "1", ["abs", R()]]]),
            (1, lambda: ["abs", R()]),
            (1, lambda: [self.r.choice(["max", "min"]), R(), R()]),
            (1, lambda: ["inexact->exact", self.r.choice(
                ["0.5", "0.125", "-2.75", "0.1", "1e-3", "3.0"])]),
            (1, lambda: ["rationalize", ["inexact->exact", self.r.choice(
                ["0.3", ".25", "0.333"])], self.r.choice(["1/10", "1/100"])]),
            (1 if b.mul > 0 else 0,
             lambda: ["expt", R(m), str(self.r.randint(-4, 6))]),
            (2, lambda: ["if", self.g_bool(env, d), R(), R()]),
            (1, lambda: self.let_form(env, d, RAT)),
            (1, lambda: self.g_int(env, d)),
        ])

    def real_bounded(self, env, b):
        """A flonum that is finite and modest (safe to take exact of)."""
        return ["max", "-1e6", ["min", "1e6", self.g_real(env, b)]]

    def g_real(self, env, b):
        v = self.var(env, REAL)
        if b.depth <= 0:
            return v if v and self.chance(0.5) else self.r.choice(FLOATS)
        d = self.sub(b)
        m = self.sub(b, max(0, b.mul - 1))
        F = lambda bb=d: self.g_real(env, bb)
        return self.pick([
            (3, lambda: v if v else self.r.choice(FLOATS)),
            (2, lambda: self.r.choice(FLOATS)),
            (2, lambda: ["exact->inexact", self.g_rat(env, d)]),
            (1, lambda: ["inexact", self.g_int(env, d)]),
            (3, lambda: [self.r.choice(["+", "-"]), F(), F()]),
            (2 if b.mul > 0 else 0, lambda: ["*", F(m), F(m)]),
            (2, lambda: ["/", F(), ["+", "1.0", ["abs", F()]]]),
            (1, lambda: ["sqrt", ["abs", F()]]),
            (1, lambda: ["exp", ["max", "-20.0", ["min", "20.0", F()]]]),
            (1, lambda: [self.r.choice(["sin", "cos", "atan"]), F()]),
            (1, lambda: ["atan", F(), ["+", "1.0", ["abs", F()]]]),
            (1, lambda: ["log", ["+", "1.0", ["abs", F()]]]),
            (2, lambda: [self.r.choice(["floor", "ceiling", "round", "truncate",
                                        "abs"]), F()]),
            (1, lambda: [self.r.choice(["max", "min"]), F(), F()]),
            (1, lambda: ["*", "1.0", self.g_rat(env, d)]),
            (2, lambda: ["if", self.g_bool(env, d), F(), F()]),
            (1, lambda: self.let_form(env, d, REAL)),
        ])

    # -- booleans, chars, strings, symbols -------------------------------
    def g_bool(self, env, b):
        v = self.var(env, BOOL)
        if b.depth <= 0:
            return v if v and self.chance(0.5) else self.r.choice(["#t", "#f"])
        d = self.sub(b)
        I = lambda: self.g_int(env, d)
        B = lambda: self.g_bool(env, d)
        opts = [
            (2, lambda: v if v else self.r.choice(["#t", "#f"])),
            (6, lambda: [self.r.choice(["<", "<=", "=", ">", ">="]), I(), I()]),
            (1, lambda: [self.r.choice(["<", "<="]), I(), I(), I()]),
            (2, lambda: [self.r.choice(["<", "="]), self.g_rat(env, d),
                         self.g_rat(env, d)]),
            (1, lambda: ["<", self.g_real(env, d), self.g_real(env, d)]),
            (3, lambda: [self.r.choice(["even?", "odd?", "zero?", "positive?",
                                        "negative?", "exact?", "integer?"]), I()]),
            (1, lambda: [self.r.choice(["exact?", "integer?", "rational?",
                                        "exact-rational?", "inexact?"]),
                         self.gen(self.r.choice([RAT, REAL]), env, d)]),
            (2, lambda: [self.r.choice(["null?", "pair?", "list?"]),
                         self.g_ilist(env, d)]),
            (2, lambda: [self.r.choice(["string?", "symbol?", "vector?",
                                        "char?", "boolean?", "number?",
                                        "exact-integer?", "procedure?"]),
                         self.g_any(env, d)]),
            (2, lambda: self.equal_form(env, d)),
            (1, lambda: ["eqv?", I(), I()]),
            (1, lambda: ["eq?", self.g_sym(env, d), self.g_sym(env, d)]),
            (2, lambda: [self.r.choice(["string=?", "string<?", "string-ci=?",
                                        "string>?"]),
                         self.g_str(env, d), self.g_str(env, d)]),
            (1, lambda: [self.r.choice(["string-prefix?", "string-suffix?"]),
                         self.g_str(env, d), self.g_str(env, d)]),
            (2, lambda: [self.r.choice(["char=?", "char<?", "char-ci=?"]),
                         self.g_char(env, d), self.g_char(env, d)]),
            (2, lambda: [self.r.choice(["char-alphabetic?", "char-numeric?",
                                        "char-whitespace?", "char-upper-case?",
                                        "char-lower-case?"]),
                         self.g_char(env, d)]),
            (2, lambda: ["not", B()]),
            (2, lambda: [self.r.choice(["and", "or"])] +
             [B() for _ in range(self.r.randint(0, 3))]),
            (2, lambda: ["if", B(), B(), B()]),
            (1, lambda: ["boolean=?", B(), B()]),
            (1, lambda: ["if", ["memv", I(), self.g_ilist(env, d)], "#t", "#f"]),
            (1, lambda: [self.r.choice(["any", "every"]),
                         self.r.choice(["even?", "odd?", "negative?"]),
                         self.g_ilist(env, d)]),
            (1, lambda: ["eof-object?", ["read-char", ["open-input-string",
                                                       self.g_str(env, d)]]]),
            (1, lambda: ["exact-nonnegative-integer?", I()]),
            (1, lambda: self.let_form(env, d, BOOL)),
            (1, lambda: self.cond_form(env, d, BOOL)),
            (1, lambda: self.local_macro(env, d)),
        ]
        for rec in self.records:
            opts.append((1, lambda rec=rec: [rec["pred"], self.pick([
                (1, lambda: [rec["make"], I(), I()]),
                (1, lambda: self.g_any(env, d))])]))
        if "or" in self.macros:
            opts.append((2, lambda: self.my_or_use(env, d)))
        return self.pick(opts)

    def g_char(self, env, b):
        v = self.var(env, CHAR)
        chars = ASCII_CHARS + ([] if self.mit else UNICODE_CHARS)
        if b.depth <= 0:
            return v if v and self.chance(0.5) else self.r.choice(chars)
        d = self.sub(b)
        return self.pick([
            (3, lambda: v if v else self.r.choice(chars)),
            (2, lambda: self.r.choice(chars)),
            (2, lambda: ["integer->char", ["+", "97", ["modulo",
                                                      self.g_int(env, d), "26"]]]),
            (2, lambda: [self.r.choice(["char-upcase", "char-downcase",
                                        "char-foldcase"]), self.g_char(env, d)]),
            (2, lambda: self.string_ref(env, d)),
            (1, lambda: ["read-char", ["open-input-string",
                                       ["string-append", self.g_str(env, d),
                                        '"x"']]]),
            (1, lambda: ["digit->char", ["modulo", self.g_int(env, d), "10"],
                         "10"]),
            (1, lambda: ["if", self.g_bool(env, d), self.g_char(env, d),
                         self.g_char(env, d)]),
        ])

    def str_lit(self):
        return self.r.choice(ASCII_STRINGS + ([] if self.mit else UNICODE_STRINGS))

    def g_str(self, env, b):
        v = self.var(env, STR)
        if b.depth <= 0:
            return v if v and self.chance(0.5) else self.str_lit()
        d = self.sub(b)
        S = lambda: self.g_str(env, d)
        exact_num = lambda: self.gen(self.r.choice([INT, RAT]), env, d)
        opts = [
            (3, lambda: v if v else self.str_lit()),
            (2, self.str_lit),
            (3, lambda: ["string-append"] + [S() for _ in range(self.r.randint(0, 3))]),
            (2, lambda: self.substring_form(env, d)),
            (1, lambda: [self.r.choice(["string-upcase", "string-downcase"]), S()]),
            (2, lambda: ["number->string", self.g_int(env, d)]
             + ([self.r.choice(["2", "8", "16"])] if self.chance(0.4) else [])),
            (1, lambda: ["number->string", self.g_rat(env, d)]),
            (0 if self.mit else 1, lambda: ["number->string", self.g_real(env, d)]),
            (1, lambda: ["symbol->string", self.g_sym(env, d)]),
            (1, lambda: ["list->string", ["list"] +
                         [self.g_char(env, d) for _ in range(self.r.randint(0, 3))]]),
            (1, lambda: ["string"] + [self.g_char(env, d)
                                      for _ in range(self.r.randint(1, 3))]),
            (1, lambda: ["make-string", self.small_count(), self.g_char(env, d)]),
            (1, lambda: ["string-copy", S()]),
            (1, lambda: ["list->string", ["reverse", ["string->list", S()]]]),
            (1, lambda: self.string_map(env, d)),
            (2, lambda: self.output_string(env, d)),
            (1, lambda: ["string-pad-left", ["number->string", self.g_int(env, d)],
                         str(self.r.randint(0, 12))]),
            (1, lambda: ["string-pad-right", S(), str(self.r.randint(0, 8))]),
            (1, lambda: self.string_mutate(env, d)),
            (1, lambda: ["guard", ["e", [["error-object?", "e"],
                                         ["error-object-message", "e"]]],
                         ["error", self.r.choice(['"boom"', '"bad thing:"']),
                          self.g_int(env, d)]]),
            (1, lambda: ["guard", ["e", [["string?", "e"], "e"]],
                         ["string-append", S(), ["raise", S()]]]),
            (1, lambda: ["if", self.g_bool(env, d), S(), S()]),
            (1, lambda: self.let_form(env, d, STR)),
            (1, lambda: ["read-line", ["open-input-string", S()]]),
            (1, lambda: ["with-output-to-string",
                         ["lambda", [], ["write", exact_num()]]]),
        ]
        if not self.mit:
            # The engines' own error messages: VM and interpreter must agree
            # word for word, but MIT phrases every one differently.
            opts.append((2, lambda: self.engine_error_message(env, d)))
        return self.pick(opts)

    def g_sym(self, env, b):
        v = self.var(env, SYM)
        if b.depth <= 0:
            return v if v and self.chance(0.5) else ["quote", self.r.choice(SYMBOLS)]
        d = self.sub(b)
        return self.pick([
            (3, lambda: v if v else ["quote", self.r.choice(SYMBOLS)]),
            (2, lambda: ["quote", self.r.choice(SYMBOLS)]),
            (1, lambda: ["string->symbol", ["string-append", '"s"',
                                            ["number->string", self.g_int(env, d)]]]),
            (0 if self.mit else 1, lambda: ["string->symbol", self.g_str(env, d)]),
            (1, lambda: ["car", ["quote", [self.r.choice(SYMBOLS), "b", "c"]]]),
            (1, lambda: ["if", self.g_bool(env, d), self.g_sym(env, d),
                         self.g_sym(env, d)]),
        ])

    # -- lists and vectors ---------------------------------------------------
    def ilist_lit(self):
        return ["quote", [str(self.r.randint(-5, 30))
                          for _ in range(self.r.randint(0, 5))]]

    def g_ilist(self, env, b):
        v = self.var(env, ILIST)
        if b.depth <= 0:
            return v if v and self.chance(0.5) else self.ilist_lit()
        d = self.sub(b)
        z = Budget(b.depth - 1, 0)
        I = lambda: self.g_int(env, d)
        L = lambda: self.g_ilist(env, d)
        return self.pick([
            (3, lambda: v if v else self.ilist_lit()),
            (2, self.ilist_lit),
            (3, lambda: ["list"] + [I() for _ in range(self.r.randint(0, 4))]),
            (2, lambda: ["cons", I(), L()]),
            (2, lambda: ["append", L(), L()]),
            (1, lambda: ["reverse", L()]),
            (3, lambda: self.map_form(env, z)),
            (1, lambda: ["map", "+", L(), L()]),
            (2, lambda: [self.r.choice(["filter", "remove"]),
                         self.r.choice(["even?", "odd?", "positive?"]), L()]),
            (1, lambda: self.filter_lambda(env, d)),
            (1, lambda: ["list-copy", L()]),
            (1, lambda: ["iota", self.small_count()] +
             ([I()] if self.chance(0.5) else [])),
            (1, lambda: ["make-list", self.small_count(), I()]),
            (1, lambda: self.list_tail(env, d)),
            (2, lambda: ["sort", L(), self.r.choice(["<", ">"])]),
            (1, lambda: ["vector->list", self.g_vec(env, d)]),
            (1, lambda: ["delete", I(), L()]),
            (1, lambda: ["delete-duplicates", L()]),
            (2, lambda: self.qq([["unquote", I()],
                                        ["unquote-splicing", L()],
                                        str(self.r.randint(0, 9)),
                                        ["unquote", I()]])),
            (1, lambda: self.named_let_build(env, z)),
            (1, lambda: self.do_build(env, z)),
            (1, lambda: ["list-tabulate", self.small_count(),
                         self.lambda1(env, z, INT, INT)]),
            (1, lambda: ["append-map", self.lambda1(
                env, z, INT, ILIST,
                body=lambda e2, x: ["list", x, self.g_int(e2, z)]), L()]),
            (1, lambda: ["filter-map", self.lambda1(
                env, z, INT, INT,
                body=lambda e2, x: ["and", ["even?", x], self.g_int(e2, z)]), L()]),
            (2, lambda: self.wind_log(env, d)),
            (1, lambda: self.callcc_wind_log(env, d)),
            # MIT runs a guard clause before unwinding to the guard (R7RS,
            # and vesper, unwind first), so the after thunk logs later.
            (0 if self.mit else 1, lambda: self.guard_wind_log(env, d)),
            (1, lambda: self.values_form(env, d, ILIST)),
            (1, lambda: ["call-with-values",
                         ["lambda", [], ["values"] +
                          [I() for _ in range(self.r.randint(0, 3))]], "list"]),
            (1, lambda: self.hash_keys(env, d)),
            (1, lambda: self.read_ints(env, d)),
            (1, lambda: ["if", self.g_bool(env, d), L(), L()]),
            (1, lambda: self.let_form(env, d, ILIST)),
            (1, lambda: self.list_mutate(env, d)),
            (1, lambda: ["string-search-all", self.r.choice(['"a"', '"l"', '"ab"']),
                         self.g_str(env, d)]),
        ])

    def vec_lit(self):
        return ["quote", ["#vec"] + [str(self.r.randint(-5, 30))
                                     for _ in range(self.r.randint(0, 5))]]

    def g_vec(self, env, b):
        v = self.var(env, VEC)
        if b.depth <= 0:
            return v if v and self.chance(0.5) else self.vec_lit()
        d = self.sub(b)
        z = Budget(b.depth - 1, 0)
        I = lambda: self.g_int(env, d)
        V = lambda: self.g_vec(env, d)
        return self.pick([
            (3, lambda: v if v else self.vec_lit()),
            (2, self.vec_lit),
            (2, lambda: ["vector"] + [I() for _ in range(self.r.randint(0, 4))]),
            (1, lambda: ["make-vector", self.small_count(), I()]),
            (2, lambda: ["list->vector", self.g_ilist(env, d)]),
            (2, lambda: ["vector-map", self.lambda1(env, z, INT, INT), V()]),
            (1, lambda: ["vector-copy", V()]),
            (1, lambda: ["vector-append", V(), V()]),
            (1, lambda: self.vector_fill_loop(env, z)),
            (1, lambda: self.vector_mutate(env, d)),
            (1, lambda: self.qq(["#vec", str(self.r.randint(0, 9)),
                                 ["unquote", I()]] +
                                # MIT 12.1 drops what ,@ splices into a
                                # vector template.
                                ([] if self.mit else
                                 [["unquote-splicing", self.g_ilist(env, d)]]))),
            (1, lambda: ["if", self.g_bool(env, d), V(), V()]),
            (1, lambda: self.let_form(env, d, VEC)),
        ])

    def datum(self, depth):
        """A random quotable datum."""
        if depth <= 0 or self.chance(0.4):
            return self.pick([
                (3, lambda: str(self.r.randint(-9, 99))),
                (2, lambda: self.r.choice(SYMBOLS)),
                (1, self.str_lit),
                (1, lambda: self.r.choice(ASCII_CHARS)),
                (1, lambda: self.r.choice(["#t", "#f"])),
                (1, lambda: self.r.choice(["1/2", "-3/4", "2.5"]
                                          if not self.mit else ["1/2", "-3/4"])),
                (1, lambda: self.r.choice(FIXNUM_EDGES)),
            ])
        return self.pick([
            (3, lambda: [self.datum(depth - 1)
                         for _ in range(self.r.randint(0, 4))]),
            (1, lambda: ["#vec"] + [self.datum(depth - 1)
                                    for _ in range(self.r.randint(0, 3))]),
            (1, lambda: ["#dot"] + [self.datum(depth - 1)
                                    for _ in range(self.r.randint(1, 3))]
             + [self.datum(0)]),
        ])

    def g_any(self, env, b):
        if b.depth <= 0:
            return self.gen(self.r.choice(PRINTABLE), env, b)
        d = self.sub(b)
        A = lambda: self.g_any(env, d)
        I = lambda: self.g_int(env, d)
        return self.pick([
            (6, lambda: self.gen(self.r.choice(PRINTABLE), env, b)),
            (2, lambda: ["quote", self.datum(3)]),
            (2, lambda: ["list", A(), A()]),
            (1, lambda: ["cons", A(), A()]),
            (1, lambda: ["vector", A(), A()]),
            (2, lambda: self.qq(["#dot", self.r.choice(SYMBOLS),
                                        ["unquote", A()],
                                        ["#vec", "x", ["unquote", I()]],
                                        ["unquote-splicing", self.g_ilist(env, d)],
                                        self.r.choice(["end", ["unquote", I()]])])),
            (1, lambda: ["assq", self.g_sym(env, d),
                         ["quote", [[s, str(i)] for i, s in
                                    enumerate(self.r.sample(SYMBOLS, 3))]]]),
            (1, lambda: ["assoc", self.g_str(env, d),
                         ["list", ["cons", '"abc"', "1"],
                          ["cons", self.g_str(env, d), "2"]]]),
            (1, lambda: ["member", I(), self.g_ilist(env, d)]),
            (1, lambda: ["memq", self.g_sym(env, d), ["quote", ["a", "b", "c"]]]),
            (1, lambda: ["list-index", self.r.choice(["even?", "odd?"]),
                         self.g_ilist(env, d)]),
            (1, lambda: ["string-search-forward",
                         self.r.choice(['"b"', '"lo"', '""']),
                         self.g_str(env, d), "0"]),
            (1, lambda: ["call-with-values",
                         ["lambda", [], [self.r.choice(["floor/", "truncate/"]),
                                         I(), self.nonzero(env, d)]], "list"]),
            (1, lambda: ["call-with-values",
                         ["lambda", [], ["exact-integer-sqrt", ["abs", I()]]],
                         "list"]),
            (1, lambda: ["string->list", self.g_str(env, d)]),
            (1, lambda: self.case_lambda_form(env, d)),
            (1, lambda: self.promise_any(env, d)),
            (1, lambda: ["read", ["open-input-string",
                                  '"%s"' % render(self.datum(3))
                                  .replace("\\", "\\\\").replace('"', '\\"')]]),
            (1, lambda: ["string->number", self.r.choice(
                ['"#x1F"', '"1/2"', '"-17"', '"abc"', '"1e3"', '"#b101"',
                 '"12345678901234567890"', '"+5"', '"."'])]),
            (1, lambda: ["apply", "list", A(), self.g_ilist(env, d)]),
            (1, lambda: self.let_values_form(env, d)),
            (1, lambda: ["vector->list", self.g_vec(env, d)]),
            (1, lambda: self.cond_arrow(env, d)),
            (1, lambda: self.wind_reentry(env, d)),
            (1, lambda: self.callcc_values(env, d)),
            (0 if self.mit else 1, lambda: self.nested_quasiquote(env, d)),
        ])

    # -- binding forms -------------------------------------------------------
    def let_form(self, env, b, ty):
        n = self.r.randint(1, 3)
        binds, e2 = [], env
        for _ in range(n):
            t = self.r.choice(PRINTABLE[:4] + [ILIST, STR])
            name = self.fresh()
            binds.append([name, self.gen(t, env, b)])
            e2 = self.bind(e2, name, t)
        body = [self.gen(ty, e2, b)]
        return ["let", binds] + body

    def letstar_form(self, env, b, ty):
        binds, e2 = [], env
        for _ in range(self.r.randint(1, 3)):
            t = self.r.choice([INT, INT, RAT, ILIST])
            name = self.fresh()
            binds.append([name, self.gen(t, e2, b)])
            e2 = self.bind(e2, name, t)
        return ["let*", binds, self.gen(ty, e2, b)]

    def lambda1(self, env, b, arg_ty, res_ty, body=None):
        x = self.fresh("x")
        e2 = self.bind(env, x, arg_ty)
        return ["lambda", [x], body(e2, x) if body else self.gen(res_ty, e2, b)]

    def lambda_app(self, env, b, ty):
        n = self.r.randint(0, 3)
        params = [self.fresh("a") for _ in range(n)]
        e2 = env
        for p in params:
            e2 = self.bind(e2, p, INT)
        if n and self.chance(0.2):
            # Rest parameter: (lambda (a . rest) ...)
            rest = self.fresh("r")
            e3 = self.bind(e2, rest, ILIST)
            return [["lambda", ["#dot"] + params + [rest], self.gen(ty, e3, b)]] + \
                [self.g_int(env, b) for _ in range(n + self.r.randint(0, 2))]
        return [["lambda", params, self.gen(ty, e2, b)]] + \
            [self.g_int(env, b) for _ in range(n)]

    def deep_tail_loop(self, env, b):
        """A loop long enough to overflow any stack unless the call in each
        tail context really is a tail call."""
        lp, i, acc = self.fresh("lp"), self.fresh("i"), self.fresh("acc")
        n = self.r.choice(["1000", "20000", "100000"])
        call = [lp, ["-", i, "1"], ["+", acc, "1"]]
        done = ["=", i, "0"]
        body = self.r.choice([
            ["if", done, acc, call],
            ["cond", [done, acc], ["else", call]],
            ["cond", [done, acc], [["-", i, "1"], "=>",
                                   ["lambda", ["j"], [lp, "j", ["+", acc, "1"]]]]],
            ["or", ["and", done, acc], call],
            ["if", done, acc, ["and", "#t", call]],
            ["case", ["zero?", i], [["#t"], acc], ["else", call]],
            ["if", done, acc, ["let", [["z", "0"]], call]],
            ["if", done, acc, ["let*", [], ["begin", "1", call]]],
            ["if", done, acc, ["when", "#t", call]],
            ["if", done, acc, ["apply", lp, ["list", ["-", i, "1"], ["+", acc, "1"]]]],
        ])
        return ["-", ["let", lp, [[i, n], [acc, "0"]], body], n]

    def deep_recursion(self, env, b):
        f, n = self.fresh("f"), self.fresh("n")
        depth = self.r.choice(["500", "3000", "10000"])
        return ["letrec", [[f, ["lambda", [n], ["if", ["=", n, "0"], "0",
                                               ["+", "1", [f, ["-", n, "1"]]]]]]],
                ["-", [f, depth], self.g_int(env, b)]]

    def when_unless(self, env, b):
        if self.chance(0.5):
            return ["when", "#t", self.g_int(env, b), self.g_int(env, b)]
        return ["unless", "#f", self.g_int(env, b)]

    def symbol_case(self, env, b):
        return ["case", self.g_sym(env, b),
                [["a", "b"], self.g_int(env, b)],
                [["foo", "if", "lambda"], self.g_int(env, b)],
                ["else", "=>", ["lambda", ["s"], ["string-length",
                                                  ["symbol->string", "s"]]]]]

    def wind_reentry(self, env, b):
        """Re-entering a dynamic-wind through a continuation reruns its
        before thunk."""
        log, k, n, c = (self.fresh("log"), self.fresh("k"), self.fresh("n"),
                        self.fresh("c"))
        push = lambda x: ["set!", log, ["cons", x, log]]
        return ["let", [[log, ["quote", []]], [k, "#f"], [n, "0"]],
                ["dynamic-wind",
                 ["lambda", [], push(["quote", "in"])],
                 ["lambda", [],
                  ["call-with-current-continuation", ["lambda", [c], ["set!", k, c]]],
                  ["set!", n, ["+", n, "1"]],
                  push(n)],
                 ["lambda", [], push(["quote", "out"])]],
                ["if", ["<", n, str(self.r.randint(1, 3))], [k, "#f"]],
                ["reverse", log]]

    def callcc_values(self, env, b):
        k = self.fresh("k")
        # MIT 12.1's call/cc continuations take exactly one argument, even
        # inside a call-with-values producer, where R7RS (and Vesper) pass
        # them on as multiple values.
        count = 1 if self.mit else self.r.randint(0, 3)
        vals = [self.g_int(env, b) for _ in range(count)]
        return ["call-with-values",
                ["lambda", [], ["call-with-current-continuation",
                                ["lambda", [k], [k] + vals]]],
                "list"]

    def nested_quasiquote(self, env, b):
        x = self.fresh("x")
        return ["let", [[x, self.g_int(env, b)]],
                ["quasiquote",
                 ["a", ["quasiquote", ["b", ["unquote", ["c", ["unquote", x]]],
                                       ["unquote", x]]],
                  ["unquote", x]]]]

    def internal_define(self, env, b, ty):
        x, f, y = self.fresh(), self.fresh("f"), self.fresh("y")
        e2 = self.bind(env, x, INT)
        e3 = self.bind(e2, y, INT)
        return [["lambda", [],
                 ["define", x, self.g_int(env, b)],
                 ["define", [f, y], self.g_int(e3, Budget(b.depth - 1, 0))],
                 ["+", [f, self.g_int(e2, b)], x]]]

    def cond_form(self, env, b, ty):
        clauses = [[self.g_bool(env, b), self.gen(ty, env, b)]
                   for _ in range(self.r.randint(1, 3))]
        clauses.append(["else", self.gen(ty, env, b)])
        return ["cond"] + clauses

    def cond_arrow(self, env, b):
        return ["cond", [["memv", self.g_int(env, b), self.g_ilist(env, b)],
                         "=>", "length"],
                ["else", "#f"]]

    def case_form(self, env, b, ty):
        return ["case", ["modulo", self.g_int(env, b), "4"],
                [["0"], self.gen(ty, env, b)],
                [["1", "2"], self.gen(ty, env, b)],
                ["else", self.gen(ty, env, b)]]

    def let_values_form(self, env, b):
        a, c, r = self.fresh(), self.fresh(), self.fresh()
        return ["let-values", [[[a, c], ["values", self.g_int(env, b),
                                         self.g_str(env, b)]],
                               [r, ["values", self.g_int(env, b), "1"]]],
                ["list", c, a, r]]

    def case_lambda_form(self, env, b):
        x, y, r = self.fresh("x"), self.fresh("y"), self.fresh("r")
        f = ["case-lambda",
             [[x], ["list", ["quote", "one"], x]],
             [[x, y], ["list", ["quote", "two"], x, y]],
             [["#dot", x, y, r], ["list", ["quote", "many"], x, y, r]]]
        if self.mit:
            # MIT 12.1 fails (cdr of #f) syntaxing a case-lambda with a
            # rest-argument clause.
            f = f[:-1]
        return [f] + [self.g_int(env, b)
                      for _ in range(self.r.randint(1, 2 if self.mit else 4))]

    # -- loops and recursion ---------------------------------------------
    def named_let_sum(self, env, b):
        lp, i, acc = self.fresh("lp"), self.fresh("i"), self.fresh("acc")
        z = Budget(b.depth - 1, 0)
        e2 = self.bind(self.bind(env, i, INT), acc, INT)
        return ["let", lp, [[i, self.small_count()], [acc, self.g_int(env, z)]],
                ["if", ["<=", i, "0"], acc,
                 [lp, ["-", i, "1"], self.g_int(e2, z)]]]

    def do_sum(self, env, b):
        i, acc = self.fresh("i"), self.fresh("acc")
        z = Budget(b.depth - 1, 0)
        e2 = self.bind(self.bind(env, i, INT), acc, INT)
        return ["do", [[i, "0", ["+", i, "1"]],
                       [acc, self.g_int(env, z), self.g_int(e2, z)]],
                [[">=", i, self.small_count()], acc]]

    def named_let_build(self, env, z):
        lp, i, acc = self.fresh("lp"), self.fresh("i"), self.fresh("acc")
        e2 = self.bind(env, i, INT)
        return ["let", lp, [[i, self.small_count()], [acc, ["quote", []]]],
                ["if", ["<=", i, "0"], acc,
                 [lp, ["-", i, "1"], ["cons", self.g_int(e2, z), acc]]]]

    def do_build(self, env, z):
        i, acc = self.fresh("i"), self.fresh("acc")
        e2 = self.bind(env, i, INT)
        return ["do", [[i, "0", ["+", i, "1"]],
                       [acc, ["quote", []], ["cons", self.g_int(e2, z), acc]]],
                [["=", i, self.small_count()], ["reverse", acc]]]

    def letrec_rec(self, env, b):
        f, n = self.fresh("f"), self.fresh("n")
        z = Budget(b.depth - 1, 0)
        e2 = self.bind(env, n, INT)
        if self.chance(0.5):
            # non-tail recursion
            body = ["if", ["<=", n, "0"], self.g_int(env, z),
                    ["+", [f, ["-", n, "1"]], self.g_int(e2, z)]]
        else:
            acc = self.fresh("acc")
            e3 = self.bind(e2, acc, INT)
            return ["letrec", [[f, ["lambda", [n, acc],
                                    ["if", ["<=", n, "0"], acc,
                                     [f, ["-", n, "1"], self.g_int(e3, z)]]]]],
                    [f, self.small_count(), self.g_int(env, z)]]
        return ["letrec", [[f, ["lambda", [n], body]]], [f, self.small_count()]]

    def mutual_rec(self, env, b):
        ev, od, n = self.fresh("ev"), self.fresh("od"), self.fresh("n")
        return ["letrec",
                [[ev, ["lambda", [n], ["if", ["<=", n, "0"], "1", [od, ["-", n, "1"]]]]],
                 [od, ["lambda", [n], ["if", ["<=", n, "0"], "0", [ev, ["-", n, "1"]]]]]],
                [ev, ["modulo", self.g_int(env, b), str(self.r.choice([7, 50, 1000]))]]]

    def map_form(self, env, z):
        return ["map", self.lambda1(env, z, INT, INT), self.g_ilist(env, z)]

    def filter_lambda(self, env, b):
        return ["filter", self.lambda1(
            env, b, INT, BOOL,
            body=lambda e2, x: [">", x, self.g_int(env, b)]), self.g_ilist(env, b)]

    def list_tail(self, env, b):
        l = self.fresh("l")
        return ["let", [[l, self.g_ilist(env, b)]],
                ["list-tail", l, ["min", self.small_count(), ["length", l]]]]

    def vector_ref(self, env, b):
        v = self.fresh("vv")
        return ["let", [[v, self.g_vec(env, b)]],
                ["if", [">", ["vector-length", v], "0"],
                 ["vector-ref", v, ["modulo", self.g_int(env, b), ["vector-length", v]]],
                 self.g_int(env, b)]]

    def list_ref(self, env, b):
        l = self.fresh("l")
        return ["let", [[l, self.g_ilist(env, b)]],
                ["if", ["pair?", l],
                 ["list-ref", l, ["modulo", self.g_int(env, b), ["length", l]]],
                 "-1"]]

    def string_ref(self, env, b):
        s = self.fresh("s")
        return ["let", [[s, self.g_str(env, b)]],
                ["if", [">", ["string-length", s], "0"],
                 ["string-ref", s, ["modulo", self.g_int(env, b), ["string-length", s]]],
                 "#\\z"]]

    def substring_form(self, env, b):
        s = self.fresh("s")
        k = self.small_count()
        if self.chance(0.5):
            return ["let", [[s, self.g_str(env, b)]],
                    ["substring", s, "0", ["min", k, ["string-length", s]]]]
        return ["let", [[s, self.g_str(env, b)]],
                ["substring", s, ["min", k, ["string-length", s]],
                 ["string-length", s]]]

    def isqrt_sum(self, env, b):
        s, r = self.fresh("s"), self.fresh("r")
        return ["call-with-values",
                ["lambda", [], ["exact-integer-sqrt", ["abs", self.g_int(env, b)]]],
                ["lambda", [s, r], ["-", s, r]]]

    def vector_fill_loop(self, env, z):
        v, i = self.fresh("vv"), self.fresh("i")
        k = self.small_count()
        e2 = self.bind(env, i, INT)
        return ["let", [[v, ["make-vector", k, "0"]]],
                ["do", [[i, "0", ["+", i, "1"]]], [["=", i, k], v],
                 ["vector-set!", v, i, self.g_int(e2, z)]]]

    def vector_mutate(self, env, b):
        v = self.fresh("vv")
        return ["let", [[v, ["vector-copy", self.g_vec(env, b)]]],
                ["if", [">", ["vector-length", v], "0"],
                 ["vector-set!", v, ["modulo", self.g_int(env, b),
                                     ["vector-length", v]], self.g_int(env, b)]],
                ["if", self.g_bool(env, b), ["vector-fill!", v, self.g_int(env, b)]],
                v]

    def list_mutate(self, env, b):
        l = self.fresh("l")
        return ["let", [[l, ["list", self.g_int(env, b), self.g_int(env, b),
                             self.g_int(env, b)]]],
                [self.r.choice(["set-car!", "set-car!"]),
                 ["cdr", l], self.g_int(env, b)],
                ["set-cdr!", ["cddr", l], self.g_ilist(env, b)],
                l]

    def string_mutate(self, env, b):
        s = self.fresh("s")
        return ["let", [[s, ["string-copy", self.g_str(env, b)]]],
                ["if", [">", ["string-length", s], "0"],
                 ["string-set!", s, "0", self.g_char(env, b)]],
                s]

    def string_map(self, env, b):
        c = self.fresh("c")
        return ["string-map",
                ["lambda", [c], ["if", ["char-alphabetic?", c], ["char-upcase", c],
                                 self.g_char(env, Budget(0, 0))]],
                self.g_str(env, b)]

    def vector_accumulate(self, env, b):
        acc, x = self.fresh("acc"), self.fresh("x")
        e2 = self.bind(self.bind(env, x, INT), acc, INT)
        z = Budget(b.depth, 0)
        on_vector = self.chance(0.5)
        return ["let", [[acc, self.g_int(env, z)]],
                ["vector-for-each" if on_vector else "for-each",
                 ["lambda", [x], ["set!", acc, self.g_int(e2, z)]],
                 self.g_vec(env, z) if on_vector else self.g_ilist(env, z)],
                acc]

    def counter_closure(self, env, b):
        n, bump, dd = self.fresh("n"), self.fresh("bump"), self.fresh("dd")
        return ["let", [[n, self.g_int(env, b)]],
                ["let", [[bump, ["lambda", [dd], ["set!", n, ["+", n, dd]], n]]],
                 [bump, self.g_int(env, b)],
                 [bump, self.g_int(env, b)]]]

    # -- control -------------------------------------------------------------
    def callcc_escape(self, env, b):
        k = self.fresh("k")
        if self.chance(0.4):
            x = self.fresh("x")
            return ["call-with-current-continuation",
                    ["lambda", [k],
                     ["for-each", ["lambda", [x], ["if", [">", x, self.g_int(env, b)],
                                                  [k, x]]],
                      self.g_ilist(env, b)],
                     self.g_int(env, b)]]
        return ["call-with-current-continuation",
                ["lambda", [k], ["+", self.g_int(env, b),
                                 ["if", self.g_bool(env, b), [k, self.g_int(env, b)],
                                  self.g_int(env, b)]]]]

    def callcc_reentry(self, env, b):
        kk, n, c = self.fresh("kk"), self.fresh("n"), self.fresh("c")
        return ["let", [[kk, "#f"], [n, "0"]],
                ["call-with-current-continuation", ["lambda", [c], ["set!", kk, c]]],
                ["set!", n, ["+", n, "1"]],
                ["if", ["<", n, str(self.r.randint(1, 5))], [kk, "#f"]],
                ["+", n, self.g_int(env, b)]]

    def values_form(self, env, b, ty):
        x, y = self.fresh(), self.fresh()
        e2 = self.bind(self.bind(env, x, INT), y, INT)
        if self.chance(0.5):
            return ["call-with-values",
                    ["lambda", [], ["values", self.g_int(env, b), self.g_int(env, b)]],
                    ["lambda", [x, y], self.gen(ty, e2, b)]]
        return ["receive", [x, y], [self.r.choice(["floor/", "truncate/"]),
                                    self.g_int(env, b), self.nonzero(env, b)],
                self.gen(ty, e2, b)]

    def guard_raise(self, env, b):
        e = self.fresh("e")
        payload = self.pick([
            (3, lambda: self.g_int(env, b)),
            (1, lambda: self.g_str(env, b)),
            (1, lambda: self.g_sym(env, b)),
            (1, lambda: self.g_ilist(env, b)),
        ])
        body = ["+", self.g_int(env, b), ["raise", payload]] if self.chance(0.7) \
            else ["if", self.g_bool(env, b), ["raise", payload], self.g_int(env, b)]
        return ["guard", [e, [["number?", e], ["+", e, "1"]],
                          [["string?", e], ["string-length", e]],
                          [["pair?", e], ["length", e]],
                          ["else", "-1"]],
                body]

    def raise_continuable(self, env, b):
        e = self.fresh("e")
        return ["with-exception-handler",
                ["lambda", [e], ["+", e, self.g_int(env, b)]],
                ["lambda", [], ["+", self.g_int(env, b),
                                ["raise-continuable", self.g_int(env, b)]]]]

    def error_irritants(self, env, b):
        e = self.fresh("e")
        return ["guard", [e, [["error-object?", e],
                              ["length", ["error-object-irritants", e]]]],
                ["error", '"failed"'] + [self.g_int(env, b)
                                          for _ in range(self.r.randint(0, 3))]]

    def engine_error_message(self, env, b):
        e = self.fresh("e")
        bad = self.r.choice([
            ["car", self.g_int(env, b)],
            ["vector-ref", ["vector", "1", "2"], "5"],
            ["+", "1", ['quote', 'a']],
            ["string-ref", '"abc"', "10"],
            ["undefined-variable-xyz"],
            ["/", self.g_int(env, b), "0"],
            ["exact", "+inf.0"],
            ["list-tail", ["list", "1"], "3"],
            ["symbol->string", "5"],
            [["lambda", ["x"], "x"]],
            ["apply", "+", "1"],
            ["vector-set!", ["vector"], "0", "1"],
            ["string->symbol", "1"],
            ["hash-table-ref", ["make-equal-hash-table"], "1"],
            ["integer->char", "-1"],
            ["exact->inexact", ['quote', 'x']],
            ["raise", ["list", "1", "2"]],
            ["assert-fail-xyz", "1"],
            ["length", ["quote", ["#dot", "1", "2"]]],
            ["let", [["x", "1"]], ["x"]],
        ])
        return ["guard", [e, [["error-object?", e], ["condition/report-string", e]],
                          ["else", ["list", ["quote", "raised"], e]]],
                bad]

    def wind_int(self, env, b):
        n, r = self.fresh("n"), self.fresh("r")
        return ["let", [[n, "0"]],
                ["let", [[r, ["dynamic-wind",
                              ["lambda", [], ["set!", n, ["+", n, "1"]]],
                              ["lambda", [], self.g_int(env, b)],
                              ["lambda", [], ["set!", n, ["*", n, "10"]]]]]],
                 ["+", r, n]]]

    def wind_log(self, env, b):
        log = self.fresh("log")
        push = lambda x: ["set!", log, ["cons", x, log]]
        inner = ["dynamic-wind", ["lambda", [], push("1")],
                 ["lambda", [], push(self.g_int(env, b))],
                 ["lambda", [], push("3")]]
        if self.chance(0.4):
            inner = ["dynamic-wind", ["lambda", [], push("0")],
                     ["lambda", [], inner], ["lambda", [], push("4")]]
        return ["let", [[log, ["quote", []]]], inner, ["reverse", log]]

    def callcc_wind_log(self, env, b):
        log, k, r = self.fresh("log"), self.fresh("k"), self.fresh("r")
        push = lambda x: ["set!", log, ["cons", x, log]]
        # The result is bound before it is pushed: (cons (call/cc ...) log)
        # would depend on the order the arguments are evaluated in, which
        # Scheme leaves open (MIT goes right to left).
        return ["let", [[log, ["quote", []]]],
                ["let", [[r, ["call-with-current-continuation",
                              ["lambda", [k],
                               ["dynamic-wind", ["lambda", [], push("1")],
                                ["lambda", [], ["if", self.g_bool(env, b),
                                                [k, self.g_int(env, b)], "7"]],
                                ["lambda", [], push("2")]]]]]],
                 push(r)],
                ["reverse", log]]

    def guard_wind_log(self, env, b):
        log, e = self.fresh("log"), self.fresh("e")
        push = lambda x: ["set!", log, ["cons", x, log]]
        return ["let", [[log, ["quote", []]]],
                ["guard", [e, ["#t", push(["if", ["number?", e], e, "-1"])]],
                 ["dynamic-wind", ["lambda", [], push("1")],
                  ["lambda", [], ["if", self.g_bool(env, b),
                                  ["raise", self.g_int(env, b)],
                                  push("5")]],
                  ["lambda", [], push("2")]]],
                ["reverse", log]]

    def parameterize_form(self, env, b):
        p, r = self.fresh("p"), self.fresh("r")
        conv = self.r.choice([None, ["lambda", ["x"], ["*", "x", "2"]]])
        mk = ["make-parameter", self.g_int(env, b)] + ([conv] if conv else [])
        return ["let", [[p, mk]],
                ["let*", [[r, ["parameterize", [[p, self.g_int(env, b)]], [p]]]],
                 ["+", r, [p]]]]

    def promise_int(self, env, b):
        n, pr, a = self.fresh("n"), self.fresh("pr"), self.fresh("a")
        return ["let", [[n, "0"]],
                ["let*", [[pr, ["delay", ["begin", ["set!", n, ["+", n, "1"]],
                                          self.g_int(env, b)]]],
                          [a, ["force", pr]],
                          [a, ["+", a, ["force", pr]]]],
                 ["+", a, n]]]

    def promise_any(self, env, b):
        return self.pick([
            (1, lambda: ["force", ["delay", self.g_any(env, b)]]),
            (1, lambda: ["force", ["make-promise", self.g_any(env, b)]]),
            (1, lambda: ["force", ["delay-force", ["delay", self.g_any(env, b)]]]),
        ])

    def string_port_count(self, env, b):
        port, lp, c, n = (self.fresh("port"), self.fresh("lp"), self.fresh("c"),
                          self.fresh("n"))
        return ["let", [[port, ["open-input-string", self.g_str(env, b)]]],
                ["let", lp, [[c, ["read-char", port]], [n, "0"]],
                 ["if", ["eof-object?", c], n,
                  [lp, ["read-char", port],
                   ["+", n, ["if", ["char-alphabetic?", c], "1", "10"]]]]]]

    def read_ints(self, env, b):
        nums = " ".join(str(self.r.randint(-99, 999)) for _ in range(self.r.randint(0, 5)))
        port, lp, x, acc = (self.fresh("port"), self.fresh("lp"), self.fresh("x"),
                            self.fresh("acc"))
        return ["let", [[port, ["open-input-string", '"%s"' % nums]]],
                ["let", lp, [[x, ["read", port]], [acc, ["quote", []]]],
                 ["if", ["eof-object?", x], ["reverse", acc],
                  [lp, ["read", port], ["cons", x, acc]]]]]

    def output_string(self, env, b):
        exact_any = lambda: self.gen(self.r.choice(
            [INT, RAT, STR, CHAR, SYM, ILIST, BOOL] + ([] if self.mit else [REAL, ANY])),
            env, b)
        port = self.fresh("port")
        return self.pick([
            (1, lambda: ["with-output-to-string",
                         ["lambda", [], [self.r.choice(["write", "display"]),
                                         exact_any()],
                          ["write-char", self.g_char(env, b)]]]),
            (1, lambda: ["call-with-output-string",
                         ["lambda", [port], ["write", exact_any(), port],
                          ["write-string", self.g_str(env, b), port]]]),
            (1, lambda: ["let", [[port, ["open-output-string"]]],
                         ["display", exact_any(), port],
                         ["newline", port],
                         ["write", exact_any(), port],
                         ["get-output-string", port]]),
        ])

    def hash_count(self, env, b):
        h, x, c = self.fresh("h"), self.fresh("x"), self.fresh("c")
        ctor = self.r.choice(["make-equal-hash-table", "make-strong-eqv-hash-table"])
        return ["let", [[h, [ctor]]],
                ["for-each", ["lambda", [x], ["hash-table-update!/default", h,
                                              ["modulo", x, "5"],
                                              ["lambda", [c], ["+", c, "1"]], "0"]],
                 self.g_ilist(env, b)],
                ["if", self.g_bool(env, b),
                 ["hash-table-size", h],
                 ["hash-table-ref/default", h, ["modulo", self.g_int(env, b), "5"], "-1"]]]

    def hash_keys(self, env, b):
        h, x = self.fresh("h"), self.fresh("x")
        return ["let", [[h, ["make-strong-eqv-hash-table"]]],
                ["for-each", ["lambda", [x], ["hash-table-set!", h, x, ["*", x, x]]],
                 self.g_ilist(env, b)],
                ["if", self.g_bool(env, b),
                 ["hash-table-delete!", h, self.g_int(env, b)]],
                ["sort", [self.r.choice(["hash-table-keys", "hash-table-values"]), h],
                 "<"]]

    def equal_form(self, env, b):
        t = self.r.choice(PRINTABLE)
        a = self.gen(t, env, b)
        return [self.r.choice(["equal?", "equal?", "eqv?"]), a,
                a if self.chance(0.4) else self.gen(t, env, b)]

    def local_macro(self, env, b):
        m = self.fresh("m")
        if self.chance(0.5):
            return ["let-syntax",
                    [[m, ["syntax-rules", [], [["_", "a", "b"], ["if", "a", "b", "#f"]]]]],
                    [m, self.g_bool(env, b), self.g_bool(env, b)]]
        return ["letrec-syntax",
                [[m, ["syntax-rules", [],
                      [["_"], "#t"], [["_", "e"], "e"],
                      [["_", "e", "r", "..."], ["if", "e", [m, "r", "..."], "#f"]]]]],
                [m] + [self.g_bool(env, b) for _ in range(self.r.randint(0, 4))]]

    # -- program features --------------------------------------------------
    def record_mutate(self, env, b, rec):
        r = self.fresh("rec")
        return ["let", [[r, [rec["make"], self.g_int(env, b), self.g_int(env, b)]]],
                [rec["setx"], r, self.g_int(env, b)],
                ["-", [rec["getx"], r], [rec["gety"], r]]]

    def swap_use(self, env, b):
        x = self.r.choice(["tmp", "a", "x"])
        y = self.r.choice(["other", "tmp2", "y"])
        if x == y:
            y += "2"
        return ["let", [[x, self.g_int(env, b)], [y, self.g_int(env, b)]],
                [self.macros["swap"], x, y],
                ["-", ["*", "2", x], y]]

    def while_use(self, env, b):
        i, acc = self.fresh("i"), self.fresh("acc")
        z = Budget(b.depth - 1, 0)
        e2 = self.bind(self.bind(env, i, INT), acc, INT)
        return ["let", [[i, "0"], [acc, self.g_int(env, z)]],
                [self.macros["while"], ["<", i, self.small_count()],
                 ["set!", acc, self.g_int(e2, z)],
                 ["set!", i, ["+", i, "1"]]],
                acc]

    def mylet_use(self, env, b):
        x, y = self.fresh(), self.fresh()
        e2 = self.bind(env, x, INT)
        e3 = self.bind(e2, y, INT)
        return [self.macros["mylet*"], [[x, self.g_int(env, b)], [y, self.g_int(e2, b)]],
                self.g_int(e3, b)]

    def inc_use(self, env, b):
        x = self.fresh()
        return ["let", [[x, self.g_int(env, b)]],
                [self.macros["inc"], x],
                [self.macros["inc"], x, self.g_int(env, b)],
                x]

    def my_or_use(self, env, b):
        if self.chance(0.4):
            # (let ((t 5)) (my-or #f t)) must see the user's t, not the
            # macro's temporary.
            return ["let", [["t", self.g_bool(env, b)]],
                    [self.macros["or"], "#f", "t"]]
        return [self.macros["or"]] + [self.g_bool(env, b)
                                      for _ in range(self.r.randint(0, 3))]

    def define_macros(self):
        forms = []
        p = self.p
        if self.chance(0.35):
            n = p + "swap!"
            self.macros["swap"] = n
            forms.append(["define-syntax", n, ["syntax-rules", [],
                          [["_", "a", "b"], ["let", [["tmp", "a"]],
                                              ["set!", "a", "b"], ["set!", "b", "tmp"]]]]])
        if self.chance(0.35):
            n = p + "or"
            self.macros["or"] = n
            forms.append(["define-syntax", n, ["syntax-rules", [],
                          [["_"], "#f"], [["_", "e"], "e"],
                          [["_", "e", "r", "..."],
                           ["let", [["t", "e"]], ["if", "t", "t", [n, "r", "..."]]]]]])
        if self.chance(0.3):
            n = p + "while"
            self.macros["while"] = n
            forms.append(["define-syntax", n, ["syntax-rules", [],
                          [["_", "c", "body", "..."],
                           ["let", "lp", [], ["when", "c", "body", "...", ["lp"]]]]]])
        if self.chance(0.3):
            n = p + "sum"
            self.macros["sum"] = n
            forms.append(["define-syntax", n, ["syntax-rules", [],
                          [["_"], "0"],
                          [["_", "x", "rest", "..."], ["+", "x", [n, "rest", "..."]]]]])
        if self.chance(0.25):
            n = p + "let*"
            self.macros["mylet*"] = n
            forms.append(["define-syntax", n, ["syntax-rules", [],
                          [["_", [], "body", "..."], ["let", [], "body", "..."]],
                          [["_", [["x", "v"], "rest", "..."], "body", "..."],
                           ["let", [["x", "v"]], [n, ["rest", "..."], "body", "..."]]]]])
        if self.chance(0.25):
            n = p + "inc!"
            self.macros["inc"] = n
            forms.append(["define-syntax", n, ["syntax-rules", [],
                          [["_", "v"], ["set!", "v", ["+", "v", "1"]]],
                          [["_", "v", "n"], ["set!", "v", ["+", "v", "n"]]]]])
        return forms

    def program(self):
        """A list of top-level forms."""
        p = self.p
        forms = []
        top = Budget(self.depth, 2)
        env = {}
        # Globals
        for _ in range(self.r.randint(0, 3)):
            t = self.r.choice([INT, INT, RAT, STR, ILIST, VEC, BOOL, CHAR, REAL])
            name = "%sg%d" % (p, self.r.randint(0, 999))
            if any(name in v for v in self.globals.values()):
                continue
            forms.append(["define", name, ["%v", ["lambda", [], self.gen(t, env, top)]]])
            self.globals.setdefault(t, []).append(name)
        if self.chance(0.3):
            h = p + "h"
            forms.append(["define", h, ["make-equal-hash-table"]])
            self.tables.append(h)
        if self.chance(0.3):
            rec = {"type": p + "point", "make": p + "make-point",
                   "pred": p + "point?", "getx": p + "point-x",
                   "gety": p + "point-y", "setx": p + "set-point-x!"}
            forms.append(["define-record-type", rec["type"],
                          [rec["make"], "x", "y"], rec["pred"],
                          ["x", rec["getx"], rec["setx"]], ["y", rec["gety"]]])
            self.records.append(rec)
        forms += self.define_macros()
        # Procedures: each may call only those defined before it.
        for i in range(self.r.randint(0, 2)):
            name = "%sf%d" % (p, i)
            arity = self.r.randint(0, 3)
            params = ["a%d" % j for j in range(arity)]
            e2 = env
            for q in params:
                e2 = self.bind(e2, q, INT)
            forms.append(["define", [name] + params, self.g_int(e2, Budget(3, 1))])
            self.funcs.append((name, arity))
        if self.chance(0.4):
            name = p + "rec"
            z = Budget(2, 0)
            e2 = self.bind(env, "n", INT)
            forms.append(["define", [name, "n"],
                          ["if", ["<=", "n", "0"], self.g_int(env, z),
                           ["+", [name, ["-", "n", "1"]], self.g_int(e2, z)]]])
            self.recs.append((name, 1))
        # Statements
        for _ in range(self.r.randint(3, 10)):
            forms.append(self.statement(env, top))
        return forms

    def statement(self, env, b):
        def mutate_global():
            t = self.r.choice(list(self.globals))
            g = self.r.choice(self.globals[t])
            return ["%p", ["lambda", [], ["set!", g, self.gen(t, env, b)], g]]

        def table_stmt():
            h = self.r.choice(self.tables)
            k = ["modulo", self.g_int(env, b), "7"]
            return ["%p", ["lambda", [],
                           ["hash-table-set!", h, k, self.g_int(env, b)],
                           ["sort", ["map", "car", ["hash-table->alist", h]], "<"]]]

        def display_stmt():
            return ["%p", ["lambda", [],
                           ["display", self.g_any(env, b)],
                           ["write-string", '" "'],
                           ["write", self.g_any(env, b)],
                           ["newline"],
                           "#t"]]

        return self.pick([
            (10, lambda: ["%p", ["lambda", [], self.g_any(env, b)]]),
            (3 if self.globals else 0, mutate_global),
            (2 if self.tables else 0, table_stmt),
            (2, display_stmt),
        ])


PRELUDE = """\
(define (%show-error e)
  (if (error-object? e)
      (begin (display "<error ") (write (condition/report-string e)) (display ">"))
      (begin (display "<raise ") (write e) (display ">"))))
(define (%p thunk)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler
        (lambda (e) (%show-error e) (newline) (k #f))
        (lambda () (write (thunk)) (newline))))))
(define (%v thunk)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler
        (lambda (e) (display "define: ") (%show-error e) (newline) (k 'failed))
        thunk))))
(define (%begin n) (newline) (display "@@") (display n) (newline))
"""

# MIT words every error differently, so with --mit an error is just a mark.
MIT_PRELUDE = PRELUDE.replace(
    '(begin (display "<error ") (write (condition/report-string e)) (display ">"))',
    '(display "<error>")')


def make_program(seed, index, mit, avoid=frozenset(KNOWN_BUGS), depth=3):
    """Program number INDEX of the run with SEED: a list of top-level forms."""
    rng = random.Random("%s/%d/%s/%d" % (seed, index, mit, depth)
                        if depth != 3 else "%s/%d/%s" % (seed, index, mit))
    g = Gen(rng, "p%d-" % index, mit, avoid)
    g.depth = depth
    return g.program()


def program_source(forms):
    return "\n".join(render(f) for f in forms) + "\n"


# ---------------------------------------------------------------------------
# Running
# ---------------------------------------------------------------------------

MIT = shutil.which("mit-scheme")


def engines(mit):
    result = [("vm", [VESPER]), ("cps", [VESPER, "--interpreter"])]
    if mit:
        result.append(("mit", [MIT, "--quiet", "--load"]))
    return result


class Result:
    __slots__ = ("out", "err", "code", "timeout", "seconds")

    def __init__(self, out, err, code, timeout, seconds):
        self.out, self.err, self.code = out, err, code
        self.timeout, self.seconds = timeout, seconds

    def status(self):
        if self.timeout:
            return "timeout"
        if self.code != 0:
            return "exit %d" % self.code
        return "ok"


def run_engine(argv, source, timeout, workdir):
    fd, path = tempfile.mkstemp(suffix=".scm", dir=workdir)
    with os.fdopen(fd, "w") as f:
        f.write(source)
    cmd = list(argv) + [path]
    if argv[0] == MIT:
        cmd += ["--eval", "(exit)"]
    start = time.time()
    try:
        done = subprocess.run(cmd, capture_output=True, timeout=timeout,
                              stdin=subprocess.DEVNULL)
        out = done.stdout.decode("utf-8", "replace")
        err = done.stderr.decode("utf-8", "replace")
        res = Result(out, err, done.returncode, False, time.time() - start)
    except subprocess.TimeoutExpired as e:
        out = (e.stdout or b"").decode("utf-8", "replace")
        err = (e.stderr or b"").decode("utf-8", "replace")
        res = Result(out, err, None, True, time.time() - start)
    finally:
        os.unlink(path)
    res.out = "\n".join(l for l in res.out.split("\n")
                        if not l.startswith("; CPS interpreter"))
    res.err = "\n".join(l for l in res.err.split("\n")
                        if not l.startswith("; CPS interpreter"))
    return res


# MIT-specific output normalisation, shared with the curated MIT probes.
sys.path.insert(0, HERE)
try:
    from run_mit_differential import normalize as mit_normalize  # noqa: E402
except Exception:  # pragma: no cover - the module is part of this tree
    def mit_normalize(text):
        return text

FLOATISH = re.compile(r"^[+-]?(\d+\.\d*|\.\d+|\d+)(e[+-]?\d+)?$")


# A built-in prints as #[compiled-procedure N ("car") car] in Vesper and with
# MIT's compiled block and addresses, #[compiled-procedure N ("list" #x10)
# #xbc #x10a1e2644], in MIT: compared by hash number alone.
COMPILED_PROCEDURE = re.compile(r"#\[compiled-procedure (\d+) [^\]]*\]")


def mit_equivalent(a, b):
    """Token-wise comparison that accepts MIT's flonum spelling and last-digit
    differences in inexact results."""
    a = a.replace("#[eof]", "#!eof")
    a = COMPILED_PROCEDURE.sub(r"#[compiled-procedure \1]", a)
    b = COMPILED_PROCEDURE.sub(r"#[compiled-procedure \1]", b)
    if a == b:
        return True
    from run_mit_differential import DATUM_TOKEN
    ta, tb = DATUM_TOKEN.findall(a), DATUM_TOKEN.findall(b)
    if len(ta) != len(tb):
        return False
    for x, y in zip(ta, tb):
        if x == y:
            continue
        if FLOATISH.match(x) and FLOATISH.match(y) and \
                any(c in x + y for c in ".e"):
            fx, fy = float(x), float(y)
            if fx == fy or math.isclose(fx, fy, rel_tol=1e-9, abs_tol=1e-300):
                continue
        return False
    return True


def split_sections(out):
    """Map program index -> its output text, from the @@N markers."""
    sections = {}
    current = None
    lines = []
    for line in out.split("\n"):
        m = re.match(r"^@@(\d+)$", line)
        if m:
            if current is not None:
                sections[current] = "\n".join(lines)
            current = int(m.group(1))
            lines = []
        elif current is not None:
            lines.append(line)
    if current is not None:
        sections[current] = "\n".join(lines)
    return sections


PANIC = re.compile(r"Returning to REPL|Fatal error|panic|Assertion failed|"
                   r"AddressSanitizer|runtime error:")


def compare(results, mit, use_stderr, expect_clean=False):
    """Return None when the engines agree, else (category, description).

    The category is what shrinking preserves: a candidate only counts as
    still failing if it fails the same way."""
    vm, cps = results["vm"], results["cps"]
    for name in ("vm", "cps"):
        r = results[name]
        if r.timeout:
            other = cps if name == "vm" else vm
            if other.timeout:
                return ("hang", "both engines timed out")
            return ("hang", "%s timed out (the other finished in %.1fs)"
                    % (name, other.seconds))
        if r.code is not None and r.code < 0:
            return ("crash", "%s killed by signal %d" % (name, -r.code))
        m = PANIC.search(r.err)
        if m:
            line = [l for l in r.err.splitlines() if PANIC.search(l)][0]
            return ("crash", "%s reported: %s" % (name, line.strip()[:200]))
    if vm.code != cps.code:
        return ("status", "exit status differs: vm %s, cps %s" % (vm.code, cps.code))
    if expect_clean and vm.code != 0:
        # Every statement of a generated program catches its own errors, so
        # one escaping to the top level (both engines alike) means the
        # program could not even be compiled, or the handler broke.
        last = (vm.err.strip().splitlines() or ["?"])[-1]
        return ("abort", "both engines stopped the program early (exit %s): %s"
                % (vm.code, last[:200]))
    if vm.out != cps.out:
        return ("stdout", "stdout differs between vm and cps")
    if use_stderr and strip_paths(vm.err) != strip_paths(cps.err):
        return ("stderr", "stderr differs between vm and cps")
    if mit and "mit" in results:
        a = [mit_normalize(l) for l in vm.out.strip().split("\n")]
        b = [mit_normalize(l) for l in results["mit"].out.strip().split("\n")]
        if results["mit"].timeout:
            return ("mit", "mit timed out")
        if len(a) != len(b) or not all(mit_equivalent(x, y) for x, y in zip(a, b)):
            return ("mit", "vesper and MIT disagree")
    return None


ERROR_MARK = re.compile(r'<error "((?:\\.|[^"\\])*)">')


def line_class(line):
    if line is None:
        return "missing"
    errors = ERROR_MARK.findall(line)
    if errors:
        # The whole messages: shrinking that only kept their shape drifted
        # from the bug at hand to unrelated pairs of errors (two undefined
        # variables, met in a different order by each engine).
        return "error " + " / ".join(errors)
    return "raise" if "<raise" in line else "value"


def signature(results):
    """What shrinking keeps fixed for a stdout divergence: the kind of the
    first line where the engines' outputs part (two different values, a value
    against an error, two wordings of an error...)."""
    a = results["vm"].out.split("\n")
    b = results["cps"].out.split("\n")
    for i in range(max(len(a), len(b))):
        x = a[i] if i < len(a) else None
        y = b[i] if i < len(b) else None
        if x != y:
            return (line_class(x), line_class(y))
    return None


def strip_paths(text):
    return re.sub(r"^\S+\.scm:\d+:\d+: ", "", text, flags=re.M)


def run_all(source, mit, timeout, workdir):
    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
        futs = {name: pool.submit(run_engine, argv, source, timeout, workdir)
                for name, argv in engines(mit)}
        return {name: f.result() for name, f in futs.items()}


def check_program(forms, mit, timeout, workdir, use_stderr, expect_clean=False):
    prelude = MIT_PRELUDE if mit else PRELUDE
    results = run_all(prelude + program_source(forms), mit, timeout, workdir)
    return compare(results, mit, use_stderr, expect_clean), results


# ---------------------------------------------------------------------------
# Shrinking
# ---------------------------------------------------------------------------

SIMPLE_ATOMS = ["0", "1", "#f", "'()", '""']


def paths(node, prefix=()):
    """Every sub-node path, outermost first."""
    yield prefix
    if isinstance(node, list):
        for i, child in enumerate(node):
            yield from paths(child, prefix + (i,))


def get_at(node, path):
    for i in path:
        node = node[i]
    return node


def replace_at(node, path, new):
    if not path:
        return new
    copy = list(node)
    copy[path[0]] = replace_at(node[path[0]], path[1:], new)
    return copy


def delete_at(node, path):
    parent = get_at(node, path[:-1])
    copy = list(parent)
    del copy[path[-1]]
    return replace_at(node, path[:-1], copy)


def size(node):
    return 1 if isinstance(node, str) else 1 + sum(size(c) for c in node)


def shrink_candidates(forms):
    """Smaller variants of FORMS, roughly biggest reduction first."""
    if len(forms) > 1:
        for i in range(len(forms)):
            yield forms[:i] + forms[i + 1:]
    for fi, form in enumerate(forms):
        before, after = forms[:fi], forms[fi + 1:]
        for path in paths(form):
            if not path:
                continue
            node = get_at(form, path)
            replacements = []
            if isinstance(node, list):
                replacements += [c for c in node if size(c) < size(node)]
                replacements += SIMPLE_ATOMS
            elif node not in SIMPLE_ATOMS:
                replacements += SIMPLE_ATOMS[:2]
            seen = set()
            for c in replacements:
                key = render(c)
                if key in seen:
                    continue
                seen.add(key)
                new = replace_at(form, path, c)
                if size(new) < size(form):
                    yield before + [new] + after
            parent = get_at(form, path[:-1])
            if isinstance(parent, list) and path[-1] >= 1 and len(parent) > 2:
                yield before + [delete_at(form, path)] + after


def shrink(forms, still_fails, jobs=4, budget_runs=300, budget_seconds=240):
    """Greedy minimisation: repeatedly take the first smaller variant that
    still fails, testing JOBS candidates at a time."""
    start = time.time()
    runs = 0
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
        while True:
            gen = shrink_candidates(forms)
            progressed = False
            while not progressed:
                if runs >= budget_runs or time.time() - start > budget_seconds:
                    return forms
                chunk = [c for _, c in zip(range(jobs), gen)]
                if not chunk:
                    return forms
                runs += len(chunk)
                for cand, ok in zip(chunk, pool.map(still_fails, chunk)):
                    if ok:
                        forms = cand
                        progressed = True
                        break


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------

def run_batch(seed, indices, args, workdir):
    """Run the programs INDICES in one process per engine. Returns a list of
    (index, forms, why) for each program whose output diverged."""
    prelude = MIT_PRELUDE if args.mit else PRELUDE
    programs = {i: make_program(seed, i, args.mit, args.avoid, args.depth)
                for i in indices}
    source = prelude + "".join(
        "(%%begin %d)\n%s" % (i, program_source(programs[i])) for i in indices)
    results = run_all(source, args.mit, args.timeout, workdir)
    why = compare(results, args.mit, args.stderr, expect_clean=True)
    if why is None:
        missing = [i for i in indices
                   if any("@@%d\n" % i not in r.out + "\n" for n, r in results.items()
                          if n != "mit")]
        if not missing:
            return []
        why = ("abort", "output stops before program %d" % missing[0])
    # Narrow down to the programs whose sections differ, or - when an engine
    # stopped early - everything from the first missing section on.
    suspects = []
    secs = {n: split_sections(r.out) for n, r in results.items()}
    for i in indices:
        outs = [secs[n].get(i) for n in secs]
        if any(o is None for o in outs) or len(set(outs)) > 1 or args.stderr \
                or args.mit:
            suspects.append(i)
    if not suspects:
        suspects = list(indices)
    found = []
    for i in suspects:
        w, _ = check_program(programs[i], args.mit, args.timeout, workdir,
                             args.stderr, expect_clean=True)
        if w:
            found.append((i, programs[i], w))
    if not found:
        # The divergence needs several programs together (shared state, or a
        # GC landing differently); report the batch as one program.
        allforms = []
        for i in indices:
            allforms += [["%begin", str(i)]] + programs[i]
        w, _ = check_program(allforms, args.mit, args.timeout, workdir, args.stderr)
        if w:
            found.append((indices[0], allforms, (w[0], w[1] + " (whole batch)")))
    return found


def describe(forms, args, workdir):
    w, results = check_program(forms, args.mit, args.timeout, workdir, args.stderr,
                               expect_clean=True)
    text = []
    text.append("divergence: %s" % (w[1] if w else "none (did not reproduce)"))
    text.append("--- program (prelude omitted; see PRELUDE in %s) ---"
                % os.path.relpath(__file__, ROOT))
    text.append(program_source(forms).rstrip())
    for name, r in results.items():
        text.append("--- %s (%s, %.1fs) stdout ---" % (name, r.status(), r.seconds))
        text.append(r.out.rstrip())
        if r.err.strip():
            text.append("--- %s stderr ---" % name)
            text.append("\n".join(r.err.rstrip().splitlines()[-8:]))
    return "\n".join(text)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--seed", default="1", help="generation seed (default 1)")
    ap.add_argument("--count", type=int, default=200,
                    help="number of programs (default 200)")
    ap.add_argument("--start", type=int, default=0,
                    help="index of the first program (default 0)")
    ap.add_argument("--batch", type=int, default=50,
                    help="programs per engine process (default 50)")
    ap.add_argument("--jobs", type=int, default=min(4, os.cpu_count() or 1),
                    help="batches run in parallel")
    ap.add_argument("--timeout", type=float, default=60.0,
                    help="seconds per engine process (default 60)")
    ap.add_argument("--mit", action="store_true",
                    help="also compare against mit-scheme (MIT-safe programs only)")
    ap.add_argument("--stderr", action="store_true",
                    help="also require identical standard error")
    ap.add_argument("--no-shrink", action="store_true",
                    help="report failing programs without minimising them")
    ap.add_argument("--shrink-runs", type=int, default=300,
                    help="maximum engine runs spent shrinking one program")
    ap.add_argument("--max-failures", type=int, default=5,
                    help="stop after this many divergent programs")
    ap.add_argument("--save", metavar="DIR",
                    help="write each minimised failing program to DIR")
    ap.add_argument("--print-program", type=int, metavar="N",
                    help="print program N of this seed and exit")
    ap.add_argument("--depth", type=int, default=3,
                    help="expression nesting depth (default 3)")
    ap.add_argument("--no-avoid", action="store_true",
                    help="also generate the shapes that trigger KNOWN_BUGS")
    ap.add_argument("--known-bugs", action="store_true",
                    help="describe the known divergences being avoided and exit")
    args = ap.parse_args(argv)
    args.avoid = frozenset() if args.no_avoid else frozenset(KNOWN_BUGS)
    if args.known_bugs:
        for name, text in KNOWN_BUGS.items():
            print("%s: %s" % (name, text))
        return 0

    if args.print_program is not None:
        sys.stdout.write(program_source(make_program(args.seed, args.print_program,
                                                     args.mit, args.avoid,
                                                     args.depth)))
        return 0
    if not os.path.exists(VESPER):
        print("FAIL: %s not built" % VESPER)
        return 1
    if args.mit and not MIT:
        print("SKIP: --mit requested but mit-scheme is not installed")
        args.mit = False

    start = time.time()
    workdir = tempfile.mkdtemp(prefix="vesper-fuzz-")
    failures = []
    try:
        indices = list(range(args.start, args.start + args.count))
        batches = [indices[i:i + args.batch] for i in range(0, len(indices), args.batch)]
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
            futures = [pool.submit(run_batch, args.seed, b, args, workdir)
                       for b in batches]
            for fut in concurrent.futures.as_completed(futures):
                failures.extend(fut.result())
        failures.sort(key=lambda f: f[0])
        for index, forms, why in failures[:args.max_failures]:
            print("=" * 72)
            print("program %d (seed %s): %s" % (index, args.seed, why[1]))
            if args.save:
                os.makedirs(args.save, exist_ok=True)
                path = os.path.join(args.save, "seed%s-prog%d-orig.scm"
                                    % (args.seed, index))
                with open(path, "w") as f:
                    f.write((MIT_PRELUDE if args.mit else PRELUDE)
                            + program_source(forms))
            if not args.no_shrink:
                _, orig = check_program(forms, args.mit, args.timeout, workdir,
                                        args.stderr, expect_clean=True)
                codes = {n: r.code for n, r in orig.items()}

                def still_fails(candidate, category=why[0], sig=signature(orig)):
                    # Same category of divergence, each engine exiting as it
                    # did originally, and the outputs parting the same way -
                    # otherwise shrinking drifts into programs that are
                    # merely ill-formed, or into a different divergence.
                    w, res = check_program(candidate, args.mit,
                                           min(args.timeout, 20.0), workdir,
                                           args.stderr,
                                           expect_clean=category == "abort")
                    return (w is not None and w[0] == category
                            and "timed out" not in w[1]
                            and all(res[n].code == c for n, c in codes.items())
                            and (category != "stdout" or signature(res) == sig))
                small = shrink(forms, still_fails, jobs=args.jobs,
                               budget_runs=args.shrink_runs)
                if still_fails(small):
                    forms = small
            print(describe(forms, args, workdir))
            if args.save:
                os.makedirs(args.save, exist_ok=True)
                path = os.path.join(args.save, "seed%s-prog%d.scm" % (args.seed, index))
                with open(path, "w") as f:
                    f.write((MIT_PRELUDE if args.mit else PRELUDE)
                            + program_source(forms))
                print("saved %s" % path)
    finally:
        shutil.rmtree(workdir, ignore_errors=True)

    elapsed = time.time() - start
    engines_used = "vm, cps" + (", mit" if args.mit else "")
    if failures:
        print("FAIL: %d of %d programs diverged (seed %s, engines %s, %.1fs)"
              % (len(failures), args.count, args.seed, engines_used, elapsed))
        return 1
    print("PASS: %d fuzzed programs agree (seed %s, engines %s, %.1fs)"
          % (args.count, args.seed, engines_used, elapsed))
    return 0


if __name__ == "__main__":
    sys.exit(main())
