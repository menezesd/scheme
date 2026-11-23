#!/usr/bin/env python3
"""GC-safety lint for vesper's C sources.

Run as `make lint` (part of `make test-all`), or directly:

    tools/lint_gc.py [--allowlist FILE | --no-allowlist] [--list-allowlisted]
                     [--strict] [--self-test] [--verbose] [FILE...]

With no FILE arguments it checks every *.c and *.h in the repository root
except the generated Unicode/stdlib tables and the C unit tests (test_*.c;
name them explicitly to check them too). Findings are printed as
`file:line: [check] explanation  [key]` and make the exit status 1.
--self-test runs the lint over built-in examples of every bug class and of
the safe idioms next to them, and fails unless exactly the bugs are found.

Checks
------
protect-scope
    gc_protect(&x) (or gc_protect2..4, GC_PROTECT2..4, GC_PROTECT_GUARD2..4,
    or a helper that protects its pointer argument for the caller) of a
    variable whose scope ends while its address is still on the GC shadow
    stack: the variable is declared in a block with no GC_GUARD of its own
    (before the protect) and no matching gc_unprotect on some path out of the
    block - its end, a return, break, continue or goto. From then on the
    root points into dead stack, which the compiler may reuse for other
    variables, and the next collection writes a moved cell's new index
    through it. It is reported when that can happen: a call that may collect
    runs while the root dangles (also on the loop's next iteration), or the
    function returns with it still pushed. Which calls may collect is
    inferred from the call graph, seeded with alloc() and the collectors.
    --strict reports every root that outlives its variable, even where
    nothing can collect before the enclosing GC_GUARD releases it. A
    parameter counts as declared in the function body.

    The analysis follows each path separately through if/else, loops,
    switch and goto (up to 16 distinct shadow stacks per point), and models
    gc_unprotect(N) with a literal N, GC_GUARD cleanup at every scope exit,
    GC_PROTECTED blocks and gc_mark/gc_release. Where it cannot keep an exact
    count (a computed gc_unprotect count, too many paths, a helper whose
    effect differs between its returns) it stops judging that path until a
    GC_GUARD cleanup restores a known state, rather than guess.

raw-vector-store
    A store into a vector-like payload that bypasses the generational write
    barrier: `vector_data_ptr(v)[i] = x`, `GET_VECTOR_PTR(v)->data[i] = x`,
    `vd->data[i] = x` for a `vector_data *vd`, a store through an `unsigned *`
    local that was set from one of those, or memcpy/memmove/memset into one.
    Use vector_set_elem(), which records an old vector pointing at a nursery
    cell in the remembered set. Not flagged, because none can create an
    unrecorded old-to-young reference: stores of constants (integer literals,
    ALL_CAPS macros, ctx.atom_*), moves within the same payload, a store
    right after a write_barrier() call, and stores into a vector made by
    make_vector/make_record in the same function with nothing that may
    collect in between (it is still in the nursery). context.c is exempt - it
    is the collector and the allocators.

last-error-overlap
    A raw snprintf/sprintf/strcpy/strcat/memcpy (or v- variant) whose
    destination is ctx.last_error and whose other arguments (sizeof aside)
    include ctx.last_error itself, or a local pointer set from it:
    overlapping source and destination is undefined behaviour. Followed
    through functions and #define macros that pass a parameter (or
    __VA_ARGS__) into such a write, so `report(ctx.last_error)` is flagged
    when report() does the raw write. show_error() formats into a local
    buffer first, so it is safe - and the lint derives that from its
    definition rather than assuming it.

Suppressing a finding
---------------------
A finding that has been checked and is a false positive is silenced with a
comment on the flagged line, or alone on the line above it:

    data[i] = x; // lint-gc: ok <reason>

The reason is mandatory; a suppression without one is itself reported.
Suppressions that no longer match a finding are reported as warnings.

Allowlist
---------
tools/lint_gc_allowlist.txt temporarily accepts known *real* findings in
files that cannot be edited yet. One finding per line:

    <file> <function> <check> <subject>   # note

where subject is the protected variable, the stored-to expression or the
called function, as printed in brackets after each finding. Line numbers are
deliberately not part of the key, so unrelated edits do not invalidate it.
Entries that match nothing are reported as warnings (fixed: delete them).
"""

import argparse
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
DEFAULT_ALLOWLIST = os.path.join(HERE, "lint_gc_allowlist.txt")
SKIP_FILES = {"stdlib_data.h"}
SKIP_PREFIXES = ("unicode_", "test_")
SUPPRESS_RE = re.compile(r"lint-gc:\s*ok\b\s*(.*)")

# ---------------------------------------------------------------------------
# Lexing
# ---------------------------------------------------------------------------

TOKEN_RE = re.compile(r"""
    (?P<ws>[ \t\r\f\v]+)
  | (?P<nl>\n)
  | (?P<lcomment>//[^\n]*)
  | (?P<bcomment>/\*.*?\*/)
  | (?P<str>"(?:\\.|[^"\\\n])*")
  | (?P<chr>'(?:\\.|[^'\\\n])*')
  | (?P<num>\.?[0-9](?:[eEpP][+-]|[A-Za-z0-9_.])*)
  | (?P<id>[A-Za-z_][A-Za-z0-9_]*)
  | (?P<op>\.\.\.|<<=|>>=|->|\+\+|--|<<|>>|<=|>=|==|!=|&&|\|\||[-+*/%&|^]=
          |[{}()\[\];,.:?~!%^&*+\-/<>=|\#\\@$`])
""", re.S | re.X)


class Tok:
    __slots__ = ("kind", "text", "line")

    def __init__(self, kind, text, line):
        self.kind, self.text, self.line = kind, text, line

    def __repr__(self):
        return "%s@%d" % (self.text, self.line)


def blank_preprocessor(text):
    """Replace preprocessor directives (with their continuation lines) by
    empty lines, keeping the line count. Returns (text, directives) where
    directives is a list of (line, full directive text)."""
    out, directives = [], []
    lines = text.split("\n")
    i = 0
    in_comment = False
    while i < len(lines):
        line = lines[i]
        stripped = line.lstrip()
        if not in_comment and stripped.startswith("#"):
            start = i
            buf = [line]
            while lines[i].endswith("\\") and i + 1 < len(lines):
                i += 1
                buf.append(lines[i])
            directives.append((start + 1, "\n".join(buf)))
            out.extend([""] * (i - start + 1))
            i += 1
            continue
        # Track block comments crudely so a '#' inside one is left alone.
        scan = re.sub(r'"(?:\\.|[^"\\])*"', '""', line)
        scan = re.sub(r"//.*", "", scan)
        pos = 0
        while True:
            if in_comment:
                j = scan.find("*/", pos)
                if j < 0:
                    break
                in_comment, pos = False, j + 2
            else:
                j = scan.find("/*", pos)
                if j < 0:
                    break
                in_comment, pos = True, j + 2
        out.append(line)
        i += 1
    return "\n".join(out), directives


def lex(text):
    """Tokens (comments and whitespace dropped) and {line: suppression}."""
    toks, suppress = [], {}
    line = 1
    pos = 0
    n = len(text)
    while pos < n:
        m = TOKEN_RE.match(text, pos)
        if not m:
            pos += 1
            continue
        kind = m.lastgroup
        s = m.group()
        if kind in ("lcomment", "bcomment"):
            sm = SUPPRESS_RE.search(s)
            if sm:
                suppress[line + s.count("\n")] = sm.group(1).strip().rstrip("*/").strip()
        elif kind not in ("ws", "nl"):
            toks.append(Tok(kind, s, line))
        line += s.count("\n")
        pos = m.end()
    return toks, suppress


def match_close(toks, i):
    """Index of the bracket closing toks[i]."""
    pairs = {"(": ")", "[": "]", "{": "}"}
    open_, close = toks[i].text, pairs[toks[i].text]
    depth = 0
    for j in range(i, len(toks)):
        t = toks[j].text
        if t == open_:
            depth += 1
        elif t == close:
            depth -= 1
            if depth == 0:
                return j
    return len(toks) - 1


def split_commas(toks):
    """Split a token list at top-level commas."""
    parts, cur, depth = [], [], 0
    for t in toks:
        if t.text in "([{":
            depth += 1
        elif t.text in ")]}":
            depth -= 1
        if t.text == "," and depth == 0:
            parts.append(cur)
            cur = []
        else:
            cur.append(t)
    if cur or parts:
        parts.append(cur)
    return parts


def texts(toks):
    return [t.text for t in toks]


def joined(toks):
    out = ""
    for t in toks:
        if out and (t.kind in ("id", "num") and (out[-1].isalnum() or out[-1] == "_")):
            out += " "
        out += t.text
    return out


# ---------------------------------------------------------------------------
# Statements
# ---------------------------------------------------------------------------

C_KEYWORDS = {"if", "else", "for", "while", "do", "switch", "case", "default",
              "return", "break", "continue", "goto", "sizeof", "typedef",
              "struct", "union", "enum"}
TYPE_WORDS = {"unsigned", "signed", "int", "char", "short", "long", "float",
              "double", "bool", "_Bool", "void", "const", "volatile", "static",
              "register", "struct", "union", "enum", "extern", "inline",
              "size_t", "ssize_t", "ptrdiff_t", "intptr_t", "uintptr_t",
              "int8_t", "int16_t", "int32_t", "int64_t", "uint8_t", "uint16_t",
              "uint32_t", "uint64_t", "FILE", "va_list", "jmp_buf", "wchar_t",
              "__attribute__"}


class Stmt:
    def __init__(self, kind, line, **kw):
        self.kind = kind
        self.line = line
        self.__dict__.update(kw)


class Parser:
    def __init__(self, toks, typedefs):
        self.t = toks
        self.typedefs = typedefs
        self.labels = {}  # label name -> Block containing it

    def block(self, i, parent):
        """toks[i] is '{'. Returns (Block, index after '}')."""
        end = match_close(self.t, i)
        blk = Stmt("block", self.t[i].line, stmts=[], end_line=self.t[end].line,
                   parent=parent)
        j = i + 1
        while j < end:
            st, j = self.stmt(j, blk)
            if st is not None:
                blk.stmts.append(st)
        return blk, end + 1

    def paren(self, i):
        """toks[i] is '('. Returns (inner tokens, index after ')')."""
        end = match_close(self.t, i)
        return self.t[i + 1:end], end + 1

    def sub(self, i, parent):
        """A sub-statement (if/loop body), wrapped so a bare statement still
        knows its enclosing block."""
        return self.stmt(i, parent)

    def stmt(self, i, blk):
        t = self.t
        tok = t[i]
        x = tok.text
        line = tok.line
        if x == "{":
            return self.block(i, blk)
        if x == ";":
            return None, i + 1
        if x == "if" and i + 1 < len(t) and t[i + 1].text == "(":
            cond, j = self.paren(i + 1)
            then, j = self.sub(j, blk)
            els = None
            if j < len(t) and t[j].text == "else":
                els, j = self.sub(j + 1, blk)
            return Stmt("if", line, cond=cond, then=then, els=els), j
        if x == "for" and t[i + 1].text == "(":
            hdr, j = self.paren(i + 1)
            parts, cur, depth = [], [], 0
            for h in hdr:
                if h.text in "([{":
                    depth += 1
                elif h.text in ")]}":
                    depth -= 1
                if h.text == ";" and depth == 0:
                    parts.append(cur)
                    cur = []
                else:
                    cur.append(h)
            parts.append(cur)
            while len(parts) < 3:
                parts.append([])
            body, j = self.sub(j, blk)
            return Stmt("loop", line, loop="for", init=parts[0], cond=parts[1],
                        step=parts[2], body=body, infinite=not parts[1],
                        decl_names=None), j
        if x == "while" and t[i + 1].text == "(":
            cond, j = self.paren(i + 1)
            body, j = self.sub(j, blk)
            return Stmt("loop", line, loop="while", init=[], cond=cond, step=[],
                        body=body, infinite=texts(cond) in (["1"], ["true"]),
                        decl_names=None), j
        if x == "do":
            body, j = self.sub(i + 1, blk)
            cond = []
            if j < len(t) and t[j].text == "while":
                cond, j = self.paren(j + 1)
                if j < len(t) and t[j].text == ";":
                    j += 1
            return Stmt("loop", line, loop="do", init=[], cond=cond, step=[],
                        body=body, infinite=texts(cond) in (["1"], ["true"]),
                        decl_names=None), j
        if x == "switch" and t[i + 1].text == "(":
            cond, j = self.paren(i + 1)
            body, j = self.sub(j, blk)
            return Stmt("switch", line, cond=cond, body=body), j
        if x in ("return",):
            j = self.to_semicolon(i + 1)
            return Stmt("return", line, expr=t[i + 1:j]), j + 1
        if x in ("break", "continue"):
            return Stmt(x, line), self.to_semicolon(i + 1) + 1
        if x == "goto":
            return Stmt("goto", line, label=t[i + 1].text), self.to_semicolon(i + 1) + 1
        if x == "case":
            j = i + 1
            depth = 0
            while j < len(t):
                if t[j].text in "([":
                    depth += 1
                elif t[j].text in ")]":
                    depth -= 1
                elif t[j].text == ":" and depth == 0:
                    break
                j += 1
            return Stmt("case", line), j + 1
        if x == "default" and i + 1 < len(t) and t[i + 1].text == ":":
            return Stmt("case", line, is_default=True), i + 2
        if tok.kind == "id" and x not in C_KEYWORDS and i + 1 < len(t) \
                and t[i + 1].text == ":":
            self.labels[x] = blk
            return Stmt("label", line, name=x), i + 2
        if x == "GC_PROTECTED":
            j = i + 1
            body, j = self.sub(j, blk)
            return Stmt("protected", line, body=body), j
        if x == "FORLIST" and t[i + 1].text == "(":
            hdr, j = self.paren(i + 1)
            args = split_commas(hdr)
            body, j = self.sub(j, blk)
            name = args[0][0].text if args and args[0] else "?"
            return Stmt("loop", line, loop="forlist", init=args[1] if len(args) > 1 else [],
                        cond=[], step=[], body=body, infinite=False,
                        decl_names=[name]), j
        # Statement-like macro taking a body: NAME(...) { ... }
        if tok.kind == "id" and i + 1 < len(t) and t[i + 1].text == "(":
            close = match_close(t, i + 1)
            if close + 1 < len(t) and t[close + 1].text == "{" \
                    and x not in C_KEYWORDS:
                body, j = self.block(close + 1, blk)
                return Stmt("loop", line, loop="macro", init=t[i:close + 1],
                            cond=[], step=[], body=body, infinite=False,
                            decl_names=None), j
        j = self.to_semicolon(i)
        return Stmt("simple", line, toks=t[i:j]), j + 1

    def to_semicolon(self, i):
        t = self.t
        depth = 0
        j = i
        while j < len(t):
            x = t[j].text
            if x in "([{":
                depth += 1
            elif x in ")]}":
                if depth == 0:
                    return j  # a missing ';' before the block's end
                depth -= 1
            elif x == ";" and depth == 0:
                return j
            j += 1
        return j


def is_type_start(toks, typedefs):
    if not toks:
        return False
    a = toks[0]
    if a.kind != "id":
        return False
    if a.text in TYPE_WORDS or a.text in typedefs:
        if len(toks) > 1 and toks[1].text == "(" and a.text not in (
                "__attribute__",):
            return False  # a call through something named like a type
        return True
    # IDENT IDENT ... or IDENT * IDENT (= | ; | , | [)
    if a.text in C_KEYWORDS:
        return False
    if len(toks) >= 2 and toks[1].kind == "id" and toks[1].text not in C_KEYWORDS:
        return True
    if len(toks) >= 3 and toks[1].text == "*":
        k = 1
        while k < len(toks) and toks[k].text in ("*", "const"):
            k += 1
        if k < len(toks) and toks[k].kind == "id" and k + 1 < len(toks) and \
                toks[k + 1].text in ("=", ";", ",", "["):
            return True
        if k < len(toks) and toks[k].kind == "id" and k + 1 == len(toks):
            return True
    return False


def parse_declaration(toks, typedefs):
    """For a declaration statement, [(name, is_static, is_pointer, type_text,
    init_toks)]; otherwise None."""
    if not is_type_start(toks, typedefs):
        return None
    is_static = any(t.text == "static" for t in toks[:4])
    # Type prefix: type words, typedef names, struct/enum tags, attributes.
    k = 0
    prefix = []
    while k < len(toks):
        x = toks[k].text
        if x == "__attribute__" and k + 1 < len(toks) and toks[k + 1].text == "(":
            k = match_close(toks, k + 1) + 1
            continue
        if x in ("struct", "union", "enum") and k + 1 < len(toks):
            prefix += [x, toks[k + 1].text]
            k += 2
            if k < len(toks) and toks[k].text == "{":
                k = match_close(toks, k) + 1
            continue
        if x in TYPE_WORDS or x in typedefs:
            prefix.append(x)
            k += 1
            continue
        if not prefix and toks[k].kind == "id":
            prefix.append(x)   # unknown typedef name
            k += 1
            continue
        break
    out = []
    for part in split_commas(toks[k:]):
        stars = 0
        j = 0
        while j < len(part) and part[j].text in ("*", "const", "volatile", "restrict"):
            stars += part[j].text == "*"
            j += 1
        name = None
        if j < len(part) and part[j].text == "(":
            # (*fp)(...) or (*arr)[n]
            inner = part[j + 1:match_close(part, j)]
            ids = [t for t in inner if t.kind == "id"]
            if ids:
                name = ids[0].text
                stars += 1
            j = match_close(part, j) + 1
        elif j < len(part) and part[j].kind == "id":
            name = part[j].text
            j += 1
        if name is None:
            continue
        if j < len(part) and part[j].text == "(":
            continue  # a function prototype
        init = []
        eq = [n for n, t in enumerate(part) if t.text == "="]
        if eq:
            init = part[eq[0] + 1:]
        out.append((name, is_static, stars > 0, " ".join(prefix), init))
    return out


# ---------------------------------------------------------------------------
# Functions and macros
# ---------------------------------------------------------------------------

class Function:
    def __init__(self, path, name, params, body_toks, line, variadic):
        self.path = path
        self.name = name
        self.params = params        # [(name, is_pointer, type_text)]
        self.toks = body_toks       # including the braces
        self.line = line
        self.variadic = variadic
        self.body = None
        self.labels = {}


def find_typedefs(toks):
    names = set()
    i = 0
    while i < len(toks):
        if toks[i].text == "typedef":
            j = i + 1
            depth = 0
            last_id = None
            fp_name = None
            while j < len(toks):
                x = toks[j].text
                if x in "({[":
                    if x == "(" and j + 2 < len(toks) and toks[j + 1].text == "*" \
                            and toks[j + 2].kind == "id" and depth == 0:
                        fp_name = toks[j + 2].text
                    depth += 1
                elif x in ")}]":
                    depth -= 1
                elif x == ";" and depth == 0:
                    break
                elif toks[j].kind == "id" and depth == 0:
                    last_id = x
                j += 1
            name = fp_name or last_id
            if name:
                names.add(name)
            i = j
        i += 1
    return names


def find_functions(path, toks):
    funcs = []
    i = 0
    n = len(toks)
    while i < n:
        x = toks[i].text
        if x == "{":
            end = match_close(toks, i)
            # Function definition: '{' right after a parameter list's ')'.
            if i > 0 and toks[i - 1].text == ")":
                close = i - 1
                depth = 0
                k = close
                while k >= 0:
                    if toks[k].text == ")":
                        depth += 1
                    elif toks[k].text == "(":
                        depth -= 1
                        if depth == 0:
                            break
                    k -= 1
                if k > 0 and toks[k - 1].kind == "id" and \
                        toks[k - 1].text not in C_KEYWORDS:
                    name = toks[k - 1].text
                    params = []
                    variadic = False
                    for part in split_commas(toks[k + 1:close]):
                        tx = texts(part)
                        if tx == ["void"] or not tx:
                            continue
                        if tx == ["..."]:
                            variadic = True
                            continue
                        ids = [t for t in part if t.kind == "id"
                               and t.text not in TYPE_WORDS]
                        pname = ids[-1].text if ids else None
                        params.append((pname, "*" in tx or "[" in tx,
                                       " ".join(tx[:-1])))
                    funcs.append(Function(path, name, params, toks[i:end + 1],
                                          toks[i].line, variadic))
            i = end + 1
            continue
        i += 1
    return funcs


DEFINE_RE = re.compile(r"#\s*define\s+([A-Za-z_]\w*)\(([^)]*)\)\s*(.*)", re.S)


def find_macros(path, directives):
    """Function-like #define macros: [(name, [params], body tokens, line)]."""
    out = []
    for line, text in directives:
        body = text.replace("\\\n", " \n")
        m = DEFINE_RE.match(body.strip())
        if not m:
            continue
        params = [p.strip() for p in m.group(2).split(",") if p.strip()]
        toks, _ = lex(m.group(3))
        for t in toks:
            t.line += line - 1
        out.append((m.group(1), params, toks, line))
    return out


# ---------------------------------------------------------------------------
# Findings
# ---------------------------------------------------------------------------

class Finding:
    def __init__(self, path, line, check, func, subject, message):
        self.path = path
        self.line = line
        self.check = check
        self.func = func
        self.subject = subject
        self.message = message

    def key(self):
        return (os.path.basename(self.path), self.func, self.check, self.subject)

    def __str__(self):
        return "%s:%d: [%s] %s  [%s %s %s %s]" % (
            os.path.relpath(self.path, ROOT) if os.path.isabs(self.path) else self.path,
            self.line, self.check, self.message, *self.key())


# ---------------------------------------------------------------------------
# protect-scope: abstract interpretation of the shadow stack
# ---------------------------------------------------------------------------

class Unknown:
    """The shadow stack is not known exactly on this path."""
    def __repr__(self):
        return "UNKNOWN"


UNKNOWN = Unknown()


class State:
    """Shadow-stack contents pushed since function entry: a tuple of
    entries (var, scope_id, line) - var is None for an anonymous root and
    ('param', i) for a pointer parameter - plus how many entries the
    function popped from its caller's part of the stack."""
    __slots__ = ("stack", "under")

    def __init__(self, stack=(), under=0):
        self.stack = stack
        self.under = under

    def __eq__(self, other):
        return isinstance(other, State) and self.stack == other.stack \
            and self.under == other.under

    def __hash__(self):
        return hash((self.stack, self.under))

    def push(self, entry):
        return State(self.stack + (entry,), self.under)

    def pop(self, n):
        k = min(n, len(self.stack))
        return State(self.stack[:len(self.stack) - k], self.under + (n - k))


MAX_PATHS = 16


def merge(*sets):
    """Join control-flow paths. Each argument is a frozenset of States (one
    per distinct path so far) or None for an unreachable point; the join
    keeps every distinct state, up to MAX_PATHS, beyond which the paths are
    no longer told apart."""
    sets = [x for x in sets if x is not None]
    if not sets:
        return None
    out = frozenset().union(*sets)
    if len(out) > MAX_PATHS:
        return frozenset([UNKNOWN])
    return out


PROTECT_N = {"gc_protect": 1, "gc_protect2": 2, "gc_protect3": 3,
             "gc_protect4": 4, "GC_PROTECT2": 2, "GC_PROTECT3": 3,
             "GC_PROTECT4": 4}
GUARD_PROTECT_N = {"GC_PROTECT_GUARD2": 2, "GC_PROTECT_GUARD3": 3,
                   "GC_PROTECT_GUARD4": 4}
GUARD_WORDS = {"GC_GUARD", "GC_PROTECT_GUARD"}


def events(toks):
    """GC-relevant events in a token sequence, in order."""
    out = []
    n = len(toks)
    i = 0
    while i < n:
        t = toks[i]
        x = t.text
        if t.kind == "id":
            if x in GUARD_WORDS and not (i + 1 < n and toks[i + 1].text == "("):
                out.append(("guard", t.line))
            elif i + 1 < n and toks[i + 1].text == "(":
                close = match_close(toks, i + 1)
                args = split_commas(toks[i + 2:close])
                if x in PROTECT_N:
                    for a in args:
                        out.append(("protect", t.line, a))
                    i = close + 1
                    continue
                if x in GUARD_PROTECT_N:
                    out.append(("guard", t.line))
                    for a in args:
                        out.append(("protect", t.line, a))
                    i = close + 1
                    continue
                if x == "gc_unprotect":
                    a = texts(args[0]) if args else []
                    if len(a) == 1 and a[0].isdigit():
                        out.append(("pop", t.line, int(a[0])))
                    else:
                        out.append(("pop?", t.line))
                    i = close + 1
                    continue
                if x == "gc_mark":
                    # NAME = gc_mark()
                    if i >= 2 and toks[i - 1].text == "=" and toks[i - 2].kind == "id":
                        out.append(("mark", t.line, toks[i - 2].text))
                    i = close + 1
                    continue
                if x == "gc_release":
                    a = texts(args[0]) if args else []
                    out.append(("release", t.line, a[0] if len(a) == 1 else None))
                    i = close + 1
                    continue
                if x == "gc_recover_after_panic":
                    out.append(("reset", t.line))
                    i = close + 1
                    continue
                if x not in C_KEYWORDS:
                    # Events inside the arguments happen first.
                    out.extend(events(toks[i + 2:close]))
                    member = i > 0 and toks[i - 1].text in (".", "->")
                    out.append(("call", t.line,
                                AllocInfo.INDIRECT if member else x, args))
                    i = close + 1
                    continue
        i += 1
    return out


# The functions that can run a collection, seeding the inference in
# infer_allocating(): everything that (transitively) calls one of them may
# collect too.
ALLOC_ROOTS = {"alloc", "gc", "minor_gc", "maybe_gc", "trigger_gc"}


class AllocInfo:
    """Which calls may trigger a collection, inferred over the whole tree."""
    allocating = set(ALLOC_ROOTS)
    defined = set()
    # Calls through a function pointer (a parameter or local) or a struct
    # member are unknowable and assumed to allocate.
    INDIRECT = "<indirect call>"


def may_allocate(name, local_names=()):
    if name == AllocInfo.INDIRECT or name in AllocInfo.allocating:
        return True
    if name in AllocInfo.defined:
        return False
    # Neither defined in the tree nor a known allocator: the C library, or
    # a function-like macro (whose argument calls are separate events) -
    # unless it names a function pointer.
    return name in local_names


def infer_allocating(funcs, macros):
    """Fixpoint over the call graph: a function or macro may collect when
    it calls something that may."""
    callees = {}
    for fn in funcs:
        locals_ = {p[0] for p in fn.params if p[0]}
        names = set()
        for ev in events(fn.toks):
            if ev[0] == "call":
                names.add(AllocInfo.INDIRECT if ev[2] in locals_ else ev[2])
        callees.setdefault(fn.name, set()).update(names)
    for name, params, toks, line in macros:
        names = {ev[2] for ev in events(toks) if ev[0] == "call"}
        callees.setdefault(name, set()).update(names)
    AllocInfo.defined = set(callees)
    allocating = set(ALLOC_ROOTS)
    changed = True
    while changed:
        changed = False
        for name, cs in callees.items():
            if name not in allocating and (cs & allocating or
                                           AllocInfo.INDIRECT in cs):
                allocating.add(name)
                changed = True
    AllocInfo.allocating = allocating


def first_allocating_call(parts):
    """(name, line) of the first call that may allocate in statements or
    token lists PARTS, else None."""
    def walk(node):
        if node is None:
            return None
        if isinstance(node, list):
            for ev in events(node):
                if ev[0] == "call" and may_allocate(ev[2]):
                    return (ev[2], ev[1])
            return None
        for attr in ("toks", "cond", "init", "step", "expr"):
            v = getattr(node, attr, None)
            if isinstance(v, list):
                r = walk(v)
                if r:
                    return r
        for attr in ("then", "els", "body"):
            r = walk(getattr(node, attr, None))
            if r:
                return r
        for child in getattr(node, "stmts", []) or []:
            r = walk(child)
            if r:
                return r
        return None
    for p in parts:
        r = walk(p)
        if r:
            return r
    return None


class Scope:
    _next = [0]

    def __init__(self, kind, line, end_line, block=None):
        Scope._next[0] += 1
        self.id = Scope._next[0]
        self.kind = kind
        self.line = line
        self.end_line = end_line
        self.block = block
        self.decls = {}      # name -> (is_static, is_pointer, type_text)
        self.guard = None    # State snapshot taken at GC_GUARD
        self.marks = {}      # gc_mark variable -> State


class LoopCtx:
    def __init__(self, kind, depth):
        self.kind = kind          # "loop" or "switch"
        self.depth = depth        # len(scopes) when the body starts
        self.breaks = []
        self.continues = []


class ScopeAnalyzer:
    def __init__(self, func, summaries, typedefs, report):
        self.f = func
        self.summaries = summaries
        self.typedefs = typedefs
        self.report = report      # callable(line, var, scope, how, protect_line)
        self.scopes = []
        self.loops = []
        self.switch_entries = []
        self.gotos = {}
        self.exits = []           # states at function exit, after cleanup
        self.reported = set()
        self.strict = False

    # -- scope bookkeeping -------------------------------------------------
    def local_names(self):
        return {n for s in self.scopes for n in s.decls}

    def lookup(self, name):
        for s in reversed(self.scopes):
            if name in s.decls:
                return s
        return None

    def entry_for(self, arg, line):
        tx = texts(arg)
        if len(tx) >= 2 and tx[0] == "&" and arg[1].kind == "id":
            if len(tx) > 2 and tx[2] == "->":
                return (None, None, line, None)
            name = tx[1]
            s = self.lookup(name)
            if s is None:
                return (None, None, line, None)   # a global or a static
            static, is_ptr, _ = s.decls[name]
            if static:
                return (None, None, line, None)
            return (name, s.id, line, None)
        if len(tx) == 1 and arg[0].kind == "id":
            for i, (pname, is_ptr, _) in enumerate(self.f.params):
                if pname == tx[0] and is_ptr and self.lookup(pname) is self.scopes[0]:
                    return (("param", i), None, line, None)
        return (None, None, line, None)

    def scope_desc(self, scope, var):
        if scope.kind == "function":
            if any(p[0] == var for p in self.f.params):
                return "a parameter of %s()" % self.f.name
            return "declared in the body of %s()" % self.f.name
        if scope.kind == "for":
            return "declared in the loop header at line %d" % scope.line
        if scope.kind == "protected":
            return "declared in the GC_PROTECTED block at line %d" % scope.line
        return "declared in the block at lines %d-%d" % (scope.line, scope.end_line)

    def flag(self, entry, why):
        var, _, pline, dangling = entry
        key = (var, pline)
        if key in self.reported:
            return
        self.reported.add(key)
        desc, how, line = dangling
        self.report(pline, var, "gc_protect(&%s): %s is %s, but its root is still "
                    "on the shadow stack after %s (line %d); %s - add GC_GUARD to "
                    "that block or gc_unprotect before it ends"
                    % (var, var, desc, how, line, why))

    def leave(self, paths, scopes, how, line, final=False):
        """Leave SCOPES at LINE: run their GC_GUARD cleanups innermost
        first. Roots of variables they declared that are still pushed now
        point into dead stack: they are reported as soon as anything may
        collect (or at once, when the function itself is returning)."""
        if paths is None:
            return None
        for sc in reversed(scopes):
            if sc.guard is not None:
                paths = sc.guard
        ids = {sc.id: sc for sc in scopes}
        out = set()
        for state in paths:
            if state is UNKNOWN:
                out.add(state)
                continue
            stack = list(state.stack)
            for k, entry in enumerate(stack):
                var, sid, pline, dangling = entry
                if sid in ids and isinstance(var, str):
                    entry = (var, None, pline,
                             (self.scope_desc(ids[sid], var), how, line))
                    stack[k] = entry
                if final and entry[3] is not None:
                    self.flag(entry, "the function returns without releasing "
                              "it, so its caller's next collection writes "
                              "through it")
                elif self.strict and entry[3] is not None:
                    self.flag(entry, "(--strict: reported whether or not "
                              "anything collects before it is released)")
            out.add(State(tuple(stack), state.under))
        return frozenset(out)

    def collect_point(self, paths, name, line):
        """A call that may allocate: a dangling root would be written by the
        collector now."""
        for state in paths or ():
            if state is UNKNOWN:
                continue
            for entry in state.stack:
                if entry[3] is not None:
                    self.flag(entry, "%s() at line %d may collect while it "
                              "dangles" % (name, line))

    def declare(self, toks):
        decl = parse_declaration(toks, self.typedefs)
        if decl:
            for name, static, ptr, ty, _ in decl:
                self.scopes[-1].decls[name] = (static, ptr, ty)

    # -- execution --------------------------------------------------------
    def find_mark(self, name):
        for sc in reversed(self.scopes):
            if name in sc.marks:
                return sc.marks[name]
        return None

    def step(self, state, ev):
        """One event on one path's state."""
        kind = ev[0]
        if kind == "reset":
            return State()
        if state is UNKNOWN:
            return state
        if kind == "protect":
            return state.push(self.entry_for(ev[2], ev[1]))
        if kind == "pop":
            return state.pop(ev[2])
        if kind == "pop?":
            return UNKNOWN
        if kind == "call":
            summ = self.summaries.get(ev[2])
            if summ is UNKNOWN:
                return UNKNOWN
            if summ:
                pops, pushes = summ
                if pops:
                    state = state.pop(pops)
                for idx in pushes:
                    args = ev[3]
                    if idx < len(args):
                        state = state.push(self.entry_for(args[idx], ev[1]))
                    else:
                        state = state.push((None, None, ev[1], None))
        return state

    def apply_events(self, evs, paths):
        for ev in evs:
            if paths is None:
                return None
            kind = ev[0]
            if kind == "guard":
                # Several guards in one block release in reverse order, so
                # the block ends at the first one's mark.
                if self.scopes[-1].guard is None:
                    self.scopes[-1].guard = paths
            elif kind == "mark":
                self.scopes[-1].marks[ev[2]] = paths
            elif kind == "release":
                target = self.find_mark(ev[2]) if ev[2] else None
                paths = target if target is not None else frozenset([UNKNOWN])
            else:
                if kind == "call" and may_allocate(ev[2], self.local_names()):
                    self.collect_point(paths, ev[2], ev[1])
                paths = frozenset(self.step(st, ev) for st in paths)
        return paths

    def run(self):
        body = self.f.body
        fscope = Scope("function", body.line, body.end_line, body)
        for pname, is_ptr, ty in self.f.params:
            if pname:
                fscope.decls[pname] = (False, is_ptr, ty)
        self.scopes = [fscope]
        paths = self.exec_stmts(body.stmts, frozenset([State()]))
        if paths is not None:
            paths = self.leave(paths, [fscope], "the end of the function",
                               body.end_line, final=True)
            self.exits.append(paths)
        return self.exits

    def exec_stmts(self, stmts, state):
        for st in stmts:
            if state is None and st.kind not in ("label", "case"):
                continue
            state = self.exec(st, state)
        return state

    def exec_block(self, blk, state, scope=None):
        scope = scope or Scope("block", blk.line, blk.end_line, blk)
        self.scopes.append(scope)
        state = self.exec_stmts(blk.stmts, state)
        if state is not None:
            state = self.leave(state, [scope], "the end of the block",
                               blk.end_line)
        self.scopes.pop()
        return state

    def exec(self, st, state):
        k = st.kind
        if k == "block":
            return self.exec_block(st, state)
        if k == "simple":
            self.declare(st.toks)
            return self.apply_events(events(st.toks), state)
        if k == "if":
            state = self.apply_events(events(st.cond), state)
            a = self.exec_sub(st.then, state)
            b = self.exec_sub(st.els, state) if st.els is not None else state
            return merge(a, b)
        if k == "loop":
            return self.exec_loop(st, state)
        if k == "protected":
            scope = Scope("protected", st.line, getattr(st.body, "end_line", st.line))
            scope.guard = state
            self.scopes.append(scope)
            s = self.exec_sub(st.body, state)
            s = self.leave(s, [scope], "the end of the GC_PROTECTED block", scope.end_line)
            self.scopes.pop()
            return s
        if k == "switch":
            state = self.apply_events(events(st.cond), state)
            ctx = LoopCtx("switch", len(self.scopes))
            self.loops.append(ctx)
            self.switch_entries.append([state, False])
            end = self.exec_sub(st.body, None)
            entry, _ = self.switch_entries.pop()
            self.loops.pop()
            outs = ctx.breaks + [end]
            if not self.has_default(st.body):
                outs.append(entry)
            return merge(*outs)
        if k == "case":
            if self.switch_entries:
                return merge(state, self.switch_entries[-1][0])
            return state
        if k == "label":
            return merge(state, *self.gotos.pop(st.name, []))
        if k == "return":
            state = self.apply_events(events(st.expr), state)
            out = self.leave(state, list(self.scopes), "the return", st.line,
                             final=True)
            if out is not None:
                self.exits.append(out)
            return None
        if k in ("break", "continue"):
            ctx = None
            for c in reversed(self.loops):
                if k == "break" or c.kind == "loop":
                    ctx = c
                    break
            if ctx is None:
                return None
            out = self.leave(state, self.scopes[ctx.depth:], "the " + k, st.line)
            (ctx.breaks if k == "break" else ctx.continues).append(out)
            return None
        if k == "goto":
            target = self.f.labels.get(st.label)
            chain = []
            b = target
            while b is not None:
                chain.append(b)
                b = b.parent
            keep = 0
            for i, s in enumerate(self.scopes):
                if s.block is not None and s.block in chain:
                    keep = i + 1
                elif s.block is None and keep == i:
                    keep = i + 1
                else:
                    break
            out = self.leave(state, self.scopes[keep:], "the goto", st.line)
            self.gotos.setdefault(st.label, []).append(out)
            return None
        return state

    @staticmethod
    def has_default(body):
        if body is None or body.kind != "block":
            return False
        return any(getattr(s, "is_default", False) for s in body.stmts)

    def exec_sub(self, st, state):
        if st is None:
            return state
        return self.exec(st, state)

    def exec_loop(self, st, state):
        scope = None
        if st.loop in ("for", "forlist", "macro"):
            scope = Scope("for", st.line, getattr(st.body, "end_line", st.line))
            self.scopes.append(scope)
            if st.loop == "for":
                self.declare(st.init)
            elif st.decl_names:
                for nm in st.decl_names:
                    scope.decls[nm] = (False, False, "unsigned")
            state = self.apply_events(events(st.init), state)
        if st.loop != "do":
            state = self.apply_events(events(st.cond), state)
        ctx = LoopCtx("loop", len(self.scopes))
        self.loops.append(ctx)
        entry = state
        end = self.exec_sub(st.body, entry)
        self.loops.pop()
        back = merge(end, *ctx.continues)
        if back is not None and any(isinstance(x, State) and
                                    any(e[3] for e in x.stack) for x in back):
            # The next iteration runs with the dangling root.
            name = first_allocating_call([st.cond, st.step, st.body])
            if name:
                self.collect_point(back, name[0], name[1])
        if back is not None:
            back = self.apply_events(events(st.step) + events(st.cond)
                                     if st.loop != "do" else events(st.cond), back)
        outs = list(ctx.breaks)
        if not st.infinite:
            outs.append(entry if st.loop != "do" else back)
            if back is not None and st.loop != "do":
                outs.append(back)
        state = merge(*outs)
        if scope is not None:
            state = self.leave(state, [scope], "the end of the loop", scope.end_line)
            self.scopes.pop()
        return state


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------

class SourceFile:
    def __init__(self, path):
        self.path = path
        with open(path, encoding="utf-8", errors="replace") as f:
            self.text = f.read()
        code, self.directives = blank_preprocessor(self.text)
        self.toks, self.suppress = lex(code)
        self.lines = self.text.split("\n")
        self.funcs = []
        self.macros = []


def function_summaries(files, typedefs, rounds=3):
    """name -> (pops, [pointer-param indices left protected]) for functions
    that leave the shadow stack changed on return, UNKNOWN when it cannot be
    told, absent when balanced."""
    summaries = {}
    for _ in range(rounds):
        new = {}
        for sf in files:
            for fn in sf.funcs:
                an = ScopeAnalyzer(fn, summaries, typedefs, lambda *a: None)
                exits = set()
                for paths in an.run():
                    if paths is not None:
                        exits |= set(paths)
                if not exits:
                    continue
                if UNKNOWN in exits or len(exits) > 1:
                    # Only matters when some exit is unbalanced.
                    if all(e is UNKNOWN or (not e.stack and not e.under)
                           for e in exits):
                        continue
                    new[fn.name] = UNKNOWN
                    continue
                e = next(iter(exits))
                if not e.stack and not e.under:
                    continue
                if all(isinstance(v, tuple) for v, _, _, _ in e.stack):
                    new[fn.name] = (e.under, [v[1] for v, _, _, _ in e.stack])
                else:
                    new[fn.name] = (e.under, [99] * len(e.stack))
        if new == summaries:
            break
        summaries = new
    return summaries


def check_scope(sf, fn, summaries, typedefs, findings, strict=False):
    def report(pline, var, message):
        findings.append(Finding(sf.path, pline, "protect-scope", fn.name, var,
                                message))
    an = ScopeAnalyzer(fn, summaries, typedefs, report)
    an.strict = strict
    an.run()


# -- raw-vector-store --------------------------------------------------------

ASSIGN_OPS = {"=", "+=", "-=", "*=", "/=", "|=", "&=", "^=", "<<=", ">>=", "%="}
MEM_FUNCS = {"memcpy", "memmove", "memset"}


def payload_expr_end(toks, i, vd_vars, ptr_vars):
    """If toks[i:] starts a vector payload pointer expression, return the
    index just past it, else None."""
    n = len(toks)
    x = toks[i].text
    if x == "vector_data_ptr" and i + 1 < n and toks[i + 1].text == "(":
        return match_close(toks, i + 1) + 1
    if x == "GET_VECTOR_PTR" and i + 1 < n and toks[i + 1].text == "(":
        j = match_close(toks, i + 1) + 1
        if j + 1 < n and toks[j].text == "->" and toks[j + 1].text == "data":
            return j + 2
        return None
    if x in vd_vars and i + 2 < n and toks[i + 1].text == "->" and \
            toks[i + 2].text == "data":
        return i + 3
    if x in ptr_vars:
        return i + 1
    return None


def is_payload_init(init, vd_vars, ptr_vars):
    """Does an initializer/assignment RHS yield a payload pointer?"""
    tx = texts(init)
    if not tx:
        return False
    k = 0
    if tx[0] == "&":
        k = 1
    while k < len(tx) and tx[k] == "(":
        k += 1
    if k < len(init):
        end = payload_expr_end(init, k, vd_vars, ptr_vars)
        if end is not None and init[k].text not in ptr_vars:
            return True
    return False


def is_vd_init(init):
    tx = texts(init)
    return "GET_VECTOR_PTR" in tx or ("vector_data" in tx and "CELL_PTR" in tx)


def harmless_rhs(rhs, base_name):
    """Constants, and loads from the same payload, cannot create a new
    old-to-young reference."""
    tx = texts(rhs)
    if not tx:
        return True
    if all(t.kind == "num" or (t.kind == "id" and re.match(r"^[A-Z][A-Z0-9_]*$", t.text))
           or t.text in "()+-*/|&~<>" or t.text in ("<<", ">>")
           for t in rhs):
        return True
    if len(tx) == 3 and tx[0] == "ctx" and tx[1] == "." and tx[2].startswith("atom_"):
        return True
    if base_name and tx and tx[0] == base_name and (len(tx) == 1 or tx[1] == "["):
        if "[" not in tx or tx.count("[") == 1:
            return True
    return False


FRESH_ALLOCATORS = {"make_vector", "make_record", "make_record_type",
                    "make_vector_like"}


def vector_source(toks, k):
    """For a payload expression starting at toks[k], the plain variable
    naming the vector it belongs to (or None)."""
    if k + 3 < len(toks) and toks[k].text in ("vector_data_ptr", "GET_VECTOR_PTR") \
            and toks[k + 1].text == "(" and toks[k + 2].kind == "id" \
            and toks[k + 3].text == ")":
        return toks[k + 2].text
    return None


def check_vector_stores(sf, fn, typedefs, findings):
    if os.path.basename(sf.path) == "context.c":
        return
    vd_vars, ptr_vars = set(), set()
    for pname, is_ptr, ty in fn.params:
        if pname and "vector_data" in ty and "const" not in ty:
            vd_vars.add(pname)

    # Statements in source order, each with the loops enclosing it.
    stmts = []

    def collect(st, loops):
        if st is None:
            return
        if st.kind == "block":
            for x in st.stmts:
                collect(x, loops)
        elif st.kind == "simple":
            stmts.append((st.toks, loops))
        elif st.kind == "if":
            stmts.append((st.cond, loops))
            collect(st.then, loops)
            collect(st.els, loops)
        elif st.kind == "loop":
            stmts.append((st.init, loops))
            inner = loops + (st,)
            stmts.append((st.cond, inner))
            stmts.append((st.step, inner))
            collect(st.body, inner)
        elif st.kind == "switch":
            stmts.append((st.cond, loops))
            collect(st.body, loops)
        elif st.kind == "protected":
            collect(st.body, loops)
        elif st.kind == "return":
            stmts.append((st.expr, loops))
    collect(fn.body, ())

    loop_allocates = {}

    def allocates_in(loop):
        if id(loop) not in loop_allocates:
            loop_allocates[id(loop)] = first_allocating_call(
                [loop.cond, loop.step, loop.body]) is not None
        return loop_allocates[id(loop)]

    def stmt_allocates(toks):
        return any(ev[0] == "call" and may_allocate(ev[2]) for ev in events(toks))

    fresh = {}       # vector variable -> (statement index, enclosing loops)
    ptr_src = {}     # payload pointer variable -> vector variable

    def still_fresh(vec, k):
        """No collection can have run between VEC's allocation and the
        store in statement K, so VEC is still in the nursery."""
        if vec not in fresh:
            return False
        c, cloops = fresh[vec]
        if any(stmt_allocates(stmts[j][0]) for j in range(c + 1, k)):
            return False
        if stmt_allocates(stmts[k][0]):
            return False
        return not any(allocates_in(lp) for lp in stmts[k][1] if lp not in cloops)

    def barrier_before(k):
        return any("write_barrier" in texts(stmts[j][0]) for j in (k - 1, k) if j >= 0)

    for k, (st, loops) in enumerate(stmts):
        if not st:
            continue
        decl = parse_declaration(st, typedefs)
        assigns = []
        if decl:
            for name, static, ptr, ty, init in decl:
                if "vector_data" in ty and ptr and "const" not in ty.split():
                    vd_vars.add(name)
                assigns.append((name, init))
        n = len(st)
        for i in range(n - 2):
            if st[i].kind == "id" and st[i + 1].text == "=" and \
                    (i == 0 or st[i - 1].text in (";", ",", "(", "{")):
                assigns.append((st[i].text, st[i + 2:]))
        for name, rhs in assigns:
            tx = texts(rhs)
            if tx and tx[0] in FRESH_ALLOCATORS:
                fresh[name] = (k, loops)
            elif name in fresh and not decl:
                del fresh[name]
            if is_payload_init(rhs, vd_vars, ptr_vars):
                ptr_vars.add(name)
                kk = 1 if tx and tx[0] == "&" else 0
                ptr_src[name] = vector_source(rhs, kk)
            elif is_vd_init(rhs):
                vd_vars.add(name)
                ptr_src[name] = vector_source(rhs, 0)

        def flag(target, how, line, base_tok, base_vec):
            vec = ptr_src.get(base_tok) or base_vec
            if vec and still_fresh(vec, k):
                return
            if barrier_before(k):
                return
            report_store(sf, fn, line, target, how, findings)

        i = 0
        while i < n:
            t = st[i]
            prev = st[i - 1].text if i > 0 else None
            boundary = prev in (None, ";", "{", "}", "(", ",", "=", "?", ":", "&&",
                                "||", "!", "return") or prev in ASSIGN_OPS
            if t.kind == "id" and t.text in MEM_FUNCS and i + 1 < n and \
                    st[i + 1].text == "(":
                close = match_close(st, i + 1)
                args = split_commas(st[i + 2:close])
                if args and args[0]:
                    a = args[0]
                    kk = 1 if a[0].text == "&" else 0
                    while kk < len(a) and a[kk].text == "(":
                        kk += 1
                    if kk < len(a) and payload_expr_end(a, kk, vd_vars, ptr_vars):
                        flag(joined(a), t.text, a[kk].line, a[kk].text,
                             vector_source(a, kk))
                i = close + 1
                continue
            if boundary and t.kind == "id":
                end = payload_expr_end(st, i, vd_vars, ptr_vars)
                if end is not None and end < n and st[end].text == "[":
                    close = match_close(st, end)
                    if close + 1 < n and st[close + 1].text in ASSIGN_OPS:
                        rhs = rhs_tokens(st, close + 2)
                        base = st[i].text if st[i].text in ptr_vars else None
                        if not harmless_rhs(rhs, base):
                            flag(joined(st[i:close + 1]), "=", t.line, st[i].text,
                                 vector_source(st, i))
                        i = close + 2
                        continue
            if boundary and t.text == "*" and i + 1 < n:
                kk = i + 1
                paren = st[kk].text == "("
                if paren:
                    kk += 1
                if kk < n and st[kk].kind == "id":
                    end = payload_expr_end(st, kk, vd_vars, ptr_vars)
                    if end is not None:
                        j = end
                        if paren:
                            j = match_close(st, i + 1) + 1
                        elif j < n and st[j].text in ("++", "--"):
                            j += 1
                        if j < n and st[j].text in ASSIGN_OPS:
                            rhs = rhs_tokens(st, j + 1)
                            base = st[kk].text if st[kk].text in ptr_vars else None
                            if not harmless_rhs(rhs, base):
                                flag(joined(st[i:j]), "=", t.line, st[kk].text,
                                     vector_source(st, kk))
            i += 1


def rhs_tokens(st, j):
    out, depth = [], 0
    while j < len(st):
        x = st[j].text
        if x in "([{":
            depth += 1
        elif x in ")]}":
            if depth == 0:
                break
            depth -= 1
        elif x in (",", ";") and depth == 0:
            break
        out.append(st[j])
        j += 1
    return out


def report_store(sf, fn, line, target, how, findings):
    if how in MEM_FUNCS:
        msg = ("%s() into the vector payload %s bypasses the write barrier; "
               "copy element by element with vector_set_elem()" % (how, target))
    else:
        msg = ("raw store to %s bypasses the write barrier - an old vector "
               "pointing at a nursery cell is never remembered; use "
               "vector_set_elem()" % target)
    findings.append(Finding(sf.path, line, "raw-vector-store", fn.name,
                            re.sub(r"\s+", "", target), msg))


# -- last-error-overlap --------------------------------------------------------

RAW_WRITERS = {"snprintf", "sprintf", "vsnprintf", "vsprintf", "strcpy",
               "strncpy", "strcat", "strncat", "memcpy", "strlcpy", "strlcat"}


def is_last_error(arg):
    tx = texts(arg)
    return len(tx) >= 3 and tx[:3] == ["ctx", ".", "last_error"] and \
        (len(tx) == 3 or tx[3] in ("+",))


def strip_sizeof(arg):
    """Drop sizeof(...) operands: they read no memory."""
    out = []
    i = 0
    while i < len(arg):
        if arg[i].text == "sizeof" and i + 1 < len(arg) and arg[i + 1].text == "(":
            i = match_close(arg, i + 1) + 1
            continue
        out.append(arg[i])
        i += 1
    return out


def mentions(arg, names):
    tx = texts(strip_sizeof(arg))
    for k in range(len(tx)):
        if tx[k:k + 3] == ["ctx", ".", "last_error"]:
            return "ctx.last_error"
        if tx[k] in names and (k == 0 or tx[k - 1] not in (".", "->")):
            return tx[k]
    return None


def last_error_writers(units):
    """Functions and macros with a parameter that reaches the source side of
    a raw write into ctx.last_error. name -> set of parameter indices ('...'
    for variadic arguments)."""
    writers = {}
    for _ in range(4):
        changed = False
        for name, params, toks, variadic in units:
            pnames = {p: i for i, p in enumerate(params) if p}
            hits = set(writers.get(name, set()))
            for i in range(len(toks) - 1):
                t = toks[i]
                if t.kind != "id" or toks[i + 1].text != "(":
                    continue
                close = match_close(toks, i + 1)
                args = split_commas(toks[i + 2:close])
                if t.text in RAW_WRITERS and args and is_last_error(args[0]):
                    srcs = args[1:]
                    for a in srcs:
                        for tok in texts(strip_sizeof(a)):
                            if tok in pnames:
                                hits.add(pnames[tok])
                            if tok in ("__VA_ARGS__", "ap", "args") and variadic:
                                hits.add("...")
                elif t.text in writers:
                    for idx in writers[t.text]:
                        if idx == "...":
                            continue
                        if idx < len(args):
                            for tok in texts(args[idx]):
                                if tok in pnames:
                                    hits.add(pnames[tok])
                    if "..." in writers[t.text]:
                        fixed = max([k for k in writers[t.text] if k != "..."] + [-1])
                        for a in args[fixed + 1:]:
                            for tok in texts(a):
                                if tok in pnames:
                                    hits.add(pnames[tok])
                                if tok == "__VA_ARGS__":
                                    hits.add("...")
            if hits != writers.get(name, set()):
                writers[name] = hits
                changed = True
        if not changed:
            break
    return {k: v for k, v in writers.items() if v}


def check_last_error(sf, fn_name, toks, writers, nparams_of, findings, is_macro=False):
    # Local pointers set from ctx.last_error alias it.
    aliases = set()
    for i in range(len(toks) - 4):
        if toks[i].kind == "id" and toks[i + 1].text == "=" and \
                texts(toks[i + 2:i + 5]) == ["ctx", ".", "last_error"]:
            aliases.add(toks[i].text)
    for i in range(len(toks) - 1):
        t = toks[i]
        if t.kind != "id" or toks[i + 1].text != "(":
            continue
        close = match_close(toks, i + 1)
        args = split_commas(toks[i + 2:close])
        if t.text in RAW_WRITERS and args and is_last_error(args[0]):
            for a in args[1:]:
                m = mentions(a, aliases)
                if m:
                    findings.append(Finding(
                        sf.path, t.line, "last-error-overlap", fn_name, t.text,
                        "%s() writes ctx.last_error while reading %s, which "
                        "overlaps it (undefined behaviour); format into a "
                        "local buffer first" % (t.text, m)))
                    break
        elif t.text in writers and t.text != fn_name:
            idxs = writers[t.text]
            positions = [k for k in idxs if k != "..."]
            fixed = nparams_of.get(t.text, 0)
            cand = [(k, args[k]) for k in positions if k < len(args)]
            if "..." in idxs:
                cand += [(k, args[k]) for k in range(fixed, len(args))]
            for k, a in cand:
                m = mentions(a, aliases)
                if m:
                    findings.append(Finding(
                        sf.path, t.line, "last-error-overlap", fn_name, t.text,
                        "%s() copies argument %d into ctx.last_error with a raw "
                        "write, but it is passed %s, which overlaps it"
                        % (t.text, k + 1, m)))
                    break


# ---------------------------------------------------------------------------

def default_files():
    out = []
    for name in sorted(os.listdir(ROOT)):
        if not name.endswith((".c", ".h")):
            continue
        if name in SKIP_FILES or name.startswith(SKIP_PREFIXES):
            continue
        out.append(os.path.join(ROOT, name))
    return out


def load_allowlist(path):
    entries = []
    if not path or not os.path.exists(path):
        return entries
    with open(path) as f:
        for n, line in enumerate(f, 1):
            body = line.split("#", 1)[0].strip()
            if not body:
                continue
            parts = body.split()
            if len(parts) != 4:
                print("%s:%d: malformed allowlist entry (want: file function "
                      "check subject)" % (path, n), file=sys.stderr)
                continue
            entries.append((tuple(parts), n))
    return entries


def analyze(paths, all_paths, strict=False):
    """Parse ALL_PATHS (for typedefs, helpers and macros) and lint PATHS.
    Returns (files, findings)."""
    files = {p: SourceFile(p) for p in all_paths}
    typedefs = set()
    for sf in files.values():
        typedefs |= find_typedefs(sf.toks)
    for sf in files.values():
        sf.funcs = find_functions(sf.path, sf.toks)
        sf.macros = find_macros(sf.path, sf.directives)
        for fn in sf.funcs:
            p = Parser(fn.toks, typedefs)
            fn.body, _ = p.block(0, None)
            fn.labels = p.labels

    infer_allocating([fn for sf in files.values() for fn in sf.funcs],
                     [m for sf in files.values() for m in sf.macros])
    summaries = function_summaries(list(files.values()), typedefs)

    units = []
    nparams_of = {}
    for sf in files.values():
        for fn in sf.funcs:
            units.append((fn.name, [p[0] for p in fn.params], fn.toks, fn.variadic))
            nparams_of[fn.name] = len(fn.params)
        for name, params, toks, line in sf.macros:
            variadic = "..." in params
            units.append((name, [p for p in params if p != "..."], toks, variadic))
            nparams_of[name] = len([p for p in params if p != "..."])
    writers = last_error_writers(units)

    findings = []
    for path in paths:
        sf = files[path]
        for fn in sf.funcs:
            check_scope(sf, fn, summaries, typedefs, findings, strict)
            check_vector_stores(sf, fn, typedefs, findings)
            check_last_error(sf, fn.name, fn.toks, writers, nparams_of, findings)
        for name, params, toks, line in sf.macros:
            check_last_error(sf, name, toks, writers, nparams_of, findings,
                             is_macro=True)
    return files, findings, summaries, writers


def apply_suppressions(files, findings, paths):
    """Split findings into (remaining, problems, warnings, used)."""
    remaining, used, problems = [], set(), []
    for f in findings:
        sf = files[f.path]
        reason = None
        for ln in (f.line, f.line - 1):
            if ln in sf.suppress:
                if ln == f.line - 1 and sf.lines[ln - 1].strip() and \
                        not sf.lines[ln - 1].strip().startswith(("//", "/*")):
                    continue  # a trailing comment on the previous statement
                reason = sf.suppress[ln]
                used.add((f.path, ln))
                break
        if reason is None:
            remaining.append(f)
        elif not reason:
            problems.append("%s:%d: [lint-gc] suppression needs a reason: "
                            "// lint-gc: ok <why this is safe>"
                            % (os.path.relpath(f.path, ROOT), f.line))
    warnings = []
    for path in paths:
        sf = files[path]
        for ln in sorted(sf.suppress):
            if (path, ln) not in used:
                warnings.append("%s:%d: warning: lint-gc suppression matches no "
                                "finding" % (os.path.relpath(path, ROOT), ln))
    return remaining, problems, warnings, used


# Every check must keep firing on its bug class and stay quiet on the safe
# idioms next to it: `make lint` runs this first. A line tagged
# `expect: <check>` must be flagged with that check; nothing else may be.
SELF_TEST = r"""
typedef struct { unsigned len; unsigned data[]; } vector_data;
struct context { char last_error[256]; unsigned atom_false; } ctx;
unsigned alloc(void);
unsigned car(unsigned x) { return x; }
unsigned alloc_cons(unsigned a, unsigned b) { return alloc() + a + b; }
unsigned make_vector(unsigned n, unsigned fill) { return alloc() + n + fill; }
unsigned *vector_data_ptr(unsigned v);
void write_barrier(unsigned target, unsigned value);
void gc_protect(unsigned *p);
void gc_unprotect(int n);
int gc_mark(void);
void gc_release(int m);
#define GC_GUARD int guard_ = gc_mark()
#define SET_ERROR(msg) snprintf(ctx.last_error, sizeof(ctx.last_error), "%s", msg)

static void protect_for_caller(unsigned *a) { gc_protect(a); }

unsigned dangling_block(unsigned x)
{
    GC_GUARD;
    if (x) {
        unsigned t = alloc_cons(x, 0);
        gc_protect(&t);                       // expect: protect-scope
        x = car(t);
    }
    return alloc_cons(x, x);
}

unsigned dangling_loop(unsigned lst)
{
    GC_GUARD;
    while (lst) {
        unsigned item = car(lst);
        gc_protect(&item);                    // expect: protect-scope
        lst = alloc_cons(item, lst);
    }
    return lst;
}

unsigned unguarded_return(unsigned x)
{
    unsigned y = alloc_cons(x, x);
    gc_protect(&y);                           // expect: protect-scope
    if (y == 7)
        return 0;
    gc_unprotect(1);
    return y;
}

unsigned helper_root(unsigned x)
{
    GC_GUARD;
    {
        unsigned kept = x;
        protect_for_caller(&kept);            // expect: protect-scope
    }
    return alloc_cons(x, 0);
}

unsigned balanced(unsigned x)
{
    GC_GUARD;
    if (x) {
        unsigned t = alloc_cons(x, 0);
        gc_protect(&t);
        x = alloc_cons(t, t);
        gc_unprotect(1);
    }
    for (unsigned i = 0; i < 3; i++) {
        GC_GUARD;
        unsigned u = alloc_cons(x, i);
        gc_protect(&u);
        x = alloc_cons(u, u);
    }
    {
        unsigned tail = x;
        gc_protect(&tail);
        if (!tail) {
            gc_unprotect(1);
            return 0;
        }
        x = alloc_cons(tail, tail);
        gc_unprotect(1);
    }
    {
        unsigned harmless = x;
        gc_protect(&harmless);   // dangles, but nothing collects before return
    }
    return x;
}

void stores(unsigned v, unsigned w, unsigned x)
{
    unsigned *data = vector_data_ptr(v);
    data[0] = x;                              // expect: raw-vector-store
    vector_data_ptr(w)[1] = x;                // expect: raw-vector-store
    vector_data *vd = (vector_data *)0;
    vd->data[2] = x;                          // expect: raw-vector-store
    memcpy(vector_data_ptr(v), data, 4);      // expect: raw-vector-store
    GET_VECTOR_PTR(w)->data[0] = x;           // expect: raw-vector-store
    unsigned *slot = &vector_data_ptr(w)[3];
    *slot = x;                                // expect: raw-vector-store
    data[1] = 0;
    data[2] = data[3];
    vd->data[3] = ctx.atom_false;
    write_barrier(v, x);
    data[4] = x;
    unsigned fresh = make_vector(2, 0);
    unsigned *fd = vector_data_ptr(fresh);
    fd[0] = x;
    fd[1] = car(x);
    unsigned stale = make_vector(2, 0);
    unsigned *sd = vector_data_ptr(stale);
    x = alloc_cons(x, x);
    sd[0] = x;                                // expect: raw-vector-store
    data[5] = x; // lint-gc: ok self-test of a suppression
}

static void set_error_from(const char *why)
{
    snprintf(ctx.last_error, sizeof(ctx.last_error), "failed: %s", why);
}

void errors(void)
{
    char buf[256];
    snprintf(ctx.last_error, sizeof(ctx.last_error), "%s", ctx.last_error); // expect: last-error-overlap
    const char *old = ctx.last_error;
    snprintf(ctx.last_error, sizeof(ctx.last_error), "x: %s", old); // expect: last-error-overlap
    set_error_from(ctx.last_error);           // expect: last-error-overlap
    SET_ERROR(ctx.last_error);                // expect: last-error-overlap
    snprintf(buf, sizeof(buf), "%s", ctx.last_error);
    snprintf(ctx.last_error, sizeof(ctx.last_error), "%s", buf);
    set_error_from(buf);
}
"""


def self_test():
    import tempfile
    fd, path = tempfile.mkstemp(suffix=".c")
    try:
        with os.fdopen(fd, "w") as f:
            f.write(SELF_TEST)
        files, findings, _, _ = analyze([path], [path])
        remaining, problems, warnings, used = apply_suppressions(files, findings, [path])
        got = {(f.line, f.check) for f in remaining}
        want = set()
        for n, line in enumerate(SELF_TEST.split("\n"), 1):
            m = re.search(r"expect: ([a-z-]+)", line)
            if m:
                want.add((n, m.group(1)))
        ok = got == want and not problems and not warnings and len(used) == 1
        if not ok:
            for f in remaining:
                if (f.line, f.check) not in want:
                    print("self-test: unexpected finding: line %d [%s] %s"
                          % (f.line, f.check, f.message))
            for line, check in sorted(want - got):
                print("self-test: missed: line %d [%s]: %s"
                      % (line, check, SELF_TEST.split("\n")[line - 1].strip()))
            for p in problems + warnings:
                print("self-test: %s" % p)
            print("lint-gc self-test: FAILED")
            return 1
        print("lint-gc self-test: %d expected findings, no others" % len(want))
        return 0
    finally:
        os.unlink(path)


def main(argv=None):
    ap = argparse.ArgumentParser(description="GC-safety lint for vesper")
    ap.add_argument("files", nargs="*")
    ap.add_argument("--allowlist", default=DEFAULT_ALLOWLIST)
    ap.add_argument("--no-allowlist", action="store_true",
                    help="report allowlisted findings too")
    ap.add_argument("--list-allowlisted", action="store_true",
                    help="print the findings the allowlist accepts")
    ap.add_argument("--strict", action="store_true",
                    help="report every root that outlives its variable's "
                         "scope, even when nothing can collect before it is "
                         "released")
    ap.add_argument("--self-test", action="store_true",
                    help="check the lint against its built-in cases and exit")
    ap.add_argument("--verbose", action="store_true",
                    help="print helper-function summaries and last_error "
                         "writers")
    args = ap.parse_args(argv)
    if args.self_test:
        return self_test()

    paths = [os.path.abspath(p) for p in args.files] if args.files else default_files()
    # Headers are always parsed (typedefs, inline helpers, macros) even when
    # only some files are reported on.
    all_paths = sorted(set(paths) | set(default_files()))
    files, findings, summaries, writers = analyze(paths, all_paths, args.strict)
    if args.verbose:
        for k, v in sorted(summaries.items(), key=lambda kv: kv[0]):
            print("summary %s: %s" % (k, v))
        for k, v in sorted(writers.items()):
            print("last_error writer %s: %s" % (k, sorted(map(str, v))))

    remaining, problems, warnings, used = apply_suppressions(files, findings, paths)

    allow = [] if args.no_allowlist else load_allowlist(args.allowlist)
    allowed_keys = {k for k, _ in allow}
    reported, accepted = [], []
    for f in remaining:
        (accepted if f.key() in allowed_keys else reported).append(f)
    matched = {f.key() for f in accepted}
    if not args.files:
        for k, n in allow:
            if k not in matched:
                warnings.append("%s:%d: warning: allowlist entry matches no "
                                "finding (fixed? remove it): %s"
                                % (os.path.relpath(args.allowlist, ROOT), n, " ".join(k)))

    reported.sort(key=lambda f: (f.path, f.line))
    for f in reported:
        print(f)
    for p in problems:
        print(p)
    if args.list_allowlisted:
        for f in sorted(accepted, key=lambda f: (f.path, f.line)):
            print("allowlisted: %s" % f)
    for w in warnings:
        print(w, file=sys.stderr)
    nfiles = len(paths)
    if reported or problems:
        print("lint-gc: %d finding(s) in %d files (%d allowlisted)"
              % (len(reported) + len(problems), nfiles, len(accepted)))
        return 1
    print("lint-gc: %d files clean (%d allowlisted finding(s), %d suppressed)"
          % (nfiles, len(accepted), len(used)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
