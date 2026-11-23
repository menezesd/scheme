#!/usr/bin/env python3
"""Drive the REPL's line editor through a pseudo-terminal.

Starts ./vesper (and ./vesper --interpreter) on a pty, types keystrokes,
and checks both what the REPL evaluates and what the terminal shows: the
output is fed through a small VT100 model, so the editor's redraws are
checked cell by cell, cursor included. Covers cursor movement and editing
keys (arrows, Home/End, Delete, Backspace, Ctrl-A/B/D/E/F/K/L/T/U/W/Y,
Alt-b/f), UTF-8, long lines that wrap, multi-line entries with continuation
prompts (open lists, strings, block comments, quote prefixes), history
navigation and the history file across sessions, bracketed paste, Ctrl-C,
Ctrl-D, and that the terminal is back in its original mode while code runs,
after exit, and after a fatal signal. Also checks that the editor stays out
of the way when output is not a terminal.

Usage: tools/test_repl_lineedit.py [--verbose] [path/to/vesper]
Exit status 0 when every check passed.
"""

import os
import re
import select
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import time
import fcntl

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
VESPER = os.path.join(ROOT, "vesper")
ROWS, COLS = 24, 80
VERBOSE = False

UP, DOWN, RIGHT, LEFT = "\x1b[A", "\x1b[B", "\x1b[C", "\x1b[D"
HOME, END, DELETE = "\x1b[H", "\x1b[F", "\x1b[3~"
PASTE_START, PASTE_END = "\x1b[200~", "\x1b[201~"


def ctrl(c):
    return chr(ord(c.upper()) - 64)


class Screen:
    """Just enough of a VT100 for the sequences the editor emits."""

    def __init__(self, rows=ROWS, cols=COLS):
        self.rows, self.cols = rows, cols
        self.cells = [[" "] * cols for _ in range(rows)]
        self.r = self.c = 0
        self.pending_wrap = False
        self.tail = ""

    def feed(self, data):
        text = self.tail + data.decode("utf-8", "replace")
        self.tail = ""
        i = 0
        while i < len(text):
            ch = text[i]
            if ch == "\x1b":
                m = re.match(r"\x1b\[([?0-9;]*)([@-~])", text[i:])
                if not m:
                    if len(text) - i < 8:
                        self.tail = text[i:]  # an incomplete sequence
                        return
                    i += 1
                    continue
                self.csi(m.group(1), m.group(2))
                i += m.end()
                continue
            if ch == "\r":
                self.c = 0
                self.pending_wrap = False
            elif ch == "\n":
                self.linefeed()
            elif ch == "\b":
                self.c = max(0, self.c - 1)
                self.pending_wrap = False
            elif ch >= " ":
                self.put(ch)
            i += 1

    def put(self, ch):
        if self.pending_wrap:
            self.c = 0
            self.linefeed()
            self.pending_wrap = False
        self.cells[self.r][self.c] = ch
        if self.c == self.cols - 1:
            self.pending_wrap = True
        else:
            self.c += 1

    def linefeed(self):
        self.pending_wrap = False
        if self.r == self.rows - 1:
            self.cells.pop(0)
            self.cells.append([" "] * self.cols)
        else:
            self.r += 1

    def csi(self, params, final):
        n = int(params) if params.isdigit() else 1
        if final == "A":
            self.r = max(0, self.r - n)
        elif final == "B":
            self.r = min(self.rows - 1, self.r + n)
        elif final == "C":
            self.c = min(self.cols - 1, self.c + n)
        elif final == "D":
            self.c = max(0, self.c - n)
        elif final == "H":
            self.r = self.c = 0
        elif final == "J":
            if params == "2":
                self.cells = [[" "] * self.cols for _ in range(self.rows)]
            else:
                self.cells[self.r][self.c:] = [" "] * (self.cols - self.c)
                for r in range(self.r + 1, self.rows):
                    self.cells[r] = [" "] * self.cols
        elif final == "K":
            self.cells[self.r][self.c:] = [" "] * (self.cols - self.c)
        self.pending_wrap = False if final in "ABCDHJ" else self.pending_wrap

    def row(self, r):
        return "".join(self.cells[r]).rstrip()

    def nonblank_rows(self):
        return [self.row(r) for r in range(self.rows) if self.row(r)]

    def dump(self):
        return "\n".join("%2d|%s" % (r, self.row(r)) for r in range(self.rows)
                         if self.row(r)) + "\ncursor at row %d col %d" % (self.r, self.c)


class Repl:
    def __init__(self, args=(), history=None, stdout_tty=True, env=None):
        self.master, self.slave = os.openpty()
        fcntl.ioctl(self.slave, termios.TIOCSWINSZ,
                    struct.pack("HHHH", ROWS, COLS, 0, 0))
        self.initial_attrs = termios.tcgetattr(self.slave)
        e = dict(os.environ)
        e["TERM"] = "xterm"
        e["VESPER_HISTORY"] = history or ""
        e.setdefault("LC_CTYPE", "en_US.UTF-8")
        e.update(env or {})
        self.proc = subprocess.Popen(
            [VESPER] + list(args), stdin=self.slave,
            stdout=self.slave if stdout_tty else subprocess.PIPE,
            stderr=self.slave, env=e, close_fds=True)
        self.out = b""
        self.mark = 0
        self.screen = Screen()

    def pump(self, timeout):
        r, _, _ = select.select([self.master], [], [], timeout)
        if not r:
            return False
        try:
            data = os.read(self.master, 65536)
        except OSError:
            return False
        if VERBOSE:
            sys.stderr.write("<< %r\n" % data)
        self.out += data
        self.screen.feed(data)
        return True

    def expect(self, pattern, timeout=20):
        """Wait until PATTERN (a regex over the output since the last
        expect) appears; return the text up to the end of the match."""
        rx = re.compile(pattern.encode() if isinstance(pattern, str) else pattern)
        end = time.time() + timeout
        while True:
            m = rx.search(self.out, self.mark)
            if m:
                text = self.out[self.mark:m.end()]
                self.mark = m.end()
                return text.decode("utf-8", "replace")
            left = end - time.time()
            if left <= 0 or (self.proc.poll() is not None and not self.pump(0.2)):
                raise Failure("timed out waiting for %r\n--- output since last "
                              "match ---\n%r\n--- screen ---\n%s"
                              % (pattern, self.out[self.mark:][-600:],
                                 self.screen.dump()))
            self.pump(min(left, 0.5))

    def settle(self, quiet=0.15, limit=5):
        """Read until the output has been quiet for QUIET seconds."""
        end = time.time() + limit
        while time.time() < end and self.pump(quiet):
            pass

    def send(self, keys):
        if VERBOSE:
            sys.stderr.write(">> %r\n" % keys)
        os.write(self.master, keys.encode() if isinstance(keys, str) else keys)

    def prompt(self):
        return self.expect(r"\]=> ")

    def entry(self, keys, value):
        """Type KEYS (ending in Enter) and expect ;Value: VALUE, then the
        next prompt. The editor draws its prompt only once the terminal is
        raw again, so keys sent after it are never seen in cooked mode."""
        self.send(keys)
        self.expect(r";Value: " + re.escape(value) + r"\r?\n")
        self.prompt()

    def finish(self, timeout=20):
        end = time.time() + timeout
        while self.proc.poll() is None and time.time() < end:
            self.pump(0.1)
        if self.proc.poll() is None:
            self.proc.kill()
            self.proc.wait()
            raise Failure("REPL did not exit")
        self.settle(0.05, 1)
        return self.proc.returncode

    def attrs_restored(self):
        # PENDIN is the kernel's own note that queued input is to be
        # reprocessed after a switch to canonical mode, not a setting.
        pendin = getattr(termios, "PENDIN", 0)
        now = termios.tcgetattr(self.slave)
        before = list(self.initial_attrs)
        now[3] &= ~pendin
        before[3] &= ~pendin
        return now[:4] == before[:4] and now[6] == before[6]

    def close(self):
        if self.proc.poll() is None:
            self.proc.kill()
            self.proc.wait()
        for fd in (self.master, self.slave):
            try:
                os.close(fd)
            except OSError:
                pass


class Failure(Exception):
    pass


def check(cond, what, repl=None):
    if not cond:
        raise Failure(what + ("\n--- screen ---\n" + repl.screen.dump() if repl else ""))


def cursor_after(repl, row_text):
    """The cursor is on the row whose text is ROW_TEXT, just after it."""
    s = repl.screen
    return s.row(s.r) == row_text.rstrip() and s.c == len(row_text)


# ---------------------------------------------------------------------------

def test_editing_keys(engine_args):
    repl = Repl(engine_args)
    try:
        repl.prompt()
        repl.settle()
        check(repl.screen.row(repl.screen.r) == "]=>" and repl.screen.c == 4,
              "prompt drawn with the cursor after it", repl)
        repl.entry("(+ 1 2)\r", "3")
        # Left/Right and insertion in the middle
        repl.entry("(+ 1 )" + LEFT + "5\r", "6")
        # Backspace, Ctrl-H
        repl.entry("(+ 1 99" + "\x7f" + ctrl("h") + "2)\r", "3")
        # Home/End sequences and Ctrl-A/Ctrl-E
        repl.entry("+ 2 3)" + HOME + "(\r", "5")
        repl.entry("* 2 3" + ctrl("a") + "(" + ctrl("e") + ")\r", "6")
        repl.entry("(- 9 4" + "\x1bOH" + "\x1bOF" + ")\r", "5")
        # Delete key and Ctrl-D on a non-empty line delete under the cursor
        repl.entry("(+ 1 22)" + LEFT * 3 + DELETE + "\r", "3")
        repl.entry("(+ 1 x2)" + LEFT * 3 + ctrl("d") + "\r", "3")
        # Ctrl-K kills to the end of the line, Ctrl-Y yanks it back
        repl.entry("(list 1 2)" + LEFT * 3 + ctrl("k") + ")\r", "(1)")
        repl.entry("(list 5)" + ctrl("a") + ctrl("k") + "(car " + ctrl("y") +
                   ")\r", "5")
        # Ctrl-U kills to the start of the line
        repl.entry("garbage here" + ctrl("u") + "(* 7 6)\r", "42")
        # Ctrl-W kills the previous word
        repl.entry("(list 1 2 junk" + ctrl("w") + ")\r", "(1 2)")
        # Ctrl-B/Ctrl-F, Ctrl-T transposes
        repl.entry("(- 21 1)" + ctrl("b") * 3 + ctrl("t") + ctrl("f") * 9 + "\r", "-9")
        # Alt-b / Alt-f move by words
        repl.entry("(list 2 3)" + "\x1bb" * 2 + "1 " + "\x1bf" * 3 + "\r", "(1 2 3)")
        # Ctrl-Left (ESC[1;5D) moves by words too
        repl.entry("(list 20 30)" + "\x1b[1;5D" * 2 + "10 \r", "(10 20 30)")
        # UTF-8: cursor movement goes by code points
        repl.entry('"aλ"' + LEFT * 2 + "x\r", '"axλ"')
        repl.entry('(string-length "λλλ")' + LEFT * 3 + ctrl("h") + "\r", "2")
        # Ctrl-L clears the screen and redraws the entry at the top
        repl.send("(+ 4 4)" + ctrl("l"))
        repl.settle()
        check(repl.screen.row(0) == "]=> (+ 4 4)" and repl.screen.r == 0 and
              repl.screen.c == 11, "Ctrl-L redraws at the top", repl)
        repl.send("\r")
        repl.expect(r";Value: 8")
        repl.prompt()
        # Ctrl-C abandons the entry, even a multi-line one
        repl.send("(+ 1\r2")
        repl.expect(r"\.\.\. ")
        repl.send(ctrl("c"))
        repl.expect(r"\^C")
        repl.prompt()
        repl.entry("(+ 2 2)\r", "4")
        # A long line wraps; editing at its start redraws it correctly
        ones = " 1" * 45
        repl.send(ones + ")")
        repl.settle()
        line = "]=> " + ones + ")"
        s = repl.screen
        check(s.row(s.r - 1) == line[:COLS] and s.row(s.r) == line[COLS:] and
              s.c == len(line) - COLS, "a long line wraps onto the next row",
              repl)
        repl.send(ctrl("a") + "(+")
        repl.settle()
        line = "]=> (+" + ones + ")"
        check(s.row(s.r) == line[:COLS] and s.row(s.r + 1) == line[COLS:] and
              s.c == 6, "Ctrl-A on a wrapped line, redrawn", repl)
        repl.send("\r")
        repl.expect(r";Value: 45")
        repl.prompt()
        repl.settle()
        # An entry exactly filling the row leaves the cursor on the next row
        exact = "(+ 1" + " 0" * 35 + "0)"    # 4 + 76 = 80 columns
        check(len("]=> " + exact) == COLS, "test setup")
        repl.send(exact)
        repl.settle()
        check(repl.screen.c == 0 and repl.screen.row(repl.screen.r - 1) ==
              "]=> " + exact, "cursor wraps after a full-width line", repl)
        repl.send("\r")
        repl.expect(r";Value: 1")
        repl.prompt()
        # Ctrl-D on an empty line ends the session
        repl.settle()
        repl.send(ctrl("d"))
        code = repl.finish()
        check(code == 0, "Ctrl-D exits 0 (got %r)" % code)
        check(repl.attrs_restored(), "terminal mode restored after Ctrl-D")
    finally:
        repl.close()


def test_multiline(engine_args):
    repl = Repl(engine_args)
    try:
        repl.prompt()
        repl.send("(define (f x)\r")
        repl.expect(r"\.\.\. ")
        repl.settle()
        check(repl.screen.row(repl.screen.r - 1) == "]=> (define (f x)" and
              cursor_after(repl, "... "), "continuation prompt", repl)
        repl.entry("(* x 10))\r", "f")
        repl.entry("(f 4)\r", "40")
        # Up/Down move between the lines of an entry before reaching history
        repl.send("(list 1\r2)" + UP + END + " 9" + DOWN + "\r")
        repl.expect(r";Value: \(1 9 2\)")
        repl.prompt()
        # A string spanning lines keeps its newline
        repl.entry('(string-length "a\rb")\r', "3")
        # Block comments, quote prefixes and datum comments continue too
        repl.send("#| a comment\r")
        repl.expect(r"\.\.\. ")
        repl.entry("still |# 42\r", "42")
        repl.send("'\r")
        repl.expect(r"\.\.\. ")
        repl.entry("sym\r", "sym")
        repl.send("#;\r")
        repl.expect(r"\.\.\. ")
        repl.entry("(ignored) 7\r", "7")
        # Reader state carries over between entries as on a plain stdin:
        # case folding directives and datum labels.
        repl.entry("'Abc\r", "abc")
        repl.send("#!no-fold-case\r")
        repl.prompt()
        repl.entry("'Abc\r", "|Abc|")
        repl.send("#!fold-case 'XyZ\r")
        repl.expect(r";Value: xyz")
        repl.prompt()
        repl.entry("(let ((x '#0=(1 . #0#))) (eq? x (cdr x)))\r", "#t")
        # Characters that look like delimiters do not count
        repl.entry("(char->integer #\\()\r", "40")
        repl.entry("(string-length \"(\")\r", "1")
        repl.entry("(symbol->string '|a(b|)\r", '"a(b"')
        # A blank entry keeps the primary prompt
        repl.send("\r")
        repl.prompt()
        repl.send("; just a comment\r")
        repl.prompt()
        # Several forms on one line all evaluate
        repl.send("1 2\r")
        repl.expect(r";Value: 1\r?\n")
        repl.expect(r";Value: 2\r?\n")
        repl.prompt()
        # Reader errors are reported and the session goes on
        repl.send(")\r")
        repl.expect(r"unexpected reader token")
        repl.prompt()
        repl.send("(car '())\r")
        repl.expect(r"car: not a pair")
        repl.prompt()
        repl.entry("(+ 1 1)\r", "2")
        # Bracketed paste: newlines are inserted, not submitted
        repl.send(PASTE_START + "(define a 5)\r(* a 2)" + PASTE_END)
        repl.settle()
        check(repl.screen.row(repl.screen.r) == "... (* a 2)",
              "pasted lines stay in the entry", repl)
        repl.send("\r")
        repl.expect(r";Value: a")
        repl.expect(r";Value: 10")
        repl.prompt()
        repl.send(ctrl("d"))
        check(repl.finish() == 0, "exit status 0")
    finally:
        repl.close()


def test_history(engine_args):
    fd, hist = tempfile.mkstemp(prefix="vesper-history-")
    os.close(fd)
    try:
        repl = Repl(engine_args, history=hist)
        try:
            repl.prompt()
            repl.entry("(+ 40 2)\r", "42")
            repl.send("(define (g y)\r")
            repl.expect(r"\.\.\. ")
            repl.entry("(+ y 1))\r", "g")
            repl.entry("(g 1)\r", "2")
            repl.entry("(g 1)\r", "2")       # a repeat is not stored twice
            repl.entry(UP + "\r", "2")
            repl.entry(UP + UP + "\r", "g")  # the multi-line entry, whole
            # Up first walks the lines of a multi-line entry, then history.
            repl.entry(UP * 6 + "\r", "42")
            # Down returns towards the line being typed, which is kept
            repl.entry("(+ 5" + UP + DOWN + ")\r", "5")
            # Ctrl-P / Ctrl-N do the same
            repl.entry(ctrl("p") + ctrl("p") + ctrl("n") + "\r", "5")
            repl.send(ctrl("d"))
            repl.finish()
        finally:
            repl.close()
        with open(hist) as f:
            lines = f.read().split("\n")
        check("(+ 40 2)" in lines, "history file has the entries: %r" % lines)
        check("(define (g y)\\n(+ y 1))" in lines,
              "multi-line entry stored on one line: %r" % lines)
        check(lines.count("(g 1)") == 1, "repeats stored once: %r" % lines)
        # A new session starts from the saved history
        repl = Repl(engine_args, history=hist)
        try:
            repl.prompt()
            repl.entry(UP + "\r", "5")
            repl.send(UP * 3)
            repl.settle()
            check(repl.screen.row(repl.screen.r - 1) == "]=> (define (g y)" and
                  repl.screen.row(repl.screen.r) == "... (+ y 1))",
                  "a recalled multi-line entry is drawn on its lines", repl)
            repl.send("\r")
            repl.expect(r";Value: g")
            repl.prompt()
            repl.send(ctrl("d"))
            repl.finish()
        finally:
            repl.close()
    finally:
        os.unlink(hist)


def test_terminal_modes(engine_args):
    repl = Repl(engine_args)
    try:
        repl.prompt()
        # While code runs (here: reading a line from the console) the
        # terminal is in its normal cooked, echoing mode.
        repl.send("(read-line)\r")
        repl.settle()
        attrs = termios.tcgetattr(repl.slave)
        check(attrs[3] & termios.ICANON and attrs[3] & termios.ECHO,
              "cooked mode during evaluation")
        repl.send("typed text\n")
        repl.expect(r';Value: "typed text"')
        repl.prompt()
        # (exit 3) restores the terminal and keeps the status
        repl.send("(exit 3)\r")
        code = repl.finish()
        check(code == 3, "(exit 3) exit status (got %r)" % code)
        check(repl.attrs_restored(), "terminal restored after (exit)")
    finally:
        repl.close()
    # A fatal signal while editing still restores the terminal
    repl = Repl(engine_args)
    try:
        repl.prompt()
        repl.settle()
        attrs = termios.tcgetattr(repl.slave)
        check(not attrs[3] & termios.ICANON, "raw mode while editing")
        repl.proc.send_signal(signal.SIGTERM)
        repl.finish()
        check(repl.proc.returncode == -signal.SIGTERM,
              "died of SIGTERM (got %r)" % repl.proc.returncode)
        check(repl.attrs_restored(), "terminal restored after SIGTERM")
    finally:
        repl.close()
    # The terminal going away mid-entry ends the session instead of
    # spinning on end-of-input.
    repl = Repl(engine_args)
    try:
        repl.prompt()
        repl.send("(+ 1")
        repl.settle()
        os.close(repl.master)
        end = time.time() + 20
        while repl.proc.poll() is None and time.time() < end:
            time.sleep(0.05)
        check(repl.proc.poll() is not None, "REPL exits when the terminal closes")
    finally:
        repl.close()


def test_not_a_terminal(engine_args):
    # Output to a pipe: no editor; the plain prompt and cooked input, as
    # before the editor existed.
    repl = Repl(engine_args, stdout_tty=False)
    try:
        repl.send("(+ 1 2)\n")
        time.sleep(0.2)
        repl.send(ctrl("d"))
        out, _ = repl.proc.communicate(timeout=30)
        check(b"\x1b" not in out, "no escape sequences when stdout is a pipe")
        check(out == b"]=> \n;Value: 3\n]=> ",
              "plain REPL output when stdout is a pipe: %r" % out)
    finally:
        repl.close()
    # Piped stdin, as `make test-repl-exit` also checks.
    done = subprocess.run([VESPER] + list(engine_args), input=b"(+ 1 2)\n",
                          capture_output=True, timeout=60)
    check(done.stdout == b"\n;Value: 3\n", "piped input: %r" % done.stdout)


TESTS = [test_editing_keys, test_multiline, test_history, test_terminal_modes,
         test_not_a_terminal]


def main(argv):
    global VERBOSE, VESPER
    args = [a for a in argv if a != "--verbose"]
    VERBOSE = len(args) != len(argv)
    if args:
        VESPER = os.path.abspath(args[0])
    if not os.path.exists(VESPER):
        print("FAIL: %s not built" % VESPER)
        return 1
    failures = 0
    start = time.time()
    for engine in ([], ["--interpreter"]):
        for test in TESTS:
            if engine and test in (test_history, test_not_a_terminal):
                continue  # engine-independent; once is enough
            name = "%s%s" % (test.__name__, " (interpreter)" if engine else "")
            t0 = time.time()
            try:
                test(engine)
                print("PASS: lineedit %s (%.1fs)" % (name, time.time() - t0))
            except (Failure, OSError, subprocess.TimeoutExpired) as e:
                failures += 1
                print("FAIL: lineedit %s: %s" % (name, e))
    if failures:
        print("lineedit: %d check group(s) failed" % failures)
        return 1
    print("PASS: REPL line editing through a pty (%.1fs)" % (time.time() - start))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
