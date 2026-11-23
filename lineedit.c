// Line editing for the interactive REPL. See lineedit.h.
//
// Only active when stdin and stdout are both terminals: the terminal is put
// in raw mode for the duration of one entry and restored as soon as the
// entry is submitted, so evaluation (and any console reads the program
// makes) runs with the terminal exactly as the user had it.
//
// The entry being edited may span several lines. It is redrawn in full on
// every change: the cursor goes back to the entry's first row, everything
// below is cleared, and the prompt and text are written again, so wrapping
// of long lines and multi-line entries need no incremental bookkeeping.

#if defined(__APPLE__)
#define _DARWIN_C_SOURCE
#else
#define _GNU_SOURCE
#endif

#include "lineedit.h"

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>
#include <wchar.h>

#define HISTORY_MAX 1000
#define ESC_TIMEOUT_MS 50

// ============================================================================
// Terminal mode
// ============================================================================

static struct termios orig_termios;
static volatile sig_atomic_t raw_active = 0;
static bool atexit_registered = false;

static const int fatal_signals[] = {SIGTERM, SIGHUP, SIGINT, SIGQUIT};
#define NUM_FATAL_SIGNALS (sizeof(fatal_signals) / sizeof(fatal_signals[0]))
static struct sigaction saved_actions[NUM_FATAL_SIGNALS];

static const char PASTE_ON[] = "\x1b[?2004h";
static const char PASTE_OFF[] = "\x1b[?2004l";

static void write_all(const char *s, size_t n)
{
    while (n > 0) {
        ssize_t w = write(STDOUT_FILENO, s, n);
        if (w < 0) {
            if (errno == EINTR)
                continue;
            return;
        }
        s += w;
        n -= (size_t)w;
    }
}

void lineedit_restore_terminal(void)
{
    if (!raw_active)
        return;
    write_all(PASTE_OFF, sizeof(PASTE_OFF) - 1);
    tcsetattr(STDIN_FILENO, TCSANOW, &orig_termios);
    raw_active = 0;
    for (size_t i = 0; i < NUM_FATAL_SIGNALS; i++)
        sigaction(fatal_signals[i], &saved_actions[i], NULL);
}

// A signal that would kill the process while the terminal is raw: put the
// terminal back, then die of the signal as if no handler were installed.
static void fatal_signal_handler(int sig)
{
    if (raw_active) {
        (void)!write(STDOUT_FILENO, PASTE_OFF, sizeof(PASTE_OFF) - 1);
        tcsetattr(STDIN_FILENO, TCSANOW, &orig_termios);
        raw_active = 0;
    }
    signal(sig, SIG_DFL);
    raise(sig);
}

static bool enable_raw(void)
{
    if (raw_active)
        return true;
    if (tcgetattr(STDIN_FILENO, &orig_termios) != 0)
        return false;
    struct termios raw = orig_termios;
    raw.c_iflag &= ~(tcflag_t)(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(tcflag_t)OPOST;
    raw.c_cflag |= CS8;
    raw.c_lflag &= ~(tcflag_t)(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 1;
    raw.c_cc[VTIME] = 0;
    // TCSANOW: TCSAFLUSH would discard typed-ahead or pasted input, and
    // neither it nor TCSADRAIN may wait on output nobody is reading. Output
    // already queued was processed when written, so nothing is lost.
    if (tcsetattr(STDIN_FILENO, TCSANOW, &raw) != 0)
        return false;
    raw_active = 1;
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = fatal_signal_handler;
    sigemptyset(&sa.sa_mask);
    for (size_t i = 0; i < NUM_FATAL_SIGNALS; i++)
        sigaction(fatal_signals[i], &sa, &saved_actions[i]);
    if (!atexit_registered) {
        atexit(lineedit_restore_terminal);
        atexit_registered = true;
    }
    write_all(PASTE_ON, sizeof(PASTE_ON) - 1);
    return true;
}

static int term_width(void)
{
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0)
        return ws.ws_col;
    return 80;
}

bool lineedit_available(void)
{
    if (!isatty(STDIN_FILENO) || !isatty(STDOUT_FILENO))
        return false;
    const char *term = getenv("TERM");
    if (term && (strcmp(term, "dumb") == 0 || strcmp(term, "cons25") == 0 ||
                 strcmp(term, "emacs") == 0))
        return false;
    struct termios t;
    return tcgetattr(STDIN_FILENO, &t) == 0;
}

// Read one byte from the terminal; -1 at end of input or on error, -2 if
// TIMEOUT_MS (when >= 0) passes first.
static int read_byte(int timeout_ms)
{
    if (timeout_ms >= 0) {
        struct pollfd p = {STDIN_FILENO, POLLIN, 0};
        int r;
        do {
            r = poll(&p, 1, timeout_ms);
        } while (r < 0 && errno == EINTR);
        if (r == 0)
            return -2;
        if (r < 0)
            return -1;
    }
    for (;;) {
        unsigned char c;
        ssize_t n = read(STDIN_FILENO, &c, 1);
        if (n == 1)
            return c;
        if (n < 0 && errno == EINTR)
            continue;
        return -1;
    }
}

// ============================================================================
// Text measurement
// ============================================================================

// Decode the UTF-8 sequence at S (at most N bytes): its code point (or the
// byte itself, if malformed) and length.
static uint32_t decode_utf8(const char *s, size_t n, size_t *adv)
{
    const unsigned char *u = (const unsigned char *)s;
    uint32_t c = u[0];
    size_t len = c < 0x80 ? 1 : (c >> 5) == 6 ? 2 : (c >> 4) == 14 ? 3
                                                : (c >> 3) == 30 ? 4 : 1;
    if (len > n)
        len = 1;
    if (len > 1) {
        uint32_t cp = c & (0x7f >> len);
        for (size_t i = 1; i < len; i++) {
            if ((u[i] & 0xc0) != 0x80) {
                *adv = 1;
                return c;
            }
            cp = (cp << 6) | (u[i] & 0x3f);
        }
        *adv = len;
        return cp;
    }
    *adv = 1;
    return c;
}

// Display width of the character at S; control characters are shown in
// caret notation (^A), except tab, which is shown as a single space.
static int char_width(const char *s, size_t n, size_t *adv)
{
    uint32_t cp = decode_utf8(s, n, adv);
    if (cp == '\t')
        return 1;
    if (cp < 32 || cp == 127)
        return 2;
    if (cp < 127)
        return 1;
    int w = wcwidth((wchar_t)cp);
    return w < 0 ? 1 : w;
}

static int str_width(const char *s)
{
    int w = 0;
    size_t n = strlen(s);
    for (size_t i = 0; i < n;) {
        size_t adv;
        w += char_width(s + i, n - i, &adv);
        i += adv;
    }
    return w;
}

// ============================================================================
// Balance: does the entry need another line?
// ============================================================================

static bool is_delimiter(char c)
{
    return c == '\0' || isspace((unsigned char)c) || c == '(' || c == ')' ||
           c == '[' || c == ']' || c == '"' || c == ';' || c == '|';
}

bool lineedit_needs_more(const char *s, size_t n)
{
    long depth = 0;
    int comment = 0;     // nesting of #| |# block comments
    bool pending = false; // a ' ` , ,@ #; or #n= still waiting for its datum
    size_t i = 0;
    while (i < n) {
        char c = s[i];
        if (comment > 0) {
            if (c == '|' && i + 1 < n && s[i + 1] == '#') {
                comment--;
                i += 2;
            } else if (c == '#' && i + 1 < n && s[i + 1] == '|') {
                comment++;
                i += 2;
            } else {
                i++;
            }
            continue;
        }
        if (c == ';') {
            while (i < n && s[i] != '\n')
                i++;
            continue;
        }
        if (c == '"' || c == '|') {
            char close = c;
            i++;
            while (i < n && s[i] != close) {
                if (s[i] == '\\')
                    i++;
                i++;
            }
            if (i >= n)
                return true; // unterminated string or |symbol|
            i++;
            pending = false;
            continue;
        }
        if (c == '#' && i + 1 < n) {
            char d = s[i + 1];
            if (d == '|') {
                comment = 1;
                i += 2;
                continue;
            }
            if (d == ';') {
                pending = true;
                i += 2;
                continue;
            }
            if (d == '\\') {
                // #\x: the character itself may be a delimiter, the rest
                // of a name (#\space) is not.
                i += 3;
                while (i < n && !is_delimiter(s[i]))
                    i++;
                pending = false;
                continue;
            }
            if (isdigit((unsigned char)d)) {
                size_t j = i + 1;
                while (j < n && isdigit((unsigned char)s[j]))
                    j++;
                if (j < n && s[j] == '=') {
                    pending = true; // #n= labels the datum that follows
                    i = j + 1;
                    continue;
                }
            }
        }
        if (c == '(' || c == '[') {
            depth++;
            pending = false;
        } else if (c == ')' || c == ']') {
            depth--;
            pending = false;
        } else if (c == '\'' || c == '`') {
            pending = true;
        } else if (c == ',') {
            pending = true;
            if (i + 1 < n && s[i + 1] == '@')
                i++;
        } else if (!isspace((unsigned char)c)) {
            pending = false;
        }
        i++;
    }
    return comment > 0 || depth > 0 || pending;
}

// Nothing but whitespace and comments: submitting it does not start a datum.
static bool is_blank(const char *s, size_t n)
{
    size_t i = 0;
    while (i < n) {
        if (isspace((unsigned char)s[i])) {
            i++;
        } else if (s[i] == ';') {
            while (i < n && s[i] != '\n')
                i++;
        } else if (s[i] == '#' && i + 1 < n && s[i + 1] == '|') {
            int depth = 1;
            i += 2;
            while (i < n && depth > 0) {
                if (s[i] == '|' && i + 1 < n && s[i + 1] == '#') {
                    depth--;
                    i += 2;
                } else if (s[i] == '#' && i + 1 < n && s[i + 1] == '|') {
                    depth++;
                    i += 2;
                } else {
                    i++;
                }
            }
        } else {
            return false;
        }
    }
    return true;
}

// ============================================================================
// History
// ============================================================================

static char **history = NULL;
static int history_len = 0;
static char *history_path = NULL;

static void history_push(char *entry)
{
    if (history_len == HISTORY_MAX) {
        free(history[0]);
        memmove(history, history + 1, (size_t)(HISTORY_MAX - 1) * sizeof(*history));
        history_len--;
    }
    if (!history) {
        history = calloc(HISTORY_MAX, sizeof(*history));
        if (!history) {
            free(entry);
            return;
        }
    }
    history[history_len++] = entry;
}

// History file lines: one entry each, with backslash and newline escaped
// so a multi-line entry stays one line.
static void history_write_entry(FILE *f, const char *entry)
{
    for (const char *p = entry; *p; p++) {
        if (*p == '\\')
            fputs("\\\\", f);
        else if (*p == '\n')
            fputs("\\n", f);
        else
            fputc(*p, f);
    }
    fputc('\n', f);
}

static char *history_decode(const char *line, size_t n)
{
    char *out = malloc(n + 1);
    if (!out)
        return NULL;
    size_t k = 0;
    for (size_t i = 0; i < n; i++) {
        if (line[i] == '\\' && i + 1 < n) {
            i++;
            out[k++] = line[i] == 'n' ? '\n' : line[i];
        } else {
            out[k++] = line[i];
        }
    }
    out[k] = '\0';
    return out;
}

static FILE *history_open(const char *mode)
{
    int flags = mode[0] == 'a' ? O_WRONLY | O_APPEND | O_CREAT
                               : O_WRONLY | O_TRUNC | O_CREAT;
    int fd = open(history_path, flags, 0600);
    if (fd < 0)
        return NULL;
    FILE *f = fdopen(fd, mode);
    if (!f)
        close(fd);
    return f;
}

static void history_load(void)
{
    FILE *f = fopen(history_path, "r");
    if (!f)
        return;
    char *line = NULL;
    size_t cap = 0;
    ssize_t n;
    int read_count = 0;
    while ((n = getline(&line, &cap, f)) >= 0) {
        while (n > 0 && (line[n - 1] == '\n' || line[n - 1] == '\r'))
            n--;
        if (n == 0)
            continue;
        char *entry = history_decode(line, (size_t)n);
        if (entry)
            history_push(entry);
        read_count++;
    }
    free(line);
    fclose(f);
    // Appending forever would grow the file without bound: once it holds
    // twice what is kept, rewrite it with just the kept entries.
    if (read_count > 2 * HISTORY_MAX) {
        FILE *w = history_open("w");
        if (w) {
            for (int i = 0; i < history_len; i++)
                history_write_entry(w, history[i]);
            fclose(w);
        }
    }
}

static void history_add(const char *entry)
{
    if (history_len > 0 && strcmp(history[history_len - 1], entry) == 0)
        return;
    char *copy = strdup(entry);
    if (!copy)
        return;
    history_push(copy);
    if (history_path) {
        FILE *f = history_open("a");
        if (f) {
            history_write_entry(f, entry);
            fclose(f);
        }
    }
}

const char *lineedit_default_history_path(void)
{
    static char path[4096];
    const char *env = getenv("VESPER_HISTORY");
    if (env)
        return env[0] ? env : NULL;
    const char *home = getenv("HOME");
    if (!home || !home[0])
        return NULL;
    int n = snprintf(path, sizeof(path), "%s/.vesper_history", home);
    return n > 0 && (size_t)n < sizeof(path) ? path : NULL;
}

// ============================================================================
// The editor
// ============================================================================

typedef struct {
    char *b;
    size_t len, cap;
} abuf;

static void ab_append(abuf *ab, const char *s, size_t n)
{
    if (ab->len + n + 1 > ab->cap) {
        size_t cap = ab->cap ? ab->cap * 2 : 256;
        while (cap < ab->len + n + 1)
            cap *= 2;
        char *nb = realloc(ab->b, cap);
        if (!nb)
            return;
        ab->b = nb;
        ab->cap = cap;
    }
    memcpy(ab->b + ab->len, s, n);
    ab->len += n;
    ab->b[ab->len] = '\0';
}

static void ab_str(abuf *ab, const char *s)
{
    ab_append(ab, s, strlen(s));
}

typedef struct {
    abuf text;            // the entry; may contain newlines
    size_t pos;           // cursor, a byte offset into text
    const char *prompt;   // for the first line
    const char *cprompt;  // for continuation lines
    int cursor_row;       // cursor row after the last refresh, from the top
    int hist_index;       // 0 is the entry being edited, 1 the newest history
    bool paste;           // inside a bracketed paste
} editor;

static abuf kill_ring;    // the last killed text, for Ctrl-Y
static bool terminal_gone = false; // the terminal reported end of input

static void refresh(editor *e)
{
    int cols = term_width();
    abuf out = {0};
    char seq[32];
    if (e->cursor_row > 0) {
        snprintf(seq, sizeof(seq), "\x1b[%dA", e->cursor_row);
        ab_str(&out, seq);
    }
    ab_str(&out, "\r\x1b[J");
    const char *s = e->text.b ? e->text.b : "";
    size_t n = e->text.len;
    int row = 0, crow = 0, ccol = 0, end_row = 0;
    size_t i = 0;
    bool first = true;
    for (;;) {
        const char *p = first ? e->prompt : e->cprompt;
        ab_str(&out, p);
        int w = str_width(p);
        if (e->pos == i) {
            crow = row + w / cols;
            ccol = w % cols;
        }
        while (i < n && s[i] != '\n') {
            size_t adv;
            int cw = char_width(s + i, n - i, &adv);
            unsigned char c = (unsigned char)s[i];
            if (c == '\t') {
                ab_append(&out, " ", 1);
            } else if (c < 32 || c == 127) {
                char caret[2] = {'^', (char)(c == 127 ? '?' : c + 64)};
                ab_append(&out, caret, 2);
            } else {
                ab_append(&out, s + i, adv);
            }
            w += cw;
            i += adv;
            if (e->pos == i) {
                crow = row + w / cols;
                ccol = w % cols;
            }
        }
        // A line that exactly fills its last row leaves the terminal in
        // its pending-wrap state; step onto the next row explicitly so the
        // row arithmetic below holds for every width.
        if (w > 0 && w % cols == 0)
            ab_str(&out, "\r\n");
        if (i >= n) {
            end_row = row + w / cols;
            break;
        }
        ab_str(&out, "\r\n");
        row += w / cols + 1;
        i++; // the newline
        first = false;
        if (e->pos == i) {
            crow = row;
            ccol = 0;
        }
    }
    if (end_row > crow) {
        snprintf(seq, sizeof(seq), "\x1b[%dA", end_row - crow);
        ab_str(&out, seq);
    }
    ab_str(&out, "\r");
    if (ccol > 0) {
        snprintf(seq, sizeof(seq), "\x1b[%dC", ccol);
        ab_str(&out, seq);
    }
    e->cursor_row = crow;
    write_all(out.b, out.len);
    free(out.b);
}

static size_t prev_char(const editor *e, size_t pos)
{
    if (pos == 0)
        return 0;
    pos--;
    while (pos > 0 && ((unsigned char)e->text.b[pos] & 0xc0) == 0x80)
        pos--;
    return pos;
}

static size_t next_char(const editor *e, size_t pos)
{
    if (pos >= e->text.len)
        return e->text.len;
    size_t adv;
    decode_utf8(e->text.b + pos, e->text.len - pos, &adv);
    return pos + adv;
}

static size_t line_start(const editor *e, size_t pos)
{
    while (pos > 0 && e->text.b[pos - 1] != '\n')
        pos--;
    return pos;
}

static size_t line_end(const editor *e, size_t pos)
{
    while (pos < e->text.len && e->text.b[pos] != '\n')
        pos++;
    return pos;
}

static void insert(editor *e, const char *s, size_t n)
{
    abuf *t = &e->text;
    size_t old = t->len;
    ab_append(t, s, n); // grow; the bytes are moved into place below
    if (t->len != old + n)
        return;
    memmove(t->b + e->pos + n, t->b + e->pos, old - e->pos);
    memcpy(t->b + e->pos, s, n);
    e->pos += n;
}

static void delete_range(editor *e, size_t from, size_t to, bool keep)
{
    if (to <= from)
        return;
    if (keep) {
        kill_ring.len = 0;
        ab_append(&kill_ring, e->text.b + from, to - from);
    }
    memmove(e->text.b + from, e->text.b + to, e->text.len - to);
    e->text.len -= to - from;
    e->text.b[e->text.len] = '\0';
    if (e->pos > to)
        e->pos -= to - from;
    else if (e->pos > from)
        e->pos = from;
}

static void set_text(editor *e, const char *s)
{
    e->text.len = 0;
    ab_append(&e->text, s, strlen(s));
    if (!e->text.b)
        ab_append(&e->text, "", 0);
    e->pos = e->text.len;
}

static bool word_char(char c)
{
    unsigned char u = (unsigned char)c;
    return u >= 0x80 || isalnum(u) || strchr("-_!?*<>=/+.:%&^~$@", c);
}

static size_t word_left(const editor *e, size_t pos)
{
    while (pos > 0 && !word_char(e->text.b[pos - 1]))
        pos--;
    while (pos > 0 && word_char(e->text.b[pos - 1]))
        pos--;
    return pos;
}

static size_t word_right(const editor *e, size_t pos)
{
    while (pos < e->text.len && !word_char(e->text.b[pos]))
        pos++;
    while (pos < e->text.len && word_char(e->text.b[pos]))
        pos++;
    return pos;
}

// Column (in characters) of POS within its line.
static size_t column_of(const editor *e, size_t pos)
{
    size_t col = 0;
    for (size_t i = line_start(e, pos); i < pos; i = next_char(e, i))
        col++;
    return col;
}

static size_t at_column(const editor *e, size_t start, size_t col)
{
    size_t i = start;
    while (col > 0 && i < e->text.len && e->text.b[i] != '\n') {
        i = next_char(e, i);
        col--;
    }
    return i;
}

static void history_move(editor *e, int dir)
{
    if (history_len < 2)
        return;
    int slot = history_len - 1 - e->hist_index;
    char *copy = strndup(e->text.b ? e->text.b : "", e->text.len);
    if (copy) {
        free(history[slot]);
        history[slot] = copy;
    }
    int next = e->hist_index + dir;
    if (next < 0 || next >= history_len)
        return;
    e->hist_index = next;
    set_text(e, history[history_len - 1 - next]);
}

// Up/Down: between the lines of a multi-line entry, else through history.
static void vertical(editor *e, int dir)
{
    size_t start = line_start(e, e->pos);
    size_t col = column_of(e, e->pos);
    if (dir < 0 && start > 0) {
        size_t prev = line_start(e, start - 1);
        e->pos = at_column(e, prev, col);
    } else if (dir > 0 && line_end(e, e->pos) < e->text.len) {
        e->pos = at_column(e, line_end(e, e->pos) + 1, col);
    } else {
        history_move(e, dir < 0 ? 1 : -1);
    }
}

static void finish_display(editor *e)
{
    e->pos = e->text.len;
    refresh(e);
    write_all("\r\n", 2);
    e->cursor_row = 0;
}

static void suspend(editor *e)
{
    size_t pos = e->pos;
    finish_display(e);
    lineedit_restore_terminal();
    raise(SIGTSTP);
    enable_raw();
    e->pos = pos;
    refresh(e);
}

// The rest of an escape sequence, after ESC. Returns false at end of input.
static bool escape(editor *e)
{
    int c = read_byte(ESC_TIMEOUT_MS);
    if (c == -1)
        return false;
    if (c == -2)
        return true; // a lone ESC
    if (c == '[') {
        char params[16];
        size_t np = 0;
        int f;
        for (;;) {
            f = read_byte(ESC_TIMEOUT_MS);
            if (f < 0)
                return f != -1;
            if (f >= 0x40 && f <= 0x7e)
                break;
            if (np + 1 < sizeof(params))
                params[np++] = (char)f;
        }
        params[np] = '\0';
        bool ctrl = strstr(params, ";5") || strstr(params, ";3");
        switch (f) {
        case 'A':
            vertical(e, -1);
            break;
        case 'B':
            vertical(e, 1);
            break;
        case 'C':
            e->pos = ctrl ? word_right(e, e->pos) : next_char(e, e->pos);
            break;
        case 'D':
            e->pos = ctrl ? word_left(e, e->pos) : prev_char(e, e->pos);
            break;
        case 'H':
            e->pos = line_start(e, e->pos);
            break;
        case 'F':
            e->pos = line_end(e, e->pos);
            break;
        case '~': {
            int code = atoi(params);
            if (code == 1 || code == 7)
                e->pos = line_start(e, e->pos);
            else if (code == 4 || code == 8)
                e->pos = line_end(e, e->pos);
            else if (code == 3)
                delete_range(e, e->pos, next_char(e, e->pos), false);
            else if (code == 200)
                e->paste = true;
            else if (code == 201)
                e->paste = false;
            break;
        }
        default:
            break;
        }
        return true;
    }
    if (c == 'O') {
        int f = read_byte(ESC_TIMEOUT_MS);
        if (f == -1)
            return false;
        if (f == 'H')
            e->pos = line_start(e, e->pos);
        else if (f == 'F')
            e->pos = line_end(e, e->pos);
        else if (f == 'A')
            vertical(e, -1);
        else if (f == 'B')
            vertical(e, 1);
        else if (f == 'C')
            e->pos = next_char(e, e->pos);
        else if (f == 'D')
            e->pos = prev_char(e, e->pos);
        return true;
    }
    // Meta (Alt) keys
    switch (c) {
    case 'b':
        e->pos = word_left(e, e->pos);
        break;
    case 'f':
        e->pos = word_right(e, e->pos);
        break;
    case 'd':
        delete_range(e, e->pos, word_right(e, e->pos), true);
        break;
    case 127:
    case 8:
        delete_range(e, word_left(e, e->pos), e->pos, true);
        break;
    default:
        break;
    }
    return true;
}

// Without raw mode (the terminal refused it): one plain line, prompted,
// read as the terminal's own line discipline delivers it.
static char *cooked_line(const char *prompt)
{
    write_all(prompt, strlen(prompt));
    abuf line = {0};
    for (;;) {
        int c = read_byte(-1);
        if (c == -1) {
            if (line.len == 0) {
                free(line.b);
                return NULL;
            }
            break;
        }
        if (c == '\n')
            break;
        char ch = (char)c;
        ab_append(&line, &ch, 1);
    }
    return line.b ? line.b : strdup("");
}

// Edit one entry. Returns it (malloc'd, without a final newline), or NULL
// at end of input.
static char *edit_entry(const char *prompt, const char *cprompt)
{
    if (terminal_gone)
        return NULL;
    if (!enable_raw())
        return cooked_line(prompt);
    editor e;
    memset(&e, 0, sizeof(e));
    e.prompt = prompt;
    e.cprompt = cprompt;
    set_text(&e, "");
    // The entry being edited is the newest history slot while it is edited.
    char *slot = strdup("");
    if (slot)
        history_push(slot);
    refresh(&e);

    char *result = NULL;
    bool done = false;
    while (!done) {
        int c = read_byte(-1);
        if (c == -1) {
            // End of input (the terminal went away): hand over what was
            // typed, balanced or not, and report the end on the next call.
            terminal_gone = true;
            if (e.text.len > 0) {
                finish_display(&e);
                result = strndup(e.text.b, e.text.len);
            }
            break;
        }
        if (e.paste && (c == '\r' || c == '\n' || c == '\t' ||
                        (c >= 32 && c != 127 && c != 27))) {
            char ch = c == '\r' ? '\n' : (char)c;
            insert(&e, &ch, 1);
            // Refresh once the paste has been read, not per byte.
            struct pollfd p = {STDIN_FILENO, POLLIN, 0};
            if (poll(&p, 1, 0) <= 0)
                refresh(&e);
            continue;
        }
        switch (c) {
        case '\r':
        case '\n':
            if (!lineedit_needs_more(e.text.b, e.text.len)) {
                finish_display(&e);
                result = strndup(e.text.b, e.text.len);
                done = true;
                continue;
            }
            insert(&e, "\n", 1);
            break;
        case 1: // Ctrl-A
            e.pos = line_start(&e, e.pos);
            break;
        case 2: // Ctrl-B
            e.pos = prev_char(&e, e.pos);
            break;
        case 3: // Ctrl-C: abandon the entry
            e.pos = e.text.len;
            refresh(&e);
            write_all("^C\r\n", 4);
            e.cursor_row = 0;
            e.text.len = 0;
            e.text.b[0] = '\0';
            e.pos = 0;
            e.hist_index = 0;
            if (history_len > 0) {
                char *blank = strdup("");
                if (blank) {
                    free(history[history_len - 1]);
                    history[history_len - 1] = blank;
                }
            }
            break;
        case 4: // Ctrl-D: end of input on an empty entry, else delete
            if (e.text.len == 0) {
                write_all("\r\n", 2);
                done = true;
                continue;
            }
            delete_range(&e, e.pos, next_char(&e, e.pos), false);
            break;
        case 5: // Ctrl-E
            e.pos = line_end(&e, e.pos);
            break;
        case 6: // Ctrl-F
            e.pos = next_char(&e, e.pos);
            break;
        case 8: // Ctrl-H
        case 127: // Backspace
            delete_range(&e, prev_char(&e, e.pos), e.pos, false);
            break;
        case 9: // Tab
            insert(&e, "\t", 1);
            break;
        case 11: // Ctrl-K: kill to the end of the line (or the newline)
        {
            size_t end = line_end(&e, e.pos);
            if (end == e.pos && end < e.text.len)
                end++;
            delete_range(&e, e.pos, end, true);
            break;
        }
        case 12: // Ctrl-L
            write_all("\x1b[H\x1b[2J", 7);
            e.cursor_row = 0;
            break;
        case 14: // Ctrl-N
            vertical(&e, 1);
            break;
        case 16: // Ctrl-P
            vertical(&e, -1);
            break;
        case 20: // Ctrl-T: transpose the characters around the cursor
            if (e.pos > 0 && e.text.len > 1) {
                size_t at = e.pos == line_end(&e, e.pos) ? prev_char(&e, e.pos)
                                                         : e.pos;
                size_t before = prev_char(&e, at);
                size_t after = next_char(&e, at);
                if (before < at && e.text.b[before] != '\n' &&
                    e.text.b[at] != '\n') {
                    char tmp[8];
                    size_t a = at - before, b = after - at;
                    memcpy(tmp, e.text.b + before, a);
                    memmove(e.text.b + before, e.text.b + at, b);
                    memcpy(e.text.b + before + b, tmp, a);
                    e.pos = after;
                }
            }
            break;
        case 21: // Ctrl-U: kill to the start of the line
            delete_range(&e, line_start(&e, e.pos), e.pos, true);
            break;
        case 23: // Ctrl-W: kill the previous whitespace-delimited word
        {
            size_t p = e.pos;
            while (p > 0 && isspace((unsigned char)e.text.b[p - 1]))
                p--;
            while (p > 0 && !isspace((unsigned char)e.text.b[p - 1]))
                p--;
            delete_range(&e, p, e.pos, true);
            break;
        }
        case 25: // Ctrl-Y: yank
            if (kill_ring.len)
                insert(&e, kill_ring.b, kill_ring.len);
            break;
        case 26: // Ctrl-Z
            suspend(&e);
            break;
        case 27:
            if (!escape(&e)) {
                done = true;
                continue;
            }
            break;
        default:
            if (c >= 32) {
                char ch = (char)c;
                insert(&e, &ch, 1);
                // Wait for the rest of a UTF-8 sequence before redrawing.
                if ((c & 0xc0) == 0xc0 || (c & 0xc0) == 0x80) {
                    struct pollfd p = {STDIN_FILENO, POLLIN, 0};
                    if (poll(&p, 1, 0) > 0)
                        continue;
                }
            }
            break;
        }
        refresh(&e);
    }

    // Drop the slot of the entry being edited.
    if (history_len > 0) {
        free(history[history_len - 1]);
        history_len--;
    }
    free(e.text.b);
    lineedit_restore_terminal();
    return result;
}

// ============================================================================
// The stream
// ============================================================================

static const char *stream_prompt = "";
static const char *stream_cprompt = "";
static char *pending = NULL;
static size_t pending_len = 0, pending_off = 0;
static bool at_datum_start = true;
static bool stream_eof = false;

void lineedit_new_datum(void)
{
    at_datum_start = true;
}

static long stream_read(char *buf, size_t size)
{
    while (pending_off >= pending_len) {
        if (stream_eof)
            return 0;
        free(pending);
        pending = NULL;
        pending_len = pending_off = 0;
        // Program output must be on the screen before the prompt is drawn.
        fflush(stdout);
        fflush(stderr);
        char *entry = edit_entry(at_datum_start ? stream_prompt : stream_cprompt,
                                 stream_cprompt);
        if (!entry) {
            stream_eof = true;
            return 0;
        }
        size_t n = strlen(entry);
        if (!is_blank(entry, n)) {
            at_datum_start = false;
            history_add(entry);
        }
        char *with_nl = realloc(entry, n + 2);
        if (!with_nl) {
            free(entry);
            errno = ENOMEM;
            return -1;
        }
        with_nl[n] = '\n';
        with_nl[n + 1] = '\0';
        pending = with_nl;
        pending_len = n + 1;
    }
    size_t n = pending_len - pending_off;
    if (n > size)
        n = size;
    memcpy(buf, pending + pending_off, n);
    pending_off += n;
    return (long)n;
}

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) ||     \
    defined(__OpenBSD__) || defined(__DragonFly__)
static int funopen_read(void *cookie, char *buf, int size)
{
    (void)cookie;
    return (int)stream_read(buf, size > 0 ? (size_t)size : 0);
}

static FILE *open_stream(void)
{
    return funopen(NULL, funopen_read, NULL, NULL, NULL);
}
#elif defined(__GLIBC__) || defined(__linux__)
static ssize_t cookie_read(void *cookie, char *buf, size_t size)
{
    (void)cookie;
    return (ssize_t)stream_read(buf, size);
}

static FILE *open_stream(void)
{
    cookie_io_functions_t io = {cookie_read, NULL, NULL, NULL};
    return fopencookie(NULL, "r", io);
}
#else
static FILE *open_stream(void)
{
    return NULL;
}
#endif

FILE *lineedit_open(const char *prompt, const char *cont_prompt,
                    const char *path)
{
    stream_prompt = prompt;
    stream_cprompt = cont_prompt;
    FILE *f = open_stream();
    if (!f)
        return NULL;
    if (path) {
        free(history_path);
        history_path = strdup(path);
        if (history_path)
            history_load();
    }
    if (!atexit_registered) {
        atexit(lineedit_restore_terminal);
        atexit_registered = true;
    }
    return f;
}
