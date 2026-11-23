#ifndef LINEEDIT_H
#define LINEEDIT_H

// A small, dependency-free line editor for the interactive REPL, in the
// spirit of linenoise: raw-mode termios editing of a (possibly multi-line)
// entry, emacs-style keys, history with a history file, and continuation
// lines while parentheses, strings or block comments are still open.
//
// The REPL does not call the editor directly. lineedit_open returns a FILE
// whose reads pull complete entries from the editor, so the reader consumes
// exactly the bytes it would have read from a cooked terminal - its case
// folding, datum labels and position tracking are unchanged.

#include <stdbool.h>
#include <stdio.h>

// True when stdin and stdout are both terminals and TERM is not one that
// cannot handle cursor movement ("dumb", "cons25", "emacs").
bool lineedit_available(void);

// A read-only stream fed by the editor. Each time it runs dry it reads one
// entry from the terminal: the first line is prompted with PROMPT when the
// stream is at a datum boundary (see lineedit_new_datum) and CONT_PROMPT
// otherwise, and continuation lines with CONT_PROMPT. HISTORY_PATH, if not
// NULL, is loaded now and appended to as entries are made. Returns NULL if
// the stream cannot be created.
FILE *lineedit_open(const char *prompt, const char *cont_prompt,
                    const char *history_path);

// The REPL is about to read a new top-level datum: the next entry, if one is
// needed, is prompted with the primary prompt.
void lineedit_new_datum(void);

// Put the terminal back in the mode it had before editing started. Safe to
// call at any time and from any state; a no-op unless an edit is active.
// Called automatically at exit and on fatal signals.
void lineedit_restore_terminal(void);

// The history file the REPL uses: $VESPER_HISTORY if set (empty disables
// it), else ~/.vesper_history. NULL when there is none. Static storage.
const char *lineedit_default_history_path(void);

// Exposed for tests: does TEXT still need more lines before it can be
// submitted (an open list, string, |symbol|, block comment, or a trailing
// quote/datum-comment prefix)?
bool lineedit_needs_more(const char *text, size_t len);

#endif // LINEEDIT_H
