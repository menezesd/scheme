#include "primitive_table.h"

static const primitive_binding builtins[] = {
    // Arithmetic
    {"+", PPLUS},
    {"-", PMINUS},
    {"*", PTIMES},
    {"/", PDIV},
    {"modulo", PMOD},
    {"remainder", PREMAINDER},
    {"truncate/", PTRUNCATEDIVREM},
    {"floor/", PFLOORDIVREM},
    {"quotient", PQUOTIENT},
    {"abs", PABS},

    // Comparison
    {"=", PEQUAL},
    {"<", PLT},
    {">", PGT},
    {"<=", PLEQ},
    {">=", PGEQ},

    // Logic
    {"not", PNOT},
    {"eq?", PEQ},
    {"eqv?", PEQV},
    {"equal?", PEQUALP},

    // List operations
    {"cons", PCONS},
    {"car", PCAR},
    {"cdr", PCDR},
    {"set-car!", PSETCAR},
    {"set-cdr!", PSETCDR},
    {"list", PLIST},
    {"length", PLENGTH},
    {"append", PAPPEND},
    {"reverse", PREVERSE},
    {"last-pair", PLASTPAIR},

    // Type predicates
    {"symbol?", PSYMP},
    {"number?", PNUMP},
    {"procedure?", PPROCP},
    {"pair?", PCONSP},
    {"null?", PNULLP},
    {"string?", PSTRINGP},
    {"char?", PCHARP},
    {"vector?", PVECTORP},
    {"boolean?", PBOOLP},
    {"list?", PLISTP},
    {"integer?", PINTEGERP},
    {"real?", PREALP},
    {"exact?", PEXACTP},
    {"inexact?", PINEXACTP},

    // I/O
    {"display", PDISPLAY},
    {"write", PWRITE},
    {"write-shared", PWRITESHARED},
    {"write-simple", PWRITESIMPLE},
    {"newline", PNEWLINE},
    {"read", PREAD},
    {"read-char", PREADCHAR},
    {"read-char-no-hang", PREADCHARNOHANG},
    {"unread-char", PUNREADCHAR},
    {"peek-char", PPEEKCHAR},
    {"write-char", PWRITECHAR},
    {"eof-object", PEOFOBJECT},
    {"eof-object?", PEOF},
    {"char-ready?", PCHARREADY},
    {"read-line", PREADLINE},
    {"load", PLOAD},

    // Ports
    {"open-input-file", POPENINPUT},
    {"open-output-file", POPENOUTPUT},
    {"close-input-port", PCLOSEINPUT},
    {"close-output-port", PCLOSEOUTPUT},
    {"input-port?", PINPUTPORTP},
    {"output-port?", POUTPUTPORTP},
    {"current-input-port", PCURRENTINPUT},
    {"current-output-port", PCURRENTOUTPUT},

    // String operations
    {"string-length", PSTRLEN},
    {"string-ref", PSTRREF},
    {"string-set!", PSTRSET},
    {"string-append", PSTRAPP},
    {"substring", PSUBSTR},
    {"string-slice", PSTRSLICE},
    {"string->symbol", PSTR2SYM},
    {"symbol->string", PSYM2STR},
    {"number->string", PNUM2STR},
    {"string->number", PSTR2NUM},
    {"make-string", PMAKESTR},
    {"string-copy", PSTRCOPY},
    {"string->list", PSTR2LIST},
    {"list->string", PLIST2STR},
    {"string-fill!", PSTRFILL},
    {"string-normalize-nfd", PSTRNFD},
    {"string-normalize-nfc", PSTRNFC},
    {"string-normalize-nfkd", PSTRNFKD},
    {"string-normalize-nfkc", PSTRNFKC},
    {"string-foldcase", PSTRFOLD},
    {"string-upcase", PSTRUP},
    {"string-downcase", PSTRDOWN},
    {"string-titlecase", PSTRTITLE},
    {"string->utf8", PSTRINGTOUTF8},
    {"utf8->string", PUTF8TOSTRING},

    // String comparisons
    {"string=?", PSTREQ},
    {"string<?", PSTRLT},
    {"string>?", PSTRGT},
    {"string<=?", PSTRLE},
    {"string>=?", PSTRGE},
    {"string-ci=?", PSTREQI},
    {"string-ci<?", PSTRLTI},
    {"string-ci>?", PSTRGTI},
    {"string-ci<=?", PSTRLEI},
    {"string-ci>=?", PSTRGEI},

    // Character operations
    {"char->integer", PCHARCODE},
    {"integer->char", PCODECHAR},
    {"char-upcase", PCHARUP},
    {"char-downcase", PCHARDOWN},
    {"char-foldcase", PCHARFOLD},

    // Character comparisons
    {"char=?", PCHAREQ},
    {"char<?", PCHARLT},
    {"char>?", PCHARGT},
    {"char<=?", PCHARLE},
    {"char>=?", PCHARGE},
    {"char-ci=?", PCHAREQI},
    {"char-ci<?", PCHARLTI},
    {"char-ci>?", PCHARGTI},
    {"char-ci<=?", PCHARLEI},
    {"char-ci>=?", PCHARGEI},

    // Character predicates
    {"char-alphabetic?", PCHARALPHA},
    {"char-numeric?", PCHARNUMERIC},
    {"char-whitespace?", PCHARWHITE},
    {"char-upper-case?", PCHARUPPER},
    {"char-lower-case?", PCHARLOWER},

    // Vector operations
    {"make-vector", PMAKEVEC},
    {"vector", PVECTOR},
    {"vector-ref", PVECREF},
    {"vector-set!", PVECSET},
    {"vector-length", PVECLEN},
    {"vector-fill!", PVECFILL},
    {"list->vector", PLIST2VEC},
    {"vector->list", PVEC2LIST},

    // Math functions
    {"sqrt", PSQRT},
    {"expt", PEXPT},
    {"sin", PSIN},
    {"cos", PCOS},
    {"tan", PTAN},
    {"asin", PASIN},
    {"acos", PACOS},
    {"atan", PATAN},
    {"log", PLOG},
    {"exp", PEXP},
    {"log1p", PLOG1P},
    {"logp1", PLOG1P},
    {"expm1", PEXPM1},
    {"sqrt1pm1", PSQRT1PM1},
    {"log1pexp", PLOG1PEXP},
    {"floor", PFLOOR},
    {"ceiling", PCEILING},
    {"truncate", PTRUNCATE},
    {"round", PROUND},

    // Random numbers (SRFI-27)
    {"random-integer", PRANDOMINTEGER},
    {"random-real", PRANDOMREAL},
    {"random-seed!", PRANDOMSEED},

    // Process control (exit is a Scheme wrapper in stdlib.scm that runs
    // dynamic-wind after-thunks first; this is the raw primitive behind it)
    {"primitive-exit", PEXIT},

    // Bitwise operations
    {"bitwise-and", PBITWISEAND},
    {"bitwise-ior", PBITWISEIOR},
    {"bitwise-not", PBITWISENOT},
    {"bitwise-xor", PBITWISEXOR},
    {"arithmetic-shift", PARITHSHIFT},

    // Bytevector operations
    {"make-bytevector", PMAKEBYTEVEC},
    {"bytevector-u8-ref", PBYTEVECREF},
    {"bytevector-u8-set!", PBYTEVECSET},
    {"bytevector-length", PBYTEVECLEN},
    {"bytevector-copy", PBYTEVECCOPY},
    {"bytevector-copy!", PBYTEVECCOPYTO},
    {"bytevector-append", PBYTEVECAPPEND},
    {"bytevector", PBYTEVEC},
    {"bytevector?", PBYTEVECUP},

    // Misc
    {"command-line", PCOMMANDLINE},
    {"write-to-string", PWRITETOSTRING},
    {"list-ref", PLISTREF},

    // Binary I/O
    {"open-binary-input-file", POPENBINARYINPUT},
    {"open-binary-output-file", POPENBINARYOUTPUT},
    {"read-bytevector", PREADBYTEVEC},
    {"read-bytevector!", PREADBYTEVECINTO},
    {"write-bytevector", PWRITEBYTEVEC},
    {"read-string", PREADSTRING},
    {"write-string", PWRITESTRING},
    {"u8-ready?", PU8READY},
    {"peek-u8", PPEEKU8},

    // File system
    {"file-exists?", PFILEEXISTS},
    {"file-regular?", PFILEREGULARP},
    {"file-directory?", PFILEDIRECTORYP},
    {"delete-file", PDELETEFILE},
    {"make-directory", PMAKEDIRECTORY},
    {"delete-directory", PDELETEDIRECTORY},
    {"rename-file", PRENAMEFILE},
    {"current-directory", PCURRENTDIRECTORY},
    {"directory-files", PDIRECTORYFILES},
    {"temporary-file-path", PTEMPFILEPATH},
    {"get-environment-variable", PGETENV},

    // Control
    {"apply", PAPPLY},
    {"call/cc", PCALLCC},
    {"call-with-current-continuation", PCALLCC},

    // Misc
    {"error", PERROR},
    {"raise-now", PRAISENOW},
    {"gensym", PGENSYM},
    {"gc-flip", PGCFLIP},
    {"gc-stats", PGCSTATS},

    // R3RS numeric tower
    {"complex?", PCOMPLEXP},
    {"rational?", PRATIONALP},
    {"numerator", PNUMERATOR},
    {"denominator", PDENOMINATOR},
    {"make-rectangular", PMAKERECT},
    {"make-polar", PMAKEPOLAR},
    {"real-part", PREALPART},
    {"imag-part", PIMAGPART},
    {"magnitude", PMAGNITUDE},
    {"angle", PANGLE},
    {"exact->inexact", PEXACT2INEXACT},
    {"inexact->exact", PINEXACT2EXACT},
    {"rationalize", PRATIONALIZE},
    {"finite?", PFINITE},
    {"infinite?", PINFINITE},
    {"nan?", PNAN},

    // String constructor
    {"string", PSTRING},

    // Transcript
    {"transcript-on", PTRANSCRIPTON},
    {"transcript-off", PTRANSCRIPTOFF},

    // R5RS multiple values
    {"values", PVALUES},
    {"call-with-values", PCALLWITHVALUES},

    // R5RS eval and environments
    {"eval", PEVAL},
    {"scheme-report-environment", PSCHEMEENV},
    {"null-environment", PNULLENV},
    {"%r7rs-environment", PR7RSENV},
    {"interaction-environment", PINTERACTIONENV},

    // String ports
    {"open-output-string", POPENOUTPUTSTRING},
    {"get-output-string", PGETOUTPUTSTRING},
    {"open-input-string", POPENINPUTSTRING},
    {"string-port?", PSTRINGPORTP},

    // Internal port setters
    {"set-current-input-port!", PSETCURRENTINPUT},
    {"set-current-output-port!", PSETCURRENTOUTPUT},
    {"set-current-error-port!", PSETCURRENTERROR},
    {"current-error-port", PCURRENTERROR},
    {"flush-output-port", PFLUSHOUTPUT},
    {"flush-output", PFLUSHOUTPUT},
    {"current-second", PCURRENTSECOND},
    {"current-jiffy", PCURRENTJIFFY},
    {"jiffies-per-second", PJIFFIESPERSECOND},
    {"get-environment-variables", PGETENVS},
    {"emergency-exit", PEMERGENCYEXIT},
    {"features", PFEATURES},

    // Hash tables
    {"make-hash-table", PMAKEHASHTABLE},
    {"make-strong-eq-hash-table", PMAKESTRONGEQHASHTABLE},
    {"make-eq-hash-table", PMAKEEQHASHTABLE},
    {"make-strong-eqv-hash-table", PMAKESTRONGEQVHASHTABLE},
    {"make-eqv-hash-table", PMAKEEQVHASHTABLE},
    {"make-equal-hash-table", PMAKEEQUALHASHTABLE},
    {"hash-table?", PHASHTABLEP},
    {"hash-table-ref", PHASHTABLEREF},
    {"hash-table-set!", PHASHTABLESET},
    {"hash-table-delete!", PHASHTABLEDELETE},
    {"hash-table-exists?", PHASHTABLEEXISTS},
    {"hash-table-size", PHASHTABLESIZE},
    {"hash-table-clear!", PHASHTABLECLEAR},
    {"hash-table-keys", PHASHTABLEKEYS},
    {"hash-table-values", PHASHTABLEVALUES},
    {"hash-table->alist", PHASHTABLEALIST},
    {"hash-table-copy", PHASHTABLECOPY},
    {"hash-table-equivalence-function", PHASHTABLEEQUIV},
    {"%hash-table-hash", PHASHTABLEHASH},
    {"hash", PHASH},
    {"hash-by-identity", PHASHBYIDENTITY},

    // R7RS-ish port predicates
    {"port-open?", PPORTOPENP},
    {"input-port-open?", PINPUTPORTOPENP},
    {"output-port-open?", POUTPUTPORTOPENP},
    {"textual-port?", PTEXTUALPORTP},
    {"binary-port?", PBINARYPORTP},

    {NULL, 0}};

const primitive_binding *primitive_bindings(void)
{
    return builtins;
}

size_t primitive_binding_count(void)
{
    size_t count = 0;
    while (builtins[count].name)
        count++;
    return count;
}
