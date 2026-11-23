/**
 * @file prim_record.c
 * @brief Record primitives behind define-record-type and MIT's record API
 *
 * Records are opaque: a distinct cell type (BT_RECORD, its type BT_RECORD_TYPE)
 * over a vector payload, so no vector operation accepts one and only these
 * primitives see inside. They are %-prefixed; the stdlib builds the public
 * procedures on top of them. Field indexes here are 0-based field numbers,
 * not payload slots.
 */

#include "prim_internal.h"

// A symbol's name for an error message, or fallback. Also how the accessor or
// constructor name the stdlib passes as "who" is read.
static const char *who_name(unsigned who, const char *fallback)
{
    return IS_SYMBOL(who) && atom_is_valid(who) ? ctx.atom_table[CELL_ID(who)]
                                                : fallback;
}

// Record type names print without the brackets of the conventional <name>, in
// messages as in #[point 12].
static void show_not_a_record_of(const char *who, unsigned rtd)
{
    const char *name = record_type_well_formed(rtd)
                           ? record_type_name_string(rtd)
                           : "record";
    size_t len = strlen(name);
    if (len > 2 && name[0] == '<' && name[len - 1] == '>')
        show_error("%s: not a %.*s record", who, (int)(len - 2), name + 1);
    else
        show_error("%s: not a %s record", who, name);
}

static bool require_record_type(unsigned rtd, const char *who)
{
    if (record_type_well_formed(rtd))
        return true;
    show_error("%s: not a record type", who);
    return false;
}

static bool same_symbol(unsigned a, unsigned b)
{
    return IS_ATOM(a) && IS_ATOM(b) && CELL_ID(a) == CELL_ID(b);
}

// Index of field name in rtd, or -1.
static int64_t field_index(unsigned rtd, unsigned field)
{
    unsigned *data = vector_data_ptr(rtd);
    unsigned count = record_type_field_count(rtd);
    for (unsigned i = 0; i < count; i++) {
        if (same_symbol(data[RECORD_TYPE_FIELDS_START + i], field))
            return i;
    }
    return -1;
}

static void show_no_such_field(const char *who, unsigned rtd, unsigned field)
{
    if (IS_SYMBOL(field) && atom_is_valid(field))
        show_error("%s: no field %s in record type %s", who,
                   who_name(field, "?"), record_type_name_string(rtd));
    else
        show_error("%s: field name is not a symbol", who);
}

// Field names must be distinct symbols. True if every one is.
static bool check_field_names(unsigned fields, const char *who)
{
    unsigned count = 0;
    if (!list_length_checked(fields, &count, who))
        return false;
    for (unsigned f = fields; f; f = cdr(f)) {
        if (!IS_SYMBOL(car(f))) {
            show_error("%s: field name is not a symbol", who);
            return false;
        }
        for (unsigned g = cdr(f); g; g = cdr(g)) {
            if (same_symbol(car(f), car(g))) {
                show_error("%s: duplicate field name %s", who,
                           who_name(car(f), "?"));
                return false;
            }
        }
    }
    return true;
}

// (%make-record-type name field-names [flags]) - flags is a list of the
// symbols opaque and sealed (see RECORD_TYPE_* in types.h).
static unsigned prim_make_record_type(unsigned argc, unsigned *argv)
{
    const char *who = "make-record-type";
    if (!IS_SYMBOL(argv[0])) {
        show_error("%s: type name is not a symbol", who);
        return TOK_ERROR;
    }
    if (!check_field_names(argv[1], who))
        return TOK_ERROR;
    unsigned flags = 0;
    if (argc == 3) {
        if (!list_length_checked(argv[2], NULL, who))
            return TOK_ERROR;
        FORLIST(f, argv[2])
        {
            unsigned flag = car(f);
            if (IS_SYMBOL(flag) && CELL_ID(flag) == intern("opaque")) {
                flags |= RECORD_TYPE_OPAQUE;
            } else if (IS_SYMBOL(flag) && CELL_ID(flag) == intern("sealed")) {
                flags |= RECORD_TYPE_SEALED;
            } else {
                show_error("%s: unknown record type flag", who);
                return TOK_ERROR;
            }
        }
    }
    return make_record_type(argv[0], argv[1], flags);
}

static unsigned prim_record_type_field_names(unsigned rtd)
{
    if (!require_record_type(rtd, "record-type-field-names"))
        return TOK_ERROR;
    GC_GUARD;
    gc_protect(&rtd);
    unsigned result = 0;
    gc_protect(&result);
    // Built back to front; re-read the payload each time, as alloc_cons can
    // collect (the payload itself does not move, but rtd's cell does).
    for (unsigned i = record_type_field_count(rtd); i > 0; i--) {
        unsigned name = vector_data_ptr(rtd)[RECORD_TYPE_FIELDS_START + i - 1];
        result = alloc_cons(name, result);
    }
    return result;
}

// (%record-type-field-index rtd field who)
static unsigned prim_record_type_field_index(unsigned argc, unsigned *argv)
{
    (void)argc; // apply_primitive_argv checked the count
    const char *who = who_name(argv[2], "record-accessor");
    if (!require_record_type(argv[0], who))
        return TOK_ERROR;
    int64_t index = field_index(argv[0], argv[1]);
    if (index < 0) {
        show_no_such_field(who, argv[0], argv[1]);
        return TOK_ERROR;
    }
    return store(index);
}

// (%record-layout rtd constructor-fields who) - #f when the constructor takes
// every field in order (the common case, which %make-record fills directly);
// otherwise a vector of the field index each argument initializes.
static unsigned prim_record_layout(unsigned argc, unsigned *argv)
{
    (void)argc; // apply_primitive_argv checked the count
    const char *who = who_name(argv[2], "record-constructor");
    if (!require_record_type(argv[0], who))
        return TOK_ERROR;
    unsigned count = 0;
    if (!list_length_checked(argv[1], &count, who))
        return TOK_ERROR;
    bool identity = count == record_type_field_count(argv[0]);
    unsigned position = 0;
    for (unsigned f = argv[1]; f; f = cdr(f), position++) {
        int64_t index = field_index(argv[0], car(f));
        if (index < 0) {
            show_no_such_field(who, argv[0], car(f));
            return TOK_ERROR;
        }
        for (unsigned g = cdr(f); g; g = cdr(g)) {
            if (same_symbol(car(f), car(g))) {
                show_error("%s: duplicate field %s", who,
                           who_name(car(f), "?"));
                return TOK_ERROR;
            }
        }
        if (index != position)
            identity = false;
    }
    if (identity)
        return ctx.atom_false;
    unsigned layout = make_vector(count, 0);
    if (layout == TOK_ERROR)
        return TOK_ERROR;
    // An index past the small-integer cache is a fresh cell, so store can
    // collect: keep the layout and the list walk rooted, and write through
    // the barrier.
    GC_GUARD;
    gc_protect(&layout);
    unsigned rest = argv[1];
    gc_protect(&rest);
    for (unsigned i = 0; rest; rest = cdr(rest), i++) {
        unsigned index = store(field_index(argv[0], car(rest)));
        vector_set_elem(layout, i, index);
    }
    return layout;
}

// (%make-record rtd layout value ...)
static unsigned prim_make_record(unsigned argc, unsigned *argv)
{
    const char *who = "make-record";
    if (argc < 2) {
        show_error("%s: expected a record type and layout", who);
        return TOK_ERROR;
    }
    if (!require_record_type(argv[0], who))
        return TOK_ERROR;
    unsigned nvalues = argc - 2;
    unsigned nfields = record_type_field_count(argv[0]);
    unsigned layout = argv[1];
    if (layout == ctx.atom_false) {
        if (nvalues != nfields) {
            show_error("%s: wrong number of field values for record type %s",
                       who, record_type_name_string(argv[0]));
            return TOK_ERROR;
        }
    } else {
        // Normally from %record-layout, but this is callable directly, so
        // check every index before writing through it.
        if (!IS_VECTOR(layout) || vector_len(layout) != nvalues) {
            show_error("%s: wrong number of field values for record type %s",
                       who, record_type_name_string(argv[0]));
            return TOK_ERROR;
        }
        unsigned *slots = vector_data_ptr(layout);
        for (unsigned i = 0; i < nvalues; i++) {
            if (!IS_NUM(slots[i]) || CELL_ID(slots[i]) < 0 ||
                CELL_ID(slots[i]) >= (int64_t)nfields) {
                show_error("%s: invalid record layout", who);
                return TOK_ERROR;
            }
        }
    }
    // argv is rooted by apply_primitive_argv, so every value (the layout
    // included) is current again once make_record returns, and nothing
    // allocates after that. The new record is in the nursery, where the
    // barrier is a no-op; it stays so the stores are right wherever the
    // record lives.
    unsigned rec = make_record(argv[0]);
    if (rec == TOK_ERROR)
        return TOK_ERROR;
    unsigned *data = vector_data_ptr(rec);
    unsigned *slots = layout == ctx.atom_false ? NULL
                                               : vector_data_ptr(argv[1]);
    for (unsigned i = 0; i < nvalues; i++) {
        unsigned slot = slots ? (unsigned)CELL_ID(slots[i]) : i;
        write_barrier(rec, argv[i + 2]);
        data[RECORD_FIELDS_START + slot] = argv[i + 2];
    }
    return rec;
}

bool record_of_type(unsigned obj, unsigned rtd)
{
    if (!IS_RECORD(obj))
        return false;
    vector_data *vd = GET_VECTOR_PTR(obj);
    return vector_data_well_formed(vd) && vd->len >= RECORD_FIELDS_START &&
           vd->data[RECORD_TYPE_SLOT] == rtd;
}

unsigned *record_field_slot(unsigned rec, unsigned rtd, unsigned index,
                            unsigned who)
{
    if (!record_of_type(rec, rtd)) {
        show_not_a_record_of(who_name(who, "record-accessor"), rtd);
        return NULL;
    }
    // Only the record's own payload is touched, so checking its stored type
    // against rtd by identity and the index against its own length is enough.
    vector_data *vd = GET_VECTOR_PTR(rec);
    int64_t i = -1;
    if (IS_FIXNUM(index))
        i = FIXNUM_VALUE(index);
    else if (IS_NUM(index))
        i = CELL_ID(index);
    if (i < 0 || i >= (int64_t)(vd->len - RECORD_FIELDS_START)) {
        show_error("%s: invalid field index", who_name(who, "record-accessor"));
        return NULL;
    }
    return &vd->data[RECORD_FIELDS_START + i];
}

static unsigned prim_register_builtin_record_type(unsigned name, unsigned rtd)
{
    static const char *const names[BRT_COUNT] = {
        [BRT_CONDITION] = "condition",
        [BRT_PROMISE] = "promise",
        [BRT_BINARY_INPUT_PORT] = "binary-input-port",
        [BRT_BINARY_OUTPUT_PORT] = "binary-output-port",
        [BRT_CHAR_SET] = "char-set",
    };
    const char *who = "%register-builtin-record-type!";
    if (!require_record_type(rtd, who))
        return TOK_ERROR;
    for (int i = 0; i < BRT_COUNT; i++) {
        if (IS_SYMBOL(name) && CELL_ID(name) == intern(names[i])) {
            ctx.builtin_record_types[i] = rtd;
            return rtd;
        }
    }
    show_error("%s: unknown builtin record type", who);
    return TOK_ERROR;
}

unsigned apply_record_primitive(unsigned prim_id, unsigned argc,
                                unsigned *argv)
{
    switch (prim_id) {
    case PMAKERECORDTYPE:
        return prim_make_record_type(argc, argv);
    case PRECORDTYPEP:
        return scheme_bool(record_type_well_formed(argv[0]));
    case PRECORDTYPENAME:
        if (!require_record_type(argv[0], "record-type-name"))
            return TOK_ERROR;
        return vector_data_ptr(argv[0])[RECORD_TYPE_NAME_SLOT];
    case PRECORDTYPEFIELDNAMES:
        return prim_record_type_field_names(argv[0]);
    case PRECORDTYPEFIELDINDEX:
        return prim_record_type_field_index(argc, argv);
    case PRECORDTYPESEALEDP:
        if (!require_record_type(argv[0], "%record-type-sealed?"))
            return TOK_ERROR;
        return scheme_bool(record_type_flags(argv[0]) & RECORD_TYPE_SEALED);
    case PRECORDLAYOUT:
        return prim_record_layout(argc, argv);
    case PMAKERECORD:
        return prim_make_record(argc, argv);
    case PRECORDP: {
        unsigned rtd = record_rtd(argv[0]);
        return scheme_bool(rtd && !(record_type_flags(rtd) &
                                    RECORD_TYPE_OPAQUE));
    }
    case PRECORDOFP:
        return scheme_bool(record_of_type(argv[0], argv[1]));
    case PRECORDTYPEDESCRIPTOR: {
        unsigned rtd = record_rtd(argv[0]);
        if (!rtd || (record_type_flags(rtd) & RECORD_TYPE_OPAQUE)) {
            show_error("record-type-descriptor: not a record");
            return TOK_ERROR;
        }
        return rtd;
    }
    case PRECORDREF: {
        unsigned *slot = record_field_slot(argv[0], argv[1], argv[2], argv[3]);
        return slot ? *slot : TOK_ERROR;
    }
    case PRECORDSET: {
        unsigned *slot = record_field_slot(argv[0], argv[1], argv[2], argv[4]);
        if (!slot)
            return TOK_ERROR;
        write_barrier(argv[0], argv[3]);
        *slot = argv[3];
        return argv[3];
    }
    case PREGISTERBUILTINRECORDTYPE:
        return prim_register_builtin_record_type(argv[0], argv[1]);
    default:
        show_error("record primitive: unsupported id %u", prim_id);
        return TOK_ERROR;
    }
}
