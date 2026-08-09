#!/usr/bin/env python3
import sys


def parse_code_range(text):
    if ".." in text:
        start, end = text.split("..", 1)
        return int(start, 16), int(end, 16)
    code = int(text, 16)
    return code, code


def add_range(ranges, start, end):
    ranges.append((start, end))


def merge_ranges(ranges):
    if not ranges:
        return []
    ranges = sorted(ranges)
    merged = [ranges[0]]
    for start, end in ranges[1:]:
        last_start, last_end = merged[-1]
        if start <= last_end + 1:
            merged[-1] = (last_start, max(last_end, end))
        else:
            merged.append((start, end))
    return merged


def parse_unicode_data(path):
    props = {
        "assigned": [],
        "decimal": [],
        "upper": [],
        "lower": [],
        "title": [],
        "punctuation": [],
        "symbol": [],
    }
    upper_map = {}
    lower_map = {}
    title_map = {}
    range_start = None
    range_category = None
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            fields = line.rstrip("\n").split(";")
            if len(fields) < 15:
                continue
            code = int(fields[0], 16)
            name = fields[1]
            category = fields[2]
            if name.endswith(", First>"):
                range_start = code
                range_category = category
                continue
            if name.endswith(", Last>"):
                if range_start is not None and range_category not in ("Cn", "Cs"):
                    add_range(props["assigned"], range_start, code)
                range_start = None
                range_category = None
                continue
            if category not in ("Cn", "Cs"):
                add_range(props["assigned"], code, code)
            if category == "Nd":
                add_range(props["decimal"], code, code)
            elif category == "Lu":
                add_range(props["upper"], code, code)
            elif category == "Ll":
                add_range(props["lower"], code, code)
            elif category == "Lt":
                add_range(props["title"], code, code)
            if category in ("Pc", "Pd", "Pe", "Pf", "Pi", "Po", "Ps"):
                add_range(props["punctuation"], code, code)
            if category in ("Sc", "Sk", "Sm", "So"):
                add_range(props["symbol"], code, code)
            if fields[12]:
                upper_map[code] = int(fields[12], 16)
            if fields[13]:
                lower_map[code] = int(fields[13], 16)
            if fields[14]:
                title_map[code] = int(fields[14], 16)
            elif fields[12]:
                title_map[code] = int(fields[12], 16)
    return (
        {key: merge_ranges(value) for key, value in props.items()},
        upper_map,
        lower_map,
        title_map,
    )


def parse_property_file(path, wanted):
    props = {name: [] for name in wanted}
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.split("#", 1)[0].strip()
            if not line or ";" not in line:
                continue
            code_range, prop = [part.strip() for part in line.split(";", 1)]
            if prop not in props:
                continue
            start, end = parse_code_range(code_range)
            add_range(props[prop], start, end)
    return {key: merge_ranges(value) for key, value in props.items()}


def parse_case_folding(path):
    simple = {}
    full = {}
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.split("#", 1)[0].strip()
            if not line:
                continue
            parts = [part.strip() for part in line.split(";")]
            if len(parts) < 3:
                continue
            code = int(parts[0], 16)
            status = parts[1]
            mapping = [int(part, 16) for part in parts[2].split()]
            if status == "T":
                continue
            if status in ("C", "S") and len(mapping) == 1:
                simple[code] = mapping[0]
            if status in ("C", "F"):
                full[code] = mapping
    return simple, full


def parse_special_casing(path):
    lower = {}
    title = {}
    upper = {}
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.split("#", 1)[0].strip()
            if not line:
                continue
            parts = [part.strip() for part in line.split(";")]
            if len(parts) < 4:
                continue
            if len(parts) > 4 and parts[4]:
                continue
            code = int(parts[0], 16)
            lower[code] = [int(part, 16) for part in parts[1].split()]
            title[code] = [int(part, 16) for part in parts[2].split()]
            upper[code] = [int(part, 16) for part in parts[3].split()]
    return lower, title, upper


def build_full_case_map(simple_map, special_map):
    full = {code: [mapped] for code, mapped in simple_map.items()}
    full.update(special_map)
    return full


def build_full_entries(mappings):
    data = []
    entries = []
    for code, mapping in sorted(mappings.items()):
        offset = len(data)
        data.extend(mapping)
        entries.append((code, offset, len(mapping)))
    return data, entries


def emit_ranges(name, ranges):
    print(f"#define {name.upper()}_COUNT {len(ranges)}")
    print(f"static const unicode_range {name}[] = {{")
    for start, end in ranges:
        print(f"    {{0x{start:04X}, 0x{end:04X}}},")
    print("};")
    print()


def main(argv):
    if len(argv) != 7:
        print(
            "usage: gen_unicode_char.py UnicodeData.txt DerivedCoreProperties.txt "
            "PropList.txt CaseFolding.txt SpecialCasing.txt out.h",
            file=sys.stderr,
        )
        return 2

    unicode_data, derived_core, prop_list, case_folding, special_casing, out_path = argv[1:]
    category_props, upper_map, lower_map, title_map = parse_unicode_data(unicode_data)
    core_props = parse_property_file(
        derived_core, ["Alphabetic", "Cased", "Case_Ignorable"]
    )
    prop_props = parse_property_file(prop_list, ["White_Space"])
    simple_fold, full_fold = parse_case_folding(case_folding)
    special_lower, special_title, special_upper = parse_special_casing(special_casing)
    full_lower = build_full_case_map(lower_map, special_lower)
    full_title = build_full_case_map(title_map, special_title)
    full_upper = build_full_case_map(upper_map, special_upper)

    full_fold_data, full_fold_entries = build_full_entries(full_fold)
    full_lower_data, full_lower_entries = build_full_entries(full_lower)
    full_title_data, full_title_entries = build_full_entries(full_title)
    full_upper_data, full_upper_entries = build_full_entries(full_upper)

    with open(out_path, "w", encoding="ascii") as out:
        old_stdout = sys.stdout
        sys.stdout = out
        try:
            print("/* Generated by tools/gen_unicode_char.py from Unicode 15.1.0 data. */")
            print("#ifndef UNICODE_CHAR_TABLES_H")
            print("#define UNICODE_CHAR_TABLES_H")
            print()
            print("#include <stdint.h>")
            print()
            print("typedef struct { uint32_t start; uint32_t end; } unicode_range;")
            print("typedef struct { uint32_t code; uint32_t folded; } unicode_simple_fold_entry;")
            print("typedef struct { uint32_t code; uint32_t offset; uint8_t length; } unicode_full_case_entry;")
            print()
            print("#define UNICODE_CHAR_VERSION \"15.1.0\"")
            emit_ranges("unicode_assigned_ranges", category_props["assigned"])
            emit_ranges("unicode_alphabetic_ranges", core_props["Alphabetic"])
            emit_ranges("unicode_cased_ranges", core_props["Cased"])
            emit_ranges("unicode_case_ignorable_ranges", core_props["Case_Ignorable"])
            emit_ranges("unicode_decimal_ranges", category_props["decimal"])
            emit_ranges("unicode_whitespace_ranges", prop_props["White_Space"])
            emit_ranges("unicode_uppercase_ranges", category_props["upper"])
            emit_ranges("unicode_lowercase_ranges", category_props["lower"])
            emit_ranges("unicode_titlecase_ranges", category_props["title"])
            emit_ranges("unicode_punctuation_ranges", category_props["punctuation"])
            emit_ranges("unicode_symbol_ranges", category_props["symbol"])
            print(f"#define UNICODE_UPCASE_COUNT {len(upper_map)}")
            print("static const unicode_simple_fold_entry unicode_upcase_table[] = {")
            for code, mapped in sorted(upper_map.items()):
                print(f"    {{0x{code:04X}, 0x{mapped:04X}}},")
            print("};")
            print()
            print(f"#define UNICODE_DOWNCASE_COUNT {len(lower_map)}")
            print("static const unicode_simple_fold_entry unicode_downcase_table[] = {")
            for code, mapped in sorted(lower_map.items()):
                print(f"    {{0x{code:04X}, 0x{mapped:04X}}},")
            print("};")
            print()
            print(f"#define UNICODE_SIMPLE_FOLD_COUNT {len(simple_fold)}")
            print("static const unicode_simple_fold_entry unicode_simple_fold_table[] = {")
            for code, folded in sorted(simple_fold.items()):
                print(f"    {{0x{code:04X}, 0x{folded:04X}}},")
            print("};")
            print()
            print(f"#define UNICODE_FULL_FOLD_COUNT {len(full_fold_entries)}")
            print(f"#define UNICODE_FULL_FOLD_DATA_COUNT {len(full_fold_data)}")
            print("static const uint32_t unicode_full_fold_data[] = {")
            for i in range(0, len(full_fold_data), 8):
                print("    " + ", ".join(f"0x{cp:04X}" for cp in full_fold_data[i:i + 8]) + ",")
            print("};")
            print()
            print("static const unicode_full_case_entry unicode_full_fold_table[] = {")
            for code, offset, length in full_fold_entries:
                print(f"    {{0x{code:04X}, {offset}, {length}}},")
            print("};")
            print()
            for label, data, entries in (
                ("lower", full_lower_data, full_lower_entries),
                ("title", full_title_data, full_title_entries),
                ("upper", full_upper_data, full_upper_entries),
            ):
                macro = label.upper()
                print(f"#define UNICODE_FULL_{macro}_COUNT {len(entries)}")
                print(f"#define UNICODE_FULL_{macro}_DATA_COUNT {len(data)}")
                print(f"static const uint32_t unicode_full_{label}_data[] = {{")
                for i in range(0, len(data), 8):
                    print("    " + ", ".join(f"0x{cp:04X}" for cp in data[i:i + 8]) + ",")
                print("};")
                print()
                print(f"static const unicode_full_case_entry unicode_full_{label}_table[] = {{")
                for code, offset, length in entries:
                    print(f"    {{0x{code:04X}, {offset}, {length}}},")
                print("};")
                print()
            print("#endif")
        finally:
            sys.stdout = old_stdout
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
