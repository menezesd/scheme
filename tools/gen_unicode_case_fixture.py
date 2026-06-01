#!/usr/bin/env python3
import sys


def parse_code_range(text):
    if ".." in text:
        start, end = text.split("..", 1)
        return int(start, 16), int(end, 16)
    code = int(text, 16)
    return code, code


def merge_ranges(ranges):
    ranges = sorted(ranges)
    merged = []
    for start, end in ranges:
        if merged and start <= merged[-1][1] + 1:
            merged[-1] = (merged[-1][0], max(merged[-1][1], end))
        else:
            merged.append((start, end))
    return merged


def parse_unicode_data(path):
    props = {
        "decimal": [],
        "upper": [],
        "lower": [],
    }
    upper = {}
    lower = {}
    title = {}
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            fields = line.rstrip("\n").split(";")
            if len(fields) < 15:
                continue
            code = int(fields[0], 16)
            category = fields[2]
            if category == "Nd":
                props["decimal"].append((code, code))
            elif category == "Lu":
                props["upper"].append((code, code))
            elif category == "Ll":
                props["lower"].append((code, code))
            if fields[12]:
                upper[code] = [int(fields[12], 16)]
            if fields[13]:
                lower[code] = [int(fields[13], 16)]
            if fields[14]:
                title[code] = [int(fields[14], 16)]
            elif fields[12]:
                title[code] = [int(fields[12], 16)]
    return {key: merge_ranges(value) for key, value in props.items()}, lower, title, upper


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
            props[prop].append(parse_code_range(code_range))
    return {key: merge_ranges(value) for key, value in props.items()}


def parse_case_folding(path):
    fold = {}
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.split("#", 1)[0].strip()
            if not line:
                continue
            parts = [part.strip() for part in line.split(";")]
            if len(parts) < 3 or parts[1] == "T":
                continue
            if parts[1] in ("C", "F"):
                fold[int(parts[0], 16)] = [int(part, 16) for part in parts[2].split()]
    return fold


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


def emit_mapping_rows(name, rows):
    data = []
    entries = []
    for code, mapping in sorted(rows.items()):
        offset = len(data)
        data.extend(mapping)
        entries.append((code, offset, len(mapping)))
    print(f"#define {name.upper()}_ROW_COUNT {len(entries)}")
    print(f"#define {name.upper()}_DATA_COUNT {len(data)}")
    print(f"static const uint32_t {name}_data[] = {{")
    for i in range(0, len(data), 8):
        print("    " + ", ".join(f"0x{code:04X}" for code in data[i:i + 8]) + ",")
    print("};")
    print(f"static const unicode_case_mapping_row {name}_rows[] = {{")
    for code, offset, length in entries:
        print(f"    {{0x{code:04X}, {offset}, {length}}},")
    print("};")
    print()


def emit_property_ranges(name, ranges):
    print(f"#define {name.upper()}_COUNT {len(ranges)}")
    print(f"static const unicode_case_property_range {name}[] = {{")
    for start, end in ranges:
        print(f"    {{0x{start:04X}, 0x{end:04X}}},")
    print("};")
    print()


def main(argv):
    if len(argv) != 7:
        print(
            "usage: gen_unicode_case_fixture.py UnicodeData.txt "
            "DerivedCoreProperties.txt PropList.txt CaseFolding.txt "
            "SpecialCasing.txt out.h",
            file=sys.stderr,
        )
        return 2

    unicode_data, derived_core, prop_list, case_folding, special_casing, out_path = argv[1:]
    category_props, lower, title, upper = parse_unicode_data(unicode_data)
    special_lower, special_title, special_upper = parse_special_casing(special_casing)
    lower.update(special_lower)
    title.update(special_title)
    upper.update(special_upper)
    fold = parse_case_folding(case_folding)
    core_props = parse_property_file(derived_core, ["Alphabetic"])
    prop_props = parse_property_file(prop_list, ["White_Space"])

    with open(out_path, "w", encoding="ascii") as out:
        old_stdout = sys.stdout
        sys.stdout = out
        try:
            print("/* Generated by tools/gen_unicode_case_fixture.py from Unicode 15.1.0 data. */")
            print("#ifndef UNICODE_CASE_TEST_DATA_H")
            print("#define UNICODE_CASE_TEST_DATA_H")
            print()
            print("#include <stdint.h>")
            print()
            print("typedef struct { uint32_t code; uint32_t offset; uint8_t length; } unicode_case_mapping_row;")
            print("typedef struct { uint32_t start; uint32_t end; } unicode_case_property_range;")
            print()
            emit_mapping_rows("unicode_case_fold", fold)
            emit_mapping_rows("unicode_case_lower", lower)
            emit_mapping_rows("unicode_case_title", title)
            emit_mapping_rows("unicode_case_upper", upper)
            emit_property_ranges("unicode_case_alphabetic", core_props["Alphabetic"])
            emit_property_ranges("unicode_case_decimal", category_props["decimal"])
            emit_property_ranges("unicode_case_whitespace", prop_props["White_Space"])
            emit_property_ranges("unicode_case_uppercase", category_props["upper"])
            emit_property_ranges("unicode_case_lowercase", category_props["lower"])
            print("#endif")
        finally:
            sys.stdout = old_stdout
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
