CC = gcc
CFLAGS ?= -Wall -Wextra -Wshadow -Wstrict-prototypes -Wmissing-prototypes \
          -Wold-style-definition -Wformat=2 -Wundef -Wdouble-promotion \
          -std=c11 -O2
LDFLAGS = -lm
UNICODE_VERSION = 15.1.0

# Source files
SRCS = main.c context.c reader.c writer.c env.c primitive_table.c feature_table.c utf8.c primitives.c macros.c eval.c bignum.c \
       prim_numeric.c prim_compare.c prim_list.c prim_string.c prim_type.c \
       prim_char.c prim_vector.c prim_math.c prim_io.c prim_port.c prim_numtower.c \
       eval_forms.c eval_cont.c compile.c code_object.c optimize.c vm.c compiled_pattern.c
OBJS = $(SRCS:.c=.o)
DEBUG_OBJS = $(SRCS:.c=.debug.o)
SAN_OBJS = $(SRCS:.c=.san.o)
HEADERS = types.h context.h reader.h writer.h env.h primitive_table.h feature_table.h utf8.h primitives.h macros.h eval.h bignum.h \
          prim_internal.h eval_internal.h bytecode.h compile_internal.h compiled_pattern.h
GENERATED = stdlib_data.h

# Target executable
TARGET = vesper
DEBUG_TARGET = vesper-debug
DEBUG_CFLAGS = -Wall -Wextra -Wshadow -Wstrict-prototypes -std=c11 -g -O0 \
               -DDEBUG -DDEBUG_GC -fno-omit-frame-pointer
DEBUG_LDFLAGS = $(LDFLAGS)
SAN_TARGET = vesper-asan
SAN_CFLAGS = $(DEBUG_CFLAGS) -fsanitize=address,undefined
SAN_LDFLAGS = $(LDFLAGS) -fsanitize=address,undefined
UBSAN_TARGET = vesper-ubsan
UBSAN_OBJS = $(SRCS:.c=.ubsan.o)
UBSAN_CFLAGS = $(DEBUG_CFLAGS) -fsanitize=undefined
UBSAN_LDFLAGS = $(LDFLAGS) -fsanitize=undefined

.PHONY: all clean distclean debug sanitize ubsan test test-interpreter test-c test-prop test-sanitize test-ubsan test-all unicode-tables

all: $(TARGET)

# Generate stdlib_data.h from stdlib.scm
stdlib_data.h: stdlib.scm
	@echo "Generating stdlib_data.h from stdlib.scm"
	@{ \
		echo "// Auto-generated from stdlib.scm - do not edit"; \
		echo "static const char stdlib_scm[] ="; \
		sed 's/\\/\\\\/g; s/"/\\"/g; s/^/    "/; s/$$/\\n"/' stdlib.scm; \
		echo ";"; \
		echo "static const size_t stdlib_scm_len = sizeof(stdlib_scm) - 1;"; \
	} > $@

unicode-tables:
	@mkdir -p .unicode
	curl -L https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/UnicodeData.txt -o .unicode/UnicodeData.txt
	curl -L https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/CompositionExclusions.txt -o .unicode/CompositionExclusions.txt
	curl -L https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/DerivedNormalizationProps.txt -o .unicode/DerivedNormalizationProps.txt
	curl -L https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/DerivedCoreProperties.txt -o .unicode/DerivedCoreProperties.txt
	curl -L https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/PropList.txt -o .unicode/PropList.txt
	curl -L https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/CaseFolding.txt -o .unicode/CaseFolding.txt
	curl -L https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/SpecialCasing.txt -o .unicode/SpecialCasing.txt
	curl -L https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/NormalizationTest.txt -o .unicode/NormalizationTest.txt
	python3 tools/gen_unicode_norm.py .unicode/UnicodeData.txt .unicode/CompositionExclusions.txt .unicode/DerivedNormalizationProps.txt unicode_norm_tables.h
	python3 tools/gen_unicode_char.py .unicode/UnicodeData.txt .unicode/DerivedCoreProperties.txt .unicode/PropList.txt .unicode/CaseFolding.txt .unicode/SpecialCasing.txt unicode_char_tables.h
	python3 tools/gen_unicode_norm_fixture.py .unicode/NormalizationTest.txt 1000 unicode_norm_test_data.h
	python3 tools/gen_unicode_case_fixture.py .unicode/UnicodeData.txt .unicode/DerivedCoreProperties.txt .unicode/PropList.txt .unicode/CaseFolding.txt .unicode/SpecialCasing.txt unicode_case_test_data.h

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS)

# Object file dependencies
$(OBJS): $(HEADERS)
main.o: main.c $(HEADERS) stdlib_data.h
context.o: context.c context.h feature_table.h types.h
reader.o: reader.c reader.h context.h types.h
writer.o: writer.c writer.h context.h types.h
env.o: env.c env.h primitive_table.h context.h types.h
primitive_table.o: primitive_table.c primitive_table.h types.h
feature_table.o: feature_table.c feature_table.h
utf8.o: utf8.c utf8.h context.h types.h
primitives.o: primitives.c primitives.h feature_table.h utf8.h prim_internal.h context.h reader.h writer.h types.h
prim_numeric.o: prim_numeric.c prim_internal.h context.h types.h
prim_compare.o: prim_compare.c prim_internal.h context.h types.h unicode_char_tables.h
prim_list.o: prim_list.c prim_internal.h context.h types.h
prim_string.o: prim_string.c prim_internal.h utf8.h context.h types.h unicode_norm_tables.h unicode_char_tables.h
prim_type.o: prim_type.c prim_internal.h context.h types.h
prim_char.o: prim_char.c prim_internal.h context.h types.h unicode_char_tables.h
prim_vector.o: prim_vector.c prim_internal.h context.h types.h
prim_math.o: prim_math.c prim_internal.h context.h types.h
prim_io.o: prim_io.c prim_internal.h context.h reader.h writer.h types.h
prim_port.o: prim_port.c prim_internal.h context.h types.h
prim_numtower.o: prim_numtower.c prim_internal.h context.h types.h
macros.o: macros.c macros.h context.h types.h
eval.o: eval.c eval.h eval_internal.h context.h env.h primitives.h macros.h reader.h types.h
eval_forms.o: eval_forms.c eval_internal.h context.h env.h macros.h types.h
eval_cont.o: eval_cont.c eval_internal.h context.h env.h types.h
bignum.o: bignum.c bignum.h
compile.o: compile.c bytecode.h compile_internal.h context.h env.h macros.h types.h
code_object.o: code_object.c bytecode.h compile_internal.h context.h types.h
optimize.o: optimize.c bytecode.h compile_internal.h context.h writer.h types.h
vm.o: vm.c bytecode.h compiled_pattern.h context.h env.h primitives.h macros.h \
      prim_internal.h utf8.h writer.h types.h
compiled_pattern.o: compiled_pattern.c compiled_pattern.h context.h types.h

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.debug.o: %.c $(HEADERS) stdlib_data.h
	$(CC) $(DEBUG_CFLAGS) -c $< -o $@

$(DEBUG_TARGET): $(DEBUG_OBJS)
	$(CC) $(DEBUG_CFLAGS) -o $@ $(DEBUG_OBJS) $(DEBUG_LDFLAGS)

debug: $(DEBUG_TARGET)

%.san.o: %.c $(HEADERS) stdlib_data.h
	$(CC) $(SAN_CFLAGS) -c $< -o $@

$(SAN_TARGET): $(SAN_OBJS)
	$(CC) $(SAN_CFLAGS) -o $@ $(SAN_OBJS) $(SAN_LDFLAGS)

sanitize: $(SAN_TARGET)

%.ubsan.o: %.c $(HEADERS) stdlib_data.h
	$(CC) $(UBSAN_CFLAGS) -c $< -o $@

$(UBSAN_TARGET): $(UBSAN_OBJS)
	$(CC) $(UBSAN_CFLAGS) -o $@ $(UBSAN_OBJS) $(UBSAN_LDFLAGS)

ubsan: $(UBSAN_TARGET)

distclean: clean
	rm -f $(GENERATED)

# Run the interpreter
run: $(TARGET)
	./$(TARGET)

# Run Scheme tests
test: $(TARGET)
	@./$(TARGET) < test.scm 2>&1 | grep -E '(^===|PASS|FAIL|^Tests:|passed|FAILED)'

test-interpreter: $(TARGET)
	@./$(TARGET) --interpreter < test.scm 2>&1 | grep -E '(^===|PASS|FAIL|^Tests:|passed|FAILED)'

# C unit tests
TEST_SRCS = test_bignum.c test_reader.c test_context.c test_macros.c test_eval.c test_pattern.c test_unicode_norm.c test_unicode_case.c
TEST_BINS = $(TEST_SRCS:.c=)

# Object files needed for interpreter tests (excludes main.o)
INTERP_OBJS = context.o reader.o writer.o env.o primitive_table.o feature_table.o utf8.o primitives.o macros.o eval.o bignum.o \
              prim_numeric.o prim_compare.o prim_list.o prim_string.o prim_type.o \
              prim_char.o prim_vector.o prim_math.o prim_io.o prim_port.o prim_numtower.o \
              eval_forms.o eval_cont.o compile.o code_object.o optimize.o vm.o compiled_pattern.o

test-c: $(TEST_BINS)
	@for t in $(TEST_BINS); do ./$$t || exit 1; done

test_bignum: test_bignum.c bignum.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test_bignum_ubsan: test_bignum.c bignum.ubsan.o
	$(CC) $(UBSAN_CFLAGS) -o $@ $^ $(UBSAN_LDFLAGS)

test_reader: test_reader.c $(INTERP_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test_context: test_context.c $(INTERP_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test_macros: test_macros.c $(INTERP_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test_eval: test_eval.c $(INTERP_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test_pattern: test_pattern.c $(INTERP_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test_unicode_norm: test_unicode_norm.c $(INTERP_OBJS) unicode_norm_test_data.h
	$(CC) $(CFLAGS) -o $@ test_unicode_norm.c $(INTERP_OBJS) $(LDFLAGS)

test_unicode_case: test_unicode_case.c $(INTERP_OBJS) unicode_case_test_data.h
	$(CC) $(CFLAGS) -o $@ test_unicode_case.c $(INTERP_OBJS) $(LDFLAGS)

# Run property tests
test-prop: $(TARGET)
	@./$(TARGET) property_tests.scm

# Run bounded sanitizer smoke tests
test-sanitize: sanitize
	@python3 tools/run_sanitize_smoke.py ./$(SAN_TARGET)

# Run bounded UBSan-only smoke tests. Useful on macOS toolchains where ASan
# can hang before main during sanitizer runtime initialization.
test-ubsan: ubsan test_bignum_ubsan
	@python3 tools/run_sanitize_smoke.py ./$(UBSAN_TARGET)
	@./test_bignum_ubsan

# Run all tests, including the UBSan-only smoke path that works on macOS
# systems where ASan can hang before main.
test-all: test test-interpreter test-c test-prop test-ubsan

clean:
	rm -f $(OBJS) $(DEBUG_OBJS) $(SAN_OBJS) $(UBSAN_OBJS) $(TARGET) \
	      $(DEBUG_TARGET) $(SAN_TARGET) $(UBSAN_TARGET) $(TEST_BINS) \
	      test_bignum_ubsan

# Format code (requires clang-format)
format:
	clang-format -i $(SRCS) $(HEADERS) $(TEST_SRCS)
