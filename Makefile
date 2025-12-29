CC = gcc
CFLAGS = -Wall -Wextra -Wshadow -Wstrict-prototypes -Wmissing-prototypes \
         -Wold-style-definition -Wformat=2 -Wundef -Wdouble-promotion \
         -std=c11 -O2
LDFLAGS = -lm

# Source files
SRCS = main.c context.c reader.c writer.c env.c primitives.c macros.c eval.c bignum.c \
       prim_numeric.c prim_compare.c prim_list.c prim_string.c prim_type.c \
       prim_char.c prim_vector.c prim_math.c prim_io.c prim_port.c prim_numtower.c \
       eval_forms.c eval_cont.c compile.c vm.c
OBJS = $(SRCS:.c=.o)
HEADERS = types.h context.h reader.h writer.h env.h primitives.h macros.h eval.h bignum.h \
          prim_internal.h eval_internal.h bytecode.h
GENERATED = stdlib_data.h

# Target executable
TARGET = lisp

.PHONY: all clean distclean debug test test-c

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

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS)

# Object file dependencies
main.o: main.c $(HEADERS) stdlib_data.h
context.o: context.c context.h types.h
reader.o: reader.c reader.h context.h types.h
writer.o: writer.c writer.h context.h types.h
env.o: env.c env.h context.h types.h
primitives.o: primitives.c primitives.h prim_internal.h context.h reader.h writer.h types.h
prim_numeric.o: prim_numeric.c prim_internal.h context.h types.h
prim_compare.o: prim_compare.c prim_internal.h context.h types.h
prim_list.o: prim_list.c prim_internal.h context.h types.h
prim_string.o: prim_string.c prim_internal.h context.h types.h
prim_type.o: prim_type.c prim_internal.h context.h types.h
prim_char.o: prim_char.c prim_internal.h context.h types.h
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
compile.o: compile.c bytecode.h context.h env.h macros.h types.h
vm.o: vm.c bytecode.h context.h env.h primitives.h macros.h types.h

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

debug: CFLAGS = -Wall -Wextra -Wshadow -Wstrict-prototypes -std=c11 -g -O0 -DDEBUG \
               -fsanitize=address,undefined -fno-omit-frame-pointer
debug: LDFLAGS += -fsanitize=address,undefined
debug: clean $(TARGET)

distclean: clean
	rm -f $(GENERATED)

# Run the interpreter
run: $(TARGET)
	./$(TARGET)

# Run Scheme tests
test: $(TARGET)
	@./$(TARGET) < test.scm 2>&1 | grep -E '(^===|PASS|FAIL|^Tests:|passed|FAILED)'

# C unit tests
TEST_SRCS = test_bignum.c test_reader.c test_context.c test_macros.c
TEST_BINS = $(TEST_SRCS:.c=)

# Object files needed for interpreter tests (excludes main.o)
INTERP_OBJS = context.o reader.o writer.o env.o primitives.o macros.o eval.o bignum.o \
              prim_numeric.o prim_compare.o prim_list.o prim_string.o prim_type.o \
              prim_char.o prim_vector.o prim_math.o prim_io.o prim_port.o prim_numtower.o \
              eval_forms.o eval_cont.o compile.o vm.o

test-c: $(TEST_BINS)
	@for t in $(TEST_BINS); do ./$$t || exit 1; done

test_bignum: test_bignum.c bignum.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test_reader: test_reader.c $(INTERP_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test_context: test_context.c $(INTERP_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test_macros: test_macros.c $(INTERP_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Run all tests
test-all: test test-c

clean:
	rm -f $(OBJS) $(TARGET) $(TEST_BINS)

# Format code (requires clang-format)
format:
	clang-format -i $(SRCS) $(HEADERS) $(TEST_SRCS)
