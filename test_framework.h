#ifndef TEST_FRAMEWORK_H
#define TEST_FRAMEWORK_H

#include <stdio.h>
#include <string.h>

// Test counters
static int tests_passed = 0;
static int tests_failed = 0;
static const char *current_test_name = NULL;

// Define a test function
#define TEST(name) static void test_##name(void)

// Run a test
#define RUN_TEST(name)                                                         \
    do {                                                                       \
        current_test_name = #name;                                             \
        test_##name();                                                         \
    } while (0)

// Assert condition is true
#define ASSERT(cond)                                                           \
    do {                                                                       \
        if (!(cond)) {                                                         \
            printf("  FAIL: %s\n", current_test_name);                         \
            printf("    Assertion failed: %s\n", #cond);                       \
            printf("    at %s:%d\n", __FILE__, __LINE__);                      \
            tests_failed++;                                                    \
            return;                                                            \
        }                                                                      \
    } while (0)

// Assert equality (for integers)
#define ASSERT_EQ(a, b)                                                        \
    do {                                                                       \
        long long _a = (long long)(a);                                         \
        long long _b = (long long)(b);                                         \
        if (_a != _b) {                                                        \
            printf("  FAIL: %s\n", current_test_name);                         \
            printf("    Expected %s == %s\n", #a, #b);                         \
            printf("    Got %lld != %lld\n", _a, _b);                          \
            printf("    at %s:%d\n", __FILE__, __LINE__);                      \
            tests_failed++;                                                    \
            return;                                                            \
        }                                                                      \
    } while (0)

// Assert string equality
#define ASSERT_STR_EQ(a, b)                                                    \
    do {                                                                       \
        const char *_a = (a);                                                  \
        const char *_b = (b);                                                  \
        if (strcmp(_a, _b) != 0) {                                             \
            printf("  FAIL: %s\n", current_test_name);                         \
            printf("    Expected \"%s\" == \"%s\"\n", _a, _b);                 \
            printf("    at %s:%d\n", __FILE__, __LINE__);                      \
            tests_failed++;                                                    \
            return;                                                            \
        }                                                                      \
    } while (0)

// Mark test as passed
#define PASS()                                                                 \
    do {                                                                       \
        printf("  PASS: %s\n", current_test_name);                             \
        tests_passed++;                                                        \
    } while (0)

// Print test summary
#define TEST_SUMMARY(module)                                                   \
    do {                                                                       \
        printf("\n=== %s tests: %d passed, %d failed ===\n", module,           \
               tests_passed, tests_failed);                                    \
        return tests_failed > 0 ? 1 : 0;                                       \
    } while (0)

#endif // TEST_FRAMEWORK_H
