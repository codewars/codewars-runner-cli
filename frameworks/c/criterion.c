#include <stdio.h>
#include <criterion/criterion.h>
#include <criterion/hooks.h>

// before running the tests, initialize
ReportHook(PRE_ALL)(struct criterion_test_set *tests) {
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);
}

//  before the test is run
ReportHook(PRE_TEST)(struct criterion_test *test) {
    printf("<DESCRIBE::>%s\n", test->category);
    printf("<IT::>%s\n",test->name);
}

// when an assertion is hit
ReportHook(ASSERT)(struct criterion_assert_stats *stats) {
    if (stats->passed) {
      puts("<PASSED::>Test Passed");
    } else {
      printf("<FAILED::>%s\n",stats->message);
    }
    puts("<COMPLETEDIN::>");
}

// when a test crashes unexpectedly
ReportHook(TEST_CRASH)(struct criterion_test_stats *stats) {
  puts("<COMPLETEDIN::>");
}

//  after a test ends
ReportHook(POST_TEST)(struct criterion_test_stats *stats) {
  printf("<COMPLETEDIN::>%f\n",stats->elapsed_time*1000);
}


