#include <stdio.h>
#include <string.h>
#include <criterion/criterion.h>
#include <criterion/hooks.h>

// before running the tests, initialize
ReportHook(PRE_ALL)(struct criterion_test_set *tests) {
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);
}

//  before the test is run
ReportHook(PRE_SUITE)(struct criterion_suite_set *set) {
    printf("\n<DESCRIBE::>%s\n", set->suite.name);
}

//  before the test is run
ReportHook(PRE_TEST)(struct criterion_test *test) {
    printf("\n<IT::>%s\n",test->name);
}

// when an assertion is hit
ReportHook(ASSERT)(struct criterion_assert_stats *stats) {
    printf("\n"); // break any lines incase printf was used
    if (stats->passed) {
      puts("<PASSED::>Test Passed");
    } else {
      printf("<FAILED::>");
      // print the assertion message, replacing \n with <:LF:>
      for (const char* p = stats->message; *p != '\0'; ++p) {
          if (*p == '\n') {
            printf("<:LF:>");
          } else {
            fputc(*p,stdout);
          }
      }
      fputc('\n',stdout);
    }
}

// when a test crashes unexpectedly
ReportHook(TEST_CRASH)(struct criterion_test_stats *stats) {
  printf("\n<FAILED::>Test Crashed<:LF:>Exit code: %d<:LF:>Signal code: %d",stats->exit_code, stats->signal);
}

//  after a test ends
ReportHook(POST_TEST)(struct criterion_test_stats *stats) {
  if (stats->timed_out) {
    puts("\n<FAILED::>Test Timed Out");
  }
  printf("\n<COMPLETEDIN::>%f\n",stats->elapsed_time*1000);
}

//  after the test is run
ReportHook(POST_SUITE)(struct criterion_suite_stats *stats) {
    puts("\n<COMPLETEDIN::>");
}

