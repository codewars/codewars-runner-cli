var expect = require('chai').expect;
var runner = require('../runner');

describe('c runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('c');

    it('should handle basic code evaluation', function(done) {
      var solution = `
                #include <stdio.h>

                int main() {
                  printf("Hello world");
                }
                `;
      runner.run({
        language: 'c',
        code: solution
      }, function(buffer) {
        expect(buffer.stdout).to.equal("Hello world");
        expect(buffer.exitCode).to.equal(0);
        done();
      });
    });
    it('should get the return code', function(done) {
      var solution = `
                #include <stdlib.h>

                int main() {
                  return 10;
                }
                `;
      runner.run({
        language: 'c',
        code: solution
      }, function(buffer) {
        expect(buffer.exitCode).to.equal(10);
        expect(buffer.exitSignal).to.equal(null);
        done();
      });
    });
    it('should catch return codes', function(done) {
      var solution = `
                #include <stdlib.h>

                int main() {
                  int *nullPointer = NULL;
                  *nullPointer = 0;
                  return 0;
                }
                `;
      runner.run({
        language: 'c',
        code: solution
      }, function(buffer) {
        expect(buffer.exitCode).to.equal(null);
        expect(buffer.exitSignal).to.equal('SIGSEGV');
        done();
      });
    });
    it('should handle setup code and imports', function(done) {
      runner.run({
        language: 'c',
        setup: `int square(int a) { return a * a ; }`,
        code: `
                    #include <stdio.h>
                    int square(int);

                    int main() {
                      printf("%i",square(5));
                    }`
      }, function(buffer) {
        expect(buffer.stdout).to.equal('25');
        done();
      });
    });
    it('should handle compile errors', function(done) {
      runner.run({
        language: 'c',
        code: `
                int main() {
                    fudge();
                    doubleFudge();
                }
            `
      }, function(buffer) {
        expect(buffer.stderr).to.contain("undefined reference to `fudge\'");
        expect(buffer.stderr).to.contain("undefined reference to `doubleFudge\'");
        expect(buffer.stderr).to.contain("error: linker command failed with exit code 1");
        done();
      });
    });
    describe('criterion test framework', function() {
      it('should be able to run a basic test', function(done) {
        runner.run({
          language: 'c',
          code: `int square(int a) { return a * a ; }`,
          fixture: `#include <criterion/criterion.h>
                        int square(int);

                        Test(basic_test, should_square_a_number) {
                            cr_assert_eq(25,square(5));
                        }`
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
          done();
        });
      });
      it('should handle setup code for a basic test', function(done) {
        runner.run({
          language: 'c',
          setup: `int square(int a) { return a * a ; }`,
          code: ' ',
          fixture: `#include <criterion/criterion.h>
                        int square(int);

                        Test(test, square) {
                            cr_assert_eq(25,square(5));
                        }`
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
          done();
        });
      });
      it('should handle basic failures', function(done) {
        runner.run({
          language: 'c',
          code: `int square(int a) { return a * a ; }`,
          fixture: `#include <criterion/criterion.h>
                        int square(int);

                        Test(basic_failure, should_raise_a_failure) {
                            cr_assert_eq(25,square(6));
                        }`
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>');
          expect(buffer.stdout).to.contain('The expression (25) == (square(6)) is false');
          done();
        });
      });

      it('should support multiple asserts', function(done) {
        runner.run({
          language: 'c',
          code: `int square(int a) { return a * a ; }`,
          fixture: `#include <criterion/criterion.h>
                        int square(int);

                        Test(multiple_assert_test, should_support_multiple_asserts) {
                            cr_assert_eq(36,square(6));
                            cr_assert_neq(36,square(5));
                            cr_assert_eq(36,square(5));
                        }`
        }, function(buffer) {
          expect(buffer.stdout.match(/<PASSED::>/g).length).to.equal(2);
          expect(buffer.stdout.match(/<FAILED::>/g).length).to.equal(1);
          done();
        });
      });
      it('should support multiple tests on one suite', function(done) {
        runner.run({
          language: 'c',
          code: `int square(int a) { return a * a ; }`,
          fixture: `#include <criterion/criterion.h>
                        int square(int);

                        Test(square_test, should_square_a_number) {
                            cr_assert_eq(36,square(6));
                            cr_assert_neq(36,square(5));
                        }

                        Test(square_test, should_square_number_5) {
                            cr_assert_eq(25,square(5));
                        }
                        `
        }, function(buffer) {
          expect(buffer.stdout.match(/<DESCRIBE::>/g).length).to.equal(1);
          expect(buffer.stdout.match(/<IT::>/g).length).to.equal(2);
          expect(buffer.stdout.match(/<PASSED::>/g).length).to.equal(3);
          done();
        });
      });
      it('should support multiple test suites', function(done) {
        runner.run({
          language: 'c',
          code: `int square(int a) { return a * a ; }`,
          fixture: `#include <criterion/criterion.h>
                        int square(int);

                        Test(basic_test, should_multiply) {
                            cr_assert_eq(36,6*6);
                            cr_assert_neq(36,5*5);
                        }

                        Test(square_test, should_square_a_number) {
                            cr_assert_eq(36,square(6));
                            cr_assert_eq(36,square(5));
                        }
                        `
        }, function(buffer) {
          expect(buffer.stdout.match(/<DESCRIBE::>/g).length).to.equal(2);
          expect(buffer.stdout.match(/<PASSED::>/g).length).to.equal(3);
          expect(buffer.stdout.match(/<FAILED::>/g).length).to.equal(1);
          done();
        });
      });
      it('should handle a crash', function(done) {
        runner.run({
          language: 'c',
          code: ' ',
          fixture: `
                    #include <criterion/criterion.h>

                    Test(misc, crash_dereference) {
                        int *i = NULL;
                        *i = 42; // CRASH: dereference i on writing
                    }
                    `
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>Test Crashed');
          expect(buffer.stdout).to.contain('<:LF:>Signal code: 11');
          done();
        });
      });
      it('should handle a timeout', function(done) {
        runner.run({
          language: 'c',
          code: ' ',
          fixture: `
                    # include <unistd.h>
                    #include <criterion/criterion.h>

                    Test(timeout, should_fail_with_a_timeout, .timeout = 1.) {
                        sleep(10);
                    }
                    `
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>Test Timed Out');
          done();
        });
      });
      it('should replace message line feeds', function(done) {
        runner.run({
          language: 'c',
          code: ' ',
          fixture: `
                    #include <criterion/criterion.h>

                    Test(timeout, simple) {
                        cr_assert(0, "This message contains \\n 2 \\n line feeds");
                    }
                    `
        }, function(buffer) {
          expect(buffer.stdout.match(/<:LF:>/g).length).to.equal(2);
          done();
        });
      });
    });
  });
});
