var expect = require('chai').expect;
var runner = require('../runner');

describe('c runner', function() {
describe('.run', function() {
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

                        Test(test, square) {
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
                setup: `int square(int a) { return a * a ; }`,
                code: ' ',
                fixture: `#include <criterion/criterion.h>
                        int square(int);

                        Test(test, basic_failure) {
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
                setup: `int square(int a) { return a * a ; }`,
                code: ' ',
                fixture: `#include <criterion/criterion.h>
                        int square(int);

                        Test(test, support_multiple_assert) {
                            cr_assert_eq(36,square(6));
                            cr_assert_neq(36,square(5));
                        }`
            }, function(buffer) {
                expect(buffer.stdout.match(/<PASSED::>/g).length).to.equal(2);
                done();
            });
        });
    });
});
});
