var expect = require('chai').expect;
var runner = require('../runner');

describe('cpp runner', function () {
    describe('.run', function () {
        it('should handle basic code evaluation', function (done) {
            var code = [
                '#include <iostream>',
                'int main()',
                '{ std::cout << "\\\"Within C++, there is a much smaller and cleaner language struggling to get out.\\\" - Bjarn Stroustrup"; }'
            ].join('\n');

            runner.run({language: 'cpp', code: code}, function (buffer) {
                expect(buffer.stdout).to.equal("\"Within C++, there is a much smaller and cleaner language struggling to get out.\" - Bjarn Stroustrup");
                done();
            });
        });

        it('should handle C++11 nonsense', function (done) {
            var code = [
                '#include "stdio.h"',
                'int main() {',
                '    auto f = []{ printf("Finally, lambdas in C++.  Now if we had typeclasses, purity and laziness we might have a reasonable functional programming language."); };',
                '    f();',
                '}'
            ].join('\n');

            runner.run({language: 'cpp', code: code}, function (buffer) {
                expect(buffer.stdout).to.equal("Finally, lambdas in C++.  Now if we had typeclasses, purity and laziness we might have a reasonable functional programming language.");
                done();
            });
        });

        it('should handle setup code and imports', function (done) {
            runner.run({
                language: 'cpp',
                setup: [
                    'int square(int a) { return a * a ; }'
                ].join('\n'),
                code: [
                    '#include <iostream>',
                    'int square(int);',
                    'int main() {',
                    '    std::cout << square(6);',
                    '}',
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.equal('36');
                done();
            });
        });

        it('should handle importing classes and member functions', function (done) {
            runner.run({
                language: 'cpp',
                setup: [
                    '#include <iostream>',
                    'class pizza {',
                    'public:',
                    '    std::string tastes() {',
                    '        return "good";',
                    '    }',
                    '};'
                ].join('\n'),
                code: [
                    '#include <iostream>',
                    '#include "setup.cpp"',
                    'int main() {',
                    '    pizza p;',
                    '    std::cout << p.tastes();',
                    '}',
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.equal('good');
                done();
            });
        });

        describe('igloo bdd', function() {
            it( 'should handle basic assertions', function(done){
                runner.run({
                    language: 'cpp',
                    code: [
                        'unsigned int NumberOne() {',
                            'return 1;',
                        '}',
                    ].join('\n'),
                    fixture: [
                        'Describe(basic_tests)',
                        '{',
                          'It(should_test_well)',
                          '{',
                            'Assert::That(NumberOne(), Equals(1));',
                          '}',
                        '};',
                    ].join('\n'),
                    testFramework: 'mocha_tdd'
                }, function(buffer) {
                    expect(buffer.stdout).to.contain('<PASSED::>');
                    done();
                });
            });

            it( 'should handle basic failures', function(done){
                runner.run({
                    language: 'cpp',
                    code: [
                        'unsigned int NumberOne() {',
                            'return 1;',
                        '}',
                    ].join('\n'),
                    fixture: [
                        'Describe(basic_tests)',
                        '{',
                          'It(should_test_well)',
                          '{',
                            'Assert::That(NumberOne(), Equals(2));',
                          '}',
                        '};',
                    ].join('\n'),
                    testFramework: 'mocha_tdd'
                }, function(buffer) {
                    expect(buffer.stdout).to.contain('<FAILED::>');
                    expect(buffer.stdout).to.contain('Expected:');
                    done();
                });
            });
            
            it( 'should record std output', function(done){
                runner.run({
                    language: 'cpp',
                    code: [
                        'unsigned int NumberOne() {',
                            'std::cout << "Hello Codewars!";',
                            'return 1;',
                        '}',
                    ].join('\n'),
                    fixture: [
                        'Describe(basic_tests)',
                        '{',
                          'It(should_test_well)',
                          '{',
                            'Assert::That(NumberOne(), Equals(2));',
                          '}',
                        '};',
                    ].join('\n'),
                    testFramework: 'mocha_tdd'
                }, function(buffer) {
                    expect(buffer.stdout).to.contain('Hello Codewars!');
                    done();
                });
            });

        });

    });
});
