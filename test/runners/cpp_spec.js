var expect = require('chai').expect;
var runner = require('../../lib/runners/cpp');

describe('c++ runner', function () {
    describe('.run', function () {
        it('should handle basic code evaluation', function (done) {
            var solution = [
                '#include <iostream>',
                'int main()',
                '{ std::cout << "\\\"Within C++, there is a much smaller and cleaner language struggling to get out.\\\" - Bjarn Stroustrup"; }'
            ].join('\n');

            runner.run({language: 'c++', solution: solution}, function (buffer) {
                expect(buffer.stdout).to.equal("\"Within C++, there is a much smaller and cleaner language struggling to get out.\" - Bjarn Stroustrup");
                done();
            });
        });
        it('should handle C++11 nonsense', function (done) {
            var solution = [
                '#include "stdio.h"',
                'int main() {',
                '    auto f = []{ printf("Finally, lambdas in C++.  Now if we had typeclasses, purity and laziness we might have a reasonable functional programming language."); };',
                '    f();',
                '}'
            ].join('\n');

            runner.run({language: 'c++', solution: solution}, function (buffer) {
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
                solution: [
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
    });
});
