var expect = require('chai').expect;
var runner = require('../../lib/runners/julia');


describe('julia runner', function () {
    describe('.run', function () {
        it('should handle basic code evaluation', function (done) {
            runner.run({
                language: 'julia',
                solution: 'println("42")'
            }, function (buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
    });
    it('should handle setup code and imports', function (done) {
        runner.run({
            language: 'julia',
            setup: [
                'module constants',
                'export G',
                'const G = 6.67e-11 # Gravitational constant in m3 / kg s2',
                'end'
            ].join('\n'),
            solution: [
                'module Foo',
                'using constants',
                'println(G)',
                'end'
            ].join('\n')
        }, function (buffer) {
            expect(buffer.stdout).to.equal('6.67e-11\n');
            done();
        });
    });
    describe('codewars test framework (FactCheck.jl)', function () {
        it('should be able to run a basic test', function (done) {
            runner.run({
                language: 'julia',
                solution: [
                    'module Solution',
                    'end'
                ].join('\n'),
                fixture: [
                    'using Test',
                    'facts("Succeeding examples") do',
                    '  context("are as they seem") do',
                    '    @fact 1 => 1',
                    '  end',
                    'end'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>Succeeding examples');
                expect(buffer.stdout).to.contain('<IT::>are as they seem');
                expect(buffer.stdout).to.contain('<PASSED::> 1 => 1');
                done();
            });
        });
        it('should be able to import solution code', function (done) {
            runner.run({
                language: 'julia',
                solution: [
                    'module Solution',
                    'export inc',
                    'inc(x) = x + 1',
                    'end'
                ].join('\n'),
                fixture: [
                    'using Test',
                    'using Solution',
                    'facts("Can pull solution code") do',
                    '  context("and it increments stuff") do',
                    '    @fact inc(1) => 2',
                    '  end',
                    'end'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>Can pull solution code');
                expect(buffer.stdout).to.contain('<IT::>and it increments stuff');
                expect(buffer.stdout).to.contain('<PASSED::> :(inc(1)) => 2');
                done();
            });
        });
        it('should be handle solution code printing as a side effect', function (done) {
            runner.run({
                language: 'julia',
                solution: [
                    'module Solution',
                    'export inc',
                    'function inc(x)',
                        'println("Yolo")',
                        'return x + 1',
                    'end',
                    'end'
                ].join('\n'),
                fixture: [
                    'using Test',
                    'using Solution',
                    'facts("Can pull solution code") do',
                    '  context("and it prints and increments stuff") do',
                    '    @fact inc(1) => 2',
                    '  end',
                    'end'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>Can pull solution code');
                expect(buffer.stdout).to.contain('Yolo\n');
                expect(buffer.stdout).to.contain('<IT::>and it prints and increments stuff');
                expect(buffer.stdout).to.contain('<PASSED::> :(inc(1)) => 2');
                done();
            });
        });
        it('should be be able to handle sad paths', function (done) {
            runner.run({
                language: 'julia',
                solution: [
                    'module Solution',
                    'end'
                ].join('\n'),
                fixture: [
                    'using Test',
                    'facts("Bad times") do',
                    '  context("are here to stay") do',
                    '    @fact 1 => 2',
                    '  end',
                    'end'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>Bad times');
                expect(buffer.stdout).to.contain('<IT::>are here to stay');
                expect(buffer.stdout).to.contain('<FAILURE::>1 => 2 [got 1]');
                done();
            });
        });
        it('should be be able to handle really sad paths', function (done) {
            runner.run({
                language: 'julia',
                solution: [
                    'module Solution',
                    'end'
                ].join('\n'),
                fixture: [
                    'using Test',
                    'facts("What are you doing?") do',
                    '  context("This is unimaginable!") do',
                    '    @fact sqrt(-1) => 2',
                    '  end',
                    'end'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>What are you doing?');
                expect(buffer.stdout).to.contain('<IT::>This is unimaginable!');
                expect(buffer.stdout).to.contain('<ERROR::>');
                expect(buffer.stdout).to.contain('DomainError()');
                expect(buffer.stdout).to.contain('\n in sqrt at math.jl');
                done();
            });
        });
    });
});