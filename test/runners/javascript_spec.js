var expect = require('chai').expect;
var runner = require('../../lib/runners/javascript');


describe( 'javascript runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'javascript', solution: 'console.log(42)'}, function(buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });

        describe('cw-2', function() {
            it( 'should handle a basic assertion', function(done){
                runner.run({language: 'javascript', solution: 'a = 1', fixture: 'Test.expect(a == 1);', testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout).to.equal('<PASSED::>Test Passed\n');
                    done();
                });
            });

            it( 'should handle comments as fixture', function(done){
                runner.run({language: 'javascript', solution: 'console.log(42)', fixture: '//', testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout).to.equal('42\n');
                    done();
                });
            });

            it( 'should handle a basic failed test', function(done){
                runner.run({language: 'javascript', solution: 'a = 1', fixture: 'Test.expect(a == 2)', testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout).to.equal('<FAILED::>Value is not what was expected\n');
                    done();
                });
            });

            it( 'should handle logging objects', function(done){
                runner.run({language: 'javascript', solution:'console.log({a: 1});', testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout).to.equal('{ a: 1 }\n');
                    done();
                });
            });

            describe('error handling', function() {
                it( 'should handle a mix of failures and successes', function(done) {
                    runner.run({language: 'javascript',
                                solution:'var a = 1',
                                fixture: 'describe("test", function(){\n' +
                                    'it("test1", function(){ Test.expect(false) });' +
                                    'it("test2", function(){ Test.expect(true)});})',
                                testFramework: 'cw-2'}, function(buffer) {
                        expect(buffer.stdout).to.contain('<FAILED::>Value is not what was expected');
                        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                        done();
                    });
                });
                it( 'should gracefully handle custom errors', function(done) {
                    runner.run({language: 'javascript',
                                solution:'var a = 1',
                                fixture: 'describe("test", function(){\n' +
                                    'it("test1", function(){ throw "boom!" });' +
                                    'it("test2", function(){ Test.expect(true)});})',
                                testFramework: 'cw-2'}, function(buffer) {
                        expect(buffer.stdout).to.contain('<ERROR::>');
                        expect(buffer.stdout).to.contain('boom!');
                        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                        done();
                    });
                });
                it( 'should gracefully handle reference errors', function(done) {
                    runner.run({language: 'javascript',
                                solution:'var a = 1',
                                fixture: 'describe("test", function(){\n' +
                                    'it("test1", function(){ b.test() });' +
                                    'it("test2", function(){ Test.expect(true)});})',
                                testFramework: 'cw-2'}, function(buffer) {
                        expect(buffer.stdout).to.contain('<ERROR::>');
                        expect(buffer.stdout).to.contain('\\n');
                        expect(buffer.stdout).to.contain('ReferenceError:');
                        expect(buffer.stdout).to.not.contain('[eval]');
                        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                        done();
                    });
                });
                it( 'should gracefully top level handle reference errors', function(done) {
                    runner.run({language: 'javascript',
                                solution:'b.test()',
                                fixture: 'describe("test", function(){\n' +
                                    'it("test2", function(){ Test.expect(true)});})',
                                testFramework: 'cw-2'}, function(buffer) {
                        expect(buffer.stdout).to.contain('<ERROR::>');
                        expect(buffer.stdout).to.contain('\\n');
                        expect(buffer.stdout).to.contain('ReferenceError:');
                        expect(buffer.stdout).to.not.contain('[eval]');
                        expect(buffer.stdout).to.not.contain('Object.Test.handleError');
                        done();
                    });
                });
            });
        });
    });
});