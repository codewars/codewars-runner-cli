var expect = require('chai').expect;
var runner = require('../../lib/runners/ruby');


describe( 'ruby runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'ruby', solution: 'puts 42'}, function(buffer) {
                expect(buffer.stdout ).to.equal('42\n');
                done();
            });
        });
    });

    describe('cw-2', function() {
        it( 'should handle a basic assertion', function(done){
            runner.run({language: 'ruby', solution: 'a = 1', fixture: 'Test.expect a == 1', testFramework: 'cw-2'}, function(buffer) {
                console.log(buffer)
                expect(buffer.stdout ).to.equal('<PASSED::>Test Passed\n');
                done();
            });
        });

        it( 'should handle a basic description', function(done){
            runner.run({language: 'ruby', solution: 'a = 1', fixture: 'describe("test") { Test.expect a == 1 }', testFramework: 'cw-2'}, function(buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>test\n<PASSED::>Test Passed\n<COMPLETEDIN::>');
                expect(buffer.stdout).to.contain('ms');
                done();
            });
        });

        describe('error handling', function() {
            it( 'should handle a mix of failures and successes', function(done) {
                runner.run({language: 'ruby',
                    solution:'a = 1',
                    fixture: 'describe "test" do\n' +
                        'it("test1") { Test.expect(false) }\n' +
                        'it("test2") { Test.expect(true) }\n' +
                        'end',
                    testFramework: 'cw-2'}, function(buffer) {
                    console.log(buffer.stdout)
                    expect(buffer.stdout).to.contain('<FAILED::>Value is not what was expected');
                    expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                    done();
                });
            });
            it( 'should gracefully handle custom errors', function(done) {
                runner.run({language: 'ruby',
                    solution:'a = 1',
                    fixture: 'describe "test" do\n' +
                        'it("test1") { raise "boom!" }\n' +
                        'it("test2") { Test.expect(true)}\n' +
                        'end',
                    testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout).to.contain('<ERROR::>');
                    expect(buffer.stdout).to.contain('boom!');
                    expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                    done();
                });
            });
            it( 'should gracefully handle reference errors', function(done) {
                runner.run({language: 'ruby',
                    solution:'a = 1',
                    fixture: 'describe "test" do\n' +
                        'it("test1") { a.idontexist() }\n' +
                        'it("test2") { Test.expect(true)}\n' +
                        'end',
                    testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout).to.contain('<ERROR::>');
                    expect(buffer.stdout).to.contain('\\n');
                    expect(buffer.stdout).to.contain('NoMethodError:');
                    expect(buffer.stdout).to.not.contain('from /cli-runner/');
                    expect(buffer.stdout).to.not.contain('-e:');
                    expect(buffer.stdout).to.not.contain('cw-2.rb');
                    expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                    done();
                });
            });
        });
    });
    describe('rspec', function() {
        it('should handle a basic assertion', function(done){
            runner.run({language: 'ruby',
                solution: 'a = 1',
                fixture: 'describe "test" do\n' +
                    'it("test1") { a.idontexist() }\n' +
                    'it("test2") { expect(true)}\n' +
                    'end',
                testFramework: 'rspec'}, function(buffer)
                {
                    expect(buffer.stdout).to.equal('wow');
                    done();
                }
            );
        });
    });
});
