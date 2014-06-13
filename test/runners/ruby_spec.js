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
    });
});