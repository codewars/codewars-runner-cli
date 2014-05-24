var expect = require('chai').expect;
var runner = require('../../lib/runners/javascript');


describe( 'javascript runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'javascript', solution: 'console.log(42)'}, function(buffer) {
                expect(buffer.stdout ).to.equal('42\n');
                done();
            });
        });

        describe('cw-2', function() {
            it( 'should handle a basic assertion', function(done){
                runner.run({language: 'javascript', solution: 'a = 1', fixture: 'Test.expect(a == 1)', testFramework: 'cw-2'}, function(buffer) {
                    expect(buffer.stdout ).to.equal('<PASSED::>Test Passed\n');
                    done();
                });
            });
        });
    });
});