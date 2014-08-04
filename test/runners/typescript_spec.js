var expect = require('chai').expect;
var runner = require('../../lib/runners/typescript');


describe( 'typescript runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'typescript', solution: 'console.log(42)'}, function(buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
    });
});