var expect = require('chai').expect;
var runner = require('../../lib/runners/perl');


describe( 'perl runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'perl', solution: 'print 42'}, function(buffer) {
                expect(buffer.stdout).to.equal('42');
                done();
            });
        });
    });
});