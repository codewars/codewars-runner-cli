var expect = require('chai').expect;
var runner = require('../../lib/runners/lua');


describe( 'lua runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'lua', solution: 'print(42)'}, function(buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
    });
});