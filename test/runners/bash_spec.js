var expect = require('chai').expect;
var runner = require('../../lib/runners/bash');


describe( 'bash runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'bash', solution: 'echo 42'}, function(buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
    });
});