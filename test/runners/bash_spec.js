var expect = require('chai').expect;
var runner = require('../runner');


describe( 'bash runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'bash', code: 'echo 42'}, function(buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
    });
});