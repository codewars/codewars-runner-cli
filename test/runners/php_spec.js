var expect = require('chai').expect;
var runner = require('../../lib/runners/php');


describe( 'php runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'php', solution: 'echo 42 . PHP_EOL;'}, function(buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
    });
});