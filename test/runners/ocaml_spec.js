var expect = require('chai').expect;
var runner = require('../../lib/runners/ocaml');


describe( 'ocaml runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'ocaml', solution: 'print_string "42\n";;'}, function(buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
    });
});
