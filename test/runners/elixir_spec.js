var expect = require('chai').expect;
var runner = require('../runner');


describe( 'elixir runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'elixir', code: 'IO.puts "This was real. And it was me. I had lived that life, and I had died that death. I was staring at the very end of me. ― Hilary Duff, Elixir (2010)"'}, function(buffer) {
                expect(buffer.stdout).to.contain('This was real');
                done();
            });
        });
    });
});