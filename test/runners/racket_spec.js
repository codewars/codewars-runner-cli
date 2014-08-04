var expect = require('chai').expect;
var runner = require('../../lib/runners/racket');


describe( 'racket runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'racket', solution: '(print 42)'}, function(buffer) {
                expect(buffer.stdout).to.equal('42');
                done();
            });
        });
        it('should handle setup code and imports', function (done) {
            runner.run({
                language: 'racket',
                setup: [
                    'module constants',
                    'export G',
                    'const G = 6.67e-11 # Gravitational constant in m3 / kg s2',
                    'end'
                ].join('\n'),
                solution: [
                    'module Foo',
                    'using constants',
                    'println(G)',
                    'end'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.equal('6.67e-11\n');
                done();
            });
        });
    });
});