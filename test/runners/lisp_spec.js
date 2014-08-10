var expect = require('chai').expect;
var runner = require('../../lib/runners/lisp');


describe( 'lisp runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'lisp', solution: '(format t "~a~%" 42)'}, function(buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });

        it('should handle setup code and imports', function (done) {
            runner.run({
                language: 'lisp',
                setup: [
                    '(defun twice (x) (+ x x))'
                ].join('\n'),
                solution: [
                    '(format t "~a~%" (twice 4))'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.equal('8\n');
                done();
            });
        });

   });
});
