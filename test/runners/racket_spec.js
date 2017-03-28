var expect = require('chai').expect;
var runner = require('../runner');


describe('racket runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'racket', code: '(print 42)'}, function(buffer) {
        expect(buffer.stdout).to.equal('42');
        done();
      });
    });

    it('should handle setup code and imports', function(done) {
      runner.run({
        language: 'racket',
        setup: [
          '#lang racket/base',
          '(provide twice)',
          '(define (twice x) (+ x x))'
        ].join('\n'),
        code: [
          "(print (twice 2))"
        ].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.equal('4');
        done();
      });
    });

  });
});
