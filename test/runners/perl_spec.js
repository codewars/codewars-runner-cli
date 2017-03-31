var expect = require('chai').expect;
var runner = require('../runner');


describe('perl runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'perl', code: 'print 42'}, function(buffer) {
        expect(buffer.stdout).to.equal('42');
        done();
      });
    });
  });
});
