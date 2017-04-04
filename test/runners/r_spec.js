var expect = require('chai').expect;
var runner = require('../runner');


describe('r runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'r', code: 'cat("What a pirate says?")'}, function(buffer) {
        expect(buffer.stdout).to.equal('What a pirate says?');
        done();
      });
    });
  });
});
