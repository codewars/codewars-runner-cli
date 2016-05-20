const expect = require('chai').expect;
const runner = require('../runner');

describe('dart runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'dart',
        code: `main() {print('Boom!');}`
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Boom!');
        done();
      });
    });
  });
});
