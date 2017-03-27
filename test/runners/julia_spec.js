var expect = require('chai').expect;
var runner = require('../runner');

describe('julia runner', function() {
  // These specs are compatible with both Julia 0.4.6
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'julia',
        code: 'print("42")'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42');
        done();
      });
    });

    it('should handle a basic assertion', function(done) {
      runner.run({
        language: 'julia',
        code: 'a = 1',
        fixture: `
          facts(()->(@fact a => 1))
        `,
        testFramework: 'Test'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('("<PASSED::>Test Passed","<:LF:>\\n")');
        done();
      });
    });

    it('should handle a basic description', function(done) {
      runner.run({
        language: 'julia',
        code: 'global a = 1',
        fixture: 'facts(()->(@fact  a => 1) ,"test")',
        testFramework: 'Test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<DESCRIBE::>test<:LF:>\n("<PASSED::>Test Passed","<:LF:>\\n")');
        done();
      });
    });
  });
});
