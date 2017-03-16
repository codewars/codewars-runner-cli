const expect = require('chai').expect;
const runner = require('../runner');

describe('nim runner', function() {
  it('should compile and run Nim', function(done) {
    runner.run({
      language: 'nim',
      solution: 'echo "42"'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
      done();
    });
  });
});
