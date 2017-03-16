const expect = require('chai').expect;
const runner = require('../runner');

describe('D runner', function() {
  it('should compile and run D', function(done) {
    runner.run({
      language: 'd',
      solution: `
import std.stdio;

void main() {
  writeln("42");
}
`
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
      done();
    });
  });
});
