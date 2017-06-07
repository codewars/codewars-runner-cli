"use strict";

const expect = require('chai').expect;
const runner = require('../runner');

describe('D runner', function() {
  it('should compile and run D', function(done) {
    runner.run({
      language: 'd',
      solution: [
        `import std.stdio;`,
        ``,
        `void main() {`,
        `  writeln("42");`,
        `}`,
      ].join('\n')
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
      done();
    });
  });

  it('should compile and run D with passing unittest', function(done) {
    runner.run({
      language: 'd',
      solution: [
        `int add(int x, int y) { return x + y; }`,
        `unittest {`,
        `  assert(add(1, 1) == 2);`,
        `}`,
        `void main() {}`,
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.equal('');
      done();
    });
  });

  it('should compile and run D with failing unittest', function(done) {
    runner.run({
      language: 'd',
      solution: [
        `int add(int x, int y) { return x - y; }`,
        `unittest {`,
        `  assert(add(1, 1) == 2);`,
        `}`,
        `void main() {}`,
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.equal('');
      expect(buffer.stderr).to.contain('core.exception.AssertError@code.d(3): unittest failure');
      done();
    });
  });
});
