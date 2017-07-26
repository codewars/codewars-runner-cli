"use strict";
const expect = require('chai').expect;
const runner = require('../runner');

describe('nim runner', function() {
  it('should compile and run Nim code', function(done) {
    runner.run({
      language: 'nim',
      solution: 'echo "42"'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
      done();
    });
  });

  it('should support setup file', function(done) {
    runner.run({
      language: 'nim',
      setup: 'proc add*(a, b: int): int = a + b',
      solution: [
        `import setup`,
        `echo add(1, 1)`
      ].join('\n')
    }, function(buffer) {
      expect(buffer.stdout).to.equal('2\n');
      done();
    });
  });


  it('should handle basic assertion', function(done) {
    runner.run({
      language: 'nim',
      solution: [
        'proc add*(x, y: int): int = x + y',
      ].join('\n'),
      fixture: [
        'suite "add":',
        '  test "1 + 1 = 2":',
        '    check(add(1, 1) == 2)',
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
      done();
    });
  });

  it('should handle basic assertion failure', function(done) {
    runner.run({
      language: 'nim',
      solution: [
        'proc add*(x, y: int): int = x - y',
      ].join('\n'),
      fixture: [
        'suite "add":',
        '  test "1 + 1 = 2":',
        '    check(add(1, 1) == 2)',
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<FAILED::>');
      done();
    });
  });

  it('should handle mixed success and failure', function(done) {
    runner.run({
      language: 'nim',
      solution: [
        'proc add*(x, y: int): int = x + y',
        'proc sub*(x, y: int): int = x + y',
      ].join('\n'),
      fixture: [
        'suite "add/sub":',
        '  test "1 + 1 = 2":',
        '    check(add(1, 1) == 2)',
        '  test "1 - 1 = 0":',
        '    check(sub(1, 1) == 0)',
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
      expect(buffer.stdout).to.contain('<FAILED::>');
      done();
    });
  });

  it('checks multiple assertions', function(done) {
    runner.run({
      language: 'nim',
      solution: [
        'proc add*(x, y: int): int = x + y',
      ].join('\n'),
      fixture: [
        'suite "add":',
        '  test "1 + x":',
        '    check:',
        '      add(1, 1) == 2',
        '      add(1, 2) == 3',
        '      add(1, 3) == 4',
        '      add(1, 4) == 5',
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
      done();
    });
  });

  it('expect macro ensures given Error is thrown', function(done) {
    runner.run({
      language: 'nim',
      solution: [
        'let v* = @[1, 2, 3]',
      ].join('\n'),
      fixture: [
        'suite "expect":',
        '  test "out of bounds error is thrown on bad access":',
        '    expect(IndexError):',
        '      discard v[4]',
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
      done();
    });
  });

  it('should handle errors', function(done) {
    runner.run({
      language: 'nim',
      solution: [
        'proc foo*(x, y: int): int =',
        '  if x != y:',
        '    raise newException(Exception, "x != y")',
        '  return x + y',
      ].join('\n'),
      fixture: [
        'suite "foo":',
        '  test "err":',
        '    check(foo(1, 0) == 1)',
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<ERROR::>');
      expect(buffer.stdout).to.contain('<LOG:ESC:Error>x != y');
      expect(buffer.stdout).to.contain('<TAB::Traceback>');
      done();
    });
  });
});

describe('Examples', function() {
  runner.assertCodeExamples('nim');
});
