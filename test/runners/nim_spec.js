const expect = require('chai').expect;
const runner = require('../runner');
const exec = require('child_process').exec;

describe('nim runner', function() {
  afterEach(function cleanup(done) {
    exec([
      'rm -rf',
      '/home/codewarrior/nimcache',
      '/home/codewarrior/solution',
      '/home/codewarrior/fixture',
      '/home/codewarrior/solution.txt',
      '/home/codewarrior/solution.nim',
      '/home/codewarrior/fixture.nim',
    ].join(' '), function(err) {
      if (err) return done(err);
      done();
    });
  });

  it('should compile and run Nim code', function(done) {
    runner.run({
      language: 'nim',
      solution: 'echo "42"'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
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
        'import unittest, codewars/formatter',
        'import solution',
        'addOutputFormatter(OutputFormatter(newCodewarsOutputFormatter()))',
        '',
        'suite "add":',
        '  test "1 + 1 = 2":',
        '    check(add(1, 1) == 2)',
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
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
        'import unittest, codewars/formatter',
        'import solution',
        'addOutputFormatter(OutputFormatter(newCodewarsOutputFormatter()))',
        '',
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
        'import unittest, codewars/formatter',
        'import solution',
        'addOutputFormatter(OutputFormatter(newCodewarsOutputFormatter()))',
        '',
        'suite "add/sub":',
        '  test "1 + 1 = 2":',
        '    check(add(1, 1) == 2)',
        '  test "1 - 1 = 0":',
        '    check(sub(1, 1) == 0)',
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
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
        'import unittest, codewars/formatter',
        'import solution',
        'addOutputFormatter(OutputFormatter(newCodewarsOutputFormatter()))',
        '',
        'suite "add":',
        '  test "1 + x":',
        '    check:',
        '      add(1, 1) == 2',
        '      add(1, 2) == 3',
        '      add(1, 3) == 4',
        '      add(1, 4) == 5',
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
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
        'import unittest, codewars/formatter',
        'import solution',
        'addOutputFormatter(OutputFormatter(newCodewarsOutputFormatter()))',
        '',
        'suite "expect":',
        '  test "out of bounds error is thrown on bad access":',
        '    expect(IndexError):',
        '      discard v[4]',
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });
});

describe('Examples', function() {
  afterEach(function cleanup(done) {
    exec([
      'rm -rf',
      '/home/codewarrior/nimcache',
      '/home/codewarrior/solution',
      '/home/codewarrior/fixture',
      '/home/codewarrior/solution.txt',
      '/home/codewarrior/solution.nim',
      '/home/codewarrior/fixture.nim',
    ].join(' '), function(err) {
      if (err) return done(err);
      done();
    });
  });
  runner.assertCodeExamples('nim');
});

