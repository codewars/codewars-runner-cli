const expect = require('chai').expect;
const runner = require('../runner');
const exec = require('child_process').exec;

describe('.run', function() {
  afterEach(function cleanup(done) {
    exec([
      'rm -rf',
      '/home/codewarrior/*.py',
      '/home/codewarrior/__pycache__',
      '/home/codewarrior/test',
    ].join(' '), function(err) {
      if (err) return done(err);
      return done();
    });
  });

  it('should handle basic code evaluation', function(done) {
    runner.run({
      language: 'python-poc',
      code: 'print(42)'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
      done();
    });
  });

  it('should handle setup file', function(done) {
    runner.run({
      language: 'python-poc',
      setup: [
        'def add(a, b): return a + b',
      ].join('\n'),
      code: [
        'import setup',
        'print(setup.add(21, 21))'
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
      done();
    });
  });

  it('should handle setup file', function(done) {
    runner.run({
      language: 'python-poc',
      setup: [
        '# name: foo',
        'def add(a, b): return a + b',
      ].join('\n'),
      code: [
        'import foo',
        'print(foo.add(21, 21))'
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
      done();
    });
  });


  it('stderr', function(done) {
    runner.run({
      language: 'python-poc',
      code: 'import sys; sys.stderr.write("Error!  Codewars cannot and will not accept any more Fibonacci kata.\\n")'
    }, function(buffer) {
      expect(buffer.stderr).to.equal("Error!  Codewars cannot and will not accept any more Fibonacci kata.\n");
      done();
    });
  });

  it('stderr', function(done) {
    runner.run({
      language: 'python-poc',
      code: 'import sys; sys.stderr.write("florp\\n"); sys.stdout.write("foop\\n")'
    }, function(buffer) {
      expect(buffer.stderr).to.equal("florp\n");
      expect(buffer.stdout).to.equal("foop\n");
      done();
    });
  });
});


describe('unittest', function() {
  afterEach(function cleanup(done) {
    exec([
      'rm -rf',
      '/home/codewarrior/*.py',
      '/home/codewarrior/__pycache__',
      '/home/codewarrior/test',
    ].join(' '), function(err) {
      if (err) return done(err);
      return done();
    });
  });

  it('should handle a basic assertion', function(done) {
    runner.run({
      language: 'python-poc',
      testFramework: 'unittest',
      solution: [
        'def add(a, b): return a + b'
      ].join('\n'),
      fixture: [
        'import unittest',
        'import solution',
        'class TestAddition(unittest.TestCase):',
        '  def test_add(self):',
        '    self.assertEqual(solution.add(1, 1), 2)'
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<PASSED::>');
      done();
    });
  });

  it('should handle a failed assetion', function(done) {
    runner.run({
      language: 'python-poc',
      testFramework: 'unittest',
      solution: [
        'def add(a, b): return a - b'
      ].join('\n'),
      fixture: [
        'import unittest',
        'import solution',
        '',
        'class TestAddition(unittest.TestCase):',
        '  def test_add(self):',
        '    self.assertEqual(solution.add(1, 1), 2)',
        ''
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<FAILED::>');
      done();
    });
  });

  it('should handle error', function(done) {
    runner.run({
      language: 'python-poc',
      testFramework: 'unittest',
      solution: [
        'def add(a, b): return a / b'
      ].join('\n'),
      fixture: [
        'import unittest',
        'import solution',
        '',
        'class TestAddition(unittest.TestCase):',
        '  def test_add(self):',
        '    self.assertEqual(solution.add(1, 0), 1)',
        ''
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<ERROR::>');
      done();
    });
  });
});
