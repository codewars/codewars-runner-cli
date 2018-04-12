var expect = require('chai').expect;
var runner = require('../runner');
const exec = require('child_process').exec;

describe('python runner', function() {
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

  // These specs are compatible with both Python 2 and 3
  ['2', '3', '3.6'].forEach(lv => {
    describe('.run', function() {
      runner.assertCodeExamples('python', lv);

      it('should handle basic code evaluation', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'import sys; sys.stdout.write("42")'
        }, function(buffer) {
          expect(buffer.stdout).to.equal('42');
          done();
        });
      });
      it('stderr', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'import sys; sys.stderr.write("Error!  Codewars cannot and will not accept any more Fibonacci kata.")'
        }, function(buffer) {
          expect(buffer.stderr).to.equal("Error!  Codewars cannot and will not accept any more Fibonacci kata.");
          done();
        });
      });
      it('stderr', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'import sys; sys.stderr.write("florp"); sys.stdout.write("foop")'
        }, function(buffer) {
          expect(buffer.stderr).to.equal("florp");
          expect(buffer.stdout).to.equal("foop");
          done();
        });
      });
    });
    describe('cw-2', function() {
      it('should handle a basic assertion', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'a = 1',
          fixture: 'test.expect(a == 1)',
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
          done();
        });
      });
      it('should handle a basic assert_equals', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'a = 1',
          fixture: 'test.assert_equals(a, 1)',
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
          done();
        });
      });
      it('should handle a basic setup', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'a = 1',
          setup: 'b = 2',
          fixture: 'test.assert_equals(b, 2)',
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
          done();
        });
      });
      it('should handle a failed assertion', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'a = 1',
          fixture: 'test.expect(a == 2)',
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stdout).to.equal('\n<FAILED::>Value is not what was expected\n');
          done();
        });
      });

      it('should handle a failed assertion', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'a.fail()',
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stderr).to.not.contain('File ');
          expect(buffer.stderr).to.not.contain(', line ');
          expect(buffer.stderr).to.not.contain('most recent call last');
          done();
        });
      });

      it('should support project mode', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          testFramework: 'cw-2',
          files: {'spec.py': 'Test.expect(True)'}
        }, function(buffer) {
          expect(buffer.stdout).to.include('\n<PASSED::>');
          done();
        });
      });
    });
    describe('unittest', function() {
      it('should handle a basic assertion', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'a = 1',
          fixture: [
            'class Test(unittest.TestCase):',
            '  def test_assert(self):',
            '    self.assertEqual(a, 1)'
          ].join('\n'),
          testFramework: 'unittest'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<PASSED::>Test Passed\n');
          done();
        });
      });
      it('should include test names', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'a = 1',
          fixture: [
            'class Test(unittest.TestCase):',
            '  def test_assert(self):',
            '    self.assertEqual(a, 1)'
          ].join('\n'),
          testFramework: 'unittest'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<IT::>test_assert');
          done();
        });
      });
      it('should handle a failed assetion', function(done) {
        runner.run({
          language: 'python',
          languageVersion: lv,
          code: 'a = 1',
          fixture: [
            'class Test(unittest.TestCase):',
            '  def test_assert(self):',
            '    self.assertEqual(a, 2, "test failed")'
          ].join('\n'),
          testFramework: 'unittest'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<FAILED::>');
          expect(buffer.stdout).to.contain('test failed');
          done();
        });
      });
      it('should handle a failed assetion', function(done) {
        runner.run({
          language: 'python',
          code: 'a = 1',
          languageVersion: lv,
          fixture: [
            'class Test(unittest.TestCase):',
            '  def test_assert(self):',
            '    raise Exception("exceptions are my favorite, I always throw them")'
          ].join('\n'),
          testFramework: 'unittest'
        }, function(buffer) {
          if (lv.startsWith('3')) {
            expect(buffer.stdout).to.contain('\n<ERROR::>Unhandled Exception');
          }
          else {
            expect(buffer.stdout).to.contain('\n<ERROR::>Unhandled Exception: exceptions are my favorite, I always throw them\n');
          }
          done();
        });
      });
    });
  });
});

describe('Output format commands', function() {
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

  for (const v of ['2', '3', '3.6']) {
    it(`should be on independent lines (Python${v} cw-2)`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'cw-2',
        code: 'a = 1',
        fixture: [
          `import sys`,
          `sys.stdout.write('foo')`,
          `test.assert_equals(a, 2)`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('\n<FAILED::>');
        done();
      });
    });

    it(`should be on independent lines (Python${v} unittest)`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'unittest',
        code: 'a = 1',
        fixture: [
          `import sys`,
          `class Test(unittest.TestCase):`,
          `  def test_assert(self):`,
          `    sys.stdout.write('foo')`,
          `    self.assertEqual(a, 2)`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('\n<FAILED::>');
        done();
      });
    });
  }
});

describe('Fixed and new features', function() {
  for (const v of ['2', '3', '3.6']) {
    it(`should not block execution on failed test by default (Python${v} cw-2)`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'cw-2',
        code: 'a = 1',
        fixture: [
          `test.assert_equals(a, 2)`,
          `test.assert_equals(a, 1)`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('\n<PASSED::>');
        done();
      });
    });

    it(`should support legacy style describe (Python${v} cw-2)`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'cw-2',
        code: 'a = 1',
        fixture: [
          `test.describe('describe')`,
          `test.assert_equals(a, 1)`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('\n<DESCRIBE::>describe');
        expect(buffer.stdout).to.include('\n<PASSED::>');
        done();
      });
    });

    it(`should support new style describe (Python${v} cw-2)`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'cw-2',
        code: 'a = 1',
        fixture: [
          `@test.describe('describe')`,
          `def describe1():`,
          `  test.assert_equals(a, 1)`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('\n<DESCRIBE::>describe');
        expect(buffer.stdout).to.include('\n<PASSED::>');
        expect(buffer.stdout).to.include('\n<COMPLETEDIN::>');
        done();
      });
    });

    it(`should support timeout (passing) (Python${v} cw-2)`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'cw-2',
        code: 'a = 1',
        fixture: [
          `@test.describe('describe')`,
          `def describe1():`,
          `  @test.timeout(0.01)`,
          `  def dummy(): test.pass_()`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('\n<DESCRIBE::>describe');
        expect(buffer.stdout).to.include('\n<PASSED::>');
        expect(buffer.stdout).to.include('\n<COMPLETEDIN::>');
        done();
      });
    });

    it(`should support timeout (failing) (Python${v} cw-2)`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'cw-2',
        code: 'a = 1',
        fixture: [
          `@test.describe('describe')`,
          `def describe1():`,
          `  @test.timeout(0.01)`,
          `  def count():`,
          `    x = 0`,
          `    while x < 10 ** 9: x += 1`,
          `    test.pass_()`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('\n<DESCRIBE::>describe');
        expect(buffer.stdout).to.include('\n<FAILED::>Exceeded time limit');
        expect(buffer.stdout).to.include('\n<COMPLETEDIN::>');
        done();
      });
    });

    it(`should support unicode output (log) (Python${v} cw-2)`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'cw-2',
        code: 'a = 1',
        fixture: 'test.uni_print(1, "a", u"\\uac00", [314159, "b", u"\\uac01"])',
      }, function(buffer) {
        expect(buffer.stdout).to.include('1 a &#44032;');
        expect(buffer.stdout).to.include('314159');
        expect(buffer.stdout).to.include('b');
        expect(buffer.stdout).to.include('&#44033;');
        done();
      });
    });

    it(`should support unicode output (test output) (Python${v} cw-2)`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'cw-2',
        code: 'a = 1',
        fixture: 'test.assert_equals(u"\\uac00", "")',
      }, function(buffer) {
        expect(buffer.stdout).to.include('\\uac00');
        done();
      });
    });
  }
});

describe('unittest no concat', function() {
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

  for (const v of ['3.x', '3.6']) {
    it(`should handle a basic assertion (${v})`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'unittest',
        solution: [
          'def add(a, b): return a + b'
        ].join('\n'),
        fixture: [
          // Test class name is no longer restricted.
          'class TestAddition(unittest.TestCase):',
          '  def test_add(self):',
          '    self.assertEqual(add(1, 1), 2)'
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('\n<PASSED::>');
        done();
      });
    });
    it(`should handle a failed assetion (${v})`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'unittest',
        solution: [
          'def add(a, b): return a - b'
        ].join('\n'),
        fixture: [
          'class TestAddition(unittest.TestCase):',
          '  def test_add(self):',
          '    self.assertEqual(add(1, 1), 2)',
          ''
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('\n<FAILED::>');
        done();
      });
    });
    it(`syntax error in solution show line numbers (${v})`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'unittest',
        solution: [
          'def add(a, b): return a b'
        ].join('\n'),
        fixture: [
          'class TestAddition(unittest.TestCase):',
          '  def test_add(self):',
          '    self.assertEqual(add(1, 1), 2)',
          ''
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('\n<ERROR::>');
        expect(buffer.stdout).to.contain('solution.py", line 1');
        expect(buffer.stdout).to.contain('SyntaxError');
        done();
      });
    });
    it(`should handle error (${v})`, function(done) {
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'unittest',
        solution: [
          'def add(a, b): return a / b'
        ].join('\n'),
        fixture: [
          'class TestAddition(unittest.TestCase):',
          '  def test_add(self):',
          '    self.assertEqual(add(1, 0), 1)',
          ''
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('\n<ERROR::>');
        done();
      });
    });
    it(`should handle tests in multiple suites (${v})`, function(done) {
      // combined into one suite
      runner.run({
        language: 'python',
        languageVersion: v,
        testFramework: 'unittest',
        solution: [
          'def add(a, b): return a + b'
        ].join('\n'),
        fixture: [
          'class TestAddition1(unittest.TestCase):',
          '  def test_add1(self):',
          '    self.assertEqual(add(1, 1), 2)',
          '',
          'class TestAddition2(unittest.TestCase):',
          '  def test_add2(self):',
          '    self.assertEqual(add(2, 2), 4)',
          '',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('\n<IT::>test_add1');
        expect(buffer.stdout).to.contain('\n<IT::>test_add2');
        done();
      });
    });
  }
});
