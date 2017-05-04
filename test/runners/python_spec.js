var expect = require('chai').expect;
var runner = require('../runner');

describe('python runner', function() {
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
          expect(buffer.stdout).to.contain('\n<ERROR::>Unhandled Exception: exceptions are my favorite, I always throw them\n');
          done();
        });
      });
    });
  });
});

describe('Output format commands', function() {
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
