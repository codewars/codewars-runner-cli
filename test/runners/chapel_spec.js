var expect = require('chai').expect;
var runner = require('../runner');

describe('chapel runner', function() {
  describe('run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'chapel',
        code: 'write("42");'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42');
        done();
      });
    });
  });
  describe('cw-2', function() {
    it('should handle a basic assertion', function(done) {
      runner.run({
        language: 'chapel',
        code: 'var a: int = 1;',
        fixture: 'Test.expect(a == 1);',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
        done();
      });
    });
    it('should handle a basic assert_equals', function(done) {
      runner.run({
        language: 'chapel',
        code: 'var a: int = 1;',
        fixture: 'Test.assertEquals(a, 1);',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
        done();
      });
    });
    it('should handle a basic setup', function(done) {
      runner.run({
        language: 'chapel',
        code: 'var a: int = 1;',
        setup: 'var b: int = 2;',
        fixture: 'Test.assertEquals(b, 2);',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
        done();
      });
    });
    it('should handle a failed assertion', function(done) {
      runner.run({
        language: 'chapel',
        code: 'var a: int = 1;',
        fixture: 'Test.expect(a == 2);',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('\n<FAILED::>Value was not what was expected\n');
        done();
      });
    });
  });
});

describe('Output format commands', function() {
  it(`should be on independent lines (Chapel-1.15.0 cw-2)`, function(done) {
    runner.run({
      language: 'chapel',
      testFramework: 'cw-2',
      code: 'var a: int = 1;',
      fixture: [
        `writeln('foo');`,
        `Test.assertEquals(a, 2);`,
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).to.include('\n<FAILED::>');
      done();
    });
  });
});
