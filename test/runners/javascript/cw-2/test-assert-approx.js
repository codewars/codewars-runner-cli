"use strict";

const expect = require('chai').expect;

const runner = require('../../../runner');

describe('Test.assertApproxEquals', function() {
  it("should allow for minor floating point errors and compare them as equal", function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 2.00000000004',
      fixture: 'Test.assertApproxEquals(a, 2);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
      done();
    });
  });
  it("should allow for minor floating point errors and compare them as equal (2)", function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 1.99999999996',
      fixture: 'Test.assertApproxEquals(a, 2);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
      done();
    });
  });
  it("should handle 0 properly and not throw DivisionByZeroError", function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 0.00000000009',
      fixture: 'Test.assertApproxEquals(a, 0);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
      done();
    });
  });
  it("should handle 0 properly and not throw DivisionByZeroError (2)", function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = -0.00000000009',
      fixture: 'Test.assertApproxEquals(a, 0);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
      done();
    });
  });
  it("should fail tests where the relative error is greater than 1e-9", function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 3.004',
      fixture: 'Test.assertApproxEquals(a, 3);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<FAILED::>');
      expect(buffer.stdout).to.contain('\n');
      done();
    });
  });
  it("should fail tests where the relative error is greater than 1e-9 (2)", function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 2.996',
      fixture: 'Test.assertApproxEquals(a, 3);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<FAILED::>');
      expect(buffer.stdout).to.contain('\n');
      done();
    });
  });
});

describe('Test.assertNotApproxEquals', function() {
  it('should pass tests where the two values are outside the rejected relative error', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 2.004',
      fixture: 'Test.assertNotApproxEquals(a, 2);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
      done();
    });
  });
  it('should pass tests where the two values are outside the rejected relative error (2)', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 1.996',
      fixture: 'Test.assertNotApproxEquals(a, 2);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
      done();
    });
  });
  it('should handle 0 properly and not throw DivisionByZeroError', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = -0.009',
      fixture: 'Test.assertNotApproxEquals(a, 0);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
      done();
    });
  });
  it('should handle 0 properly and not throw DivisionByZeroError (2)', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 0.009',
      fixture: 'Test.assertNotApproxEquals(a, 0);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
      done();
    });
  });
  it('should fail a test where the two floats are within the rejected relative error', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 3.00000000004',
      fixture: 'Test.assertNotApproxEquals(a, 3);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<FAILED::>');
      expect(buffer.stdout).to.contain('\n');
      done();
    });
  });
  it('should fail a test where the two floats are within the rejected relative error (2)', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 2.99999999996',
      fixture: 'Test.assertNotApproxEquals(a, 3);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<FAILED::>');
      expect(buffer.stdout).to.contain('\n');
      done();
    });
  });
});

describe('Fix Codewars/codewars.com#962', function() {
  describe('Test.assertApproxEquals', function() {
    it('should allow for an absolute error range of 1e-9 when the actual value returned is 0', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = 0',
        fixture: `
Test.assertApproxEquals(a, 1e-15);
Test.assertApproxEquals(a, 1e-14);
Test.assertApproxEquals(a, 1e-13);
Test.assertApproxEquals(a, 1e-12);
Test.assertApproxEquals(a, 1e-11);
Test.assertApproxEquals(a, 1e-10);
Test.assertApproxEquals(a, 1e-9);
Test.assertApproxEquals(a, -1e-9);
Test.assertApproxEquals(a, -1e-10);
Test.assertApproxEquals(a, -1e-11);
Test.assertApproxEquals(a, -1e-12);
Test.assertApproxEquals(a, -1e-13);
Test.assertApproxEquals(a, -1e-14);
Test.assertApproxEquals(a, -1e-15);
`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.not.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should allow for an absolute error range of *no more than* 1e-9 when the actual value returned is 0', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = 0',
        fixture: `
Test.assertApproxEquals(a, 1e-8);
Test.assertApproxEquals(a, 1e-7);
Test.assertApproxEquals(a, 1e-6);
Test.assertApproxEquals(a, 1e-5);
Test.assertApproxEquals(a, 1e-4);
Test.assertApproxEquals(a, 1e-3);
Test.assertApproxEquals(a, 1e-2);
Test.assertApproxEquals(a, 1e-1);
Test.assertApproxEquals(a, 1);
Test.assertApproxEquals(a, -1);
Test.assertApproxEquals(a, -1e-1);
Test.assertApproxEquals(a, -1e-2);
Test.assertApproxEquals(a, -1e-3);
Test.assertApproxEquals(a, -1e-4);
Test.assertApproxEquals(a, -1e-5);
Test.assertApproxEquals(a, -1e-6);
Test.assertApproxEquals(a, -1e-7);
Test.assertApproxEquals(a, -1e-8);
`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.not.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should treat 1e-9 as an absolute error margin and not a relative one when Math.abs(expected) <= 1 (passing tests only)', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = [1e-8 - 1e-10, 1e-8 - 1e-11, 1e-8 - 1e-12, 1e-8 - 1e-13, 1e-8 - 1e-14, 1e-8 - 1e-15, 1e-8 + 1e-15, 1e-8 + 1e-14, 1e-8 + 1e-13, 1e-8 + 1e-12, 1e-8 + 1e-11, 1e-8 + 1e-10], b = a.map(n => -n)',
        fixture: 'for (let i = 0; i < a.length; i++) { Test.assertApproxEquals(a[i], 1e-8); Test.assertApproxEquals(b[i], -1e-8); }',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.not.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should treat 1e-9 as an absolute error margin and not a relative one when Math.abs(expected) <= 1 (failing tests only)', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = [1e-8 - 1e-8, 1e-8 - 1e-7, 1e-8 - 1e-6, 1e-8 - 1e-5, 1e-8 - 1e-4, 1e-8 - 1e-3, 1e-8 - 1e-2, 1e-8 - 1e-1, 1e-8 + 1e-1, 1e-8 + 1e-2, 1e-8 + 1e-3, 1e-8 + 1e-4, 1e-8 + 1e-5, 1e-8 + 1e-6, 1e-8 + 1e-7, 1e-8 + 1e-8], b = a.map(n => -n)',
        fixture: 'for (let i = 0; i < a.length; i++) { Test.assertApproxEquals(a[i], 1e-8); Test.assertApproxEquals(b[i], -1e-8); }',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.not.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should treat 1e-9 as a relative error margin and not an absolute one when Math.abs(expected) > 1 (passing tests only)', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = [1000000 - 1e-8, 1000000 - 1e-7, 1000000 - 1e-6, 1000000 - 1e-5, 1000000 - 1e-4, 1000000 + 1e-4, 1000000 + 1e-5, 1000000 + 1e-6, 1000000 + 1e-7, 1000000 + 1e-8], b = a.map(n => -n)',
        fixture: 'for (let i = 0; i < a.length; i++) { Test.assertApproxEquals(a[i], 1000000); Test.assertApproxEquals(b[i], -1000000); }',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.not.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should treat 1e-9 as a relative error margin and not an absolute one when Math.abs(expected) > 1 (failing tests only)', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = [1000000 - 1e-2, 1000000 - 1e-1, 1000000 - 1, 1000000 - 10, 1000000 - 100, 1000000 + 100, 1000000 + 10, 1000000 + 1, 1000000 + 1e-1, 1000000 + 1e-2], b = a.map(n => -n)',
        fixture: 'for (let i = 0; i < a.length; i++) { Test.assertApproxEquals(a[i], 1000000); Test.assertApproxEquals(b[i], -1000000); }',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.not.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
  });
  describe('Test.assertNotApproxEquals', function() {
    it('should reject an absolute difference of 1e-9 or less if the actual value is 0', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = 0',
        fixture: `
Test.assertNotApproxEquals(a, 1e-15);
Test.assertNotApproxEquals(a, 1e-14);
Test.assertNotApproxEquals(a, 1e-13);
Test.assertNotApproxEquals(a, 1e-12);
Test.assertNotApproxEquals(a, 1e-11);
Test.assertNotApproxEquals(a, 1e-10);
Test.assertNotApproxEquals(a, 1e-9);
Test.assertNotApproxEquals(a, -1e-9);
Test.assertNotApproxEquals(a, -1e-10);
Test.assertNotApproxEquals(a, -1e-11);
Test.assertNotApproxEquals(a, -1e-12);
Test.assertNotApproxEquals(a, -1e-13);
Test.assertNotApproxEquals(a, -1e-14);
Test.assertNotApproxEquals(a, -1e-15);
`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.not.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should accept an absolute difference greater than 1e-9 if the actual value is 0', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = 0',
        fixture: `
Test.assertNotApproxEquals(a, 1e-8);
Test.assertNotApproxEquals(a, 1e-7);
Test.assertNotApproxEquals(a, 1e-6);
Test.assertNotApproxEquals(a, 1e-5);
Test.assertNotApproxEquals(a, 1e-4);
Test.assertNotApproxEquals(a, 1e-3);
Test.assertNotApproxEquals(a, 1e-2);
Test.assertNotApproxEquals(a, 1e-1);
Test.assertNotApproxEquals(a, 1);
Test.assertNotApproxEquals(a, -1);
Test.assertNotApproxEquals(a, -1e-1);
Test.assertNotApproxEquals(a, -1e-2);
Test.assertNotApproxEquals(a, -1e-3);
Test.assertNotApproxEquals(a, -1e-4);
Test.assertNotApproxEquals(a, -1e-5);
Test.assertNotApproxEquals(a, -1e-6);
Test.assertNotApproxEquals(a, -1e-7);
Test.assertNotApproxEquals(a, -1e-8);
`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.not.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should treat 1e-9 as an absolute error margin and not a relative one when Math.abs(unexpected) <= 1 (failing tests only)', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = [1e-8 - 1e-10, 1e-8 - 1e-11, 1e-8 - 1e-12, 1e-8 - 1e-13, 1e-8 - 1e-14, 1e-8 - 1e-15, 1e-8 + 1e-15, 1e-8 + 1e-14, 1e-8 + 1e-13, 1e-8 + 1e-12, 1e-8 + 1e-11, 1e-8 + 1e-10], b = a.map(n => -n)',
        fixture: 'for (let i = 0; i < a.length; i++) { Test.assertNotApproxEquals(a[i], 1e-8); Test.assertNotApproxEquals(b[i], -1e-8); }',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.not.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should treat 1e-9 as an absolute error margin and not a relative one when Math.abs(unexpected) <= 1 (passing tests only)', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = [1e-8 - 1e-8, 1e-8 - 1e-7, 1e-8 - 1e-6, 1e-8 - 1e-5, 1e-8 - 1e-4, 1e-8 - 1e-3, 1e-8 - 1e-2, 1e-8 - 1e-1, 1e-8 + 1e-1, 1e-8 + 1e-2, 1e-8 + 1e-3, 1e-8 + 1e-4, 1e-8 + 1e-5, 1e-8 + 1e-6, 1e-8 + 1e-7, 1e-8 + 1e-8], b = a.map(n => -n)',
        fixture: 'for (let i = 0; i < a.length; i++) { Test.assertNotApproxEquals(a[i], 1e-8); Test.assertNotApproxEquals(b[i], -1e-8); }',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.not.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should treat 1e-9 as a relative error margin and not an absolute one when Math.abs(unexpected) > 1 (failing tests only)', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = [1000000 - 1e-8, 1000000 - 1e-7, 1000000 - 1e-6, 1000000 - 1e-5, 1000000 - 1e-4, 1000000 + 1e-4, 1000000 + 1e-5, 1000000 + 1e-6, 1000000 + 1e-7, 1000000 + 1e-8], b = a.map(n => -n)',
        fixture: 'for (let i = 0; i < a.length; i++) { Test.assertNotApproxEquals(a[i], 1000000); Test.assertNotApproxEquals(b[i], -1000000); }',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.not.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should treat 1e-9 as a relative error margin and not an absolute one when Math.abs(unexpected) > 1 (passing tests only)', function(done) {
      runner.run({
        language: 'javascript',
        code: 'var a = [1000000 - 1e-2, 1000000 - 1e-1, 1000000 - 1, 1000000 - 10, 1000000 - 100, 1000000 + 100, 1000000 + 10, 1000000 + 1, 1000000 + 1e-1, 1000000 + 1e-2], b = a.map(n => -n)',
        fixture: 'for (let i = 0; i < a.length; i++) { Test.assertNotApproxEquals(a[i], 1000000); Test.assertNotApproxEquals(b[i], -1000000); }',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.not.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
  });
});
