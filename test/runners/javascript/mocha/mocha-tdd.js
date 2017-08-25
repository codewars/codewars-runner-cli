"use strict";

const expect = require('chai').expect;

const runner = require('../../../runner');

describe('mocha tdd', function() {
  it('should handle outputting objects', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};console.log(this);',
      fixture: 'var assert = require("chai").assert;suite("test", function(){test("should be 2", function(){assert.equal(2, a.b);})});',
      testFramework: 'mocha_tdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });
  it('should handle failures', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};',
      fixture: 'var assert = require("assert"); suite("test", function(){suite("failures", function(){test("should be 1", function(){assert.equal(1, a.b);})})});',
      testFramework: 'mocha_tdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<FAILED::>');
      done();
    });
  });
  it('should handle chai failures', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};',
      fixture: 'var assert = require("chai").assert; suite("test", function(){suite("failures", function(){test("should be 1", function(){assert.equal(1, a.b);})})});',
      testFramework: 'mocha_tdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<FAILED::>');
      done();
    });
  });
  it('should handle errors', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};',
      fixture: 'suite("test", function(){suite("failures", function(){test("should be 1", function(){throw new Error("test error");})})});',
      testFramework: 'mocha_tdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<ERROR::>');
      done();
    });
  });

  it('should have formatting commands on independent lines', function(done) {
    runner.run({
      language: 'javascript',
      solution: `//`,
      fixture: [
        `const assert = require('chai').assert;`,
        `suite("tests", function() {`,
        `  test("test", function() {`,
        `    process.stdout.write('foo');`,
        `    assert.equal(1, 1);`,
        `  });`,
        `});`,
      ].join('\n'),
      testFramework: 'mocha_tdd',
    }, function(buffer) {
      expect(buffer.stdout).to.contain('foo\n<PASSED::>Passed\n');
      done();
    });
  });
});
