"use strict";

const expect = require('chai').expect;

const runner = require('../../../runner');

describe('cw-2', function() {
  it('should handle outputting objects', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};',
      fixture: 'Test.expect(false, a);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('{ b: 2 }');
      expect(buffer.stdout).to.contain('<FAILED::>');
      done();
    });
  });
  it('should handle outputting objects with 0.10.33', function(done) {
    // only 0.10.33 allows us to declare a without var
    runner.run({
      language: 'javascript',
      languageVersion: '0.10.33',
      code: 'a = {b: 2};',
      fixture: 'Test.expect(false, a);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('{ b: 2 }');
      expect(buffer.stdout).to.contain('<FAILED::>');
      done();
    });
  });

  it('should handle a basic assertion', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 1',
      fixture: 'Test.expect(a == 1);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
      done();
    });
  });

  it('should handle comments as fixture', function(done) {
    runner.run({
      language: 'javascript',
      code: 'console.log(42)',
      fixture: '//',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('42\n');
      done();
    });
  });

  it('should handle a basic failed test', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 1',
      fixture: 'Test.expect(a == 2)',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('\n<FAILED::>Value is not what was expected\n');
      done();
    });
  });

  it('should handle logging objects', function(done) {
    runner.run({
      language: 'javascript',
      code:'console.log({a: 1});',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('{ a: 1 }\n');
      done();
    });
  });

  it('should handled nested describes', function(done) {
    runner.run({
      language: 'javascript',
      code: `var a = 1`,
      fixture: `
                    describe("top", function(){
                        describe("2nd", function(){
                            it("should a", function(){
                                Test.expect(true);
                            });
                        });
                        it("should b", function(){
                            Test.expect(true);
                        });
                    });
                `,
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.include(`<DESCRIBE::>top\n\n<DESCRIBE::>2nd\n\n<IT::>should a\n`);
      done();
    });
  });

  it('should have formatting commands on independent lines', function(done) {
    runner.run({
      language: 'javascript',
      solution: `process.stdout.write('foo');`,
      fixture: 'Test.expect(true);',
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.equal('foo\n<PASSED::>Test Passed\n');
      done();
    });
  });

  describe('projectMode support', function() {
    it('should handle test cases', function(done) {
      runner.run({
        language: 'javascript',
        files: {
          'helper.js': 'module.exports.name = "helper"',
          'spec.js': `
                      var helper = require("./helper.js");
                      var assert = require("chai").assert;
                      describe("test", function(){
                          it("should have a name", function(){
                              assert.equal('helper', helper.name);
                          })
                      });
                  `
        },
        testFramework: 'cw-2'
      }, function(buffer) {
        // console.log(buffer.stdout)
        // console.log(buffer.stderr)
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });
  });

  describe("async handling", function() {
    it('should throw a timeout if code runs too long', function(done) {
      runner.run({
        language: 'javascript',
        code: 'function solution() {}',
        fixture: `
                        describe("test", 2, function(){
                            it("should do something", function(done){
                            });
                        });
                    `,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.include("<ERROR::>`it` function timed out. Function ran longer than 2ms\n\n<COMPLETEDIN::>");
        done();
      });
    });

    it('should render in proper order', function(done) {
      runner.run({
        language: 'javascript',
        code: 'function solution(cb) {setTimeout(() => cb("ok"), 0)}',
        fixture: `
                        describe("test", true, function(){
                            it("should do something", function(done){
                                solution((msg) => {
                                    Test.assertEquals(msg, "ok");
                                    done();
                                });
                                console.log("ran solution");
                            });
                        });
                    `,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.include("<IT::>should do something\nran solution\n\n<PASSED::>Test Passed: Value == \'ok\'\n\n<COMPLETEDIN::>");
        done();
      });
    });
  });

  describe('error handling', function() {
    it('should handle a mix of failures and successes', function(done) {
      runner.run({
        language: 'javascript',
        code:'var a = 1',
        fixture: 'describe("test", function(){\n' +
                            'it("test1", function(){ Test.expect(false) });' +
                            'it("test2", function(){ Test.expect(true)});})',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<FAILED::>Value is not what was expected');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
    it('should gracefully handle custom errors', function(done) {
      runner.run({
        language: 'javascript',
        code:'var a = 1',
        fixture: 'describe("test", function(){\n' +
                            'it("test1", function(){ throw "boom!" });' +
                            'it("test2", function(){ Test.expect(true)});})',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<ERROR::>');
        expect(buffer.stdout).to.contain('boom!');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
    it('should gracefully handle reference errors', function(done) {
      runner.run({
        language: 'javascript',
        code:'var a = 1',
        fixture: 'describe("test", function(){\n' +
                            'it("test1", function(){ b.test() });' +
                            'it("test2", function(){ Test.expect(true)});})',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<ERROR::>');
        expect(buffer.stdout).to.contain('<:LF:>');
        expect(buffer.stdout).to.contain('ReferenceError:');
        expect(buffer.stdout).to.not.contain('[eval]');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
    it('should gracefully top level handle reference errors', function(done) {
      runner.run({
        language: 'javascript',
        code:'b.test()',
        fixture: 'describe("test", function(){\n' +
                            'it("test2", function(){ Test.expect(true)});})',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<ERROR::>');
        expect(buffer.stdout).to.contain('<:LF:>');
        expect(buffer.stdout).to.contain('ReferenceError:');
        expect(buffer.stdout).to.not.contain('[eval]');
        expect(buffer.stdout).to.not.contain('Object.Test.handleError');
        done();
      });
    });
  });

  it('should support options files', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var name = require("/workspace/config.js").name',
      fixture: `
                    var expect = require('chai').expect;
                    describe("files", function(){
                        it("should be able to require the file", function(){
                            expect(name).to.equal("example");
                        });
                    });
                `,
      files: {"config.js": "module.exports.name = 'example';"},
      testFramework: 'cw-2'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });

  describe('Fix #259', function() {
    it('should prevent global reassignment cheat', function(done) {
      runner.run({
        language: 'javascript',
        languageVersion: '6.6.0',
        code: `
                    global = Object.assign({}, global);
                    global.Test = Object.assign({}, Test);
                    var od = global.Test.describe;
                    global.Test.describe = function() {
                      return od("Fake test suite", function() {
                        Test.it('fake test', function() {
                          Test.expect(true);
                        });
                      });
                    };
                    `,
        fixture: `Test.describe('Fail', function() { Test.it('should fail', function() { Test.expect(false); }); });`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.not.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<ERROR::>');
        done();
      });
    });
  });

  describe('Test.randomNumber()', function() {
    it('should generate random integer in range [0, 100]', function(done) {
      runner.run({
        language: 'javascript',
        languageVersion: '6.6.0',
        code: 'const a = Array.from({length: 1000}, _ => Test.randomNumber());',
        fixture: 'Test.expect(a.every(x => Number.isInteger(x) && x >= 0 && x <= 100));',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
        done();
      });
    });
    it('should be uniformly distributed', function(done) {
      runner.run({
        language: 'javascript',
        languageVersion: '6.6.0',
        code: 'const a = Array.from({length: 101}, _ => 0); for (let i = 0; i < 1e7; ++i) ++a[Test.randomNumber()];',
        fixture: 'Test.expect(a.every(x => Math.abs(1 - 100*x/1e7) <= 0.2));',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
        done();
      });
    });
  });
});
