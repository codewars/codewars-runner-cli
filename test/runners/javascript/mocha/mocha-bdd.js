"use strict";

const expect = require('chai').expect;

const runner = require('../../../runner');

describe('mocha bdd', function() {
  it('should handle outputting objects', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};console.log(this);',
      fixture: 'var assert = require("chai").assert;describe("test", function(){it("should be 2", function(){assert.equal(2, a.b);})});',
      testFramework: 'mocha_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });
  it('should handle no trailing semicolons', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = 2',
      fixture: 'var assert = require("chai").assert;describe("test", function(){it("should be 2", function(){assert.equal(2, a);})});',
      testFramework: 'mocha_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });
  it('should handle failures', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};',
      fixture: 'var assert = require("chai").assert;describe("test", function(){describe("failures", function(){it("should be 1", function(){assert.equal(1, a.b);})})});',
      testFramework: 'mocha_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<FAILED::>');
      done();
    });
  });

  it('should handle errors', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};',
      fixture: 'describe("test", function(){describe("failures", function(){it("should be 1", function(){throw new Error("test error");})})});',
      testFramework: 'mocha_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<ERROR::>');
      done();
    });
  });

  it('should support options files', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var name = require("./config.js").name',
      fixture: `
                    var expect = require('chai').expect;
                    describe("files", function(){
                        it("should be able to require the file", function(){
                            expect(name).to.equal("example");
                        });
                    });
                `,
      files: {"config.js": "module.exports.name = 'example';"},
      testFramework: 'mocha_bdd'
    }, function(buffer) {
      console.log(buffer.stdout);
      console.log(buffer.stderr);
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });

  it('should load chai-display', function(done) {
    runner.run({
      language: 'javascript',
      code: 'global.name = "example";',
      fixture: `
                    var expect = require('chai').expect;
                    require("chai-display");
                    describe("files", function(){
                        it("should be able to require the library", function(){
                            expect(global.name).to.equal("example");
                        });
                    });
                `,
      testFramework: 'mocha_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });

  describe('projectMode support', function() {
    it('should handle tests', function(done) {
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
        testFramework: 'mocha_bdd'
      }, function(buffer) {
        // console.log(buffer.stdout)
        // console.log(buffer.stderr)
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });
  });

  it('should have formatting commands on independent lines', function(done) {
    runner.run({
      language: 'javascript',
      solution: `//`,
      fixture: [
        `const assert = require('chai').assert;`,
        `describe("tests", function() {`,
        `  it("test", function() {`,
        `    process.stdout.write('foo');`,
        `    assert.equal(1, 1);`,
        `  });`,
        `});`,
      ].join('\n'),
      testFramework: 'mocha_bdd',
    }, function(buffer) {
      expect(buffer.stdout).to.contain('foo\n<PASSED::>Passed\n');
      done();
    });
  });
});
