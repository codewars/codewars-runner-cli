"use strict";

const expect = require('chai').expect;

const runner = require('../../runner');

describe('language versions', function() {
  ['0.10.x', '6.x', '8.x'].forEach(function(version) {
    it('should run for version ' + version, function(done) {
      runner.run({
        language: 'javascript',
        languageVersion: version,
        code: 'console.log(42)'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });
    it('should run for version ' + version + '/babel', function(done) {
      runner.run({
        language: 'javascript',
        languageVersion: version + '/babel',
        code: 'console.log(42)'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });

    it('should run mocha in using Node ' + version, function(done) {
      let v = 'v' + version.replace(/\.x/, '').replace(/\./, '\\.');
      runner.run({
        language: 'javascript',
        languageVersion: version,
        code: 'var a = process.version;',
        fixture: `var assert = require("chai").assert;
describe("test", function(){
it("should be the right version", function(){
    assert.match(a, /^${v}/);
});
});`,
        testFramework: 'mocha_bdd',
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });
  });

  it('should handle Node8 features', function(done) {
    runner.run({
      language: 'javascript',
      languageVersion: '8.x',
      code: `console.log(typeof require('util').promisify)`,
    }, function(buffer) {
      expect(buffer.stdout).to.equal('function\n');
      done();
    });
  });
});
