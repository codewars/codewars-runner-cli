"use strict";

const expect = require('chai').expect;

const runner = require('../../../runner');

describe('karma bdd', function() {
  it('warmup test', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};',
      fixture: 'describe("test", function(){it("should be 2", function(){assert.equal(2, a.b);})});',
      testFramework: 'karma_bdd'
    }, function() {
      done();
    });
  });
  it('should handle basic tests', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};',
      fixture: 'describe("test", function(){it("should be 2", function(){assert.equal(2, a.b);})});',
      testFramework: 'karma_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });
  it('should handle ES6 transformations', function(done) {
    runner.run({
      language: 'javascript',
      code: `\
const a = [1, 2, 3];
let b = [];
for(let v of a) b.push(v + 1);
let symbolTest = Symbol();
`,
      fixture: `\
describe("test", () => {
  it("should be have incremented the values", () => {
    assert.deepEqual(b, [2,3,4]);
  });
  it("should handle symbols", () => {
    assert.equal(typeof symbolTest, 'symbol');
  });
});
`,
      testFramework: 'karma_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      expect(buffer.stdout).to.not.contain('<FAILED:>');
      done();
    });
  });
  it('should handle loading Angular', function(done) {
    runner.run({
      language: 'javascript',
      setup: '// @include-external angular@1.5',
      code: `
angular.module('testModule', [])
.factory('testService', function() {
    return {
            double(input) {
                return input * 2;
            }
    };
});`,
      fixture: `
describe("test", function(){
beforeEach(module('testModule'));
it("should multiply", inject(function(testService) {
    expect(testService).to.be.ok;
    expect(testService.double(2)).to.eql(4);
}));
});`,
      testFramework: 'karma_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<IT::>should multiply');
      expect(buffer.stdout).to.contain('\n<PASSED::>');
      done();
    });
  });

  it('should handle failures', function(done) {
    runner.run({
      language: 'javascript',
      setup: '// @include-external angular@1.5',
      code: `
angular.module('testModule', [])
.factory('testService', function() {
    return {
            double(input) {
                return input * 3;
            }
    };
});`,
      fixture: `
describe("test", function(){
beforeEach(module('testModule'));
it("should multiply", inject(function(testService) {
    expect(testService.double(2)).to.eql(4);
}));
});`,
      testFramework: 'karma_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<IT::>should multiply');
      expect(buffer.stdout).to.contain('\n<FAILED::>');
      done();
    });
  });

  it('should handle code errors', function(done) {
    runner.run({
      language: 'javascript',
      setup: '// @include-external angular@1.5',
      code: `
angular.module('testModule', [])
.factory('testService', function() {
    return {
            double(input) {
                return input2 * 2;
            }
    };
});`,
      fixture: `
describe("test", function(){
beforeEach(module('testModule'));
it("should multiply", inject(function(testService) {
    expect(testService.double(2)).to.eql(4);
}));
});`,
      testFramework: 'karma_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<FAILED::>');
      done();
    });
  });

  it('should handle test errors', function(done) {
    runner.run({
      language: 'javascript',
      setup: '// @include-external angular@1.5',
      code: `
angular.module('testModule', [])
.factory('testService', function() {
    return {
            double(input) {
                return input * 2;
            }
    };
});`,
      fixture: `
describe("test", function(){
beforeEach(module('testModule'));
it("should multiply", inject(function(testService) {
    expect(testService.double(2)).to.eql(4);
    expect(testServie.double(3)).to.eql(6);
}));
});`,
      testFramework: 'karma_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<FAILED::>');
      done();
    });
  });

  it('should handle projectMode', function(done) {
    runner.run({
      language: 'javascript',
      files: {
        '.runner/config.json': '{}',
        '.runner/setup.sh': 'echo 123',
        'test.css': '.a {font-weight: bold}',
        'main.js': 'var a = {b: 2};',
        'spec.js': 'describe("test", function(){it("should be 2", function(){assert.equal(2, a.b);})});'
      },
      testFramework: 'karma_bdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });
});
