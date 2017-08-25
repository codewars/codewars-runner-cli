"use strict";

const expect = require('chai').expect;

const runner = require('../../../runner');

describe('karma tdd', function() {
  it('should handle basic tests', function(done) {
    runner.run({
      language: 'javascript',
      code: 'var a = {b: 2};',
      fixture: 'suite("test", function(){test("should be 2", function(){assert.equal(2, a.b);})});',
      testFramework: 'karma_tdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
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
suite("test", function(){
setup(module('testModule'));
test("should multiply", inject(function(testService) {
    expect(testService).to.be.ok;
    expect(testService.double(2)).to.eql(4);
}));
});`,
      testFramework: 'karma_tdd'
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
suite("test", function(){
setup(module('testModule'));
test("should multiply", inject(function(testService) {
    expect(testService.double(2)).to.eql(4);
}));
});`,
      testFramework: 'karma_tdd'
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
suite("test", function(){
setup(module('testModule'));
test("should multiply", inject(function(testService) {
    expect(testService.double(2)).to.eql(4);
}));
});`,
      testFramework: 'karma_tdd'
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
suite("test", function(){
setup(module('testModule'));
test("should multiply", inject(function(testService) {
    expect(testService.double(2)).to.eql(4);
    expect(testServie.double(3)).to.eql(6);
}));
});`,
      testFramework: 'karma_tdd'
    }, function(buffer) {
      expect(buffer.stdout).to.contain('\n<FAILED::>');
      done();
    });
  });
});
