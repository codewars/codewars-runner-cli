var expect = require('chai').expect;
var runner = require('../runner');

describe('erlang runner', function() {
  describe('.run', function() {
    it('should handle setup code and imports', function(done) {
      runner.run({
        language: 'erlang',
        setup: [
          '-module(foo).',
          '-export([bar/0]).',
          'bar() -> io:format("baz").'
        ].join('\n'),
        code: [
          'foo:bar(), init:stop().'
        ].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.equal('baz');
        done();
      });
    });
    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'erlang',
        code: 'io:fwrite("42\n"), init:stop().'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });
    it('should be running the most recent erlang version', function(done) {
      runner.run({
        language: 'erlang',
        code: 'io:fwrite(erlang:system_info(otp_release)), init:stop().'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('18');
        done();
      });
    });
  });
  describe('codewars test framework (eunit)', function() {
    it('should be able to run a basic test', function(done) {
      runner.run({
        language: 'erlang',
        code: [
          '-module(solution).',
          '-export([foo/0]).',
          'foo() -> "bar".',
        ].join('\n'),
        fixture: [
          'foo_test() -> "bar" = solution:foo().'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer.stderr);
        expect(buffer.stdout).to.contain('Test passed.');
        done();
      });
    });
  });
});
