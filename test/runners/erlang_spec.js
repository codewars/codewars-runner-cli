"use strict";

const expect = require('chai').expect;
const runner = require('../runner');
const exec = require('child_process').exec;

describe('Erlang', function() {
  describe('running', function() {
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

    it('should have Erlang/OTP 20', function(done) {
      runner.run({
        language: 'erlang',
        code: 'io:fwrite(erlang:system_info(otp_release)), init:stop().'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('20');
        done();
      });
    });
  });

  describe('testing with EUnit', function() {
    afterEach(function cleanup(done) {
      exec('rm -rf /workspace/erlang/src/*.erl /workspace/erlang/test/*.erl /workspace/erlang/_build/test', function(err) {
        if (err) return done(err);
        done();
      });
    });

    // NOTE a test module with single top-level test case behaves oddly.
    // The test's 'desc' becomes "module 'test_module_name'"
    it('should handle basic assertion (single test case)', function(done) {
      runner.run({
        language: 'erlang',
        testFramework: 'eunit',
        solution: [
          '-module(solution).',
          '-export([add/2]).',
          'add(A, B) -> A + B.',
        ].join('\n'),
        fixture: [
          '-module(basic_tests).',
          '-include_lib("eunit/include/eunit.hrl").',
          'add_test() -> ?assertEqual(2, solution:add(1, 1)).',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain("<IT::>module 'basic_tests'");
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should handle basic assertion', function(done) {
      runner.run({
        language: 'erlang',
        testFramework: 'eunit',
        solution: [
          '-module(solution).',
          '-export([add/2]).',
          'add(A, B) -> A + B.',
        ].join('\n'),
        fixture: [
          '-module(basic_tests).',
          '-include_lib("eunit/include/eunit.hrl").',
          'add_test_() ->',
          '  {"Addition",',
          '    {"add(1, 1)", ?_assertEqual(2, solution:add(1, 1))}}.',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should handle basic assertion failure', function(done) {
      runner.run({
        language: 'erlang',
        testFramework: 'eunit',
        solution: [
          '-module(solution).',
          '-export([add/2]).',
          'add(A, B) -> A - B.',
        ].join('\n'),
        fixture: [
          '-module(basic_tests).',
          '-include_lib("eunit/include/eunit.hrl").',
          'add_test_() ->',
          '  {"Addition",',
          '    {"add(1, 1)", ?_assertEqual(2, solution:add(1, 1))}}.',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<FAILED::>');
        done();
      });
    });

    it('should handle basic assertion failure from error', function(done) {
      runner.run({
        language: 'erlang',
        testFramework: 'eunit',
        solution: [
          '-module(solution).',
          '-export([add/2]).',
          'add(_A, _B) -> throw(fail).',
        ].join('\n'),
        fixture: [
          '-module(basic_tests).',
          '-include_lib("eunit/include/eunit.hrl").',
          'add_test_() ->',
          '  {"Addition",',
          '    {"add(1, 1)", ?_assertEqual(2, solution:add(1, 1))}}.',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<ERROR::>');
        done();
      });
    });


    it('should handle mixed success and failure', function(done) {
      runner.run({
        language: 'erlang',
        testFramework: 'eunit',
        solution: [
          '-module(solution).',
          '-export([add/2]).',
          'add(A, B) -> A - B.',
        ].join('\n'),
        fixture: [
          '-module(basic_tests).',
          '-include_lib("eunit/include/eunit.hrl").',
          'add_test_() ->',
          '  {"Addition",',
          '    [{"add(1, 0)", ?_assertEqual(1, solution:add(1, 0))},',
          '     {"add(1, 1)", ?_assertEqual(2, solution:add(1, 1))}]}.',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        done();
      });
    });

    it('should handle nested groups', function(done) {
      runner.run({
        language: 'erlang',
        testFramework: 'eunit',
        solution: [
          '-module(solution).',
          '-export([add/2]).',
          'add(A, B) -> A + B.',
        ].join('\n'),
        fixture: [
          '-module(basic_tests).',
          '-include_lib("eunit/include/eunit.hrl").',
          'add_test_() ->',
          '  {"Addition",',
          '    {"add(1, x)",',
          '      [{"add(1, 0)", ?_assertEqual(1, solution:add(1, 0))},',
          '       {"add(1, 1)", ?_assertEqual(2, solution:add(1, 1))}]}}.',
        ].join('\n'),
      }, function(buffer) {
        const expected = [
          '<DESCRIBE::>',
          '  <DESCRIBE::>',
          '    <IT::><PASSED::><COMPLETEDIN::>',
          '    <IT::><PASSED::><COMPLETEDIN::>',
          '  <COMPLETEDIN::>',
          '<COMPLETEDIN::>'
        ].join('').replace(/\s/g, '');
        expect(buffer.stdout.match(/<(?:DESCRIBE|IT|PASSED|FAILED|ERROR|COMPLETEDIN)::>/g).join('')).to.equal(expected);
        done();
      });
    });

    it('should handle setup file', function(done) {
      runner.run({
        language: 'erlang',
        testFramework: 'eunit',
        setup: [
          '-module(setup).',
          '-export([f/2]).',
          'f(A, B) -> A + B.',
        ].join('\n'),
        solution: [
          '-module(solution).',
          '-export([add/2]).',
          'add(A, B) -> setup:f(A, B).',
        ].join('\n'),
        fixture: [
          '-module(basic_tests).',
          '-include_lib("eunit/include/eunit.hrl").',
          'add_test_() ->',
          '  {"Addition",',
          '    {"add(1, 1)", ?_assertEqual(2, solution:add(1, 1))}}.',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });


    it('should allow logging', function(done) {
      runner.run({
        language: 'erlang',
        testFramework: 'eunit',
        solution: [
          '-module(solution).',
          '-export([add/2]).',
          'add(A, B) ->',
          '  io:fwrite("A = ~w, B = ~w\n", [A, B]),',
          '  A + B.',
        ].join('\n'),
        fixture: [
          '-module(basic_tests).',
          '-include_lib("eunit/include/eunit.hrl").',
          'add_test_() ->',
          '  {"Addition",',
          '    {"add(1, 1)", ?_assertEqual(2, solution:add(1, 1))}}.',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('A = 1, B = 1');
        done();
      });
    });

    it('should show error message when the tests are invalid', function(done) {
      runner.run({
        language: 'erlang',
        testFramework: 'eunit',
        solution: [
          '-module(f).',
          '-export([fib/1]).',
          `fib(0) -> 1;`,
          `fib(1) -> 1;`,
          `fib(N) when N > 1 -> fib(N-1) + fib(N-2).`,
        ].join('\n'),
        fixture: [
          '-module(bad_tests).',
          '-include_lib("eunit/include/eunit.hrl").',
          ``,
          `fib_test_() ->`,
          `  {"Fib",`,
          `    {"works for some inputs",`,
          `      [?_assertEqual(1, f:fib(0)),`,
          `       ?_assertEqual(1, f:fib(1)),`,
          `       ?_assertEqual(2, f:fib(2)),`,
          `       ?_assertEqual(3, f:fib(3)),`,
          `       ?_assertEqual(5, f:fib(4)),`,
          `       ?_assertEqual(8, f:fib(5))]},`,
          `    {"fib(31)", ?_assertEqual(2178309, f:fib(31))},`,
          `    {"fib(-1) errors", ?_assertException(error, function_clause, f:fib(-1))}}.`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<ERROR::>');
        expect(buffer.stdout).to.contain('*** bad test descriptor ***');
        done();
      });
    });
  });
});

describe('Examples', function() {
  afterEach(function cleanup(done) {
    exec('rm -rf /workspace/erlang/src/*.erl /workspace/erlang/test/*.erl /workspace/erlang/_build/test', function(err) {
      if (err) return done(err);
      done();
    });
  });
  runner.assertCodeExamples('erlang');
});
