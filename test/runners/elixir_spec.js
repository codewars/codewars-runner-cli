var expect = require('chai').expect;
var runner = require('../runner');

describe('elixir runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('elixir');

    it('handles basic code evaluation', function(done) {
      runner.run({
        language: 'elixir',
        code: 'IO.puts "This was real. And it was me. I had lived that life, and I had died that death. I was staring at the very end of me. ― Hilary Duff, Elixir (2010)"'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('This was real');
        done();
      });
    });

    describe('ex_unit', function() {
      it('handles a simple test unit defininition', function(done) {
        runner.run({
          language: 'elixir',
          code: 'defmodule AEqual1 do\n def a, do: 1\nend',
          fixture: [
            'defmodule TestAEqual1 do',
            '  use ExUnit.Case',
            '  import AEqual1, only: [a: 0]',
            '  test "a is equal 1" do',
            '    assert a() == 1',
            '  end',
            'end',
          ].join('\n'),
          testFramework: 'ex_unit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<DESCRIBE::>TestAEqual1');
          expect(buffer.stdout).to.contain('\n<IT::>a is equal 1\n\n<PASSED::>Test Passed\n');
          expect(buffer.stdout).to.match(/<COMPLETEDIN::>Finished in [0-9.]+ seconds \([0-9.]+s on load, [0-9.]+s on tests\)/);
          done();
        });
      });

      it('handles test unit with a couple of defininitions', function(done) {
        runner.run({
          language: 'elixir',
          code: [
            'defmodule YesNo do',
            ' def yes, do: "yes"',
            ' def no, do: "no"',
            'end',
          ].join('\n'),
          fixture: [
            'defmodule TestYesNo do',
            '  use ExUnit.Case',
            '  test ".yes is yes" do',
            '    assert YesNo.yes == "yes"',
            '  end',
            '  test ".no is no" do',
            '    assert YesNo.no == "no"',
            '  end',
            'end',
          ].join('\n'),
          testFramework: 'ex_unit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<DESCRIBE::>TestYesNo');
          expect(buffer.stdout).to.contain('\n<IT::>.yes is yes\n\n<PASSED::>Test Passed\n');
          expect(buffer.stdout).to.contain('\n<IT::>.no is no\n\n<PASSED::>Test Passed\n');
          expect(buffer.stdout).to.match(/<COMPLETEDIN::>Finished in [0-9.]+ seconds \([0-9.]+s on load, [0-9.]+s on tests\)/);
          done();
        });
      });
      it('handles test which does not pass', function(done) {
        runner.run({
          language: 'elixir',
          code: [
            'defmodule NotPass do',
            '  def can_i_enter?, do: :no',
            'end',
          ].join('\n'),
          fixture: [
            'defmodule TestNotPass do',
            '  use ExUnit.Case',
            '  test "passes if works" do',
            '    assert NotPass.can_i_enter? == :yes',
            '  end',
            'end',
          ].join('\n'),
          testFramework: 'ex_unit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<DESCRIBE::>TestNotPass');
          expect(buffer.stdout).to.contain('\n<IT::>passes if works\n\n<FAILED::>passes if works');
          expect(buffer.stdout).to.match(/<COMPLETEDIN::>Finished in [0-9.]+ seconds \([0-9.]+s on load, [0-9.]+s on tests\)/);
          done();
        });
      });
      it('handles test unit with pass and not-pass tests', function(done) {
        runner.run({
          language: 'elixir',
          code: [
            'defmodule YesBrokenNo do',
            '  def yes, do: "yes"',
            '  def no, do: "yes"',
            'end',
          ].join('\n'),
          fixture: [
            'defmodule TestYesBrokenNo do',
            '  use ExUnit.Case',
            '  test ".yes is yes" do',
            '    assert YesBrokenNo.yes == "yes"',
            '  end',
            '  test ".no is no" do',
            '    assert YesBrokenNo.no == "no"',
            '  end',
            'end',
          ].join('\n'),
          testFramework: 'ex_unit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<DESCRIBE::>TestYesBrokenNo');
          expect(buffer.stdout).to.contain('\n<IT::>.yes is yes\n\n<PASSED::>Test Passed\n');
          expect(buffer.stdout).to.contain('\n<IT::>.no is no\n\n<FAILED::>.no is no');
          expect(buffer.stdout).to.match(/<COMPLETEDIN::>Finished in [0-9.]+ seconds \([0-9.]+s on load, [0-9.]+s on tests\)/);
          done();
        });
      });

      describe('compilation errors', function() {
        it('catches compilation error of the code module', function(done) {
          runner.run({
            language: 'elixir',
            code: [
              'defodule BrokenModule do',
              '  def broken, do: true',
              'end',
            ].join('\n'),
            fixture: [
              'defmodule TestBrokenModule do',
              'end',
            ].join('\n'),
            testFramework: 'ex_unit'
          }, function(buffer) {
            console.log(buffer);
            expect(buffer.stdout).to.contain('\n<ERROR::>solution:1: undefined function defodule/2\n');
            done();
          });
        });
        it('catches compilation error of the fixture module', function(done) {
          runner.run({
            language: 'elixir',
            code: [
              'defmodule BrokenFixture do',
              '  def broken, do: true',
              'end',
            ].join('\n'),
            fixture: [
              'defmodule TestBrokenFixture do',
              '  -> hanging_arrow ',
              'end',
            ].join('\n'),
            testFramework: 'ex_unit'
          }, function(buffer) {
            expect(buffer.stdout).to.contain('\n<ERROR::>fixture:2: unhandled operator ->');
            done();
          });
        });
      });

      describe('runtime errors', function() {
        it('catches runtime error of the code module', function(done) {
          runner.run({
            language: 'elixir',
            code: [
              'defmodule RuntimeIssue do',
              '  def make_issue do',
              '    5 = 8',
              '  end',
              'end',
            ].join('\n'),
            fixture: [
              'defmodule TestRuntimeIssue do',
              '  use ExUnit.Case',
              '  test "have runtime issue" do',
              '    RuntimeIssue.make_issue',
              '  end',
              'end',
            ].join('\n'),
            testFramework: 'ex_unit'
          }, function(buffer) {
            expect(buffer.stdout).to.contain('\n<DESCRIBE::>TestRuntimeIssue');
            expect(buffer.stdout).to.contain('\n<IT::>have runtime issue\n\n<FAILED::>have runtime issue');
            done();
          });
        });

        it('catches runtime error of the fixture module', function(done) {
          runner.run({
            language: 'elixir',
            code: [
              'defmodule FixtureIssue do',
              '  def no_issue do',
              '    :no_issue',
              '  end',
              'end',
            ].join('\n'),
            fixture: [
              'defmodule TestFixtureIssue do',
              '  use ExUnit.Case',
              '  test "have fixture issue" do',
              '    ModuleNotExist.call_it()',
              '  end',
              'end',
            ].join('\n'),
            testFramework: 'ex_unit'
          }, function(buffer) {
            expect(buffer.stdout).to.contain('\n<DESCRIBE::>TestFixtureIssue');
            expect(buffer.stdout).to.contain('\n<IT::>have fixture issue\n\n<FAILED::>have fixture issue');
            done();
          });
        });
      });

      describe('no fixtures', function() {
        // Due to ExUnit.start, it can add COMPLETEDIN:: at the end.
        // That's why we match with .contain and not .equal
        it('handles correctly empty fixture', function(done) {
          runner.run({
            language: 'elixir',
            code: 'IO.puts "Hello"',
            fixture: '',
            testFramework: 'ex_unit'
          }, function(buffer) {
            expect(buffer.stdout).to.contain('Hello\n');
            done();
          });
        });
        it('handles correctly fixture with just comments', function(done) {
          runner.run({
            language: 'elixir',
            code: 'IO.puts "Hello comment"',
            fixture: '# this is comment',
            testFramework: 'ex_unit'
          }, function(buffer) {
            expect(buffer.stdout).to.contain('Hello comment\n');
            done();
          });
        });
      });

      it('should have output format command on independent line', function(done) {
        runner.run({
          language: 'elixir',
          testFramework: 'ex_unit',
          code: '#',
          fixture: [
            'defmodule Tests do',
            '  use ExUnit.Case',
            '  test "test" do',
            '    IO.write "foo"',
            '    assert 1 == 2',
            '  end',
            'end',
          ].join('\n'),
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<DESCRIBE::>');
          done();
        });
      });
    });
  });
});
