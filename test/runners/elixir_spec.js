var expect = require('chai').expect;
var runner = require('../runner');

describe('elixir runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('elixir');

    it('handles basic code evaluation', function(done) {
      runner.run({language: 'elixir', code: 'IO.puts "This was real. And it was me. I had lived that life, and I had died that death. I was staring at the very end of me. ― Hilary Duff, Elixir (2010)"'}, function(buffer) {
        expect(buffer.stdout).to.contain('This was real');
        done();
      });
    });

    describe('ex_unit', function() {
      it('handles a simple test unit defininition', function(done) {
        runner.run({language: 'elixir', code: 'defmodule AEqual1 do\n def a, do: 1\nend', fixture: 'defmodule TestAEqual1 do\nuse ExUnit.Case\nimport AEqual1, only: [a: 0]\ntest "a is equal 1" do\nassert a() == 1\nend\nend', testFramework: 'ex_unit'}, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>TestAEqual1');
          expect(buffer.stdout).to.contain('<IT::>a is equal 1\n<PASSED::>Test Passed\n');
          expect(buffer.stdout).to.match(/<COMPLETEDIN::>Finished in [0-9.]+ seconds \([0-9.]+s on load, [0-9.]+s on tests\)/);
          done();
        });
      });
      it('handles test unit with a couple of defininitions', function(done) {
        runner.run({language: 'elixir', code: 'defmodule YesNo do\n def yes, do: "yes"\n def no, do: "no"\nend', fixture: 'defmodule TestYesNo do\nuse ExUnit.Case\ntest ".yes is yes" do\nassert YesNo.yes == "yes"\nend\ntest ".no is no" do\nassert YesNo.no == "no"\nend\nend', testFramework: 'ex_unit'}, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>TestYesNo');
          expect(buffer.stdout).to.contain('<IT::>.yes is yes\n<PASSED::>Test Passed\n');
          expect(buffer.stdout).to.contain('<IT::>.no is no\n<PASSED::>Test Passed\n');
          expect(buffer.stdout).to.match(/<COMPLETEDIN::>Finished in [0-9.]+ seconds \([0-9.]+s on load, [0-9.]+s on tests\)/);
          done();
        });
      });
      it('handles test which does not pass', function(done) {
        runner.run({language: 'elixir', code: 'defmodule NotPass do\n def can_i_enter?, do: :no\nend', fixture: 'defmodule TestNotPass do\nuse ExUnit.Case\ntest "passes if works" do\nassert NotPass.can_i_enter? == :yes\nend\nend', testFramework: 'ex_unit'}, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>TestNotPass');
          expect(buffer.stdout).to.contain('<IT::>passes if works\n<FAILED::>passes if works');
          expect(buffer.stdout).to.match(/<COMPLETEDIN::>Finished in [0-9.]+ seconds \([0-9.]+s on load, [0-9.]+s on tests\)/);
          done();
        });
      });
      it('handles test unit with pass and not-pass tests', function(done) {
        runner.run({language: 'elixir', code: 'defmodule YesBrokenNo do\n def yes, do: "yes"\n def no, do: "yes"\nend', fixture: 'defmodule TestYesBrokenNo do\nuse ExUnit.Case\ntest ".yes is yes" do\nassert YesBrokenNo.yes == "yes"\nend\ntest ".no is no" do\nassert YesBrokenNo.no == "no"\nend\nend', testFramework: 'ex_unit'}, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>TestYesBrokenNo');
          expect(buffer.stdout).to.contain('<IT::>.yes is yes\n<PASSED::>Test Passed\n');
          expect(buffer.stdout).to.contain('<IT::>.no is no\n<FAILED::>.no is no');
          expect(buffer.stdout).to.match(/<COMPLETEDIN::>Finished in [0-9.]+ seconds \([0-9.]+s on load, [0-9.]+s on tests\)/);
          done();
        });
      });

      describe('compilation errors', function() {
        it('catches compilation error of the code module', function(done) {
          runner.run({language: 'elixir', code: 'defodule BrokenModule do\n def broken, do: true\nend', fixture: 'defmodule TestBrokenModule do\nend', testFramework: 'ex_unit'}, function(buffer) {
            console.log(buffer);
            expect(buffer.stdout).to.contain('<ERROR::>solution:1: undefined function defodule/2\n');
            done();
          });
        });
        it('catches compilation error of the fixture module', function(done) {
          runner.run({language: 'elixir', code: 'defmodule BrokenFixture do\n def broken, do: true\nend', fixture: 'defmodule TestBrokenFixture do\n -> hanging_arrow \n end', testFramework: 'ex_unit'}, function(buffer) {
            expect(buffer.stdout).to.contain('<ERROR::>fixture:2: unhandled operator ->');
            done();
          });
        });
      });

      describe('runtime errors', function() {
        it('catches runtime error of the code module', function(done) {
          runner.run({language: 'elixir', code: 'defmodule RuntimeIssue do\n def make_issue do\n 5 = 8\nend\nend', fixture: 'defmodule TestRuntimeIssue do\nuse ExUnit.Case\ntest "have runtime issue" do\n RuntimeIssue.make_issue \nend\nend', testFramework: 'ex_unit'}, function(buffer) {
            expect(buffer.stdout).to.contain('<DESCRIBE::>TestRuntimeIssue');
            expect(buffer.stdout).to.contain('<IT::>have runtime issue\n<FAILED::>have runtime issue');
            done();
          });
        });
        it('catches runtime error of the fixture module', function(done) {
          runner.run({language: 'elixir', code: 'defmodule FixtureIssue do\n def no_issue do\n :no_issue\nend\nend', fixture: 'defmodule TestFixtureIssue do\nuse ExUnit.Case\ntest "have fixture issue" do\n ModuleNotExist.call_it() \nend\nend', testFramework: 'ex_unit'}, function(buffer) {
            expect(buffer.stdout).to.contain('<DESCRIBE::>TestFixtureIssue');
            expect(buffer.stdout).to.contain('<IT::>have fixture issue\n<FAILED::>have fixture issue');
            done();
          });
        });
      });

      describe('no fixtures', function() {
        // Due to ExUnit.start, it can add COMPLETEDIN:: at the end.
        // That's why we match with .contain and not .equal
        it('handles correctly empty fixture', function(done) {
          runner.run({language: 'elixir', code: 'IO.puts "Hello"', fixture: '', testFramework: 'ex_unit'}, function(buffer) {
            expect(buffer.stdout).to.contain('Hello\n');
            done();
          });
        });
        it('handles correctly fixture with just comments', function(done) {
          runner.run({language: 'elixir', code: 'IO.puts "Hello comment"', fixture: '# this is comment', testFramework: 'ex_unit'}, function(buffer) {
            expect(buffer.stdout).to.contain('Hello comment\n');
            done();
          });
        });
      });
    });
  });
});
