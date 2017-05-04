var expect = require('chai').expect;
var runner = require('../runner');


describe('ruby runner', function() {
  describe('.run', function() {
    // runner.assertCodeExamples('ruby');

    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'ruby',
        code: 'puts 42'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });

    it('should support githubRepo downloading', function(done) {
      runner.run({
        language: 'ruby',
        code: [
          'require "sample"',
          'puts Sample.new.message',
        ].join('\n'),
        githubRepo: 'jhoffner/test'
      }, function(buffer) {
        console.log(buffer.stdout);
        expect(buffer.stdout).to.contain('sample\n');
        done();
      });
    });

    it('should support gist downloading', function(done) {
      runner.run({
        language: 'ruby',
        code: 'puts `ls`',
        setup: '# @config: gist 3acc7b81436ffe4ad20800e242ccaff6',
      }, function(buffer) {
        console.log(buffer.stdout);
        expect(buffer.stdout).to.contain('gist.js\n');
        done();
      });
    });

    it('should support config bash-file', function(done) {
      runner.run({
        language: 'ruby',
        code: 'puts `ls`',
        setup: [
          '# @config: github-repo jhoffner/test',
          '# @config: bash-file start.sh',
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('test.txt\n');
        done();
      });
    });

    it('should support additional files', function(done) {
      runner.run({
        language: 'ruby',
        code: 'puts `ls`',
        files: {
          'myconfig.rb': 'puts 123'
        }
      }, function(buffer) {
        expect(buffer.stdout).to.contain('myconfig.rb');
        done();
      });
    });

    describe('cw-2', function() {
      it('should handle a basic assertion', function(done) {
        runner.run({
          language: 'ruby',
          code: 'a = 1',
          fixture: 'Test.expect a == 1',
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
          done();
        });
      });

      it('should handle a basic description', function(done) {
        runner.run({
          language: 'ruby',
          code: 'a = 1',
          fixture: 'describe("test") { Test.expect a == 1 }',
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>test\n\n<PASSED::>Test Passed\n\n<COMPLETEDIN::>');
          expect(buffer.stdout).to.contain('ms');
          done();
        });
      });

      describe('error handling', function() {
        it('should handle a mix of failures and successes', function(done) {
          runner.run({
            language: 'ruby',
            code: 'a = 1',
            fixture: [
              'describe "test" do',
              '  it("test1") { Test.expect(false) }',
              '  it("test2") { Test.expect(true) }',
              'end',
            ].join('\n'),
            testFramework: 'cw-2'
          }, function(buffer) {
            console.log(buffer.stdout);
            expect(buffer.stdout).to.contain('<FAILED::>Value is not what was expected');
            expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
            done();
          });
        });
        it('should gracefully handle custom errors', function(done) {
          runner.run({
            language: 'ruby',
            code: 'a = 1',
            fixture: [
              'describe "test" do',
              '  it("test1") { raise "boom!" }',
              '  it("test2") { Test.expect(true) }',
              'end',
            ].join('\n'),
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
            language: 'ruby',
            code: 'a = 1',
            fixture: [
              'describe "test" do',
              '  it("test1") { a.idontexist() }',
              '  it("test2") { Test.expect(true) }',
              'end',
            ].join('\n'),
            testFramework: 'cw-2'
          }, function(buffer) {
            expect(buffer.stdout).to.contain('<ERROR::>');
            expect(buffer.stdout).to.contain('<:LF:>');
            expect(buffer.stdout).to.contain('NoMethodError:');
            expect(buffer.stdout).to.not.contain('from /cli-runner/');
            // expect(buffer.stdout).to.not.contain('-e:');
            // expect(buffer.stdout).to.not.contain('cw-2.rb');
            expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
            done();
          });
        });

        it('should prevent short circuiting', function(done) {
          runner.run({
            language: 'ruby',
            code: [
              "def example",
              "  Test.expect(true);",
              "  raise 'early error'",
              "end"
            ].join("\n"),
            fixture: [
              'describe "test" do',
              '  it("test1") { example }',
              '  it("test2") { Test.expect(false) }',
              'end'
            ].join('\n'),
            testFramework: 'cw-2'
          }, function(buffer) {
            expect(buffer.stdout).to.contain('<ERROR::>');
            done();
          });
        });
      });

      it('should have output format command on independent line', function(done) {
        runner.run({
          language: 'ruby',
          testFramework: 'cw-2',
          code: '#',
          fixture: [
            `print 'foo'`,
            `Test.expect(false)`,
          ].join('\n'),
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<FAILED::>');
          done();
        });
      });
    });

    describe('rspec', function() {
      it('should handle a basic assertion', function(done) {
        runner.run({
          language: 'ruby',
          code: '$a = 1',
          fixture: [
            'describe "test" do',
            '  it("test2") { expect($a).to eq(1) }',
            'end',
          ].join('\n'),
          testFramework: 'rspec'
        }, function(buffer) {
          expect(buffer.stdout).to.equal('\n<DESCRIBE::>test\n\n<IT::>test2\n\n<PASSED::>Test Passed\n\n<COMPLETEDIN::>\n\n<COMPLETEDIN::>\n');
          done();
        });
      });
      it('should support let', function(done) {
        runner.run({
          language: 'ruby',
          code: '$a = 1',
          fixture: [
            'describe "test" do',
            '  let(:b) { $a }',
            '  it("test2") { expect(b).to eq(1) }',
            'end'
          ].join('\n'),
          testFramework: 'rspec'
        }, function(buffer) {
          expect(buffer.stdout).to.equal('\n<DESCRIBE::>test\n\n<IT::>test2\n\n<PASSED::>Test Passed\n\n<COMPLETEDIN::>\n\n<COMPLETEDIN::>\n');
          done();
        });
      });
      it('should handle a basic failed assertion', function(done) {
        runner.run({
          language: 'ruby',
          code: '$a = 1',
          fixture: [
            'describe "test" do',
            '  it("test2") { expect($a).to eq(2) }',
            'end',
          ].join('\n'),
          testFramework: 'rspec'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>test\n\n<IT::>test2');
          expect(buffer.stdout).to.contain('<FAILED::>');
          expect(buffer.stdout).to.not.contain('<PASSED::>');
          expect(buffer.stdout).to.not.contain('simplified backtrace');
          done();
        });
      });
      it('should handle errored code', function(done) {
        runner.run({
          language: 'ruby',
          code: 'a = 1',
          fixture: [
            'describe "test" do',
            '  it("test1") { a.idontexist() }',
            '  it("test2") { expect(true) }',
            'end',
          ].join('\n'),
          testFramework: 'rspec'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>test');
          expect(buffer.stdout).to.contain('<IT::>test1');
          expect(buffer.stdout).to.contain('<IT::>test2');
          expect(buffer.stdout).to.contain('<ERROR::>');
          done();
        });
      });
      it('should prevent short circuiting', function(done) {
        runner.run({
          language: 'ruby',
          code: [
            "def example",
            "  expect(true);",
            "  raise 'early error'",
            "end"
          ].join("\n"),
          fixture: [
            'describe "test" do',
            '  it("test1") { example }',
            '  it("test2") { expect(false) }',
            'end'
          ].join('\n'),
          testFramework: 'rspec'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<ERROR::>');
          done();
        });
      });

      it('should have output format command on independent line', function(done) {
        runner.run({
          language: 'ruby',
          code: '#',
          fixture: [
            `describe "test" do`,
            `  it("test2") do`,
            `    print 'foo'`,
            `    expect(1).to eq(2)`,
            `  end`,
            `end`,
          ].join('\n'),
          testFramework: 'rspec'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<FAILED::>');
          done();
        });
      });
    });

    describe('potpourri', function() {
      it('can run redis', function(done) {
        runner.run({
          language: 'ruby',
          code: [
            'puts `ls`',
            'fork do',
            '  exec "redis-server"',
            'end',
            "require 'redis'",
            'r = Redis.new',
            "r.set('a', 'b')"
          ].join('\n'),
          fixture: "Test.assert_equals(r.get('a'), 'b')",
          testFramework: 'cw-2'
        }, function(buffer) {
          console.log(buffer.stderr);
          expect(buffer.stdout).to.contain('<PASSED::>Test Passed: Value == \"b\"');
          done();
        });
      });
    });
  });
});
