"use strict";

const expect = require('chai').expect;
const runner = require('../runner');
const exec = require('child_process').exec;

describe('rust runner', function() {
  describe('running', function() {
    afterEach(function cleanup(done) {
      exec('rm -f /workspace/rust/src/*.rs', function(err) {
        if (err) return done(err);
        done();
      });
    });

    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'rust',
        code: `
          fn main() {
            println!("Bam");
          }
        `
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Bam\n');
        done();
      });
    });

    it('should allow mods inside setup', function(done) {
      runner.run({
        language: 'rust',
        setup: `
          use std::thread;
          use std::sync::mpsc::channel;
        `,
        code: `
          fn main() {
            let (tx, rx) = channel();
            thread::spawn(move|| {
                tx.send(10).unwrap();
            });

            println!("{}", rx.recv().unwrap());
          }
        `
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`10`);
        done();
      });
    });
  });

  describe('examples', function() {
    runner.assertCodeExamples('rust');
  });

  describe('testing', function() {
    afterEach(function cleanup(done) {
      exec('rm -f /workspace/rust/src/*.rs', function(err) {
        if (err) return done(err);
        done();
      });
    });

    it('should handle basic tests', function(done) {
      runner.run({
        language: 'rust',
        code: `
          fn test_function() -> i32 {
            69
          }
        `,
        fixture: `
          #[test]
          fn returns_number() {
            assert_eq!(test_function(),69);
          }
        `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<IT::>returns_number`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
        expect(buffer.stderr).to.be.empty;
        done();
      });
    });

    it('should handle tests with setup', function(done) {
      runner.run({
        language: 'rust',
        setup: `
          use std::thread;
          use std::sync::mpsc::channel;
        `,
        code: `
          fn async_thingo() {
            let (tx, rx) = channel();
            thread::spawn(move|| {
                tx.send(10).unwrap();
            });
          }

          fn doubler(n: i32) -> i32 {
            n * 2
          }
        `,
        fixture: `
          #[test]
          fn doubler_works() {
            assert_eq!(doubler(2),4);
          }
          `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<IT::>doubler_works`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
        expect(buffer.stderr).to.be.empty;
        done();
      });
    });

    it('should handle failed tests', function(done) {
      runner.run({
        language: 'rust',
        setup: `
          use std::thread;
          use std::sync::mpsc::channel;
        `,
        code: `
          fn doubler(n: i32) -> i32 {
            n * 2
          }
        `,
        fixture: `
          #[test]
          fn doubler_failure() {
            assert_eq!(doubler(2),3);
          }
          `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<IT::>doubler_failure`);
        expect(buffer.stdout).to.contain(`<FAILED::>Test Failed`);
        done();
      });
    });

    it('should handle success and failed tests', function(done) {
      runner.run({
        language: 'rust',
        code: `
          fn doubler(n: i32) -> i32 {
            n * 2
          }
        `,
        fixture: `
          #[test]
          fn doubler_success() {
            assert_eq!(doubler(2),4);
          }

          #[test]
          fn doubler_failure() {
            assert_eq!(doubler(2),3);
          }
        `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<IT::>doubler_success`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
        expect(buffer.stdout).to.contain(`<IT::>doubler_failure`);
        expect(buffer.stdout).to.contain(`<FAILED::>Test Failed`);
        done();
      });
    });

    it('can use rand', function(done) {
      runner.run({
        language: 'rust',
        code: 'fn add(x: i32, y: i32) -> i32 { x + y }',
        // need `self::` because `extern crate` loads to current namespace and tests are in `mod tests`.
        fixture: `
          extern crate rand;
          use self::rand::Rng;
          use self::rand::distributions::{IndependentSample, Range};

          #[test]
          fn returns_sum() {
            let between = Range::new(1, 10);
            let mut rng = rand::thread_rng();
            let x = between.ind_sample(&mut rng);
            let y = between.ind_sample(&mut rng);
            assert_eq!(add(x, y), x + y);
          }
        `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<IT::>returns_sum`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
        expect(buffer.stderr).to.be.empty;
        done();
      });
    });

    it('can use rand, works even if the solution imports it', function(done) {
      runner.run({
        language: 'rust',
        code: `
          extern crate rand;
          use rand::Rng;
          fn add(x: i32, y: i32) -> i32 {
            let mut rng = rand::thread_rng();
            if rng.gen() {
              rng.gen::<i32>()
            } else {
              x + y
            }
          }
        `,
        fixture: `
          extern crate rand;
          use self::rand::Rng;
          use self::rand::distributions::{IndependentSample, Range};

          #[test]
          fn returns_sum() {
            let between = Range::new(1, 10);
            let mut rng = rand::thread_rng();
            let x = between.ind_sample(&mut rng);
            let y = between.ind_sample(&mut rng);
            println!("x: {}, y: {}", x, y);
            assert_eq!(add(x, y), x + y);
          }
        `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stderr).not.to.contain('an extern crate named `rand` has already been imported in this module');
        done();
      });
    });

    it('should include custom messages on failure', function(done) {
      runner.run({
        language: 'rust',
        code: `
          fn add(x:i32, y:i32) -> i32 {
            if x == 1 {
              x + y
            } else {
              x + y + 1
            }
          }
        `,
        fixture: `
          use super::*;
          #[test] fn returns_sum2_1() { assert_eq!(add(2, 1), 3, "testing addition with {} and {}", 2, 1); }
        `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<IT::>returns_sum2_1`);
        expect(buffer.stdout).to.contain(`<FAILED::>Test Failed`);
        expect(buffer.stdout).to.contain(`testing addition with 2 and 1`);
        done();
      });
    });

    it('should show users logs for failed tests', function(done) {
      runner.run({
        language: 'rust',
        code: `
          fn add(x:i32, y:i32) -> i32 {
            if x == 1 {
              println!("[passing test] x: {}, y: {}", x, y);
              x + y
            } else {
              println!("[failing test] x: {}, y: {}", x, y);
              x + y + 1
            }
          }
        `,
        fixture: `
          use super::*;
          #[test] fn returns_sum1_1() { assert_eq!(add(1, 1), 2); }
          #[test] fn returns_sum2_1() { assert_eq!(add(2, 1), 3); }
        `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<IT::>returns_sum1_1`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
        expect(buffer.stdout).not.to.contain(`[passing test] x: 1, y: 1`);
        expect(buffer.stdout).to.contain(`[failing test] x: 2, y: 1`);
        done();
      });
    });

    it('should output correctly for some edge cases', function(done) {
      runner.run({
        language: 'rust',
        code: 'fn add(x:i32, y:i32) -> i32 { x + y }',
        fixture: `
          use super::*;
          #[test] fn returns_sum1_1() { assert_eq!(add(1, 1), 2); }
          #[test] fn single_quotes() { assert_eq!("abc'", "'cba"); }
          #[test] fn back_slashes() { assert_eq!("abc\\\\'", "'\\\\cba"); }
          #[test] fn line_feed_issue() {
            print!("foo");
            assert_eq!(1, 2);
          }
        `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<IT::>returns_sum1_1`);
        expect(buffer.stdout).to.contain(`\n<PASSED::>Test Passed`);
        expect(buffer.stdout).to.contain(`\n<FAILED::>Test Failed<:LF:>assertion failed: \`(left == right)\` (left: \`"abc\\\\'"\`, right: \`"'\\\\cba"\`)`);
        expect(buffer.stdout).to.contain(`\n<FAILED::>Test Failed<:LF:>assertion failed: \`(left == right)\` (left: \`"abc'"\`, right: \`"'cba"\`)`);
        expect(buffer.stdout).to.contain(`\n<FAILED::>Test Failed<:LF:>assertion failed: \`(left == right)\` (left: \`1\`, right: \`2\`)`);
        done();
      });
    });

    it('should output correct structure', function(done) {
      runner.run({
        language: 'rust',
        code: 'fn add(x:i32, y:i32) -> i32 { x + y }',
        fixture: `
          use super::*;
          #[test] fn returns_sum1_1() { assert_eq!(add(1, 1), 2); }
          #[test] fn returns_sum1_2() { assert_eq!(add(1, 2), 3); }
          #[test] fn returns_sum1_3() { assert_eq!(add(1, 3), 4); }
          #[test] fn returns_sum1_4() { assert_eq!(add(1, 4), 5); }
        `,
        testFramework: 'rust'
      }, function(buffer) {
        const expected = [
          '<IT::><PASSED::><COMPLETEDIN::>',
          '<IT::><PASSED::><COMPLETEDIN::>',
          '<IT::><PASSED::><COMPLETEDIN::>',
          '<IT::><PASSED::><COMPLETEDIN::>'
        ].join('').replace(/\s/g, '');
        expect(buffer.stdout.match(/<(?:IT|PASSED|FAILED|COMPLETEDIN)::>/g).join('')).to.equal(expected);
        done();
      });
    });
  });

  describe('invalid code and warnings', function() {
    afterEach(function cleanup(done) {
      exec('rm -f /workspace/rust/src/*.rs', function(err) {
        if (err) return done(err);
        done();
      });
    });

    it('should handle invalid code', function(done) {
      runner.run({
        language: 'rust',
        code: [
          `fn main() {`,
          `  println!("Bam);`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stderr).to.contain('unterminated double quote');
        done();
      });
    });

    it('should handle broken and unused code', function(done) {
      runner.run({
        language: 'rust',
        setup: `
          use std::thread;
          use std::sync::mpsc::channel;
        `,
        code: `
          fn async_thingo() {
            let (tx, rx) = channel();
            thread::spawn(move|| {
                tx.send(10).unwrap();
            });
          }

          fn unused_func() {
            println!("Never called");
          }

          fn broken_func() {
            println!("This is broken...";
          }

          fn doubler(n: i32) -> i32 {
            n * 2
          }
        `,
        fixture: `
        #[test]
        fn doubler_works() {
          assert_eq!(doubler(2),4);
        }
          `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stderr).to.contain(`unclosed delimiter`);
        done();
      });
    });

    it('should ignore unused code warnings', function(done) {
      runner.run({
        language: 'rust',
        setup: `
          use std::thread;
          use std::sync::mpsc::channel;
        `,
        code: `
          fn async_thingo() {
            let (tx, rx) = channel();
            thread::spawn(move|| {
                tx.send(10).unwrap();
            });
          }

          fn unused_func() {
            println!("Never called");
          }

          fn doubler(n: i32) -> i32 {
            n * 2
          }
        `,
        fixture: `
        #[test]
        fn doubler_works() {
          assert_eq!(doubler(2),4);
        }
          `,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<IT::>doubler_works`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
        expect(buffer.stderr).to.be.empty;
        done();
      });
    });

    it('should handle broken test code', function(done) {
      runner.run({
        language: 'rust',
        code: 'fn doubler(n: i32) -> i32 { n * 2 }',
        fixture: [
          `#[test]`,
          `fn doubler_works( {`,
          `  assert_eq!(doubler(2),4);`,
          `}`,
        ].join('\n'),
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stderr).to.contain(`un-closed delimiter`);
        done();
      });
    });

    it('should allow non snake case', function(done) {
      runner.run({
        language: 'rust',
        code: 'fn add(x: i32, y: i32) -> i32 { x + y }',
        fixture: `#[test] fn testAdd() { assert_eq!(add(1, 1), 2); }`,
        testFramework: 'rust'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stderr).to.be.empty;
        done();
      });
    });
  });
});
