const expect = require('chai').expect;
const runner = require('../runner');

describe('rust runner', function() {
  describe('.run', function() {
    // Basic code run
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

    it('should handle invalid code', function(done) {
      runner.run({
        language: 'rust',
        code: `
          fn main() {
            println!("Bam);
          }
        `
      }, function(buffer) {
        expect(buffer.stderr).to.contain('unterminated double quote');
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

    it('should handle basic tests', function(done) {
      runner.run({
        language: 'rust',
        code: `
          fn test_function() -> i32 {
            return 69;
          }
        `,
        fixture: `
          #[test]
          fn returns_number() {
            assert_eq!(test_function(),69);
          }
        `,
        testFramework: 'cargo'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<DESCRIBE::>returns_number`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
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
            return n * 2;
          }
        `,
        fixture: `
        #[test]
        fn doubler_works() {
          assert_eq!(doubler(2),4);
        }
          `,
        testFramework: 'cargo'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<DESCRIBE::>doubler_works`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
        done();
      });
    });

  });
});
