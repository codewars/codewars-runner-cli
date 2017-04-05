const expect = require('chai').expect;
const runner = require('../runner');

describe('rust runner', function() {
  describe('.run', function() {
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
        expect(buffer.stdout).to.contain(`<DESCRIBE::>doubler_works`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
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
        expect(buffer.stderr).to.contain(`incorrect close delimiter`);
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
        expect(buffer.stdout).to.contain(`<DESCRIBE::>doubler_works`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
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
        expect(buffer.stdout).to.contain(`<DESCRIBE::>doubler_failure`);
        expect(buffer.stdout).to.contain(`<FAILED::>Test Failed`);
        done();
      });
    });

    it('should handle broken test code', function(done) {
      runner.run({
        language: 'rust',
        code: `
          fn doubler(n: i32) -> i32 {
            n * 2
          }
        `,
        fixture: `
          #[test]
          fn doubler_works( {
            assert_eq!(doubler(2),4);
          }
        `,
        testFramework: 'rust'
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stderr).to.contain(`un-closed delimiter`);
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
        expect(buffer.stdout).to.contain(`<DESCRIBE::>doubler_success`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
        expect(buffer.stdout).to.contain(`<DESCRIBE::>doubler_failure`);
        expect(buffer.stdout).to.contain(`<FAILED::>Test Failed`);
        done();
      });
    });

  });
});
