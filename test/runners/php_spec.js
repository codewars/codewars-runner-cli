var expect = require('chai').expect;
var runner = require('../runner');


describe('php runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('php');

    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'php',
        solution: 'echo 42;'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42');
        done();
      });
    });

    it('should handle bad syntax', function(done) {
      runner.run({
        language: 'php',
        solution: `fliggaflagam!!!`
      }, function(buffer) {
        expect(buffer.stderr).to.contain('syntax error');
        done();
      });
    });

    it('should handle thrown exceptions', function(done) {
      runner.run({
        language: 'php',
        solution: `throw new Exception('Rawr!');`
      }, function(buffer) {
        expect(buffer.stderr).to.contain('Rawr!');
        done();
      });
    });

    it('should handle undefined functions', function(done) {
      runner.run({
        language: 'php',
        solution: `fliggaflagam();`
      }, function(buffer) {
        expect(buffer.stderr).to.contain('Uncaught Error');
        done();
      });
    });

    it('should handle the latest and greatest of PHP 7', function(done) {
      runner.run({
        language: 'php',
        solution: [
          `function sumOfInts(int ...$ints) { return array_sum($ints); }`,
          `echo sumOfInts(2, '3', 4.1);`,
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.equal('9');
        done();
      });
    });

    describe('cw-2', function() {
      it('should handle some basic tests', function(done) {
        runner.run({
          language: 'php',
          solution: 'function double($a) { return $a * 2; }',
          fixture: '$test->assert_equals(double(1, 2), 2);',
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });

      it('should be able to reference preloaded code', function(done) {
        runner.run({
          language: 'php',
          setup: `class SomeClass { const CONSTANT = 42; }`,
          solution: `function theConstant() { return SomeClass::CONSTANT; }`,
          fixture: `$test->assert_equals(theConstant(), SomeClass::CONSTANT);`,
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });

      it('should handle failed tests', function(done) {
        runner.run({
          language: 'php',
          solution: `function double($a) { return $a * 2; }`,
          fixture: `$test->assert_equals(double(1), 6);`,
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>');
          done();
        });
      });

      it('should handle bad assertions', function(done) {
        runner.run({
          language: 'php',
          solution: `const CONSTANT = 42;`,
          fixture: `$test->assert_equals(CONSTANT, 'apples');`,
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>');
          done();
        });
      });

      it('should handle thrown exceptions', function(done) {
        runner.run({
          language: 'php',
          solution: `$pizza = 'yummy';`,
          fixture: `throw new Exception('Roffle!');`,
          testFramework: 'cw-2'
        }, function(buffer) {
          expect(buffer.stderr).to.contain('Roffle!');
          done();
        });
      });

      it('should have output format command on independent line', function(done) {
        runner.run({
          language: 'php',
          testFramework: 'cw-2',
          solution: '//',
          fixture: [
            '$test->describe("tests", function() {',
            '  global $test;',
            '  $test->it("test", function() {',
            '    global $test;',
            '    echo "foo";',
            '    $test->assert_equals(1, 2);',
            '  });',
            '});',
          ].join('\n'),
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<FAILED::>');
          done();
        });
      });
    });

    describe('phpunit', function() {
      it('should handle some basic tests', function(done) {
        runner.run({
          language: 'php',
          solution: `function double($a) { return $a * 2; }`,
          fixture: [
            `class DoubleMethod extends TestCase {`,
            `    public function testDouble() {`,
            `        $this->assertEquals(double(1), 2);`,
            `    }`,
            `}`,
          ].join('\n'),
          testFramework: 'phpunit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });
      it('should handle multiple tests', function(done) {
        runner.run({
          language: 'php',
          solution: `function double($a) { return $a * 2; }`,
          fixture: [
            `class DoubleMethod extends TestCase {`,
            `    public function testDouble() {`,
            `        $this->assertEquals(double(1), 2);`,
            `    }`,
            `    public function testDouble2() {`,
            `        $this->assertEquals(double(2), 4);`,
            `    }`,
            `    public function testDouble3() {`,
            `        $this->assertEquals(double(4), 8);`,
            `    }`,
            `}`,
          ].join('\n'),
          testFramework: 'phpunit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<IT::>testDouble\n');
          expect(buffer.stdout).to.contain('\n<IT::>testDouble2\n');
          expect(buffer.stdout).to.contain('\n<IT::>testDouble3\n');
          done();
        });
      });

      it('should render console output', function(done) {
        runner.run({
          language: 'php',
          solution: [
            `function double($a) {`,
            `  print("this was a triumph\n");`,
            `  return $a * 2;`,
            `}`,
          ].join('\n'),
          fixture: [
            `class DoubleMethod extends TestCase {`,
            `    public function testDouble() {`,
            `        $this->assertEquals(double(1), 2);`,
            `    }`,
            `}`,
          ].join('\n'),
          testFramework: 'phpunit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<PASSED::>');
          expect(buffer.stdout).to.contain('this was a triumph');
          done();
        });
      });

      it('should handle console output without linewraps', function(done) {
        runner.run({
          language: 'php',
          solution: [
            `function double($a) {`,
            `  print("this was a triumph");`,
            `  return $a * 2;`,
            `}`,
          ].join('\n'),
          fixture: [
            `class DoubleMethod extends TestCase {`,
            `    public function testDouble() {`,
            `        print("I'm making a note here, huge success");`,
            `        $this->assertEquals(double(1), 2);`,
            `    }`,
            `}`,
          ].join('\n'),
          testFramework: 'phpunit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<PASSED::>');
          expect(buffer.stdout).to.contain('huge success');
          expect(buffer.stdout).to.contain('this was a triumph');
          done();
        });
      });

      it('should be able to reference preloaded code', function(done) {
        runner.run({
          language: 'php',
          setup: [
            `class SomeClass {`,
            `    const CONSTANT = 42;`,
            `}`,
          ].join('\n'),
          solution: [
            `function theConstant() {`,
            `    return SomeClass::CONSTANT;`,
            `}`,
          ].join('\n'),
          fixture: [
            `class TheConstantMethod extends TestCase {`,
            `    public function testConstantMethod() {`,
            `        $this->assertEquals(theConstant(), SomeClass::CONSTANT);`,
            `    }`,
            `}`,
          ].join('\n'),
          testFramework: 'phpunit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });

      it('should handle failed tests', function(done) {
        runner.run({
          language: 'php',
          solution: `function double($a) { return $a * 2; }`,
          fixture: [
            `class TheConstantMethod extends TestCase {`,
            `    public function testConstantMethod() {`,
            `        $this->assertEquals(double(1), 6);`,
            `    }`,
            `}`,
          ].join('\n'),
          testFramework: 'phpunit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>');
          expect(buffer.stdout).to.not.contain('<PASSED::>');
          done();
        });
      });


      it('should render output on failed tests', function(done) {
        runner.run({
          language: 'php',
          solution: `function greet($s) { return 'hello, ' . $s; }`,
          fixture: [
            `class GreetingTest extends TestCase {`,
            `    public function testGreet() {`,
            `        $this->assertEquals(greet('Joe'), 'Hello, Joe');`,
            `    }`,
            `}`,
          ].join('\n'),
          testFramework: 'phpunit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>');
          expect(buffer.stdout).to.contain('hello, Joe');
          expect(buffer.stdout).to.contain('Hello, Joe');
          done();
        });
      });

      it('should handle bad assertions', function(done) {
        runner.run({
          language: 'php',
          solution: `const CONSTANT = 42;`,
          fixture: [
            `class TheConstantMethod extends TestCase {`,
            `    public function testConstantMethod() {`,
            `        $this->assertEquals(CONSTANT, 'apples');`,
            `    }`,
            `}`,
          ].join('\n'),
          testFramework: 'phpunit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>');
          expect(buffer.stdout).to.not.contain('<PASSED::>');
          done();
        });
      });

      it('should handle thrown exceptions', function(done) {
        runner.run({
          language: 'php',
          solution: `$pizza = 'yummy';`,
          fixture: [
            `class TheConstantMethod extends TestCase {`,
            `    public function testConstantMethod() {`,
            `        throw new Exception('Waffles!');`,
            `    }`,
            `}`,
          ].join('\n'),
          testFramework: 'phpunit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('Waffles!');
          done();
        });
      });

      it('should fail on PHP errors', function(done) {
        runner.run({
          language: 'php',
          solution: `function double($a) { return $a * 2; }`,
          fixture: [
            `class Double extends TestCase {`,
            `    public function testBadDouble() {`,
            `        $this->assertEquals(double(), 2);`,
            `    }`,
            `}`,
          ].join('\n'),
          testFramework: 'phpunit'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>');
          expect(buffer.stdout).to.not.contain('<PASSED::>');
          done();
        });
      });
    });
  });
});
