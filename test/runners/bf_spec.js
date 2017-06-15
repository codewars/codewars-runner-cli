var expect = require('chai').expect;
var runner = require('../runner');

describe("BF Runner", function() {
  describe("Basic Run", function() {
    runner.assertCodeExamples('bf');
    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'bf', code: '++++++++++[>+++>+++++++>+++++++++>++++++++++>+++++++++++<<<<<-]>>++.>>+.>--..+++.<<<<++.>>---.>>.+++.------.<-.<<<+.'}, function(buffer) {
        expect(buffer.stdout).to.equal("Hello World!");
        done();
      });
    });
    it('should ignore anything not in "+-.,<>[]" as comments', function(done) {
      runner.run({language: 'bf', code: `++++++++++ Initialize cell #0 to 10
[
  "while" loop begins
  >+++ Go to cell #1 and add 3
  >+++++++ Go to cell #2 and add 7
  >+++++++++ Go to cell #3 and add 9
  >++++++++++ Go to cell #4 and add 10
  >+++++++++++ Go to cell #5 and add 11
  <<<<<- Return to cell #0 and decrement its value
  "while" loop ends
]
[
  Now cell #0 has value 0,
  cell #1 has value 70,
  cell #2 has value 100,
  cell #3 has value 110,
  and cell #4 has value 30:
  0 | 70 | 100 | 110 | 30 | 0 | ...
  Note that this is what is known as a "comment loop".
  In a comment loop, all special characters in Brainfuck are ignored
  PROVIDED THAT: the value of the current cell is 0
  AND: all opening and closing square brackets "[]" are balanced
]
>>++. Print "H"
>>+. Print "e"
>--. Print "l"
. Print "l"
+++. Print "o"
<<<<++. Print " " (spacebar character)
>>---. Print "W"
>>. Print "o"
+++. Print "r"
------. Print "l"
<-. Print "d"
<<<+. Print "!"`}, function(buffer) {
        expect(buffer.stdout).to.equal("Hello World!");
        done();
      });
    });
    it("should handle nested loops properly", function(done) {
      runner.run({language: 'bf', code: '++++++++>+++++++++<[->[->>+<<]>>[-<+<+>>]<<<]>>.'}, function(buffer) {
        expect(buffer.stdout).to.equal("H");
        done();
      });
    });
    it("should handle a relatively complex program without any issues", function(done) {
      runner.run({language: 'bf', code: '+++++++++++>+>>>>++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<+>>[-]]<<<<<<<]>>>>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++++++++++++++++++++++++++++++++++++++++++++++.[-]<<<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]'}, function(buffer) {
        expect(buffer.stdout).to.equal("1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89");
        done();
      });
    });
  });
  describe('Test Integration', function() {
    it('should handle "Hello World" program with no input provided', function(done) {
      runner.run({
        language: 'bf',
        code: '++++++++++[>+++>+++++++>+++++++++>++++++++++>+++++++++++<<<<<-]>>++.>>+.>--..+++.<<<<++.>>---.>>.+++.------.<-.<<<+.',
        fixture: 'Test.assertEquals(runBF(), "Hello World!");',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.not.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        expect(buffer.stdout).to.contain('Hello World!');
        done();
      });
    });
    it('should handle basic input and pass it to the BF program when provided', function(done) {
      runner.run({
        language: 'bf',
        code: ',>,<[->[->>+<<]>>[-<+<+>>]<<<]>>.',
        fixture: `Test.assertEquals(runBF(String.fromCharCode(9, 8)), "H");
Test.assertEquals(runBF(String.fromCharCode(3, 5)), String.fromCharCode(15));
Test.assertEquals(runBF(String.fromCharCode(15, 12)), String.fromCharCode(180));
Test.assertEquals(runBF(String.fromCharCode(1, 1)), String.fromCharCode(1));`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.not.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should have 8-bit cells that wrap as per the standard implementation', function(done) {
      runner.run({
        language: 'bf',
        code: ',>,<[->[->>+<<]>>[-<+<+>>]<<<]>>.',
        fixture: `Test.assertEquals(runBF(String.fromCharCode(32, 10)), String.fromCharCode(64));
Test.assertEquals(runBF(String.fromCharCode(48, 49)), String.fromCharCode(48));
Test.assertEquals(runBF(String.fromCharCode(127, 45)), String.fromCharCode(83));`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.not.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should set the cell under the pointer to -1 when EOF is reached which is 1 of the 3 possible standard implementations as described in http://www.hevanet.com/cristofd/brainfuck/epistle.html', function(done) {
      runner.run({
        language: 'bf',
        code: ',+[-.,+]',
        fixture: `Test.assertEquals(runBF("Codewars"), "Codewars");
Test.assertEquals(runBF("@jhoffner"), "@jhoffner");
Test.assertEquals(runBF("@kazk"), "@kazk");
Test.assertEquals(runBF("@donaldsebleung"), "@donaldsebleung");
Test.assertEquals(runBF("Brainf**k"), "Brainf**k");
Test.assertEquals(runBF("BF"), "BF");`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.not.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should provide a useful error message for invalid BF code', function(done) {
      runner.run({
        language: 'bf',
        code: '++++++++++[>+++>+++++++>+++++++++>++++[++++++>+++++++++++<<<<<-]>>++.>>+.>--..+++.<<<<++.>>---.>>.+++.------.<-.<<<+.',
        fixture: 'Test.assertEquals(runBF(), "Hello World!");',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stderr).to.contain('Error');
        done();
      });
    });
    it('should provide a useful error message for invalid BF code (2)', function(done) {
      runner.run({
        language: 'bf',
        code: '++++++++++[>+++>+++++++>+++++++++>++++++]++++>++++++]+++++<<<<<-]>>++.>>+.>--..+++.<<<<++.>>---.>>.+++.------.<-.<<<+.',
        fixture: 'Test.assertEquals(runBF(), "Hello World!");',
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stderr).to.contain('Error');
        done();
      });
    });
    it('should provide a useful error meesage for invalid BF code (3)', function(done) {
      runner.run({
        language: 'bf',
        code: ',>[[,<[->[->>+<<]>>[-<+<+[>>]<<<]>>[.[[',
        fixture: `Test.assertEquals(runBF(String.fromCharCode(9, 8)), "H");
Test.assertEquals(runBF(String.fromCharCode(3, 5)), String.fromCharCode(15));
Test.assertEquals(runBF(String.fromCharCode(15, 12)), String.fromCharCode(180));`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stderr).to.contain('Error');
        done();
      });
    });
    it('should provide a useful error meesage for invalid BF code (4)', function(done) {
      runner.run({
        language: 'bf',
        code: ',>,<[->[-]>>+<<]]>]>[-<]]]]]+<+>]>]]<<<]]>>].]',
        fixture: `Test.assertEquals(runBF(String.fromCharCode(9, 8)), "H");
Test.assertEquals(runBF(String.fromCharCode(3, 5)), String.fromCharCode(15));
Test.assertEquals(runBF(String.fromCharCode(15, 12)), String.fromCharCode(180));`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stderr).to.contain('Error');
        done();
      });
    });
    it('should handle a basic failed test properly', function(done) {
      runner.run({
        language: 'bf',
        code: '++++++++++[>++++++++++>+++++++++++>+++>++++++++++++<<<<-]>++++.---.>--..+++.>++.>-.<<.+++.------.<-.',
        fixture: `Test.assertEquals(runBF(), "Hello World!");`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.not.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should handle a mixture of passed and failed tests', function(done) {
      runner.run({
        language: 'bf',
        code: '+++++++++++++++.',
        fixture: `Test.assertEquals(runBF(String.fromCharCode(3, 5)), String.fromCharCode(15));
Test.assertEquals(runBF(String.fromCharCode(9, 8)), "H");
Test.assertEquals(runBF(String.fromCharCode(5, 3)), String.fromCharCode(15));
Test.assertEquals(runBF(String.fromCharCode(32, 10)), String.fromCharCode(64));`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.not.contain('<ERROR::>');
        done();
      });
    });
    it('should handle errors properly when spec methods are being used (JavaScript CW-2 Framework Specific Test)', function(done) {
      runner.run({
        language: 'bf',
        code: '++++++++++[>+++>+++++++>+++++++++>++++++]++++>++++++]+++++<<<<<-]>>++.>>+.>--..+++.<<<<++.>>---.>>.+++.------.<-.<<<+.',
        fixture: `Test.describe("Your BF Hello World Program", function() {
  var program = require('fs').readFileSync('/home/codewarrior/solution.txt', 'utf8');
  Test.it('should return the string "Hello World!"', function() {
    Test.assertEquals(runBF(), "Hello World!");
  });
  Test.it("should be shorter than 110 characters", function() {
    Test.expect(program.length < 110, "Your BF program must be shorter than 110 characters");
  });
  Test.it("should not contain any comments (i.e. non-command characters)", function() {
    Test.expect(!/[^+\\-.,\\[\\]<>]/.test(program), "Your program should not contain comments");
  });
});`,
        testFramework: 'cw-2'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<DESCRIBE::>');
        expect(buffer.stdout).to.contain('<IT::>');
        expect(buffer.stdout).to.contain('<PASSED::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.contain('<ERROR::>');
        done();
      });
    });
  });
});
