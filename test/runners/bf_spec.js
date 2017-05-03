var expect = require('chai').expect;
var runner = require('../runner');

describe("BF Runner", function() {
  describe("Basic Run", function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'bf', code: '++++++++++[>+++>+++++++>+++++++++>++++++++++>+++++++++++<<<<<-]>>++.>>+.>--..+++.<<<<++.>>---.>>.+++.------.<-.<<<+.'}, function(buffer) {
        expect(buffer.stdout).to.equal("Hello World!");
        done();
      });
    });
    it('should ignore anything not in "+-.,<>[]" as comments', function(done) {
      runner.run({language: 'bf', code: "++++++++++ Initialize cell #0 to 10\
[\
  \"while\" loop begins\
  >+++ Go to cell #1 and add 3\
  >+++++++ Go to cell #2 and add 7\
  >+++++++++ Go to cell #3 and add 9\
  >++++++++++ Go to cell #4 and add 10\
  >+++++++++++ Go to cell #5 and add 11\
  <<<<<- Return to cell #0 and decrement its value\
  \"while\" loop ends\
]\
[\
  Now cell #0 has value 0,\
  cell #1 has value 70,\
  cell #2 has value 100,\
  cell #3 has value 110,\
  and cell #4 has value 30:\
  0 | 70 | 100 | 110 | 30 | 0 | ...\
  Note that this is what is known as a \"comment loop\".\
  In a comment loop, all special characters in Brainfuck are ignored\
  PROVIDED THAT: the value of the current cell is 0\
  AND: all opening and closing square brackets \"[]\" are balanced\
]\
>>++. Print \"H\"\
>>+. Print \"e\"\
>--. Print \"l\"\
. Print \"l\"\
+++. Print \"o\"\
<<<<++. Print \" \" (spacebar character)\
>>---. Print \"W\"\
>>. Print \"o\"\
+++. Print \"r\"\
------. Print \"l\"\
<-. Print \"d\"\
<<<+. Print \"!\""}, function(buffer) {
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
  });
});
