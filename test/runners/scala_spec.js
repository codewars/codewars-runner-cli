var expect = require('chai').expect;
var runner = require('../runner');

describe('scala runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      var code = [
        'object HelloWorld {',
        'def main(args: Array[String]) {',
        'println("Hello, world!")',
        '}',
        '}'
      ].join('\n');

      runner.run({language: 'scala', code: code}, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.equal("Hello, world!\n");
        done();
      });
    });
    it('should handle setup code and imports', function(done) {
      runner.run({
        language: 'scala',
        setup: [
          'package problems',
          'object Lists {',
          'def last(list: List[Any]): Option[Any] = list match {',
          'case Nil      => None',
          'case x :: Nil => Some(x)',
          'case _ :: xs  => last(xs)',
          '}',
          '}'
        ].join('\n'),
        code: [
          'import problems.Lists._',
          'object Test extends App {',
          '  println("Starting tests...")',
          '  println(last(List(1,2,3,4,5)))',
          '}'
        ].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.contain('Starting tests...');
        expect(buffer.stdout).to.contain('5');
        done();
      });
    });
  });
});
