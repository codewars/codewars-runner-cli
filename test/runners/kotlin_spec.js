var expect = require('chai').expect;
var runner = require('../runner');


describe('kotlin runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'kotlin',
        code: `
fun main(args : Array<String>) {
    println(42)
}
`
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });
    it('should handle setup code', function(done) {
      runner.run({
        language: 'kotlin',
        setup: `
fun greet(name : String) : String {
    return "Hello, " + name
}
`,
        code: `
fun main(args : Array<String>) {
    println(greet("Joe"))
}
`
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Hello, Joe\n');
        done();
      });
    });
  });
});
