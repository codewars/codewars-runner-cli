var expect = require('chai').expect;
var runner = require('../runner');

describe("BF Runner", function () {
  describe(".run", function () {
    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'bf', code: '++++++++++[>+++>+++++++>+++++++++>++++++++++>+++++++++++<<<<<-]>>++.>>+.>--..+++.<<<<++.>>---.>>.+++.------.<-.<<<+.'}, function(buffer) {
        expect(buffer.stdout).to.equal("Hello World!");
        done();
      });
    });
  });
});
