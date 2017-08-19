var expect = require('chai').expect;
var runner = require('../runner');


describe('coffeescript runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('coffeescript');

    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'coffeescript', code: 'console.log 42'}, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });

    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'coffeescript',
        code: [
          'fill = (container, liquid = "coffee") ->',
          '  "Filling the #{container} with #{liquid}..."',
          '',
          'console.log fill("cup")',
        ].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Filling the cup with coffee...\n');
        done();
      });
    });

    describe('cw-2', function() {
      it('should handle a basic assertion', function(done) {
        runner.run({language: 'coffeescript', code: 'a = 1', fixture: 'Test.expect a == 1', testFramework: 'cw-2'}, function(buffer) {
          expect(buffer.stdout).to.equal('\n<PASSED::>Test Passed\n');
          done();
        });
      });
    });

    it('should handle comments as fixture', function(done) {
      runner.run({language: 'coffeescript', code: 'console.log(42)', fixture: '#', testFramework: 'cw-2'}, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });

    it('should handle a basic failed test', function(done) {
      runner.run({language: 'coffeescript', code: 'a = 1', fixture: 'Test.expect(a == 2)', testFramework: 'cw-2'}, function(buffer) {
        expect(buffer.stdout).to.equal('\n<FAILED::>Value is not what was expected\n');
        done();
      });
    });

    it('should handle logging objects', function(done) {
      runner.run({language: 'coffeescript', code:'console.log {a: 1}', testFramework: 'cw-2'}, function(buffer) {
        expect(buffer.stdout).to.equal('{ a: 1 }\n');
        done();
      });
    });
  });
});
