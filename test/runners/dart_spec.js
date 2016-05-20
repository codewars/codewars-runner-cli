const expect = require('chai').expect;
const runner = require('../runner');

describe('dart runner', function() {
  describe('.run', function() {
    // Basic code run
    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'dart',
        code: `
          main() {
            print('Bam');
          }
        `
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Bam');
        done();
      });
    });

    it('should handle invalid code', function(done) {
      runner.run({
        language: 'dart',
        code: `
          main() {
            print('Bam') // Looks like a semi-colon is missing...
          }
        `
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Bam');
        done();
      });
    });

    it('should run async code', function(done) {
      runner.run({
        language: 'dart',
        code: `
          import 'dart:async';
          main() async {
            var str = await new Future.value('Bam');
            print(str);
          }
        `
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Bam');
        done();
      });
    });

  });
});
