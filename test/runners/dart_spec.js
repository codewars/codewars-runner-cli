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
        expect(buffer.stdout).to.equal('Bam\n');
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
        expect(buffer.stderr).to.contain('semicolon expected');
        done();
      });
    });

    it('should error when trying to add imports to the code section', function(done) {
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
        expect(buffer.stderr).to.contain(`unexpected token 'import'`);
        done();
      });
    });

    it('should allow imports inside setup', function(done) {
      runner.run({
        language: 'dart',
        setup: `import 'dart:async';`,
        code: `
          main() async {
            var str = await new Future.value('Bam');
            print(str);
          }
        `
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`Bam\n`);
        done();
      });
    });

    it('should handle basic tests', function(done) {
      runner.run({
        language: 'dart',
        code: `testFunction() => 50;`,
        fixture: `
          test('function returns 50', () {
            expect(testFunction(), equals(50));
          });`,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`All tests passed`);
        done();
      });
    });

    it('should handle advanced tests with setup', function(done) {
      runner.run({
        language: 'dart',
        setup: `import 'dart:async';`,
        code: `testFunction() => new Future.value(50);`,
        fixture: `
          test('function returns 50', () async {
            expect(await testFunction(), equals(50));
          });`,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`All tests passed`);
        done();
      });
    });

  });
});
