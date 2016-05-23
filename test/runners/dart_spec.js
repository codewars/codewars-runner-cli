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
        expect(buffer.stdout).to.contain(`<IT::>function returns 50`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
        done();
      });
    });

    it('should handle advanced tests with setup', function(done) {
      runner.run({
        language: 'dart',
        setup: `import 'dart:async';`,
        code: `
          testFunction() => new Future.value(50);
          otherFunction() => 32;
          `,
        fixture: `
          test('function returns 50', () async {
            expect(await testFunction(), equals(50));
          });

          test('other function returns 32', () {
            expect(otherFunction(), equals(32));
          });
          `,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<IT::>function returns 50`);
        expect(buffer.stdout).to.contain(`<PASSED::>Test Passed`);
        expect(buffer.stdout).to.contain(`<IT::>other function returns 32`);
        expect(buffer.stdout).to.not.contain(`<FAILED::>Test Failed`);
        done();
      });
    });

    it('should handle errors in testIntegration', function(done) {
      runner.run({
        language: 'dart',
        setup: `import 'dart:async';`,
        code: `
          testFunction() => new Future.value(50);
          otherFunction() => 32;
          `,
        fixture: `
          test('function returns 50, () async {
            expect(await testFunction(), equals(50));
          });

          test('other function returns 32', () {
            expect(otherFunction(), equals(32));
          });
          `,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<ERROR::>`);
        expect(buffer.stdout).to.contain(`unterminated string literal`);
        done();
      });
    });

    it('should error for spec when trying to add imports to the code section', function(done) {
      runner.run({
        language: 'dart',
        code: `
          import 'dart:async';
          testFunction() async => 50;
        `,
        fixture: `
          test('function returns 50', () {
            expect(testFunction(), equals(50));
          });`,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<ERROR::>`);
        expect(buffer.stdout).to.contain(`unexpected token 'import'`);
        done();
      });
    });

  });
});
