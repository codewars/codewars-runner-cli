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

    it('should handle invalid code', function(done) {
      runner.run({
        language: 'dart',
        setup: `import 'dart:async';`,
        code: `
          class Person {
            String firstName;
            String lastName;

            Person(this.firstName,this.lastName
          }
          `,
        fixture: `
          test('can create a new Person object, () async {
            expect(new Person('Bill','Smith'), new isInstanceOf<Person>());
          });
          `,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<ERROR::>`);
        expect(buffer.stdout).to.contain(`unbalanced`);
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

    it('should be a simple failed test example', function(done) {
      runner.run({
        language: 'dart',
        setup: `import 'dart:async';`,
        code: `
          returnFive() => 5;
          `,
        fixture: `
        test('Should fail', () {
          expect(returnFive(), equals(4));
        });
          `,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<FAILED::>`);
        done();
      });
    });

    it('should return an error message about missing semicolon', function(done) {
      runner.run({
        language: 'dart',
        setup: `import 'dart:async';`,
        code: `
          returnFive() => 5;
          `,
        fixture: `
        test('Should fail', () {
          expect(returnFive(), equals(4))
        });
          `,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<ERROR::>`);
        expect(buffer.stdout).to.contain('semicolon expected');
        done();
      });
    });

    it('should return an error message because there is some random text', function(done) {
      runner.run({
        language: 'dart',
        setup: `import 'dart:async';`,
        code: `
          returnFive() => 5;
          `,
        fixture: `
        random stuffs..
        test('Should fail', () {
          expect(returnFive(), equals(4))
        });
          `,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<ERROR::>`);
        expect(buffer.stdout).to.not.contain('undefined');
        done();
      });
    });

    it('should return an error message because there is more random text', function(done) {
      runner.run({
        language: 'dart',
        setup: `import 'dart:async';`,
        code: `
          returnFive() => 5;
          `,
        fixture: `
        what is a test...
          `,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<ERROR::>`);
        expect(buffer.stdout).to.not.contain('undefined');
        done();
      });
    });

    it('should return an error message because the code is invalid', function(done) {
      runner.run({
        language: 'dart',
        setup: `import 'dart:async';`,
        code: `
          what is dart programming??
          `,
        fixture: `
          test('Should fail', () {
            expect(returnFive(), equals(4));
          });
          `,
        testFramework: 'test'
      }, function(buffer) {
        expect(buffer.stdout).to.contain(`<ERROR::>`);
        expect(buffer.stdout).to.not.contain('undefined');
        done();
      });
    });

  });
});
