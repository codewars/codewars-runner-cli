"use strict";

const expect = require('chai').expect;
const runner = require('../runner');
const fs = require('fs');
const path = require('path');
const yaml = require('js-yaml');
const exec = require('child_process').exec;

describe('groovy-runner', function() {
  before(function startDaemon(done) {
    this.timeout(0);
    exec('gradle --daemon --offline test', {
      cwd: '/runner/frameworks/gradle',
    }, (err) => {
      if (err) return done(err);
      console.log('Started Gradle daemon');
      done();
    });
  });

  describe('running', function() {
    afterEach(function cleanup(done) {
      exec('rm -rf /home/codewarrior/project', function(err) {
        if (err) return done(err);
        done();
      });
    });

    it('should handle basic code evaluation (script)', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        solution: [
          `println 'Hello World! from Groovy'`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Hello World! from Groovy\n');
        done();
      });
    });

    it('should handle basic code evaluation (static void main)', function(done) {
      // class needs to be `Main`
      this.timeout(0);
      runner.run({
        language: 'groovy',
        solution: [
          `class Main {`,
          `  static void main(String... args) {`,
          `    println 'Hello World! from Groovy'`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Hello World! from Groovy\n');
        done();
      });
    });

    it('should handle setup code', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        setup: [
          `class Greeter {`,
          `  static String greet(String name) {`,
          '    return "Hello, ${name}"',
          `  }`,
          `}`,
        ].join('\n'),
        solution: [
          `println Greeter.greet("Joe")`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Hello, Joe\n');
        done();
      });
    });

    it('should handle setup code with package declaration', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        setup: [
          `package setup`,
          `class Greeter {`,
          `  static String greet(String name) {`,
          '    return "Hello, ${name}"',
          `  }`,
          `}`,
        ].join('\n'),
        solution: [
          `println setup.Greeter.greet("Joe")`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Hello, Joe\n');
        done();
      });
    });

    it('should handle setup code with arbitrary package declaration', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        setup: [
          `package a.b.c.d.e`,
          `class Greeter {`,
          `  static String greet(String name) {`,
          '    return "Hello, ${name}"',
          `  }`,
          `}`,
        ].join('\n'),
        solution: [
          `println a.b.c.d.e.Greeter.greet("Joe")`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Hello, Joe\n');
        done();
      });
    });
  });

  describe('testing with JUnit', function() {
    afterEach(function cleanup(done) {
      exec('rm -rf /home/codewarrior/project', function(err) {
        if (err) return done(err);
        done();
      });
    });

    it('should handle basic assertion', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        testFramework: 'junit4',
        solution: [
          `class Adder {`,
          `  static def add(a, b) { return a + b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.Test`,
          ``,
          `class TestAdd {`,
          `  @Test`,
          `  void addTest() {`,
          `    assert Adder.add(1, 1) == 2`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should handle basic assertion with package declared', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        testFramework: 'junit4',
        solution: [
          `package kata`,
          ``,
          `class Adder {`,
          `  static def add(a, b) { return a + b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.Test`,
          ``,
          `class TestAdd {`,
          `  @Test`,
          `  void addTest() {`,
          `    assert kata.Adder.add(1, 1) == 2`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should handle basic assertion failure', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        testFramework: 'junit4',
        solution: [
          `class Adder {`,
          `  static def add(a, b) { return a - b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.Test`,
          ``,
          `class TestAdd {`,
          `  @Test`,
          `  void addTest() {`,
          `    assert Adder.add(1, 1) == 2`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<FAILED::>');
        done();
      });
    });

    it('should handle basic assertion failure by error', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        testFramework: 'junit4',
        solution: [
          `class Adder {`,
          `  static def add(a, b) { return a / b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.Test`,
          ``,
          `class TestAdd {`,
          `  @Test`,
          `  void addTest() {`,
          `    assert Adder.add(1, 0) == 1`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<ERROR::>');
        done();
      });
    });


    it('should handle basic assertion failure with message', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        testFramework: 'junit4',
        solution: [
          `class Adder {`,
          `  static def add(a, b) { return a - b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.Test`,
          ``,
          `class TestAdd {`,
          `  @Test`,
          `  void addTest() {`,
          `    assert Adder.add(1, 1) == 2 : "add(1, 1) should return 2"`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.contain('add(1, 1) should return 2');
        done();
      });
    });


    it('can have multiple suites', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        testFramework: 'junit4',
        solution: [
          `class Adder {`,
          `  static def add(a, b) { return a + b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.Test`,
          ``,
          `class TestAddPositives {`,
          `  @Test`,
          `  void addTest() {`,
          `    assert Adder.add(1, 1) == 2`,
          `  }`,
          `}`,
          `class TestAddNegatives {`,
          `  @Test`,
          `  void addTest() {`,
          `    assert Adder.add(-1, -1) == -2`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        const expected = [
          '<DESCRIBE::>',
          '  <IT::><PASSED::><COMPLETEDIN::>',
          '<COMPLETEDIN::>',
          '<DESCRIBE::>',
          '  <IT::><PASSED::><COMPLETEDIN::>',
          '<COMPLETEDIN::>',
        ].join('').replace(/\s/g, '');
        expect(buffer.stdout.match(/<(?:DESCRIBE|IT|PASSED|FAILED|COMPLETEDIN)::>/g).join('')).to.equal(expected);
        done();
      });
    });


    it('should allow logging', function(done) {
      this.timeout(0);
      runner.run({
        language: 'groovy',
        testFramework: 'junit4',
        solution: [
          `class Adder {`,
          `  static def add(a, b) {`,
          '    println "a = ${a}, b = ${b}"',
          `    return a + b`,
          `  }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.Test`,
          ``,
          `class TestAdd {`,
          `  @Test`,
          `  void addTest() {`,
          `    assert Adder.add(1, 1) == 2`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('a = 1, b = 1');
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });
  });

  describe.skip('testing with Spock', function() {
    // TODO Add tests for Spock
  });

  describe('Example Challenges', function() {
    afterEach(function cleanup(done) {
      exec('rm -rf /home/codewarrior/project', function(err) {
        if (err) return done(err);
        done();
      });
    });

    forEachExamples(function(framework, name, example) {
      describe(`${framework}: "${name}" example`, function() {
        it('should define an initial code block', function() {
          expect(example.initial).to.be.a('string');
        });

        it('should have a passing solution', function(done) {
          this.timeout(0);
          runner.run({
            language: 'groovy',
            setup: example.setup,
            solution: example.answer,
            fixture: example.fixture,
            testFramework: framework,
          }, function(buffer) {
            expect(buffer.stdout).to.contain('<PASSED::>');
            expect(buffer.stdout).to.not.contain('<FAILED::>');
            expect(buffer.stdout).to.not.contain('<ERROR::>');
            done();
          });
        });
      });
    });
  });
});


function forEachExamples(cb) {
  const examples = yaml.safeLoad(fs.readFileSync(path.join(__dirname, `../../examples/groovy.yml`), 'utf8'));
  for (const framework of Object.keys(examples)) {
    for (const example of Object.keys(examples[framework])) {
      cb(framework, example, examples[framework][example]);
    }
  }
}
