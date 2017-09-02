"use strict";

const expect = require('chai').expect;
const runner = require('../runner');
const fs = require('fs');
const path = require('path');
const yaml = require('js-yaml');
const exec = require('child_process').exec;
const prewarm = require('../../lib/utils/when-prewarmed').setupMocha;

describe('groovy-runner', function() {
  this.timeout(0);

  prewarm();

  afterEach(function cleanup(done) {
    exec('rm -rf /home/codewarrior/project', function(err) {
      if (err) return done(err);
      done();
    });
  });

  describe('running', function() {

    it('should handle basic code evaluation (script)', function(done) {
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
    it('should handle basic assertion', function(done) {
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

  describe('testing with Spock', function() {
    it('should handle basic assertion', function(done) {
      runner.run({
        language: 'groovy',
        testFramework: 'spock',
        solution: [
          `class Adder {`,
          `  static def add(a, b) { return a + b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import spock.lang.Specification`,
          ``,
          `class AdderSpec extends Specification {`,
          `  def "Adder.add returns the sum"() {`,
          `    expect:`,
          `    Adder.add(1, 1) == 2`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should handle basic assertion with package declared', function(done) {
      runner.run({
        language: 'groovy',
        testFramework: 'spock',
        solution: [
          `package kata`,
          ``,
          `class Adder {`,
          `  static def add(a, b) { return a + b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import spock.lang.Specification`,
          ``,
          `class AdderSpec extends Specification {`,
          `  def "Adder.add returns the sum"() {`,
          `    expect:`,
          `    kata.Adder.add(1, 1) == 2`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should handle basic assertion failure', function(done) {
      runner.run({
        language: 'groovy',
        testFramework: 'spock',
        solution: [
          `class Adder {`,
          `  static def add(a, b) { return a - b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import spock.lang.Specification`,
          ``,
          `class AdderSpec extends Specification {`,
          `  def "Adder.add returns the sum"() {`,
          `    expect:`,
          `    Adder.add(1, 1) == 2`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<FAILED::>');
        done();
      });
    });

    it('should handle basic assertion failure by error', function(done) { // No <ERROR::>
      runner.run({
        language: 'groovy',
        testFramework: 'spock',
        solution: [
          `class Adder {`,
          `  static def add(a, b) { return a / b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import spock.lang.Specification`,
          ``,
          `class AdderSpec extends Specification {`,
          `  def "Adder.add returns the sum"() {`,
          `    expect:`,
          `    Adder.add(1, 0) == 1`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).not.to.contain('<ERROR::>');
        expect(buffer.stdout).to.contain('<FAILED::>');
        expect(buffer.stdout).to.contain('java.lang.ArithmeticException: Division by zero');
        done();
      });
    });

    it('can have multiple suites', function(done) {
      runner.run({
        language: 'groovy',
        testFramework: 'spock',
        solution: [
          `class Adder {`,
          `  static def add(a, b) { return a + b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import spock.lang.Specification`,
          ``,
          `class AdderSpec1 extends Specification {`,
          `  def "Adder.add returns the sum"() {`,
          `    expect:`,
          `    Adder.add(1, 1) == 2`,
          `  }`,
          `}`,
          `class AdderSpec2 extends Specification {`,
          `  def "Adder.add returns the sum"() {`,
          `    expect:`,
          `    Adder.add(-1, -1) == -2`,
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
      runner.run({
        language: 'groovy',
        testFramework: 'spock',
        solution: [
          `class Adder {`,
          `  static def add(a, b) {`,
          '    println "a = ${a}, b = ${b}"',
          `    return a + b`,
          `  }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import spock.lang.Specification`,
          ``,
          `class AdderSpec extends Specification {`,
          `  def "Adder.add returns the sum"() {`,
          `    expect:`,
          `    Adder.add(1, 1) == 2`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('a = 1, b = 1');
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should support @Unroll', function(done) {
      runner.run({
        language: 'groovy',
        testFramework: 'spock',
        solution: [
          `class Adder {`,
          `  static def add(a, b) { return a + b }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import spock.lang.Specification`,
          `import spock.lang.Unroll`,
          ``,
          `class AdderSpec extends Specification {`,
          `  @Unroll`,
          `  def "Adder.add(#a, #b) returns the sum #c"() {`,
          `    expect:`,
          `    Adder.add(a, b) == c`,
          ``,
          `    where:`,
          `    a | b || c`,
          `    1 | 1 || 2`,
          `    1 | 2 || 3`,
          `    1 | 3 || 4`,
          `    1 | 4 || 5`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        const expected = [
          '<DESCRIBE::>',
          '  <IT::><PASSED::><COMPLETEDIN::>',
          '  <IT::><PASSED::><COMPLETEDIN::>',
          '  <IT::><PASSED::><COMPLETEDIN::>',
          '  <IT::><PASSED::><COMPLETEDIN::>',
          '<COMPLETEDIN::>',
        ].join('').replace(/\s/g, '');
        expect(buffer.stdout.match(/<(?:DESCRIBE|IT|PASSED|FAILED|COMPLETEDIN)::>/g).join('')).to.equal(expected);
        done();
      });
    });
  });

  describe('Example Challenges', function() {
    forEachExamples(function(framework, name, example) {
      describe(`${framework}: "${name}" example`, function() {
        it('should define an initial code block', function() {
          expect(example.initial).to.be.a('string');
        });

        it('should have a passing solution', function(done) {
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
