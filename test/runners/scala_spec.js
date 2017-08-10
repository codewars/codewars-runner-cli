"use strict";

const expect = require('chai').expect;
const runner = require('../runner');
const fs = require('fs');
const path = require('path');
const yaml = require('js-yaml');
const exec = require('child_process').exec;

describe('scala-runner', function() {
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

    it('should handle basic code evaluation', function(done) {
      this.timeout(0);
      runner.run({
        language: 'scala',
        solution: [
          `object Main extends App {`,
          `  println("Hello World! from Scala")`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.equal("Hello World! from Scala\n");
        done();
      });
    });

    it('should handle setup code', function(done) {
      this.timeout(0);
      runner.run({
        language: 'scala',
        setup: [
          `package example`,
          ``,
          `object Adder {`,
          `  def add(a: Int, b: Int) = a + b`,
          `}`,
        ].join('\n'),
        solution: [
          `import example.Adder._`,
          `object Main extends App {`,
          `  println(add(1, 1))`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.equal('2\n');
        done();
      });
    });

    it('should handle setup code in arbitrary package', function(done) {
      this.timeout(0);
      runner.run({
        language: 'scala',
        setup: [
          `package a.b.c.d.example`,
          ``,
          `object Adder {`,
          `  def add(a: Int, b: Int) = a + b`,
          `}`,
        ].join('\n'),
        solution: [
          `import a.b.c.d.example.Adder._`,
          `object Main extends App {`,
          `  println(add(1, 1))`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.equal('2\n');
        done();
      });
    });
  });

  describe('testing with ScalaTest', function() {
    afterEach(function cleanup(done) {
      exec('rm -rf /home/codewarrior/project', function(err) {
        if (err) return done(err);
        done();
      });
    });

    it('should handle basic assertion', function(done) {
      this.timeout(0);
      runner.run({
        language: 'scala',
        testFramework: 'scalatest',
        solution: [
          `package example`,
          ``,
          `object Adder {`,
          `  def add(a: Int, b: Int) = a + b`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.runner.RunWith`,
          `import org.scalatest.junit.JUnitRunner`,
          `import org.scalatest.FunSuite`,
          ``,
          `import example.Adder._`,
          ``,
          `@RunWith(classOf[JUnitRunner])`,
          `class AdditionSuite extends FunSuite {`,
          `  test("add(1, 1)") {`,
          `    assert(add(1, 1) == 2)`,
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
        language: 'scala',
        testFramework: 'scalatest',
        solution: [
          `package example`,
          ``,
          `object Adder {`,
          `  def add(a: Int, b: Int) = a - b`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.runner.RunWith`,
          `import org.scalatest.junit.JUnitRunner`,
          `import org.scalatest.FunSuite`,
          ``,
          `import example.Adder._`,
          ``,
          `@RunWith(classOf[JUnitRunner])`,
          `class AdditionSuite extends FunSuite {`,
          `  test("add(1, 1)") {`,
          `    assert(add(1, 1) == 2)`,
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
        language: 'scala',
        testFramework: 'scalatest',
        solution: [
          `package example`,
          ``,
          `object Adder {`,
          `  def add(a: Int, b: Int) = a / b`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.runner.RunWith`,
          `import org.scalatest.junit.JUnitRunner`,
          `import org.scalatest.FunSuite`,
          ``,
          `import example.Adder._`,
          ``,
          `@RunWith(classOf[JUnitRunner])`,
          `class AdditionSuite extends FunSuite {`,
          `  test("add(1, 0)") {`,
          `    assert(add(1, 0) == 1)`,
          `  }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<ERROR::>');
        done();
      });
    });

    it('can have multiple suites', function(done) {
      this.timeout(0);
      runner.run({
        language: 'scala',
        testFramework: 'scalatest',
        solution: [
          `package example`,
          ``,
          `object Adder {`,
          `  def add(a: Int, b: Int) = a + b`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.runner.RunWith`,
          `import org.scalatest.junit.JUnitRunner`,
          `import org.scalatest.FunSuite`,
          ``,
          `import example.Adder._`,
          ``,
          `@RunWith(classOf[JUnitRunner])`,
          `class PosAdditionSuite extends FunSuite {`,
          `  test("add(1, 1)") {`,
          `    assert(add(1, 1) == 2)`,
          `  }`,
          `}`,
          ``,
          `@RunWith(classOf[JUnitRunner])`,
          `class NegAdditionSuite extends FunSuite {`,
          `  test("add(-1, -1)") {`,
          `    assert(add(-1, -1) == -2)`,
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
        language: 'scala',
        testFramework: 'scalatest',
        solution: [
          `package example`,
          ``,
          `object Adder {`,
          `  def add(a: Int, b: Int): Int = {`,
          '    println(s"a = $a, b = $b")',
          `    a + b`,
          `  }`,
          `}`,
        ].join('\n'),
        fixture: [
          `import org.junit.runner.RunWith`,
          `import org.scalatest.junit.JUnitRunner`,
          `import org.scalatest.FunSuite`,
          ``,
          `import example.Adder._`,
          ``,
          `@RunWith(classOf[JUnitRunner])`,
          `class AdditionSuite extends FunSuite {`,
          `  test("add(1, 1)") {`,
          `    assert(add(1, 1) == 2)`,
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

  describe.skip('testing with JUnit', function() {
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
            language: 'scala',
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
  const examples = yaml.safeLoad(fs.readFileSync(path.join(__dirname, `../../examples/scala.yml`), 'utf8'));
  for (const framework of Object.keys(examples)) {
    for (const example of Object.keys(examples[framework])) {
      cb(framework, example, examples[framework][example]);
    }
  }
}
