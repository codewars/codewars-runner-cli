var expect = require('chai').expect;
var runner = require('../runner');


describe('lua runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'lua',
        code: 'print(42)'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });
  });
});


describe('busted', function() {
  it('should handle basic code assertion', function(done) {
    runner.run({
      language: 'lua',
      solution: [
        `local kata = {}`,
        `function kata.add(a, b)`,
        `  return a + b`,
        `end`,
        `return kata`,
      ].join('\n'),
      fixture: [
        `local kata = require 'solution'`,
        `describe("add", function()`,
        `  it("should add numbers", function()`,
        `    assert.are.same(2, kata.add(1, 1))`,
        `  end)`,
        `end)`,
      ].join('\n')
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });

  it('should handle basic code assertion failure', function(done) {
    runner.run({
      language: 'lua',
      solution: [
        `local kata = {}`,
        `function kata.add(a, b)`,
        `  return a - b`,
        `end`,
        `return kata`,
      ].join('\n'),
      fixture: [
        `local kata = require 'solution'`,
        `describe("add", function()`,
        `  it("should add numbers", function()`,
        `    assert.are.same(2, kata.add(1, 1))`,
        `  end)`,
        `end)`,
      ].join('\n')
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<FAILED::>');
      done();
    });
  });

  it('should handle mixed success and failure', function(done) {
    runner.run({
      language: 'lua',
      solution: [
        `local kata = {}`,
        `function kata.add(a, b)`,
        `  return a - b`,
        `end`,
        `return kata`,
      ].join('\n'),
      fixture: [
        `local kata = require 'solution'`,
        `describe("add", function()`,
        `  it("should add numbers", function()`,
        `    assert.are.same(0, kata.add(0, 0))`,
        `  end)`,
        `  it("should add numbers", function()`,
        `    assert.are.same(2, kata.add(1, 1))`,
        `  end)`,
        `end)`,
      ].join('\n')
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      expect(buffer.stdout).to.contain('<FAILED::>');
      done();
    });
  });

  it('should handle error', function(done) {
    runner.run({
      language: 'lua',
      solution: [
        `local kata = {}`,
        `function kata.add(a, b)`,
        `  error("Error")`,
        `  return a - b`,
        `end`,
        `return kata`,
      ].join('\n'),
      fixture: [
        `local kata = require 'solution'`,
        `describe("add", function()`,
        `  it("should add numbers", function()`,
        `    assert.equal(2, kata.add(1, 1))`,
        `  end)`,
        `end)`,
      ].join('\n')
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<ERROR::>');
      expect(buffer.stdout).to.contain('./solution.lua:3: Error');
      done();
    });
  });

  it('should output nested describes', function(done) {
    runner.run({
      language: 'lua',
      solution: [
        `local kata = {}`,
        `function kata.add(a, b)`,
        `  return a + b`,
        `end`,
        `return kata`,
      ].join('\n'),
      fixture: [
        `-- from busted website`,
        `describe("Busted unit testing framework", function()`,
        `  describe("should be awesome", function()`,
        `    it("should be easy to use", function()`,
        `      assert.truthy("Yup.")`,
        `    end)`,
        ``,
        `    it("should have lots of features", function()`,
        `      assert.are.same({ table = "great"}, { table = "great" })`,
        `      assert.are_not.equal({ table = "great"}, { table = "great"})`,
        `      assert.truthy("this is a string")`,
        `      assert.True(1 == 1)`,
        `      assert.is_true(1 == 1)`,
        `      assert.falsy(nil)`,
        `      assert.has_error(function() error("Wat") end, "Wat")`,
        `    end)`,
        ``,
        `    it("should provide some shortcuts to common functions", function()`,
        `      assert.are.unique({{ thing = 1 }, { thing = 2 }, { thing = 3 }})`,
        `    end)`,
        ``,
        `    it("should have mocks and spies for functional tests", function()`,
        `      local kata = require 'solution'`,
        `      spy.on(kata, "add")`,
        `      kata.add(1, 1)`,
        `      assert.spy(kata.add).called()`,
        `      assert.spy(kata.add).called_with(1, 1)`,
        `    end)`,
        `  end)`,
        `end)`,
      ].join('\n')
    }, function(buffer) {
      const nested = [
        '<DESCRIBE::>',
        '  <DESCRIBE::>',
        '    <IT::><PASSED::><COMPLETEDIN::>',
        '    <IT::><PASSED::><COMPLETEDIN::>',
        '    <IT::><PASSED::><COMPLETEDIN::>',
        '    <IT::><PASSED::><COMPLETEDIN::>',
        '  <COMPLETEDIN::>',
        '<COMPLETEDIN::>'
      ].join('').replace(/\s/g, '');
      expect(buffer.stdout.match(/<(?:DESCRIBE|IT|PASSED|COMPLETEDIN)::>/g).join('')).to.equal(nested);
      done();
    });
  });

  it('should allow solution to log', function(done) {
    runner.run({
      language: 'lua',
      solution: [
        `local kata = {}`,
        `function kata.add(a, b)`,
        `  print(a)`,
        `  print(b)`,
        `  return a + b`,
        `end`,
        `return kata`,
      ].join('\n'),
      fixture: [
        `require 'busted.runner'()`,
        `local kata = require 'solution'`,
        ``,
        `describe("add", function()`,
        `  it("should add numbers", function()`,
        `    assert.are.same(3, kata.add(1, 2))`,
        `  end)`,
        `end)`,
      ].join('\n')
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      expect(buffer.stdout).to.contain('1');
      expect(buffer.stdout).to.contain('2');
      done();
    });
  });

  it('should support opts.setup', function(done) {
    runner.run({
      language: 'lua',
      setup: [
        `local setup = {}`,
        `function setup.add(a, b)`,
        `  return a + b`,
        `end`,
        `setup.answer = 42`,
        `return setup`,
      ].join('\n'),
      solution: [
        `local setup = require 'setup'`,
        `return {`,
        `  add = setup.add,`,
        `  ans = setup.answer`,
        `}`,
      ].join('\n'),
      fixture: [
        `local kata = require 'solution'`,
        `describe("add", function()`,
        `  it("should add numbers", function()`,
        `    assert.are.same(2, kata.add(1, 1))`,
        `  end)`,
        `  it("should have 42", function()`,
        `    assert.are.same(42, kata.ans)`,
        `  end)`,
        `end)`,
      ].join('\n')
    }, function(buffer) {
      expect(buffer.stdout).to.contain('<PASSED::>');
      done();
    });
  });

  it('should have output format commands on independent lines', function(done) {
    runner.run({
      language: 'lua',
      solution: [
        `local kata = {}`,
        `function kata.add(a, b)`,
        `  io.write(a)`,
        `  io.write(b)`,
        `  return a + b`,
        `end`,
        `return kata`,
      ].join('\n'),
      fixture: [
        `require 'busted.runner'()`,
        `local kata = require 'solution'`,
        ``,
        `describe("add", function()`,
        `  it("should add numbers", function()`,
        `    assert.are.same(3, kata.add(1, 2))`,
        `  end)`,
        `end)`,
      ].join('\n')
    }, function(buffer) {
      expect(buffer.stdout).to.contain('12');
      expect(buffer.stdout).to.contain('\n<PASSED::>');
      done();
    });
  });
});

describe('Examples', function() {
  runner.assertCodeExamples('lua');
});
