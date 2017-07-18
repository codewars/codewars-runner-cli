"use strict";

const expect = require('chai').expect;
const runner = require('../runner');

describe('R runner', function() {
  describe('running', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'r',
        code: 'cat("Hello World!\\n")'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Hello World!\n');
        done();
      });
    });

    it('should support setup code', function(done) {
      runner.run({
        language: 'r',
        setup: 'add <- function(a, b) { a + b }',
        solution: [
          `source("setup.R")`,
          `cat(sprintf("%d\\n", add(1, 1)))`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.equal('2\n');
        done();
      });
    });
  });

  describe('testing', function() {
    it('should handle basic assertion', function(done) {
      runner.run({
        language: 'r',
        solution: 'add <- function(a, b) { a + b }',
        fixture: [
          `test_that("addition works", {`,
          `  expect_equal(add(1, 1), 2)`,
          `})`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('<PASSED::>');
        done();
      });
    });

    it('should handle basic assertion failure', function(done) {
      runner.run({
        language: 'r',
        solution: 'add <- function(a, b) { a - b }',
        fixture: [
          `test_that("addition works", {`,
          `  expect_equal(add(1, 1), 2)`,
          `})`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('<FAILED::>');
        done();
      });
    });

    it('should handle basic assertion failure from error', function(done) {
      runner.run({
        language: 'r',
        solution: 'add <- function(a, b) { "1" + a + b }',
        fixture: [
          `test_that("addition works", {`,
          `  expect_equal(add(1, 1), 2)`,
          `})`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('<ERROR::>');
        done();
      });
    });

    it('should support context', function(done) {
      runner.run({
        language: 'r',
        solution: 'add <- function(a, b) { a + b }',
        fixture: [
          `context("Addition")`,
          `test_that("addition works", {`,
          `  expect_equal(add(1, 1), 2)`,
          `})`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('<DESCRIBE::>');
        done();
      });
    });

    it('should handle success and failure in single test case', function(done) {
      runner.run({
        language: 'r',
        solution: 'add <- function(a, b) { a - b }',
        fixture: [
          `test_that("addition works", {`,
          `  expect_equal(add(0, 0), 0)`,
          `  expect_equal(add(1, 1), 2)`,
          `})`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('<PASSED::>');
        expect(buffer.stdout).to.include('<FAILED::>');
        done();
      });
    });

    it('should support setup code', function(done) {
      runner.run({
        language: 'r',
        setup: 'f <- function(a, b) { a + b }',
        solution: [
          `source("setup.R")`,
          `add <- f`,
        ].join('\n'),
        fixture: [
          `test_that("addition works", {`,
          `  expect_equal(add(1, 1), 2)`,
          `})`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.include('<PASSED::>');
        done();
      });
    });

    it('should allow logging', function(done) {
      runner.run({
        language: 'r',
        solution: [
          `add <- function(a, b) {`,
          `  cat(sprintf("a: %d\\n", a))`,
          `  a + b`,
          `}`,
          `sub <- function(a, b) {`,
          `  cat(sprintf("a: %d\\n", a))`,
          `  a + b`,
          `}`,
        ].join('\n'),
        fixture: [
          `test_that("addition works", {`,
          `  expect_equal(add(1, 1), 2)`,
          `})`,
          `test_that("subtraction works", {`,
          `  expect_equal(sub(2, 1), 1)`,
          `})`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('a: 1');
        expect(buffer.stdout).to.contain('a: 2');
        done();
      });
    });

    it('should output correct format', function(done) {
      runner.run({
        language: 'r',
        solution: 'add <- function(a, b) { a + b }',
        fixture: [
          `context("Addition")`,
          `test_that("addition works", {`,
          `  expect_equal(add(1, 1), 2)`,
          `})`,
        ].join('\n'),
      }, function(buffer) {
        const expected = [
          '<DESCRIBE::>',
          '  <IT::><PASSED::><COMPLETEDIN::>',
          '<COMPLETEDIN::>'
        ].join('').replace(/\s/g, '');
        expect(buffer.stdout.match(/<(?:DESCRIBE|IT|PASSED|FAILED|COMPLETEDIN)::>/g).join('')).to.equal(expected);
        done();
      });
    });

    it('should support multiple assertions', function(done) {
      runner.run({
        language: 'r',
        solution: [
          `add <- function(a, b) {`,
          `  if (b == 3) {`,
          `    a - b`,
          `  } else {`,
          `    a + b`,
          `  }`,
          `}`
        ].join('\n'),
        fixture: [
          `test_that("addition works", {`,
          `  expect_equal(add(1, 1), 2)`,
          `  expect_equal(add(1, 2), 3)`,
          `  expect_equal(add(1, 3), 4)`,
          `  expect_equal(add(1, 4), 5)`,
          `  expect_equal(add(1, 5), 6)`,
          `})`,
        ].join('\n'),
      }, function(buffer) {
        const expected = [
          '<IT::>',
          '  <PASSED::>',
          '  <PASSED::>',
          '  <FAILED::>',
          '  <PASSED::>',
          '  <PASSED::>',
          '<COMPLETEDIN::>'
        ].join('').replace(/\s/g, '');
        expect(buffer.stdout.match(/<(?:DESCRIBE|IT|PASSED|FAILED|COMPLETEDIN)::>/g).join('')).to.equal(expected);
        done();
      });
    });

    it('should have formatting commands on independent lines', function(done) {
      runner.run({
        language: 'r',
        solution: [
          'add <- function(a, b) {',
          '  cat(sprintf("a = %d, b = %d", a, b))',
          '  a + b',
          '}',
        ].join('\n'),
        fixture: [
          `test_that("addition works", {`,
          `  expect_equal(add(1, 1), 2)`,
          `})`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('a = 1, b = 1\n<PASSED::>');
        done();
      });
    });
  });

  describe('examples', function() {
    runner.assertCodeExamples('r');
  });
});
