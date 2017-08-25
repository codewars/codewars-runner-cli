"use strict";

const fs = require('fs');
const path = require('path');

module.exports = {
  solutionOnly(opts, runCode) {
    const entry = path.join(opts.dir, 'main.R');
    if (opts.setup) fs.writeFileSync(path.join(opts.dir, 'setup.R'), opts.setup);
    fs.writeFileSync(entry, opts.solution);
    runCode({
      name: 'Rscript',
      args: ['--no-save', entry],
      options: {
        cwd: opts.dir,
      }
    });
  },

  testIntegration(opts, runCode) {
    const entry = path.join(opts.dir, 'run-tests.R');
    if (opts.setup) fs.writeFileSync(path.join(opts.dir, 'setup.R'), opts.setup);
    fs.writeFileSync(path.join(opts.dir, 'solution.R'), opts.solution);
    fs.writeFileSync(path.join(opts.dir, 'tests.R'), opts.fixture);
    fs.writeFileSync(entry, [
      `library(testthat)`,
      `source("/runner/frameworks/r/codewars-reporter.R")`,
      `source("solution.R")`,
      `test_file("tests.R", reporter=CodewarsReporter$new())`,
    ].join('\n'));

    runCode({
      name: 'Rscript',
      args: ['--no-save', entry],
      options: {
        cwd: opts.dir,
      },
    });
  }
};
