"use strict";

const fs = require('fs');
const path = require('path');

module.exports = {
  solutionOnly(opts, runCode) {
    const solution = path.join(opts.dir, 'solution.nim');
    fs.writeFileSync(solution, opts.solution);
    if (opts.setup)
      fs.writeFileSync(path.join(opts.dir, 'setup.nim'), opts.setup);
    runCode({
      name: 'nim',
      args: nimArgs(solution),
      options: {cwd: opts.dir}
    });
  },

  testIntegration(opts, runCode) {
    const entry = path.join(opts.dir, 'tests.nim');
    fs.writeFileSync(entry, [
      'import unittest, codewars/formatter',
      'addOutputFormatter(OutputFormatter(newCodewarsOutputFormatter()))',
      'import solution',
      'include fixture',
    ].join('\n'));
    fs.writeFileSync(path.join(opts.dir, 'solution.nim'), opts.solution);
    fs.writeFileSync(path.join(opts.dir, 'fixture.nim'), opts.fixture);
    if (opts.setup)
      fs.writeFileSync(path.join(opts.dir, 'setup.nim'), opts.setup);
    runCode({
      name: 'nim',
      args: nimArgs(entry),
      options: {cwd: opts.dir}
    });
  },

  sanitizeStdErr(opts, stderr) {
    // Remove output from Nim
    // Error: execution of an external program failed: 'path/to/file '
    return stderr.replace(/^Error: execution of an external program failed: .*$/m, '');
  },
};

function nimArgs(file) {
  return [
    'compile',
    '--run',            // run the compiled program
    '--define:release', // release build
    '--warnings:off',   // turn all warnings off
    '--hints:off',      // turn all hints off
    '--verbosity:0',    // set Nim's verbosity to minimal
    '--stackTrace:on',  // turn stack tracing on
    '--lineTrace:on',   // turn line tracing on
    '--checks:on',      // turn all runtime checks on
    '--path:/runner/frameworks/nim',
    file
  ];
}
