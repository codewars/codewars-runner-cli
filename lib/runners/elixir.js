"use strict";

const temp = require('temp').track();

const codeWriteSync = require('../utils/code-write-sync');

module.exports = {
  solutionOnly(opts, runCode) {
    runCode({
      name: 'elixir',
      args: ['-e', solution(opts)]
    });
  },
  testIntegration(opts, runCode) {
    switch (opts.testFramework) {
      case 'ex_unit':
      case 'exunit':
        return prepareExUnit(opts, runCode);

      default:
        throw new Error('Test framework is not supported');
    }
  },
};

function prepareExUnit(opts, run) {
  var dir = temp.mkdirSync('elixir');
  var solutionFile = codeWriteSync('elixir', solution(opts), dir, 'solution');
  var fixtureFile = codeWriteSync('elixir', opts.fixture, dir, 'fixture');

  var code = `
        Code.load_file("frameworks/elixir/cw_runner.ex")
        CWRunner.run("${solutionFile}", "${fixtureFile}")
    `;

  run({name: 'elixir', 'args': ['-e', code]});
}

function solution(opts) {
  return opts.setup ? `${opts.setup}\n${opts.solution}` : opts.solution;
}
