var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      runCode({
        name: 'elixir',
        args: ['-e', solution(opts)]
      });
    },
    testIntegration: function(runCode) {
      switch (opts.testFramework) {
        case 'ex_unit':
        case 'exunit':
          return prepareExUnit(opts, runCode);

        default:
          throw 'Test framework is not supported';
      }
    },
  });
};

function prepareExUnit(opts, run) {
  var dir = temp.mkdirSync('elixir');
  var solutionFile = util.codeWriteSync('elixir', solution(opts), dir, 'solution', true);
  var fixtureFile = util.codeWriteSync('elixir', opts.fixture, dir, 'fixture', true);

  var code = `
        Code.load_file("frameworks/elixir/cw_runner.ex")
        CWRunner.run("${solutionFile}", "${fixtureFile}")
    `;

  run({name: 'elixir', 'args': ['-e', code]});
}

function solution(opts) {
  return opts.setup ? `${opts.setup}\n${opts.solution}` : opts.solution;
}
