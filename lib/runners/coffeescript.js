var shovel = require('../shovel');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      runCode({
        name: 'coffee',
        args: ['-e', [opts.setup ? opts.setup : '', opts.solution].join('\n')]
      });
    },
    testIntegration: function(runCode) {
      switch (opts.testFramework) {
        case 'cw':
        case 'cw-2':
          return prepareCw2(opts, runCode);

        default:
          throw 'Test framework is not supported';
      }
    }
  });
};

function prepareCw2(opts, run) {

  var code = `
        require('./frameworks/javascript/cw-2')
        assert = require('assert')
        Test.handleError ->
            ${indentLines(opts.setup, 13)}
            ${indentLines(opts.solution, 13)}

            do ->
                Test = global.Test
                describe = global.describe
                it = global.it
                before = global.before
                after = global.after

                ${indentLines(opts.fixture, 17)}
    `;

  run({name: 'coffee', 'args': ['-e', code]});
}

function indentLines(lines, spaces) {
  if (!lines) return '';
  var padding = new Array(spaces).join(' ');
  return lines.split("\n").map((line, ndx) => `${ndx == 0 ? '' : padding}${line}`).join("\n");
}
