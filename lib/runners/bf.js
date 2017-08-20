"use strict";

const shovel = require('../shovel');
const writeFileSync = require('../utils/write-file-sync');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      const solutionFile = writeFileSync('/home/codewarrior', 'code.b', opts.solution);
      runCode({
        name: 'bf',
        args: ['-c29999', solutionFile]
      });
    },
    testIntegration: function(runCode, fail) {
      switch (opts.testFramework) {
        case 'cw-2':
          var fixture = `
require('/runner/frameworks/javascript/cw-2');
${opts.setup};
const runBF = require('/runner/frameworks/bf/run-bf.js');
${opts.fixture};
`;
          runCode({
            name: 'node',
            args: ['-e', fixture, '--no-deprecation'],
            options: {
              cwd: opts.dir
            }
          });
          break;
        default:
          throw new Error('Test framework is not supported');
      }
    }
  });
};
