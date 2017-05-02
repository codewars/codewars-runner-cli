var shovel = require('../shovel'),
    util = require('../util'),
    javascript = require('./javascript');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      var solutionFile = util.writeFileSync('/home/codewarrior', 'code.b', opts.solution, true);
      runCode({
        name: 'bf',
        args: [solutionFile]
      });
    },
    testIntegration: function(runCode, fail) {
      switch (opts.testFramework) {
        case 'cw-2':
          const execSync = require('child_process').execSync;
          module.exports.runBf = function (input) {
            return execSync(`bf /workspace/solution.txt`, { input })
          }
          var solutionFile = util.writeFileSync('/home/codewarrior', 'code.b', opts.solution, true);
          runCode({
            name: 'bf',
            args: [solutionFile]
          });
          break;
        default:
          throw 'Test framework is not supported';
      }
    }
  });
};
