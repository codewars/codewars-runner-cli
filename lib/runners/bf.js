var shovel = require('../shovel'),
    util = require('../util'),
    javascript = require('./javascript'),
    childProcess = require('child_process');

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
          // var execSync = childProcess.execSync;
          var execFileSync = childProcess.execFileSync;
          var solutionFile = util.writeFileSync('/home/codewarrior', 'code.b', opts.solution, true);
          module.exports.runBf = function(input) {
            // return execSync(`bf /workspace/solution.txt`, {input});
            var inputFile = util.writeFileSync('/home/codewarrior', 'input.txt', input, true);
            return execFileSync('beef', ['--input-file', inputFile, solutionFile]);
          };
          return javascript.prepareCw2(opts, runCode, fail);
          break;
        default:
          throw 'Test framework is not supported';
      }
    }
  });
};
