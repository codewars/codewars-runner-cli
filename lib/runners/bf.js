var shovel = require('../shovel'),
    util = require('../util');

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
        // TODO: Add Test Framework options as they become available
        default:
          throw 'Test framework is not supported';
      }
    }
  });
};
