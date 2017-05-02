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
        case 'cw-2':
          // TODO
        break;
        default:
          throw 'Test framework is not supported';
      }
    }
  });
};
