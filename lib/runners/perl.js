var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
  temp.track();
  var dir = temp.mkdirSync('perl');

  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      var file = util.codeWriteSync('perl', opts.solution, dir, 'solution.pl');
      runCode({name: 'perl', 'args': [file]});
    },
    testIntegration: function(runCode) {
      throw 'Test framework is not supported';
    }
  });
};
