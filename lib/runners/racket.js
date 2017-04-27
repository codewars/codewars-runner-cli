var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
  temp.track();
  var racketCodeDir = temp.mkdirSync('racket'),
      args = ['-l', 'racket/base'];
  shovel.start(opts, cb, {
    solutionOnly: function(run) {
      if (opts.setup) {
        var setupFile = util.codeWriteSync('racket', opts.setup, racketCodeDir, 'setup.rkt');
        args.push('-t', setupFile);
      }
      args.push('-e', opts.solution);
      run({
        name: 'racket',
        args: args
      });
    },
    testIntegration: function(run) {
      throw 'Test framework is not supported';
    }
  });
};
