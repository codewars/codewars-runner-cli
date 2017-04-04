var shovel = require('../shovel');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(run) {
      if (opts.setup) {
        opts.solution = opts.setup + opts.solution;
      }
      run({
        name: 'R',
        args: ['--vanilla', '--slave', '-e', opts.solution]
      });
    },
    testIntegration: function(run) {
      throw new Error('Test framework is not supported');
    }
  });
};
