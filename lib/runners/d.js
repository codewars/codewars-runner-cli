const shovel = require('../shovel');
const writeFileSync = require('../util').writeFileSync;

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      runCode({
        name: 'dmd',
        args: ['-run', writeFileSync(opts.dir, 'code.d', opts.solution, true)],
        options: {
          cwd: opts.dir
        }
      });
    },

    testIntegration: function(runCode) {
      throw new Error('Test integration is not supported');
    }
  });
};
