const shovel = require('../shovel');
const writeFileSync = require('../util').writeFileSync;

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function (runCode) {
      runCode({
        name: 'nim',
        args: ['compile', '--run', '--hints:off', '--verbosity:0', writeFileSync(opts.dir, 'code.nim', opts.solution, true)]
      });
    },

    testIntegration: function (runCode) {
      throw new Error('Test integration is not supported.');
    }
  });
};
