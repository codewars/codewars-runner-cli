var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
  temp.track();
  var lispCodeDir = temp.mkdirSync('lisp'),
      args = [
        '--noinform', // Disable banner
        '--disable-ldb',  // Disable the low-level debugger
        '--lose-on-corruption', // Don't try to recover
        '--non-interactive' // No REPL
      ];
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      if (opts.setup) {
        var setupFile = util.codeWriteSync('lisp', opts.setup, lispCodeDir, 'setup.lisp');
        args.push('--load', setupFile);
      }
      args.push('--eval', opts.solution);
      runCode({name: 'sbcl', args: args});
    },
    testIntegration: function(runCode) {
      throw 'Test framework is not supported';
    }
  });
};
