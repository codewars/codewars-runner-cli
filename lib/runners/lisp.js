var shovel = require('../shovel'),
    fs = require('fs'),
    path = require('path'),
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
        solutionOnly: function () {
            if (opts.setup) {
                var setupFile = path.join(lispCodeDir, 'setup.lisp');
                fs.writeFileSync(setupFile, opts.setup);
                args.push('--load', setupFile);
            }
            args.push('--eval', opts.solution);
            return {
                name: 'sbcl',
                args: args
            };
        },
        fullProject: function () {
            // TODO
        }
    });
};