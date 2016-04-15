var shovel = require('../shovel');

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function (run) {
            run({
                name: 'ocaml',
                args: ['-stdin'],
                stdin: opts.solution
            });
        }
    });
};
