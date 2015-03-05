var shovel = require('../shovel');

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function () {
            return {
                name: 'ocaml',
                args: ['-stdin'],
                stdin: opts.solution
            };
        }
    });
};
