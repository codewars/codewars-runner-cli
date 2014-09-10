var shovel = require('../shovel');

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function () {
            if (opts.setup) {
		opts.solution = opts.setup + opts.solution;
            }
            return {
                name: 'elixir',
                args: ['-e', opts.solution]
            };
        },
        fullProject: function () {
            throw new Error('Test framework is not supported');
        }
    });
};
