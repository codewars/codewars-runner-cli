var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config');

module.exports.run = function run(opts, cb)
{
    shovel.start(opts, cb, {
        solutionOnly: function ()
        {
            var code = opts.solution;

            if (opts.setup)
            {
                code = opts.setup + ';\n' + code;
            }

            return {name: 'php', 'args': ['-r', code]};
        },
        fullProject: function ()
        {
            throw 'Test framework is not supported'
        }
    });
};






