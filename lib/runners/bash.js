var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp');

module.exports.run = function run(opts, cb)
{
    temp.track();
    var dir = temp.mkdirSync('bash');

    shovel.start(opts, cb, {
        solutionOnly: function ()
        {
            var file = util.codeWriteSync('bash', opts.solution, dir, 'solution.sh')
            return {name: 'bash', 'args': [file]};
        },
        fullProject: function ()
        {
            throw 'Test framework is not supported'
        }
    });
};






