var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp');

module.exports.run = function run(opts, cb)
{
    temp.track();
    var dir = temp.mkdirSync('go');

    shovel.start(opts, cb, {
        solutionOnly: function ()
        {
            var file = util.codeWriteSync('go', opts.solution, dir, 'solution.go')
            return {name: 'go', 'args': ['run', file]};
        },
        fullProject: function ()
        {
            throw 'Test framework is not supported'
        }
    });
};






