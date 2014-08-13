var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp');

module.exports.run = function run(opts, cb)
{
    temp.track();
    var dir = temp.mkdirSync('fsharp');

    shovel.start(opts, cb, {
        solutionOnly: function ()
        {
            var file = util.codeWriteSync('fsharp', opts.solution, dir, 'solution.fsx');
            return {name: 'fsharpi', 'args': [file]};
        },
        fullProject: function ()
        {
            throw 'Test framework is not supported'
        }
    });
};






