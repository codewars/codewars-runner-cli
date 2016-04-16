var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp');

module.exports.run = function(opts, cb)
{
    temp.track();
    var dir = temp.mkdirSync('fsharp');

    shovel.start(opts, cb, {
        solutionOnly: function (run)
        {
            var file = util.codeWriteSync('fsharp', opts.solution, dir, 'solution.fsx');
            run({name: 'fsharpi', 'args': [file]});
        },
        testIntegration: function (run)
        {
            throw 'Test framework is not supported'
        }
    });
};






