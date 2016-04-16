var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp');

module.exports.run = function run(opts, cb)
{
    temp.track();
    var dir = temp.mkdirSync('lua');

    shovel.start(opts, cb, {
        solutionOnly: function (run)
        {
            var file = util.codeWriteSync('lua', opts.solution, dir, 'solution.lua')
            run({name: 'lua', 'args': [file]});
        },
        testIntegration: function (run)
        {
            throw 'Test framework is not supported'
        }
    });
};






