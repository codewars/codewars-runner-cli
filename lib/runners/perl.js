var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp');

module.exports.run = function run(opts, cb)
{
    temp.track();
    var dir = temp.mkdirSync('perl');

    shovel.start(opts, cb, {
        solutionOnly: function (run)
        {
            var file = util.codeWriteSync('perl', opts.solution, dir, 'solution.pl')
            run({name: 'perl', 'args': [file]});
        },
        fullProject: function (run)
        {
            throw 'Test framework is not supported'
        }
    });
};