var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp');

module.exports.run = function run(opts, cb)
{
    temp.track();
    var dir = temp.mkdirSync('perl');

    shovel.start(opts, cb, {
        solutionOnly: function ()
        {
            var file = util.codeWriteSync('perl', opts.solution, dir, 'solution.pl')
            return {name: 'perl', 'args': [file]};
        },
        fullProject: function ()
        {
            throw 'Test framework is not supported'
        }
    });
};






