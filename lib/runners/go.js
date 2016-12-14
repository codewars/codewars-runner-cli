var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp');

module.exports.run = function run(opts, cb)
{
    temp.track();
    var dir = temp.mkdirSync('go');

    shovel.start(opts, cb, {
        solutionOnly: function (runCode)
        {
            var file = util.codeWriteSync('go', opts.solution, dir, 'solution.go')
            runCode({name: 'go', 'args': ['run', file]});
        },
        testIntegration: function (runCode)
        {
            throw 'Test framework is not supported'
        }
    });
};