var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    exec = require('child_process').exec;

function compile(file, cb)
{
    exec('tsc ' + file, function(err, stdout, stderr)
    {
        file = file.replace('.ts', '.js');
        cb(file);
    });
}

module.exports.run = function run(opts, cb)
{
    shovel.start(opts, cb, {
        solutionOnly: function ()
        {
            return function(run)
            {
                var solutionFile = util.codeWriteSync('typescript', opts.solution, '/tmp/typescript', 'solution.ts', true);
                compile(solutionFile, function(file)
                {
                    run({name: 'node', 'args': [file]});
                });
            }
        },
        fullProject: function ()
        {
            throw 'Test framework is not supported'
        }
    });

};






