var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp'),
    exec = require('child_process').exec;

function compile(file, cb) {
    exec('tsc ' + file, cb);
}

module.exports.run = function run(opts, cb) {
    var dir = temp.mkdirSync('typescript');
    shovel.start(opts, cb, {
        solutionOnly: function () {
            // TODO: Support Setup Code
            return function (run) {
                var solutionFile = util.codeWriteSync('typescript', opts.solution, dir, 'solution.ts', true);
                compile(solutionFile, function (error, stdout, stderr) {
                    if (stdout) console.log(stdout);
                    if (stderr) throw new Error(stderr);
                    if (error !== null) throw error;
                    run({name: 'node', args: [solutionFile.replace('.ts', '.js')]});
                });
            }
        },
        fullProject: function () {
            throw new Error('Test framework is not supported');
        }
    });

};