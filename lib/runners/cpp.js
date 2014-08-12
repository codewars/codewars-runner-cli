var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp'),
    exec = require('child_process').exec;

function compile(args, cb) {
    // http://en.wikipedia.org/wiki/C%2B%2B14
    args.unshift('clang++');
    exec(args.join(' '), cb);
}

module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('cpp');

    shovel.start(opts, cb, {
        solutionOnly: function () {
            return function (run) {
                var executable = path.join(dir, 'solution'),
                    solutionFile = util.codeWriteSync('c++', opts.solution, dir, 'solution.cpp'),
                    args = [solutionFile, '-o', executable];
                if (opts.setup) {
                    var setupFile = util.codeWriteSync('cpp', opts.setup, lispCodeDir, 'setup.cpp');
                    args.unshift(setupFile);
                }
                compile(args, function (error, stdout, stderr) {
                    if (stdout) console.log(stdout);
                    if (stderr) {
                        console.error(stderr);
                        return;
                    }
                    if (error !== null) {
                        console.error('exec error: ' + error);
                        return;
                    }
                    run({'name': executable, 'args': []});
                });
            }
        },
        fullProject: function () {
            throw 'Test framework is not supported';
        }
    });
};
