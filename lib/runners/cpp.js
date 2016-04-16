var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp'),
    exec = require('child_process').exec;

function compile(args, cb) {
    args.unshift('clang++', '-std=c++11');
    exec(args.join(' '), cb);
}

module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('cpp');

    shovel.start(opts, cb, {
        solutionOnly: function (run) {
            var executable = path.join(dir, 'solution'),
                solutionFile = util.codeWriteSync('c++', opts.solution, dir, 'solution.cpp'),
                args = [solutionFile, '-o', executable];
            if (opts.setup) {
                var setupFile = util.codeWriteSync('cpp', opts.setup, dir, 'setup.cpp');
                args.unshift(setupFile);
            }
            compile(args, function (error, stdout, stderr) {
                if (stdout) console.log(stdout);
                if (stderr) throw new Error(stderr);
                if (error !== null) throw error;
                run({'name': executable, 'args': []});
            });
        },
        testIntegration: function (run) {
            throw new Error('Test framework is not supported');
        }
    });
};
