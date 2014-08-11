var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp'),
    exec = require('child_process').exec;

function compile(args, cb) {
    args.unshift('tcc');
    exec(args.join(' '), cb);
}

module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('c');

    shovel.start(opts, cb, {
        solutionOnly: function () {
            return function (run) {
                var executable = path.join(dir, 'solution'),
                    solutionFile = util.codeWriteSync('c', opts.solution, dir, 'solution.c'),
                    args = [solutionFile, '-o', executable];
                compile(args, function () {
                    run({'name': executable, 'args': []});
                });
            }
        },
        fullProject: function () {
            throw 'Test framework is not supported';
        }
    });
};