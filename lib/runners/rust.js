var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp'),
    exec = require('child_process').exec;

function compile(args, cb) {
    args.unshift('rustc');
    exec(args.join(' '), cb);
}

module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('rust');

    shovel.start(opts, cb, {
        solutionOnly: function (run) {
            var executable = path.join(dir, 'solution'),
                solutionFile = util.codeWriteSync('rust', opts.solution, dir, 'solution.rs'),
                args = [solutionFile, '-o', executable];
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