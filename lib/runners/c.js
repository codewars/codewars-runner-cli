var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('c');

    shovel.start(opts, cb, {
        solutionOnly: function () {
            return function (run) {
                var executable = path.join(dir, 'solution'),
                    solutionFile = util.codeWriteSync('c', opts.solution, dir, 'solution.c'),
                    args = ['tcc', '-o', executable, solutionFile];
                if (opts.setup) {
                    var setupFile = util.codeWriteSync('c', opts.setup, dir, 'setup.c');
                    args.push(setupFile);
                }
                util.exec(args.join(' '), function () { run({'name': executable, 'args': []}); });
            }
        },
        fullProject: function () {
            return function (run) {
                var executable = path.join(dir, 'solution'),
                    solutionFile = util.codeWriteSync('c', opts.solution, dir, 'solution.c'),
                    fixtureFile = util.codeWriteSync('c', opts.fixture, dir, 'fixture.c'),
                    args = ['tcc', '-o', executable, fixtureFile, solutionFile, './frameworks/c/test.c', '-I./frameworks/c'];
                if (opts.setup) {
                    var setupFile = util.codeWriteSync('c', opts.setup, dir, 'setup.c');
                    args.push(setupFile);
                }
                util.exec(args.join(' '), function () {run({'name': executable, 'args': []});});
            }
        }
    });
};