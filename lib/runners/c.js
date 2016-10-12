var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('c');

    shovel.start(opts, cb, {
        solutionOnly: function (run) {
            var executable = path.join(dir, 'solution'),
                solutionFile = util.codeWriteSync('c', opts.solution, dir, 'solution.c'),
                args = ['clang', '-o', executable, solutionFile];
            if (opts.setup) {
                var setupFile = util.codeWriteSync('c', opts.setup, dir, 'setup.c');
                args.push(setupFile);
            }
            util.exec(args.join(' '), function () { run({'name': executable, 'args': []}); });
        },
        testIntegration: function (run) {
            var executable = path.join(dir, 'solution'),
                solutionFile = util.codeWriteSync('c', opts.solution, dir, 'solution.c'),
                fixtureFile = util.codeWriteSync('c', opts.fixture, dir, 'fixture.c'),
                args = ['clang', '-o', executable, fixtureFile, solutionFile, './frameworks/c/test.c', '-I./frameworks/c'];
            if (opts.setup) {
                var setupFile = util.codeWriteSync('c', opts.setup, dir, 'setup.c');
                args.push(setupFile);
            }
            util.exec(args.join(' '), function () {run({'name': executable, 'args': []});});
        },
        sanitizeStdErr: function(error)
        {
            error = error || ''
            return error.replace(/clang.*-std=c[^\s]+/g, '')
                        .replace(/Error: Command failed:/g, '')
                        .replace(/\/tmp.*(solution\.cpp|solution)[:0-9]*/g, '')
                        .replace('\n', '')
                        .replace('  ', ' ')
                        .replace(opts.setup || '', '')
                        .replace(opts.fixture || '', '');
        }
    });
};

