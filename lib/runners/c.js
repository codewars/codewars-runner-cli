var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path');

module.exports.run = function run(opts, cb) {
    var dir = '/workspace'

    shovel.start(opts, cb, {
        solutionOnly: function (run) {
            var executable = path.join(dir, 'solution'),
                solutionFile = util.codeWriteSync('c', opts.solution, dir, 'solution.c'),
                args = ['clang-3.6', '-std=c11', solutionFile, '-o', executable, '-lm'];
            if (opts.setup) {
                var setupFile = util.codeWriteSync('c', opts.setup, dir, 'setup.c');
                args.push(setupFile);
            }
            util.exec(args.join(' '), {passthrough: true}, function (error, stdout, stderr) {
                if (error && error != '') return run(error.toString());
                opts.publish('stdout', stdout);
                run({'name': executable, 'args': []});
            });
        },
        testIntegration: function (run) {
            var executable = path.join(dir, 'solution'),
                solutionFile = util.codeWriteSync('c', opts.solution, dir, 'solution.c'),
                fixtureFile = util.codeWriteSync('c', opts.fixture, dir, 'fixture.c'),
                args = ['clang-3.6', '-std=c11', fixtureFile, solutionFile, '-o', executable, './frameworks/c/criterion.c', '-I./frameworks/c', '-lcriterion', '-lm'];
            if (opts.setup) {
                var setupFile = util.codeWriteSync('c', opts.setup, dir, 'setup.c');
                args.push(setupFile);
            }
            util.exec(args.join(' '), {passthrough: true}, function (error, stdout, stderr) {
                if (error && error != '') return run(error.toString());
                opts.publish('stdout', stdout);
                run({'name': executable, 'args': ['-q','-j1']});
            });
        },
        sanitizeStdErr: function(error)
        {
            error = error || ''
            return error.replace(/clang.*-std=c[^\s]+/g, '')
                        .replace(/Error: Command failed:/g, '')
                        .replace(/\/tmp.*(solution\.c|solution)[:0-9]*/g, '')
                        .replace('\n', '')
                        .replace('  ', ' ')
                        .replace(opts.setup || '', '')
                        .replace(opts.fixture || '', '');
        }
    });
};

