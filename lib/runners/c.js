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
                args = ['clang', '-std=c99', solutionFile, '-o', executable];
            if (opts.setup) {
                var setupFile = util.codeWriteSync('c', opts.setup, dir, 'setup.c');
                args.push(setupFile);
            }
            util.exec(args.join(' '), {passthrough: true}, function (error, stdout, stderr) {
                if (stdout) console.log(stdout);
                if (error && error != '') return run(error.toString());
                run({'name': executable, 'args': []});
            });
        },
        testIntegration: function (run) {
            var executable = path.join(dir, 'solution'),
                solutionFile = util.codeWriteSync('c', opts.solution, dir, 'solution.c'),
                fixtureFile = util.codeWriteSync('c', opts.fixture, dir, 'fixture.c'),
                args = ['clang', '-std=c99', fixtureFile, solutionFile, '-o', executable, './frameworks/c/criterion.c', '-I./frameworks/c', '-lcriterion'];
            if (opts.setup) {
                var setupFile = util.codeWriteSync('c', opts.setup, dir, 'setup.c');
                args.push(setupFile);
            }
            util.exec(args.join(' '), {passthrough: true}, function (error, stdout, stderr) {
                if (stdout) console.log(stdout);
                if (error && error != '') return run(error.toString());
                run({'name': executable, 'args': ['-q']});
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

