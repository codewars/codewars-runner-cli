var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp'),
    exec = require('child_process').exec,
    fs = require('fs'),
    cppDir = path.resolve(__dirname, '..', '..', 'frameworks', 'cpp'),
    main = fs.readFileSync(path.resolve(cppDir, 'main.cpp'));

function compile(args, cb) {
    args.unshift('clang++-3.6', '-stdlib=libc++ -std=c++1y');
    exec(args.join(' '), cb);
}

module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('cpp');

    var args = [];
    if(opts.setup) {
        var setupFile = util.codeWriteSync('cpp', opts.setup, dir, 'setup.h');
        args.push(setupFile);
        opts.solution = '#include "setup.h"\n' + opts.solution;
    }

    shovel.start(opts, cb, {
        solutionOnly: function (runCode, fail) {
            var executable = path.join(dir, 'solution'),
                solutionFile = util.codeWriteSync('cpp', opts.solution, dir, 'solution.cpp');
            args = [solutionFile, '-o', executable];
            compile(args, function (error, stdout, stderr) {
                if (error) return fail(error, stdout, stderr);
                opts.publish('stdout', stdout);
                runCode({'name': executable, 'args': []});
            });
        },
        testIntegration: function (runCode, fail) {
            var executable = path.join(dir, 'solution'),
                solutionFile = util.codeWriteSync('cpp', [
                    '#include <igloo/igloo_alt.h>',
                    'using namespace igloo;',
                    opts.solution,
                    opts.fixture,
                    main
                ].join('\n'), dir, 'solution.cpp');
            args = ['-isystem', cppDir, solutionFile, '-o', executable];
            compile(args, function (error, stdout, stderr) {
                if (error) return fail(error, stdout, stderr);
                opts.publish('stdout', stdout);
                runCode({'name': executable, 'args': []});
            });
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
