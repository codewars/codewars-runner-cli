var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp'),
    child_process = require('child_process');


// TODO: Not DRY, fix this
function exec(command, cb) {
    child_process.exec(command, function (error, stdout, stderr) {
        if (stdout) console.log(stdout);
        if (stderr) throw new Error(stderr);
        if (error !== null) throw error;
        cb();
    });
}

module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('nasm');

    shovel.start(opts, cb, {
        solutionOnly: function () {
            // TODO: Support setup code; setup code should either be NASM or C, and we should automatically detect it
            return function (run) {
                var executable = path.join(dir, 'solution'),
                    solutionFile = util.codeWriteSync('nasm', opts.solution + '\n', dir, 'solution.asm'),
                    nasmCommand = ['nasm', '-felf64', solutionFile].join(' '),
                    objectFile = solutionFile.replace(/\.[^\.]+$/, '.o'),
                    // Check for whether we need to link against libc
                    linker = opts.solution.search(/global\W+_start/) == -1 ? "gcc" : "ld";
                    linkerCommand = [linker, objectFile, '-o', executable].join(' ');
                exec(nasmCommand, function () {
                    exec(linkerCommand, function () {
                        run({'name': executable, 'args': []});
                    });
                });
            }
        },
        fullProject: function () {
            throw new Error('Test framework is not supported');
        }
    });
};
