var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp'),
    path = require('path'),
    exec = require('child_process').exec;

function compile(args, cb) {
    args.unshift('gcc', '`gnustep-config --objc-flags`');
    exec(args.join(' '), cb);
}


module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('objc');
    var args = [];

    shovel.start(opts, cb, {
        solutionOnly: function (runCode, fail) {
            var executable = path.join(dir, 'solution');

            opts.solution =
                '#import <Foundation/Foundation.h>\n' +
                'int main (int argc, const char * argv[]) {\n' +
                opts.solution + '\n' +
                'return 0;\n' +
                '}';

            var solutionFile = util.codeWriteSync('objc', opts.solution, dir, 'solution.m');

            args = [solutionFile, '-lgnustep-base', '-lobjc', '-o', executable];

            compile(args, function (error, stdout, stderr) {
                if (error) return fail(error, stdout, stderr);
                opts.publish('stdout', stdout);
                runCode({'name': executable, 'args': []});
            });
        }
    });
};
