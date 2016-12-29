var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp'),
    path = require('path'),
    exec = require('child_process').exec;

function compile(args, cb) {
    args.unshift('gcc', '`gnustep-config --objc-flags`');
    exec(args.join(' '), cb);
}

function getCode(opts)
{
    const code = [];

    if (opts.format === "default")
    {
        code.push("#import <Foundation/Foundation.h>");
        code.push("int main (int argc, const char * argv[]) {");
        code.push(opts.solution);
        code.push("return 0;");
        code.push("}");
    }
    else
    {
        if (opts.setupHeader)
            code.push(opts.setupHeader);

        if (opts.codeHeader)
            code.push(opts.codeHeader);

        if (opts.setup)
            code.push(opts.setup);

        code.push(opts.solution);
        code.push(opts.fixture);
    }

    return code.join("\n");
}

module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('objc');
    var args = [];

    shovel.start(opts, cb, {
        solutionOnly: function (runCode, fail) {
            var executable = path.join(dir, 'solution');

            opts.solution = getCode(opts);

            var solutionFile = util.codeWriteSync('objc', opts.solution, dir, 'solution.m');

            args = [solutionFile, '-lgnustep-base', '-lobjc', '-o', executable];

            compile(args, function (error, stdout, stderr) {
                if (error) return fail(error, stdout, stderr);
                opts.publish('stdout', stdout);
                runCode({'name': executable, 'args': []});
            });
        },
        testIntegration: function (runCode, fail) {

            opts.solution = getCode(opts);

            var executable = path.join(dir, 'solution');
            var solutionFile = util.codeWriteSync('objc', opts.solution, dir, 'solution.m');

            args = [solutionFile, '-lgnustep-base', '-lobjc', '-o', executable];

            compile(args, function (error, stdout, stderr) {
                if (error) return fail(error, stdout, stderr);
                opts.publish('stdout', stdout);
                runCode({'name': executable, 'args': []});
            });
        },
        sanitizeStdErr: function (err) {
            return err;
        }
    });
};
