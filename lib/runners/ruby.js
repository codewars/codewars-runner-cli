var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp');

module.exports.run = function run(opts, cb)
{
    shovel.start(opts, cb, {
        before: function() {
            // if a github repo was provided, add the workspace to the load path so that requires work correctly
            if (opts.githubRepo) {
                opts.setup = `$LOAD_PATH << '/workspace'\n${opts.setup || ''}`;
            }
        },
        solutionOnly: function (runCode)
        {
            var code = opts.solution;

            if (opts.setup)
            {
                code = opts.setup + '\n' + code;
            }

            runCode({name: 'ruby', args: ['-e', code], options: {cwd: opts.dir}});
        },
        testIntegration: function (runCode)
        {
            switch (opts.testFramework)
            {
                case 'cw':
                case 'cw-2':
                    return prepareCw2(opts, runCode);
                case 'rspec':
                    return prepareRSpec(opts, runCode);

                default:
                    throw 'Test framework is not supported'
            }
        },
        sanitizeStdErr: function(error)
        {
            return error.replace(/[\w/-]*(cw-2.rb):[\d]*:in( `(measure|wrap_error|it|describe)'<:LF:>)?/g, '')
                        .replace(/-e:[\d]*:in/g, '')
                        .replace('  ', ' ')
                        .replace(/<:LF:> `(block in )?(<main>|describe|it)'/g, '')
                        .replace('  ', ' ');
        },
        sanitizeStdOut: function(stdout)
        {
            return this.sanitizeStdErr(stdout);
        }
    });
};

function prepareCw2(opts, exec)
{
    var code = ["require('/runner/frameworks/ruby/cw-2')"];

    if (opts.setup)
    {
        code.push(opts.setup);
    }

    code.push(opts.solution);
    code.push(opts.fixture);

    exec({
        name: 'ruby',
        args: ['-e', code.join('\n')],
        options: { cwd: opts.dir }
    });
}

function prepareRSpec(opts, exec)
{
    var entry = [
        "`rm -rf /workspace/spec.rb`",
        `require "${util.codeWriteSync('ruby', opts.solution, opts.dir, 'solution.rb')}"`,
        `require "${util.codeWriteSync('ruby', opts.fixture, opts.dir, 'fixture.rb')}"`,
    ];

    if (opts.setup) {
        // have the file remove itself from the file system after it is loaded, so that it cannot be read by users trying to solve
        entry.unshift("`rm -rf /workspace/setup.rb`");
        entry.unshift(`require "${util.codeWriteSync('ruby', opts.setup, opts.dir, 'setup.rb')}"`);
    }

    var specFile = util.codeWriteSync('ruby', entry.join('\n'), opts.dir, 'spec.rb');

    exec({
        name: 'rspec',
        args: [specFile, '--require', '/runner/frameworks/ruby/cwrspecformatter.rb', '--format', 'CwRSpecFormatter'],
        options: { cwd: opts.dir }
    });
}

module.exports.prepareRSpec = prepareRSpec;
module.exports.prepareCw2 = prepareCw2;
