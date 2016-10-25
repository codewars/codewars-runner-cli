var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp');

module.exports.run = function run(opts, cb)
{
    shovel.start(opts, cb, {
        solutionOnly: function (exec)
        {
            var code = opts.solution;

            if (opts.setup)
            {
                code = opts.setup + '\n' + code;
            }

            exec({name: 'ruby', 'args': ['-e', code]});
        },
        testIntegration: function (exec)
        {
            switch (opts.testFramework)
            {
                case 'cw':
                case 'cw-2':
                    return prepareCw2(opts, exec);
                case 'rspec':
                    return prepareRSpec(opts, exec);

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

    // write any optional files to the same directory
    util.writeFilesSync(null, opts.files, false);

    exec({name: 'ruby', 'args': ['-e', code.join('\n')]});
}

function prepareRSpec(opts, exec)
{
    var entry = [
        `require "${util.codeWriteSync('ruby', opts.solution, null, 'solution.rb')}"`,
        `require "${util.codeWriteSync('ruby', opts.fixture, null, 'fixture.rb')}"`,
    ];

    if (opts.setup) {
        // have the file remove itself from the file system after it is loaded, so that it cannot be read by users trying to solve
        entry.unshift("`rm -rf /workspace/setup.rb`");
        entry.unshift(`require "${util.codeWriteSync('ruby', opts.setup, null, 'setup.rb')}"`);
    }

    var specFile = util.codeWriteSync('ruby', entry.join('\n'), null, 'spec.rb');

    // write any optional files to the same directory
    util.writeFilesSync(null, opts.files, false);

    exec({name: 'rspec', 'args': [specFile, '--require', '/runner/frameworks/ruby/cwrspecformatter.rb', '--format', 'CwRSpecFormatter']});
}

module.exports.prepareRSpec = prepareRSpec;
module.exports.prepareCw2 = prepareCw2;
