var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config');

module.exports.run = function run(opts, cb)
{
    shovel.start(opts, cb, {
        solutionOnly: function ()
        {
            var code = opts.solution;

            if (opts.setup)
            {
                code = opts.setup + ';\n' + code;
            }

            return {name: 'node', 'args': ['-e', code]};
        },
        fullProject: function ()
        {
            switch (opts.testFramework)
            {
                case 'cw-2':
                    return prepareCw2(opts);

                default:
                    throw 'Test framework is not supported'
            }
        },
        sanitizeStdErr: function(error)
        {
            error = error || ''
            return error.replace(/(\()?\/codewars\/[(\w\/-\d.js:) ;]*/g, '')
                        .replace(/( Object. )?[\(]?\[eval\][-:\w\d\)]* at/g, '')
                        .replace(/Module._compile.*/g, '')
                        .replace('Object.Test.handleError ', '')
                        .replace('  ', ' ');
        },
        sanitizeStdOut: function(stdout)
        {
            return this.sanitizeStdErr(stdout);
        }
    });
};


function prepareCw2(opts)
{
    var code = config.snippets.javascript.requireCw2;
    code += "var assert = require('assert');\n";

    code += config.snippets.javascript.start;

    if (opts.setup)
    {
        code += opts.setup + ';\n';
    }

    code += opts.solution + ';\n';

    // run tests inline to the context but within their own scope. Redefine the key test methods in case they were
    // locally redefined within the solution.
    code += ';' + config.snippets.javascript.inlineTestFixture.start;
    code += opts.fixture;
    code += config.snippets.javascript.inlineTestFixture.end;
    code += config.snippets.javascript.end;

    return {name: 'node', 'args': ['--harmony', '-e', code]};
}



