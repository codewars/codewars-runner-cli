var shovel = require('../shovel'),
    config = require('../config'),
    fs = require('fs');

module.exports.run = function run(opts, cb)
{
    shovel.start(opts, cb, {
        solutionOnly: function ()
        {
            var code = opts.solution;

            if (opts.setup)
            {
                code = opts.setup + '\n' + code;
            }

            return {name: 'python', 'args': ['-c', code]};
        },
        fullProject: function ()
        {
            switch (opts.testFramework)
            {
                case 'cw-2':
                    return prepareCw2(opts);
                case 'unittest':
                    return prepareUnittest(opts);

                default:
                    throw 'test framework is not supported';
            }
        },
        sanitizeStdErr: function(err)
        {
            // get rid of some of the noisy content. We remove line numbers since
            // they don't match up to what the user sees and will only confuse them.
            return err.replace(/File "(<string>)?", line \d*,/g, '')
                      .replace(' (most recent call last)', '')
                      .replace(' in in ', ' in ');
        }
    });
};


function prepareCw2(opts)
{
    var code = [config.snippets.python.requireCw2, opts.setup, opts.solution, opts.fixture].join("\n")

    // taking out for now. Needed to add setup code in and it needs to be available within both solution and fixture scope,
    // while also not being trivial to read its contents.
//    fs.writeFileSync('solution.py', opts.solution);
    return {name: 'python', 'args': ['-c', code]};
}

function prepareUnittest(opts)
{
    var code = opts.solution + "\n";
    code += config.snippets.python.requireUnittest;
    code += opts.fixture + "\n";
    code += config.snippets.python.defaultTestSuite + "\n";
    code += config.snippets.python.runUnittest;
    return {name: 'python', 'args': ['-c', code]};
}
