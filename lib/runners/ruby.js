var shovel = require('../shovel'),
    util = require('../util'),
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

            return {name: 'ruby', 'args': ['-e', code]};
        },
        fullProject: function ()
        {
            switch (opts.testFramework)
            {
                case 'cw-2':
                    return prepareCw2(opts);
                case 'rspec':
                    return prepareRSpec(opts);

                default:
                    throw 'Test framework is not supported'
            }
        }
    });
};

function prepareCw2(opts)
{
    var code = ["require('./frameworks/ruby/cw-2')"];

    if (opts.setup)
    {
        code.push(opts.setup);
    }

    code.push(opts.solution);
    code.push(opts.fixture);

    return {name: 'ruby', 'args': ['-e', code.join('\n')]};
}

function prepareRSpec(opts)
{
    var code = [];

    if (opts.setup)
    {
        code.push(opts.setup);
    }
    code.push(opts.solution);
    code.push(opts.fixture);

    fs.writeFileSync('solution.rb', code.join('\n'));

    return {name: 'rspec', 'args': ['solution.rb', '--require', './frameworks/ruby/cwrspecformatter.rb', '--format', 'CwRSpecFormatter']};
}
