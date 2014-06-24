var shovel = require('../shovel'),
    config = require('../config');

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
        }
    });
};


function prepareCw2(opts)
{
    var code = config.snippets.python.requireCw2;
    //TODO add setup and stuff
    code += opts.solution + "\n";
    code += opts.fixture;
    return {name: 'python', 'args': ['-c', code]};
}

function prepareUnittest(opts)
{
    var code = opts.solution + "\n";
    code += config.snippets.python.requireUnittest;
    code += opts.fixture + "\n";
    code += config.snippets.python.runUnittest;
    return {name: 'python', 'args': ['-c', code]};
}
