var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function(run) {
            if (opts.setup) {
                opts.solution = opts.setup + opts.solution;
            }
            run({
                name: 'elixir',
                args: ['-e', opts.solution]
            });
        },
        testIntegration: function(run) {
            switch (opts.testFramework) {
                case 'ex_unit':
                    return prepareExUnit(opts, run);

                default:
                    throw 'Test framework is not supported'
            }
        },
    });
};

function prepareExUnit(opts, run) {
    var code = [];

    var dir = temp.mkdirSync('elixir');
    var solutionFile = util.codeWriteSync('elixir', opts.solution, dir, 'solution', true);
    var fixtureFile = util.codeWriteSync('elixir', opts.fixture, dir, 'fixture', true);

    code.push('Code.load_file("frameworks/elixir/cw_runner.ex")');
    code.push('CWRunner.run("'+solutionFile+'", "'+fixtureFile+'")');

    run({name: 'elixir', 'args': ['-e', code.join('\n')]});
}
