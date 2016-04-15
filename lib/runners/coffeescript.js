var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config');

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function (run) {
            var code = opts.solution;

            if (opts.setup) {
                code = opts.setup + ';\n' + code;
            }

            run({name: 'coffee', 'args': ['-e', code]});
        },
        fullProject: function (run) {
            switch (opts.testFramework) {
                case 'cw':
                case 'cw-2':
                    return prepareCw2(opts, run);

                default:
                    throw 'Test framework is not supported'
            }
        }
    });
};

function prepareCw2(opts, run) {

    var code = config.snippets.javascript.requireCw2;
    code += "`\n" + config.snippets.javascript.start + "`\n";
    if (opts.setup) {
        code += opts.setup + '\n';
    }

    code += opts.solution + '\n';
    code += '`\n' + config.snippets.javascript.inlineTestFixture.start + '`\n';
    code += opts.fixture;
    code += '\n`' + config.snippets.javascript.inlineTestFixture.end + '\n';
    code += config.snippets.javascript.end + "`";

    run({name: 'coffee', 'args': ['-e', code]});
}