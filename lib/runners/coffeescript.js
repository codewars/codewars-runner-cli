var shovel = require('../shovel'),
    util = require('../util' ),
    config = require('../config');

module.exports.run = function run(opts, cb){
    shovel.start(opts, cb, {
        solutionOnly: function() {
            var code = opts.solution;

            if (opts.setup) {
                code = opts.setup + ';\n' + code;
            }

            return {name: 'coffee', 'args': ['-e', code]};
        },
        fullProject: function() {
            switch (opts.testFramework) {
                case 'cw-2':
                    return prepareCw2(opts);

                default:
                    throw 'Test framework is not supported'
            }
        }
    });
};

function prepareCw2(opts) {
    var code = config.snippets.javascript.requireCw2;
    if(opts.setup) {
        code += opts.setup + '\n';
    }

    code += opts.solution + '\n';
    code += '`\n' + config.snippets.javascript.inlineTestFixture.start + '`\n';
    code += opts.fixture;
    code += '\n`' + config.snippets.javascript.inlineTestFixture.end + '`';

    return {name: 'coffee', 'args': ['-e', code]};
}



