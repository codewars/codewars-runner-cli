var flow = require('../flow'),
    util = require('../util');

module.exports.run = function run(opts, cb){
    flow.start(opts, cb, {
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
    var code = "require('./frameworks/javascript/cw-2')\n";
    if(opts.setup) {
        code += opts.setup + '\n';
    }

    code += opts.solution + '\n';
    code += '`\n(function() { var Test = this.Test, describe = this.describe, it = this.it, before = this.before, after = this.after;`\n';
    code += opts.fixture;
    code += '\n`})();`';

    return {name: 'coffee', 'args': ['-e', code]};
}



