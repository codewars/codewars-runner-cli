var flow = require('../flow');

module.exports.run = function run(opts, cb){
    flow.start(opts, cb, {
        solutionOnly: function() {
            var code = opts.solution;

            if (opts.setup) {
                code = opts.setup + '\n' + code;
            }

            return {name: 'python', 'args': ['-c', code]};
        }
    });
};
