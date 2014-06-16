var shovel = require('../shovel');

module.exports.run = function run(opts, cb){
    shovel.start(opts, cb, {
        solutionOnly: function() {
            var code = opts.solution;

            if (opts.setup) {
                code = opts.setup + '\n' + code;
            }

            return {name: 'csharp', 'args': ['-e', code]};
        }
    });
};


