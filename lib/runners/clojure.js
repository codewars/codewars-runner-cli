var shovel = require('../shovel');

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function () {
            return {
                name: 'java',
                args: ['-jar', '/codewars/jvm-runner/target/jvm-runner-0.1.1-standalone.jar'],
                stdin: JSON.stringify(opts)
            };
        },
        fullProject: function () {
            return {
                name: 'java',
                args: ['-jar', '/codewars/jvm-runner/target/jvm-runner-0.1.1-standalone.jar'],
                stdin: JSON.stringify(opts)
            };
        }
    });
};
