var shovel = require('../shovel'),
    path = require('path');

module.exports.run = function run(opts, cb) {
    var uberJar = path.resolve(__dirname, '../../jvm-runner/target/jvm-runner-0.1.1-standalone.jar');
    shovel.start(opts, cb, {
        solutionOnly: function () {
            return {
                name: 'java',
                args: ['-jar', uberJar],
                stdin: JSON.stringify(opts)
            };
        },
        fullProject: function () {
            return {
                name: 'java',
                args: ['-jar', uberJar],
                stdin: JSON.stringify(opts)
            };
        }
    });
};
