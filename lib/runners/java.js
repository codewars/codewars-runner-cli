var shovel = require('../shovel'),
    config = require('../config'),
    fs = require('fs');

module.exports.run = function run(opts, cb) {
    //write the code to file
    fs.writeFileSync('Solution.java', opts.solution);
    if (opts.fixture) {
        fs.writeFileSync('TestFixture.java', opts.fixture);
    }

    //Compile the code
    shovel.compile(opts, function (buffer) {
            //This is the callback that is called after the code compiles

            //run the code
            shovel.start(opts, cb, {
                solutionOnly: function () {
                    return {name: 'java', 'args': ['-XX:ErrorFile=/dev/null', 'Solution']};
                },
                fullProject: function () {
                    return {name: 'java', 'args': [
			'-XX:ErrorFile=/dev/null',
                        '-cp',
                        ['.',
                            '/root/.m2/repository/junit/junit/4.11/*',
                            '/root/.m2/repository/org/hamcrest/hamcrest-core/1.3/*',
                            'frameworks/java/'].join(':'),
                        'CwTestRunner',
                        'TestFixture']};
                }
            });
        }, { //this compiles the code
            solutionOnly: function () {
                return {name: 'javac', 'args': ['Solution.java']};
            },
            fullProject: function () {
                return {name: 'javac', 'args': ['-cp',
                    ['.', '/root/.m2/repository/junit/junit/4.11/*'].join(":"),
                    'frameworks/java/CwRunListener.java',
                    'frameworks/java/CwTestRunner.java',
                    'Solution.java',
                    'TestFixture.java']};
            }
        },
        //This will suppress the buffer report if the code compiles
        true);
};
