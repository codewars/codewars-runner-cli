var shovel = require('../shovel'),
    config = require('../config'),
    fs = require('fs');

module.exports.run = function run(opts, cb){
    //write the code to file
    fs.writeFileSync('Solution.java', opts.solution);
    if(opts.fixture)
    {
        fs.writeFileSync('TestFix.java', opts.fixture);
    }
    //TODO write setup and test framework
    //Compile the code
    shovel.compile(opts, function(buffer) {
        //This is the callback that is called after the code compiles

        //run the code
        shovel.start(opts, cb, {
            solutionOnly: function() {
                return {name: 'java', 'args': ['Solution']};
            },
            fullProject: function() {
                return {name: 'java', 'args': ['-cp',
                                               '.:frameworks/java/junit.jar:'+
                                               'frameworks/java/hamcrest-core.jar:'+
                                               'frameworks/java/',
                                               'CwTestRunner',
                                               'TestFix']};
            }
        });
    }, { //this compiles the code
        solutionOnly: function() {
            return {name: 'javac', 'args': ['Solution.java']};
        },
        fullProject: function() {
            return {name: 'javac', 'args': ['-cp',
                                            '.:frameworks/java/junit.jar',
                                            'frameworks/java/CwRunListener.java',
                                            'frameworks/java/CwTestRunner.java',
                                            'Solution.java',
                                            'TestFix.java']};
        }
    },
    //This will suppress the buffer report if the code compiles
    true);
};


function prepareCw2(opts) {
    var code = config.snippets.python.requireCw2;
    //TODO add setup and stuff
    code += opts.solution + "\n";
    code += opts.fixture;
    return {name: 'python', 'args': ['-c', code]};
}

function prepareUnittest(opts) {
    var code = config.snippets.python.requireUnittest;
    code += opts.solution + "\n";
    code += opts.fixture + "\n";
    code += config.snippets.python.runUnittest;
    return {name: 'python', 'args': ['-c', code]};
}
