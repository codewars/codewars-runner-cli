var shovel = require('../shovel'),
    config = require('../config'),
    fs = require('fs');

module.exports.run = function run(opts, cb){
    //write the code to file
    fs.writeFileSync('Solution.java', opts.solution);
    //TODO write setup and test framework
    //Compile the code
    shovel.start({language: 'java'}, function(buffer) {
        //TODO: add error handling for this
        console.log(buffer);
        //run the code
        //We do not need multiple cases here, because everything is in a single file
        opts.fixture = false;
        shovel.start(opts, cb, {
            solutionOnly: function() {
                return {name: 'java', 'args': ['Solution']};
            },
        });
    }, { solutionOnly: function() {
        return {name: 'javac', 'args': ['Solution.java']}
    }
    });
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
