var shovel = require('../shovel'),
    config = require('../config'),
    fs = require('fs'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
    temp.track();
    var racketCodeDir = temp.mkdirSync('racket');
    var args = [];
    shovel.start(opts, cb, {
        solutionOnly: function () {
            if (opts.setup) { 
              var setupFile = fs.path.join(racketCodeDir, 'setup.rkt');
              fs.writeFileSync(setupFile, opts.setup);
              args.push('-C', setupFile);
            }
            args.push('-e', opts.solution);
            return {
                name: 'racket',
                args: args 
	    };
        },
        fullProject: function () {
            var solutionFile = fs.path.join(racketCodeDir, 'solution.rkt');
            fs.writeFileSync(solutionFile, opts.solution);
            args.push('-C', solutionFile);
            if (opts.setup) { 
              var setupFile = fs.path.join(racketCodeDir, 'setup.rkt');
              fs.writeFileSync(setupFile, opts.setup);
              args.push('-C', setupFile);
            }

            return {
                name: 'racket',
                args: args
	    };
        }
    });
};
