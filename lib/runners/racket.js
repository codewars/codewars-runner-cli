var shovel = require('../shovel'),
    config = require('../config'),
    fs = require('fs'),
    path = require('path'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
    temp.track();
    var racketCodeDir = temp.mkdirSync('racket');
    var args = ['-l', 'racket/base'];
    shovel.start(opts, cb, {
        solutionOnly: function () {
            if (opts.setup) { 
                var setupFile = path.join(racketCodeDir, 'setup.rkt');
                fs.writeFileSync(setupFile, opts.setup);
                args.push('-t', setupFile);
            }
            args.push('-e', opts.solution);
            return {
                name: 'racket',
                args: args 
	    };
        },
        fullProject: function () {
            if (opts.setup) { 
              var setupFile = path.join(racketCodeDir, 'setup.rkt');
              fs.writeFileSync(setupFile, opts.setup);
              args.push('-t', setupFile);
            }

            var solutionFile = fs.path.join(racketCodeDir, 'solution.rkt');
            fs.writeFileSync(solutionFile, opts.solution);
            args.push('-t', solutionFile);

            return {
                name: 'racket',
                args: args
	    };
        }
    });
};
