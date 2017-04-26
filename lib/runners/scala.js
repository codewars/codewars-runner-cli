var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    exec = require('child_process').exec,
    temp = require('temp');

module.exports.run = function run(opts, cb) {
  temp.track();
  var dir = temp.mkdirSync('scala');

  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      var classDirectory = path.join(dir, 'classes'),
          solutionFile = util.codeWriteSync('scala', opts.solution, dir),
          solutionClassName = path.basename(solutionFile, '.scala'),
          args = ['scalac', '-d', classDirectory, solutionFile];

      if (opts.setup) {
        var setupFile = util.codeWriteSync('scala', opts.setup, dir);
        args.push(setupFile);
      }
      util.mkdirParentSync(classDirectory);
      exec(args.join(' '), function(error) {
        if (error) return fail(error);
        runCode({
          'name': 'scala',
          'args': ['-classpath', classDirectory, solutionClassName]
        });
      });
    },
    testIntegration: function(runCode, fail) {
      throw new Error('Test framework is not supported');
    }
  });
};
