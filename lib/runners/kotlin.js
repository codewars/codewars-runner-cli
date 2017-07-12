var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp');

var KOTLIN_BASE_DIR = '/usr/local/lib/kotlin/kotlinc/bin/';
var KOTLIN_DEFAULT_SOLUTION = 'Solution';
var KOTLIN_DEFAULT_SETUP = 'Setup';

module.exports.run = function run(opts, cb) {
  temp.track();
  var dir = temp.mkdirSync('kotlin');

  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      var classDirectory = path.join(dir, 'classes'),
          solutionFile = util.codeWriteSync('kotlin', opts.solution, dir, KOTLIN_DEFAULT_SOLUTION),
          solutionClassName = path.basename(solutionFile, '.kt'),
          args = [KOTLIN_BASE_DIR + 'kotlinc', '-d', classDirectory, solutionFile],
          runArgs = ['-classpath', classDirectory, solutionClassName + 'Kt'];

      if (opts.setup) {
        var setupFile = util.codeWriteSync('kotlin', opts.setup, dir, KOTLIN_DEFAULT_SETUP);
        args.push(setupFile);
      }
      util.mkdirParentSync(classDirectory);
      util.exec(args.join(' '), function(error) {
        if (error) return fail(error);
        runCode({
          'name': KOTLIN_BASE_DIR + 'kotlin',
          'args': runArgs
        });
      });
    },
    testIntegration: function(run) {
      throw new Error('Test framework is not supported');
    }
  });
};
