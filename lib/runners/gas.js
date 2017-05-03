var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp');

module.exports.run = function(opts, cb) {
  temp.track();
  var dir = temp.mkdirSync('gas');

  shovel.start(opts, cb, {
    solutionOnly: function(run) {
      var executable = path.join(dir, 'solution'),
          solutionFile = util.codeWriteSync('gas', opts.solution + '\n', dir, 'solution.s'),
          objectFile = solutionFile.replace(/\.[^\.]+$/, '.o'),
          gasCommand = ['gcc', '-c', solutionFile, '-o', objectFile].join(' '),
        // Check for whether we need to link against libc
          linker = opts.solution.search(/\.global\W+_start/) == -1 ? "gcc" : "ld",
          linkerCommand = [linker, objectFile, '-o', executable].join(' ');
      util.exec(gasCommand, function() {
        util.exec(linkerCommand, function() {
          run({'name': executable, 'args': []});
        });
      });
    },
    testIntegration: function(run) {
      throw new Error('Test framework is not supported');
    }
  });
};
