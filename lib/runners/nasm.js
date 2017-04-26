var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
  temp.track();
  var dir = temp.mkdirSync('nasm');

  shovel.start(opts, cb, {
    solutionOnly: function(run) {
      // TODO: Support setup code; setup code should either be NASM or C, and we should automatically detect it
      var executable = path.join(dir, 'solution'),
          solutionFile = util.codeWriteSync('nasm', opts.solution + '\n', dir, 'solution.asm'),
          nasmCommand = ['nasm', '-felf64', solutionFile].join(' '),
          objectFile = solutionFile.replace(/\.[^\.]+$/, '.o'),
        // Check for whether we need to link against libc
          linker = opts.solution.search(/global\W+_start/) == -1 ? "gcc" : "ld",
          linkerCommand = [linker, objectFile, '-o', executable].join(' ');
      util.exec(nasmCommand, function() {
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
