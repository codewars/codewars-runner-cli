var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
  temp.track();
  var dir = temp.mkdirSync('arm');

  shovel.start(opts, cb, {
    solutionOnly: function(run) {
      var executable = path.join(dir, 'solution'),
          solutionFile = util.codeWriteSync('arm', opts.solution + '\n', dir, 'solution.s'),
          objectFile = solutionFile.replace(/\.[^\.]+$/, '.o'),
          armCommand = ['arm-linux-gnueabi-as', solutionFile, '-o', objectFile].join(' '),
        // Check for whether we need to link against libc
          linker = opts.solution.search(/\.glob[a]?l\W+_start/) == -1 ? "arm-linux-gnueabi-gcc-4.7" : "arm-linux-gnueabi-ld",
          linkerCommand = [linker, objectFile, '-o', executable].join(' ');
      util.exec(armCommand, function() {
        util.exec(linkerCommand, function() {
          run({'name': 'qemu-arm', 'args': ['-L', '/usr/arm-linux-gnueabi/', executable]});
        });
      });
    },
    testIntegration: function(run) {
      throw new Error('Test framework is not supported');
    }
  });
};
