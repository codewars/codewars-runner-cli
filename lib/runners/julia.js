"use strict";

const temp = require('temp');

const shovel = require('../shovel');
const codeWriteSync = require('../utils/code-write-sync');

module.exports.run = function run(opts, cb) {
  temp.track();
  var juliaCodeDir = temp.mkdirSync('julia');
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      codeWriteSync('julia', opts.code, juliaCodeDir, 'solution1.jl', true);
      if (opts.setup) codeWriteSync('julia', opts.setup, juliaCodeDir);

      let runCmd1 = `
              include("${juliaCodeDir}/solution1.jl")
            `;

      runCode({
        name: 'julia',
        args: ['-ie', runCmd1]
      });
    },
    testIntegration: function(runCode) {
      codeWriteSync('julia', opts.code, juliaCodeDir, 'solution.jl', true);

      if (opts.setup) codeWriteSync('julia', opts.setup, juliaCodeDir);

      let runCmd = `
              include("frameworks/julia/Test.jl")
              include("${juliaCodeDir}/solution.jl")
              using Test
              ${opts.fixture}
            `;

      runCode({
        name: 'julia',
        args: ['-ie', runCmd]
      });
    }
  });
};
