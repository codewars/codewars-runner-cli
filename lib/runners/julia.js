'use strict';

var shovel = require('../shovel'),
    codeWriteSync = require('../util').codeWriteSync,
    temp = require('temp');

module.exports.run = function run(opts, cb) {
  temp.track();
  var juliaCodeDir = temp.mkdirSync('julia');
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      if (opts.setup) codeWriteSync('julia', opts.setup, juliaCodeDir);
      runCode({
        name: 'julia',
        args: ['-P', ['push!(LOAD_PATH, "', juliaCodeDir, '", "frameworks/julia")'].join(""),
          '-e', opts.solution]
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
