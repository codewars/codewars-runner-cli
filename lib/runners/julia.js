"use strict";

const temp = require('temp').track();

const shovel = require('../shovel');
const codeWriteSync = require('../utils/code-write-sync');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      const dir = temp.mkdirSync('julia');
      codeWriteSync('julia', opts.code, dir, 'solution1.jl');
      if (opts.setup) codeWriteSync('julia', opts.setup, dir);

      let runCmd1 = `
              include("${dir}/solution1.jl")
            `;

      runCode({
        name: 'julia',
        args: ['-ie', runCmd1]
      });
    },
    testIntegration: function(runCode) {
      const dir = temp.mkdirSync('julia');
      codeWriteSync('julia', opts.code, dir, 'solution.jl');

      if (opts.setup) codeWriteSync('julia', opts.setup, dir);

      let runCmd = `
              include("frameworks/julia/Test.jl")
              include("${dir}/solution.jl")
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
