"use strict";

const temp = require('temp');

const shovel = require('../shovel');
const codeWriteSync = require('../utils/code-write-sync');

module.exports.run = function run(opts, cb) {
  temp.track();
  var haskellCodeDir = temp.mkdirSync('haskell');
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      if (opts.setup) codeWriteSync('haskell', opts.setup, haskellCodeDir);
      runCode({
        name: 'runhaskell',
        args: [
          '--', '-fno-warn-tabs',
          '-i' + ['frameworks/haskell', haskellCodeDir].join(':'),
          codeWriteSync('haskell', opts.solution, haskellCodeDir, "Main.hs")
        ]
      });
    },
    testIntegration: function(runCode) {
      if (opts.setup) codeWriteSync('haskell', opts.setup, haskellCodeDir);
      var solutionFileName = codeWriteSync('haskell', opts.solution, haskellCodeDir, "Main.hs"),
          fixtureFileName = solutionFileName.split('/').pop() == "Main.hs" ?
            codeWriteSync('haskell', opts.fixture, haskellCodeDir) :
            codeWriteSync('haskell', opts.fixture, haskellCodeDir, "Main.hs");

      process.env["solutionFileName"] = solutionFileName;

      runCode({
        name: 'runhaskell',
        args: [
          '--', '-fno-warn-tabs',
          '-i' + ['frameworks/haskell', haskellCodeDir].join(':'),
          fixtureFileName,
        ],
        options: {env: process.env}
      });
    }
  });
};
