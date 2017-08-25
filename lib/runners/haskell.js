"use strict";

const temp = require('temp').track();

const codeWriteSync = require('../utils/code-write-sync');

module.exports = {
  solutionOnly(opts, runCode) {
    const dir = temp.mkdirSync('haskell');
    if (opts.setup) codeWriteSync('haskell', opts.setup, dir);
    runCode({
      name: 'runhaskell',
      args: [
        '--', '-fno-warn-tabs',
        '-i' + ['frameworks/haskell', dir].join(':'),
        codeWriteSync('haskell', opts.solution, dir, "Main.hs")
      ]
    });
  },
  testIntegration(opts, runCode) {
    const dir = temp.mkdirSync('haskell');
    if (opts.setup) codeWriteSync('haskell', opts.setup, dir);
    const solutionFileName = codeWriteSync('haskell', opts.solution, dir, "Main.hs");
    const fixtureFileName = solutionFileName.split('/').pop() == "Main.hs" ?
          codeWriteSync('haskell', opts.fixture, dir) :
          codeWriteSync('haskell', opts.fixture, dir, "Main.hs");

    process.env["solutionFileName"] = solutionFileName;

    runCode({
      name: 'runhaskell',
      args: [
        '--', '-fno-warn-tabs',
        '-i' + ['frameworks/haskell', dir].join(':'),
        fixtureFileName,
      ],
      options: {env: process.env}
    });
  }
};
