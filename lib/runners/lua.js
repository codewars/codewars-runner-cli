"use strict";

const writeFileSync = require('../utils/write-file-sync');

module.exports = {
  solutionOnly(opts, runCode) {
    runCode({
      name: 'lua',
      args: [writeFileSync(opts.dir, 'solution.lua', opts.solution)],
      options: {cwd: opts.dir}
    });
  },

  testIntegration(opts, runCode) {
    writeFileSync(opts.dir, 'solution.lua', opts.solution);
    if (opts.setup) writeFileSync(opts.dir, 'setup.lua', opts.setup);
    runCode({
      name: 'busted',
      args: [
        writeFileSync(opts.dir, 'fixture.lua', opts.fixture),
        '--output=/runner/frameworks/lua/codewars.lua'
      ],
      options: {cwd: opts.dir}
    });
  }
};
