"use strict";

const shovel = require('../shovel');
const writeFileSync = require('../utils/write-file-sync');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly(runCode) {
      runCode({
        name: 'lua',
        args: [writeFileSync(opts.dir, 'solution.lua', opts.solution)],
        options: {cwd: opts.dir}
      });
    },

    testIntegration(runCode) {
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
  });
};
