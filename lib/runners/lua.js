"use strict";

const shovel = require('../shovel');
const writeFileSync = require('../util').writeFileSync;

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly(runCode) {
      runCode({
        name: 'lua',
        args: [writeFileSync(opts.dir, 'solution.lua', opts.solution, true)],
        options: {cwd: opts.dir}
      });
    },

    testIntegration(runCode) {
      writeFileSync(opts.dir, 'solution.lua', opts.solution, true);
      if (opts.setup) writeFileSync(opts.dir, 'setup.lua', opts.setup, true);
      runCode({
        name: 'busted',
        args: [
          writeFileSync(opts.dir, 'fixture.lua', opts.fixture, true),
          '--output=/runner/frameworks/lua/codewars.lua'
        ],
        options: {cwd: opts.dir}
      });
    }
  });
};
