"use strict";

const path = require('path');

const fs = require('fs-extra');
const temp = require('temp');

const shovel = require('../shovel');
const ruby = require('./ruby');

module.exports.run = function run(opts, cb) {
  temp.track();
  var dir = temp.mkdirSync('bash');

  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      const file = path.join(dir, 'solution.sh');
      fs.outputFileSync(file, opts.solution);
      runCode({name: opts.languageVersion || 'sh', 'args': [file]});
    },
    testIntegration: function(runCode) {
      process.env.SHELL = opts.languageVersion || 'sh';

      if (!opts.projectMode) {
        prepareSetup(opts);
        opts.solution = "#";
        opts.fixture = `\`rm -rf /workspace/fixture.rb\` ; ${opts.fixture}`;
      }

      ruby.prepareRSpec(opts, runCode);
    }
  });
};

function prepareSetup(opts) {
  opts.setup = `
        require '/runner/frameworks/ruby/shell'
        ${opts.setup || ''}
    `;
}
