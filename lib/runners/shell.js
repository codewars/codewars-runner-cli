"use strict";

const path = require('path');

const fs = require('fs-extra');
const temp = require('temp').track();

const prepareRSpec = require('./ruby/prepare-rspec');

module.exports = {
  solutionOnly(opts, runCode) {
    const dir = temp.mkdirSync('bash');
    const file = path.join(dir, 'solution.sh');
    fs.outputFileSync(file, opts.solution);
    runCode({name: opts.languageVersion || 'sh', 'args': [file]});
  },
  testIntegration(opts, runCode) {
    process.env.SHELL = opts.languageVersion || 'sh';

    if (!opts.projectMode) {
      prepareSetup(opts);
      opts.solution = "#";
      opts.fixture = `\`rm -rf /workspace/fixture.rb\` ; ${opts.fixture}`;
    }

    prepareRSpec(opts, runCode);
  }
};

function prepareSetup(opts) {
  opts.setup = `
        require '/runner/frameworks/ruby/shell'
        ${opts.setup || ''}
    `;
}
