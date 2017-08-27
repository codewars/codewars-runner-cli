"use strict";

const prepareEntryFile = require('./prepare-entry-file');

module.exports = function prepareRSpec(opts, runCode) {
  runCode({
    name: 'rspec',
    args: [
      prepareEntryFile(opts),
      '--require', '/runner/frameworks/ruby/cwrspecformatter.rb',
      '--format', 'CwRSpecFormatter'
    ],
    options: {cwd: opts.dir}
  });
};
