"use strict";

const prepareRSpec = require('./prepare-rspec');
const prepareCw2 = require('./prepare-cw2');

module.exports = {
  modifyOpts(opts) {
    // if a github repo was provided, add the workspace to the load path so that requires work correctly
    if (opts.githubRepo || opts.files || opts.gist) {
      opts.setup = `$LOAD_PATH << '/home/codewarrior'\n${opts.setup || ''}`;
    }
  },
  solutionOnly(opts, runCode) {
    var code = opts.solution;
    if (opts.setup) {
      code = opts.setup + '\n' + code;
    }
    runCode({
      name: 'ruby',
      args: ['-e', code],
      options: {cwd: opts.dir}
    });
  },
  testIntegration(opts, runCode) {
    switch (opts.testFramework) {
      case 'cw':
      case 'cw-2':
        return prepareCw2(opts, runCode);
      case 'rspec':
        return prepareRSpec(opts, runCode);

      default:
        throw new Error('Test framework is not supported');
    }
  },
  sanitizeStdErr: sanitize,
  sanitizeStdOut: sanitize,
};

function sanitize(opts, s) {
  return s
    .replace(/[\w/-]*(cw-2.rb):[\d]*:in( `(measure|wrap_error|it|describe)'<:LF:>)?/g, '')
    .replace(/-e:[\d]*:in/g, '')
    .replace('  ', ' ')
    .replace(/<:LF:> `(block in )?(<main>|describe|it)'/g, '')
    .replace('  ', ' ');
}
