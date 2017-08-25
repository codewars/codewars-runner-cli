"use strict";

const maybeTransform = require('./maybe-transform');
const createNodeRunCommand = require('./create-node-run-command');

module.exports = function execNode(opts, code, runCode, fail) {
  try {
    code = maybeTransform(code, opts);
    const args = ['-e', code, '--no-deprecation'];
    if (opts.requires) {
      for (const f of opts.requires) args.push("--require", f);
    }
    runCode(createNodeRunCommand(args, opts));
  }
  catch (ex) {
    fail(ex);
  }
};
