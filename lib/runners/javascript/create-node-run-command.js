"use strict";

const fs = require('fs');

const versionInfo = require('./version-info');

module.exports = function createNodeRunCommand(args, opts) {
  const version = versionInfo(opts);
  const nodeCmd = `/usr/local/n/versions/node/${version.node}/bin/node`;
  if (!fs.existsSync(nodeCmd)) {
    throw new Error(`Invalid node version: ${version.node}\n`);
  }
  return {
    name: nodeCmd,
    args: args,
    options: {
      cwd: opts.dir
    }
  };
};
