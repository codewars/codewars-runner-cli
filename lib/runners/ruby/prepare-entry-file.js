"use strict";

const path = require('path');

const fs = require('fs-extra');

// used when a single file will be used as the entry point. It will include the other files separately
module.exports = function prepareEntryFile(opts, require) {
  // if there is no require and an entryPath is provided than just use that file directly
  if (!require && opts.entryPath) return opts.entryPath;

  const entry = [
    "`rm -rf /workspace/.entry.rb`",
    require || ''
  ];

  if (opts.entryPath) {
    entry.push(`require "${opts.entryPath}"`);
  }
  else {
    // TODO is opts.dir necessary? it's hard-coded as /workspace here
    if (opts.setup) {
      const setupFile = path.join(opts.dir, 'setup.rb');
      fs.outputFileSync(setupFile, opts.setup);
      entry.push(`require "${setupFile}"`);
      // have the file remove itself from the file system after it is loaded, so that it cannot be read by users trying to solve
      entry.push("`rm -rf /workspace/setup.rb`");
    }

    const solutionFile = path.join(opts.dir, 'solution.rb');
    fs.outputFileSync(solutionFile, opts.solution);
    entry.push(`require "${solutionFile}"`);

    if (opts.fixture) {
      const fixtureFile = path.join(opts.dir, '.spec.rb');
      fs.outputFileSync(fixtureFile, opts.fixture);
      entry.push(`require "${fixtureFile}"`);
    }
  }
  const entryFile = path.join(opts.dir, '.entry.rb');
  fs.outputFileSync(entryFile, entry.join('\n'));
  return entryFile;
};
