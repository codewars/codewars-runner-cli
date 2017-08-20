"use strict";

const path = require('path');

const fs = require('fs-extra');
const Convert = require('ansi-to-html');

module.exports = {
  solutionOnly(opts, runCode) {
    runCode({name: 'crystal', args: ['eval', opts.solution]});
  },
  testIntegration(opts, runCode) {
    // instead of using a tmp directory we use the special crystal directory so that we can
    // use the installed shards without having to copy
    const dir = '/home/codewarrior/crystal';
    fs.writeFileSync(path.join(dir, 'solution.cr'), opts.solution);
    fs.writeFileSync(path.join(dir, 'fixture.cr'), opts.fixture);
    if (opts.setup) fs.writeFileSync(path.join(dir, 'setup.cr'), opts.setup);

    var spec = `require "./formatter"`;
    if (opts.setup) spec += `\nrequire "./setup"`;
    spec += `\nrequire "./solution"`;
    spec += `\nrequire "./fixture"`;

    fs.writeFileSync(path.join(dir, 'spec.cr'), spec);
    runCode({name: 'crystal', args: ['spec', 'spec.cr'], options: {cwd: dir}});
  },
  transformBuffer(opts, buffer) {
    const convert = new Convert();
    buffer.stdout = convert.toHtml(buffer.stdout);

    var finished = buffer.stdout.search(/(?! )\d* examples?/);
    if (finished > 0) {
      var orig = buffer.stdout;
      buffer.stdout = orig.substr(0, finished).replace("Failures:\n", "Failure Summary:\n");
    }

    // crystal likes to write its compile errors to stdout, so lets swap them around
    if (buffer.stdout.indexOf('Error in ') === 0) {
      buffer.stderr = buffer.stdout;
      buffer.stdout = '';
    }
  },
  outputType: 'raw'
};
