"use strict";

const path = require('path');

const fs = require('fs-extra');

module.exports = {
  solutionOnly(opts, runCode) {
    const psFile = path.join(opts.dir, 'code.ps1');
    fs.outputFileSync(psFile, opts.solution);

    runCode({
      name: 'powershell',
      args: [
        '-NoLogo',
        '-NoProfile',
        '-NonInteractive',
        '-ExecutionPolicy',
        'ByPass',
        '-File',
        psFile
      ],
      options: {
        cwd: opts.dir,
      }
    });
  },

  testIntegration(opts, runCode, fail) {
    const psFile = path.join(opts.dir, 'code.ps1');
    const psTests = path.join(opts.dir, 'test.ps1');
    fs.outputFileSync(psFile, opts.solution);
    fs.outputFileSync(psTests, `. ${psFile};\n` + opts.fixture);

    runCode({
      name: 'powershell',
      args: [
        '-NoLogo',
        '-NoProfile',
        '-NonInteractive',
        '-ExecutionPolicy',
        'ByPass',
        '-Command',
        [
          `& Import-Module Pester;`,
          `& {`,
          `Invoke-Pester`,
          `-Script ${psTests}`,
          `-OutputFile result.xml`,
          `-OutputFormat NunitXml`,
          `-Quiet;`,
          `/runner/frameworks/powershell/ConvertFrom-NUnit.ps1 -File result.xml`,
          `}`,
        ].join(' '),
      ],
      options: {
        cwd: opts.dir,
      }
    });
  }
};
