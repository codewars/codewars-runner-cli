"use strict";

const path = require('path');
const exec = require('child_process').exec;

const fs = require('fs-extra');
const temp = require('temp').track();

module.exports = {
  solutionOnly(opts, run) {
    const dir = temp.mkdirSync('nasm');
    // TODO: Support setup code; setup code should either be NASM or C, and we should automatically detect it
    const executable = path.join(dir, 'solution');
    const solutionFile = path.join(dir, 'solution.asm');
    fs.outputFileSync(solutionFile, opts.solution + '\n');
    const nasmCommand = ['nasm', '-felf64', solutionFile].join(' ');
    const objectFile = path.join(dir, 'solution.o');
    // Check for whether we need to link against libc
    const linker = opts.solution.search(/global\W+_start/) == -1 ? "gcc" : "ld";
    const linkerCommand = [linker, objectFile, '-o', executable].join(' ');
    exec(nasmCommand, function() {
      exec(linkerCommand, function() {
        run({'name': executable, 'args': []});
      });
    });
  },
  testIntegration(opts, run) {
    throw new Error('Test framework is not supported');
  }
};
