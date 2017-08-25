"use strict";

const path = require('path');
const exec = require('child_process').exec;

const fs = require('fs-extra');
const temp = require('temp').track();

module.exports = {
  solutionOnly(opts, run) {
    const dir = temp.mkdirSync('arm');
    const executable = path.join(dir, 'solution');
    const solutionFile = path.join(dir, 'solution.s');
    fs.outputFileSync(solutionFile, opts.solution + '\n');
    const objectFile = path.join(dir, 'solution.o');
    const armCommand = ['arm-linux-gnueabi-as', solutionFile, '-o', objectFile].join(' ');
    // Check for whether we need to link against libc
    const linker = opts.solution.search(/\.glob[a]?l\W+_start/) == -1 ? "arm-linux-gnueabi-gcc-4.7" : "arm-linux-gnueabi-ld";
    const linkerCommand = [linker, objectFile, '-o', executable].join(' ');
    exec(armCommand, function() {
      exec(linkerCommand, function() {
        run({'name': 'qemu-arm', 'args': ['-L', '/usr/arm-linux-gnueabi/', executable]});
      });
    });
  },
  testIntegration(opts, run) {
    throw new Error('Test framework is not supported');
  }
};
