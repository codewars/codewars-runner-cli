"use strict";

const path = require('path');
const exec = require('child_process').exec;

const fs = require('fs-extra');
const temp = require('temp').track();

const cppDir = '/runner/frameworks/cpp/';
const main = fs.readFileSync('/runner/frameworks/cpp/main.cpp');

module.exports = {
  solutionOnly(opts, runCode, fail) {
    const dir = temp.mkdirSync('cpp');
    if (opts.setup) handleSetup(opts, dir);
    const executable = path.join(dir, 'solution');
    const solutionFile = path.join(dir, 'solution.cpp');
    fs.outputFileSync(solutionFile, opts.solution);
    compile([solutionFile, '-o', executable], function(error, stdout, stderr) {
      if (error) return fail(error, stdout, stderr);
      opts.publish('stdout', stdout);
      runCode({name: executable, args: []});
    });
  },

  testIntegration(opts, runCode, fail) {
    const dir = temp.mkdirSync('cpp');
    if (opts.setup) handleSetup(opts, dir);
    const executable = path.join(dir, 'solution');
    const solutionFile = path.join(dir, 'solution.cpp');
    fs.outputFileSync(solutionFile, [
      '#include <igloo/igloo_alt.h>',
      'using namespace igloo;',
      opts.solution,
      opts.fixture,
      main
    ].join('\n'));
    compile(['-isystem', cppDir, solutionFile, '-o', executable], function(error, stdout, stderr) {
      if (error) return fail(error, stdout, stderr);
      opts.publish('stdout', stdout);
      runCode({name: executable, args: []});
    });
  },

  sanitizeStdErr(opts, error) {
    return error
      .replace(/clang.*-std=c[^\s]+/g, '')
      .replace(/Error: Command failed:/g, '')
      .replace(/\/tmp.*(solution\.cpp|solution)[:0-9]*/g, '')
      .replace('\n', '')
      .replace('  ', ' ')
      .replace(opts.setup || '', '')
      .replace(opts.fixture || '', '');
  }
};

function compile(args, cb) {
  exec(`clang++-3.6 -stdlib=libc++ -std=c++1y ${args.join(' ')}`, cb);
}

function handleSetup(opts, dir) {
  fs.outputFileSync(path.join(dir, 'setup.h'), opts.setup);
  opts.solution = '#include "setup.h"\n' + opts.solution;
}
