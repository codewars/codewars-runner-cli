"use strict";

const fs = require('fs');
const path = require('path');

const temp = require('temp');

const shovel = require('../shovel');
const codeWriteSync = require('../utils/code-write-sync');

module.exports.run = function(opts, cb) {
  temp.track();
  var erlangCodeDir = temp.mkdirSync('erlang');
  shovel.start(opts, cb, {
    solutionOnly(runCode) {
      var setup = opts.setup ? compileFileSync(opts.setup, erlangCodeDir) : '';
      runCode({
        name: 'erl',
        args: [
          '-pz', erlangCodeDir,
          '-noshell',
          '-eval', [setup, opts.solution].join('')
        ],
        options: {
          env: {
            HOME: process.env['HOME'],
            ERL_CRASH_DUMP: "/dev/null"
          }
        },
      });
    },

    testIntegration(runCode, fail) {
      const solutionModule = getModuleName(opts.solution);
      if (solutionModule === null) return fail(new Error("Failed to extract module name."));
      const fixtureModule = getModuleName(opts.fixture);
      if (fixtureModule === null) return fail(new Error("Failed to extract module name."));

      if (opts.setup) {
        const setupModule = getModuleName(opts.setup);
        if (setupModule === null)
          return fail(new Error("Failed to extract module name."));
        if (setupModule === solutionModule)
          return fail(new Error("The name of the preloaded module must be different from the solution module."));
        fs.writeFileSync(path.join('/workspace/erlang/src', `${setupModule}.erl`), opts.setup);
      }

      fs.writeFileSync(path.join('/workspace/erlang/src', `${solutionModule}.erl`), opts.solution);
      fs.writeFileSync(path.join('/workspace/erlang/test', `${fixtureModule}.erl`), opts.fixture);

      runCode({
        name: 'rebar3',
        args: [
          'eunit',
        ],
        options: {
          cwd: '/workspace/erlang',
        }
      });
    },

    sanitizeStdOut(s) {
      return s.replace(/^Verifying dependencies\.\.\.\n/m, '')
        .replace(/^Linking \S+ to \S+\n/gm, '')
        .replace(/^Compiling \S+\n/gm, '')
        .replace(/^Performing EUnit tests\.\.\.\n/m, '')
        .replace('\u001b\[1mError running tests\n\u001b\[0m', '');
    }
  });
};

function erlangCompileCommand(fileName, erlangCodeDir) {
  return `compile:file("${fileName}", {outdir,"${erlangCodeDir}"}),`;
}

function compileFileSync(code,erlangCodeDir) {
  return erlangCompileCommand(codeWriteSync('erlang', code, erlangCodeDir), erlangCodeDir);
}

function getModuleName(code) {
  const m = code.match(/^-module\(([a-zA-Z][a-zA-Z\d_]*)\)/m);
  return (m === null) ? null : m[1];
}
