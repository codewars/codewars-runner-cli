"use strict";

const path = require('path');
const exec = require('child_process').exec;

const fs = require('fs-extra');

module.exports = {
  solutionOnly(opts, runCode, fail) {
    const codeFile = path.join(opts.dir, 'code.cs');
    fs.outputFileSync(codeFile, opts.solution);
    const exe = path.join(opts.dir, 'solution.exe');
    const args = ['mcs', '-out:' + exe, codeFile];

    if (opts.setup) {
      const setupFile = path.join(opts.dir, 'setup.cs');
      fs.outputFileSync(setupFile, opts.setup);
      args.push(setupFile);
    }

    opts.publish('status', 'Compiling...');
    exec(args.join(' '), function(error, stdout, stderr) {
      if (error || stderr) {
        return fail({
          stdout: stdout,
          stderr: (stderr ? stderr + '\n' : '') + (error ? error + '' : ''),
          exitCode: error && error.code,
          exitSignal: error && error.signal,
        });
      }

      runCode({name: 'mono', args: [exe]});
    });
  },
  testIntegration(opts, runCode, fail) {
    const nunitPath = '/runner/frameworks/csharp/nunit/bin';
    const nunitAssemblies = ['nunit.core.dll', 'nunit.framework.dll', 'nunit.core.interfaces.dll', 'nunit.util', 'Newtonsoft.Json.dll'].join(',');
    const dll = path.join(opts.dir, 'test.dll');
    const args = [
      'mcs',
      '-out:' + dll,
      `-lib:${opts.dir},/runner/frameworks/csharp/mono-4.5,${nunitPath}`,
      '-langversion:Default',
      '-sdk:4.5',
      '-warn:2',
      '-target:library',
      '-r:' + nunitAssemblies,
      '-r:System.Numerics.dll',
      '-r:System.Drawing.dll',
      '-r:System.Data.dll',
      '-r:System.Data.SQLite.dll',
      '-r:System.Data.SQLite.Linq.dll',
      '-r:System.IO.dll',
      '-r:System.Linq.dll',
      '-r:System.Linq.Dynamic.dll',
      '-r:System.Linq.Expressions.dll',
      '-r:System.Messaging.dll',
      '-r:System.Threading.Tasks.dll',
      '-r:System.Xml.dll',
      '-r:Mono.Linq.Expressions.dll',
    ];

    if (opts.services.includes("mongodb")) {
      args.push('-r:MongoDB.Bson.dll');
      args.push('-r:MongoDB.Driver.Core.dll');
      args.push('-r:MongoDB.Driver.dll');
      args.push('-r:MongoDB.Dynamic.dll');
    }

    if (opts.services.includes("redis")) {
      args.push('-r:StackExchange.Redis.dll');
    }

    if (opts.services.includes("postgres")) {
      args.push('-r:Npgsql.dll');
    }

    // include any references that have been included
    if (opts.references) {
      opts.references.forEach(ref => {
        let name = `-r:${ref}`;
        if (args.indexOf(name) === -1) {
          args.push(name);
        }
      });
    }

    if (opts.files) {
      const files = opts.filteredFilePaths('cs');
      if (files.length > 0) args.push.apply(args, files);
    }
    if (opts.solution) {
      const file = path.join(opts.dir, 'code.cs');
      fs.outputFileSync(file, opts.solution);
      args.push(file);
    }
    if (opts.fixture) {
      const file = path.join(opts.dir, 'fixture.cs');
      fs.outputFileSync(file, opts.fixture);
      args.push(file);
    }
    if (opts.setup) {
      const file = path.join(opts.dir, 'setup.cs');
      fs.outputFileSync(file, opts.setup);
      args.push(file);
    }

    // compile
    opts.publish('status', 'Compiling...');
    exec(args.join(' '), function(error, stdout, stderr) {
      if (error || stderr) {
        return fail({
          stdout: stdout,
          stderr: (stderr ? stderr + '\n' : '') + (error ? error + '' : ''),
          exitCode: error && error.code,
          exitSignal: error && error.signal,
        });
      }

      runCode({
        name: "mono",
        args: [
          path.join(nunitPath, 'nunit-console.exe'),
          '-nologo',
          '-noresult',
          dll
        ],
        options: {env: process.env}
      });
    });
  }
};
