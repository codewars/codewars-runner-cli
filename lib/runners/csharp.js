var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    nunitAssemblies = ['nunit.core.dll', 'nunit.framework.dll', 'nunit.core.interfaces.dll', 'nunit.util', 'Newtonsoft.Json.dll'].join(','),
    nunitPath = '/runner/frameworks/csharp/nunit/bin';

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode, fail) {
      var codeFile = util.codeWriteSync('csharp', opts.solution, opts.dir, 'code.cs'),
          exe = path.join(opts.dir, 'solution.exe'),
          args = ['mcs', '-out:' + exe, codeFile];

      if (opts.setup) {
        args.push(util.codeWriteSync('csharp', opts.setup, opts.dir, 'setup.cs'));
      }

      compile(args, function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);
        runCode({name: 'mono', args: [exe]});
      });
    },
    testIntegration: function(runCode, fail) {
      // copydir('/runner/frameworks/nunit', opts.dir);

      var dll = path.join(opts.dir, 'test.dll'),
          args = [
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
        args = args.concat(opts.filteredFilePaths('cs'));
      }
      if (opts.solution) {
        args.push(util.codeWriteSync('csharp', opts.solution, opts.dir, 'code.cs'));
      }
      if (opts.fixture) {
        args.push(util.codeWriteSync('csharp', opts.fixture, opts.dir, 'fixture.cs'));
      }
      if (opts.setup) {
        args.push(util.codeWriteSync('csharp', opts.setup, opts.dir, 'setup.cs'));
      }

      // compile
      compile(args, function(error, stdout, stderr) {
        if (error) return fail(error, stdout, stderr);

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
  });

  function compile(args, cb) {
    opts.publish('status', 'Compiling...');
    util.exec(args.join(' '), {stdout: 'ignore', handleError: true}, cb);
  }
};

