var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp').track(),
    path = require('path'),
    nunitAssemblies = ['nunit.core.dll', 'nunit.core.interfaces.dll', 'nunit.util.dll', 'nunit.framework.dll', 'Newtonsoft.Json.dll', 'EntityFramework.dll', 'Qualified.dll'].join(','),
    nunitPath = path.resolve(__dirname, '..', '..', 'frameworks', 'csharp', 'nunit');

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function (exec) {
            var dir = temp.mkdirSync('csharp'),
                codeFile = util.codeWriteSync('csharp', opts.solution, dir, 'code.cs'),
                exe = path.join(dir, 'solution.exe'),
                args = ['mcs', '-out:' + exe, codeFile];

            if (opts.setup) {
                args.push(util.codeWriteSync('csharp', opts.setup, dir, 'setup.cs'));
            }
            compile(args, function (error) {
                if (error) return exec(error);
                opts.publish('status', 'Compiling...');
                exec({'name': 'mono', 'args': [exe]});
            });
        },
        testIntegration: function (exec) {
            var dir = temp.mkdirSync('csharp'),
                fixtureFile = util.codeWriteSync('csharp', opts.fixture, dir, 'fixture.cs'),
                codeFile = util.codeWriteSync('csharp', opts.solution, dir, 'code.cs'),
                testDLL = path.join(dir, 'test.dll'),
                args = [
                    'mcs',
                    '-out:' + testDLL,
                    '-lib:' + nunitPath,
                    '-r:' + nunitAssemblies,
                    '-reference:System.Numerics.dll',
                    '-reference:System.Drawing.dll',
                    '-reference:System.Data.dll',
                    '-reference:System.IO.dll',
                    '-reference:System.Linq.dll',
                    '-reference:System.Linq.Dynamic.dll',
                    '-reference:System.Linq.Expressions.dll',
                    '-reference:System.Messaging.dll',
                    '-reference:System.Threading.Tasks.dll',
                    '-reference:System.Xml.dll',
                    '-reference:System.Web.dll',
                    '-reference:Mono.Linq.Expressions.dll',
                    '-target:library',
                    '-warn:2',
                    codeFile,
                    fixtureFile
                ];

            if (opts.setup) {
                args.push(util.codeWriteSync('csharp', opts.setup, dir, 'setup.cs'));
            }

            // compile
            compile(args, function (json) {
                if (json) return exec(json);

                opts.publish('status', 'Compiling...');
                
                // execute
                exec({
                    name: "mono",
                    args: [
                        path.join(nunitPath, 'nunit-console.exe'),
                        '-nologo',
                        '-noresult',
                        testDLL
                    ],
                    options: {env: process.env}
                });
            });
        }
    });

    function compile(args, cb) {
        util.exec(args.join(' '), {stdout: 'ignore', handleError: true}, cb);
    }
};

