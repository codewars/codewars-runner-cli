var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp').track(),
    path = require('path'),
    nunitAssemblies = ['nunit.core.dll', 'nunit.core.interfaces.dll', 'nunit.util.dll', 'nunit.framework.dll', 'Newtonsoft.Json.dll', 'EntityFramework.dll', 'Qualified.dll'].join(','),
    nunitPath = path.resolve(__dirname, '..', '..', 'frameworks', 'csharp', 'nunit');

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function (runCode, fail) {
            var dir = temp.mkdirSync('csharp'),
                codeFile = util.codeWriteSync('csharp', opts.solution, dir, 'code.cs'),
                exe = path.join(dir, 'solution.exe'),
                args = ['mcs', '-out:' + exe, codeFile];

            if (opts.setup) {
                args.push(util.codeWriteSync('csharp', opts.setup, dir, 'setup.cs'));
            }
            
            compile(args, function (error, stdout, stderr) {
                if (error) return fail(error, stdout, stderr);
                runCode({'name': 'mono', 'args': [exe]});
            });
        },
        testIntegration: function (runCode, fail) {
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
            compile(args, function (error, stdout, stderr) {
                if (error) return fail(error, stdout, stderr);
                
                // execute
                runCode({
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
        opts.publish('status', 'Compiling...');
        util.exec(args.join(' '), {stdout: 'ignore', handleError: true}, cb);
    }
};

