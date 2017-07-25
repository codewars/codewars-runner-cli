var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp'),
    exec = require('child_process').exec,
    fs = require('fs'),
    frameworkPath = '/runner/frameworks/powershell/';;

module.exports.run = function run(opts, cb) {

    temp.track();
    var dir = temp.mkdirSync('powershell');

    shovel.start(opts, cb, {
        solutionOnly: function (runCode) {
            var psFile = util.codeWriteSync('powershell', opts.solution, dir, 'code.ps1');

            runCode({
                name: 'powershell',
                args: ['-NoLogo',
                    '-NoProfile',
                    '-NonInteractive',
                    '-ExecutionPolicy',
                    'ByPass',
                    '-File',
                    psFile
                ],
                options: { cwd: opts.dir }
            });
        },

        testIntegration: function (runCode, fail) {
            var psFile = util.codeWriteSync('powershell', opts.solution, dir, 'code.ps1');

            opts.fixture = `. ${psFile};\n ` + opts.fixture;
            psTests = util.codeWriteSync('powershell', opts.fixture, dir, 'test.ps1');

            runCode({
                name: 'powershell',
                args: ['-NoLogo',
                    '-NoProfile',
                    '-NonInteractive',
                    '-ExecutionPolicy',
                    'ByPass',
                    '-Command',
                    `& Import-Module Pester; & { Invoke-Pester -Script ${psTests} -OutputFile result.xml -OutputFormat NunitXml -Quiet; ${frameworkPath}/ConvertFrom-NUnit.ps1 -File result.xml }`
                ],
                options: { cwd: opts.dir }

            });

        }
    });

};