var shovel = require('../shovel'),
    tmp = require('temporary'),
    path = require('path');

var nunitAssemblies = 'nunit.core.dll,nunit.core.interfaces.dll,nunit.util.dll,nunit.framework.dll';
var nunitPath = 'bin/csharp/nunit/';

module.exports.run = function run(opts, cb)
{
    // Allways use temp file even if code can be passed by arg.
    var file = new tmp.File();
    opts.tempFiles = (opts.tempFiles || []);
    opts.tempFiles.push(file.path);
    var outFile = new tmp.File();


    var code = opts.solution;
    if (opts.setup)
    {
        code = opts.setup + '\n' + code;
    }
    file.writeFileSync(code);

    if(opts.fixture)
    {
        var fixtureFile = new tmp.File();
        opts.tempFiles.push(fixtureFile);
        fixtureFile.writeFileSync(opts.fixture);
    }
    shovel.start(opts, cb,
    {
        compile:
        {
            solutionOnly: function()
            {
                return {name: 'mcs', args: ['-langversion:4', '-out:' + outFile.path, file.path]};
            },
            fullProject: function()
            {
                return {name: 'mcs', args: ['-langversion:4', '-target:library', '-lib:' + nunitPath, '-r:' + nunitAssemblies, '-out:' + '/tmp/out.dll', file.path, fixtureFile.path]};
            }
        },
        solutionOnly: function ()
        {
            opts.tempFiles = [outFile.path];
            return {name: 'mono', args: [outFile.path]};
        },
        fullProject: function()
        {
            opts.tempFiles = [outFile.path];
            process.env.MONO_PATH = nunitPath;
            return {name: 'mono', args: [path.resolve(process.env.MONO_PATH, 'nunit-console.exe'), '-nologo', '-noresult', '/tmp/out.dll'], options: {env: process.env } };
        }
    });
};

