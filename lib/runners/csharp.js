var shovel = require('../shovel'),
	tmp=require('temporary');

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
    shovel.start(opts, cb,
    {
        compile:
        {
            solutionOnly: function()
            {
                if(opts.solution)
                {
                    return {name: 'mcs', args: ['-langversion:4', '-out:' + outFile.path, file.path]};
                }
                else
                {
                    return {name: 'mcs', args: ['-langversion:4', '-out:' + outFile.path, opts.solutionFile]};
                }
            }
        },
        solutionOnly: function ()
        {
            opts.tempFiles = [outFile.path];
            return {name: 'mono', args: [outFile.path]};
        }
    });
};

