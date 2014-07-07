var shovel = require('../shovel'),
    config = require('../config'),
    mkdirParentSync = require('../util').mkdirParentSync,
    fs = require('fs'),
    path = require('path'),
    temp = require('temp');

// Infer the name and directory for where a Haskell module should be written to
// based on its module declaration
function juliaFileName(code, defaultFileName) {
    var match = /module\s+([a-z|A-Z|-|0-9]*)/.exec(code);
    return match !== null ? match[1] + ".jl" : defaultFileName;
}

// Infer file name from a Julia module,
// make parent directories if necessary,
// write code to file name,
// and output file name
// TODO: Not DRY, fix me
function juliaWriteSync(code, codeDir, defaultFileName) {
    var fileName = juliaFileName(code, defaultFileName);
    if (!(typeof fileName == 'string' || fileName instanceof String))
        throw new Error(["Could not determine valid Julia module name from code:\n\n", code].join(""));
    fileName = path.join(codeDir, fileName);
    if (fs.existsSync(fileName))
        throw new Error(["Could not write Julia code to file ", fileName,
                         " because file already exists:\n\n", code].join(""));
    mkdirParentSync(path.dirname(fileName));
    fs.writeFileSync(fileName, code);
    return fileName;
}

module.exports.run = function run(opts, cb) {
    temp.track();
    var juliaCodeDir = temp.mkdirSync('julia');
    shovel.start(opts, cb, {
        solutionOnly: function () {
            if (opts.setup) juliaWriteSync(opts.setup, juliaCodeDir);
            return {
                name: 'julia',
                args: ['-P', ['push!(LOAD_PATH, "',juliaCodeDir,'")'].join(""), '-e', opts.solution]};
        },
        fullProject: function () {
            var solutionFileName = juliaWriteSync(opts.solution, juliaCodeDir, "Solution.jl");
            if (opts.setup) juliaWriteSync(opts.setup, juliaCodeDir);
            return {
                name: 'julia',
                args: ['-P', ['push!(LOAD_PATH, "', juliaCodeDir ,'")'].join(""),'-e', opts.fixture]};
        }
    });
};
