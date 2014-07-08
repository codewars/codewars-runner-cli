var shovel = require('../shovel'),
    config = require('../config'),
    mkdirParentSync = require('../util').mkdirParentSync,
    fs = require('fs'),
    path = require('path'),
    temp = require('temp');

// Infer the name and directory for where a Haskell module should be written to
// based on its module declaration
function haskellFileName(code, defaultFileName) {
    var match = /module\s+([A-Z]([a-z|A-Z|0-9]|\.[A-Z])*)/.exec(code);
    return match !== null ? match[1].replace(/\./g, '/') + ".hs" : defaultFileName;
}

// Infer file name from a Haskell module,
// make parent directories if necessary,
// write code to file name,
// and output file name
function haskellWriteSync(code, codeDir, defaultFileName) {
    var fileName = haskellFileName(code, defaultFileName);
    if (!(typeof fileName == 'string' || fileName instanceof String))
        throw new Error(["Could not determine valid Haskell module name from code:\n\n", code].join(""));
    fileName = path.join(codeDir, fileName);
    if (fs.existsSync(fileName))
        throw new Error(["Could not write Haskell code to file ", fileName,
                         " because file already exists:\n\n", code].join(""));
    mkdirParentSync(path.dirname(fileName));
    fs.writeFileSync(fileName, code);
    return fileName;
}

module.exports.run = function run(opts, cb) {
    temp.track();
    var haskellCodeDir = temp.mkdirSync('haskell');
    shovel.start(opts, cb, {
        solutionOnly: function () {
            if (opts.setup) haskellWriteSync(opts.setup, haskellCodeDir);
            return {
                name: 'runhaskell',
                args: ['-i' + ['frameworks/haskell', haskellCodeDir].join(':'),
                       haskellWriteSync(opts.solution, haskellCodeDir, "Main.hs")]};
        },
        fullProject: function () {
            var solutionFileName = haskellWriteSync(opts.solution, haskellCodeDir, "Main.hs"),
                fixtureFileName = solutionFileName.split('/').pop() == "Main.hs" ?
                    haskellWriteSync(opts.fixture, haskellCodeDir) :
                    haskellWriteSync(opts.fixture, haskellCodeDir, "Main.hs");
            if (opts.setup) haskellWriteSync(opts.setup, haskellCodeDir);
            return {
                name: 'runhaskell',
                args: ['-i' + ['frameworks/haskell', haskellCodeDir].join(':'),
	               fixtureFileName]};
        }
    });
};
