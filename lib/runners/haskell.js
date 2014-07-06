var shovel = require('../shovel'),
    config = require('../config'),
    fs = require('fs'),
    dirname = require('path').dirname;

// Infer the name and directory for where a Haskell module should be written to
// based on its module declaration
function haskellFileName(code, defaultFileName) {
    var match = /module\s+([A-Z]([a-z|A-Z|0-9]|\.[A-Z])*)/.exec(code);
    return match != null ? match[1].replace('.', '/') + ".hs" : defaultFileName;
}

// mkdir -p
function mkdirParentSync(dirPath, mode) {
    var dirs = dirPath.split("/"), partialPath;
    for (idx in dirs) {
        partialPath = dirs.slice(0, idx + 1).join("/");
        if (!fs.existsSync(partialPath))
            fs.mkdirSync(partialPath, mode);
    }
}

// Infer file name from a Haskell module,
// make parent directories if necessary,
// write code to file name,
// and output file name
function haskellWriteSync(code, defaultFileName) {
    var fileName = haskellFileName(code, defaultFileName);
    if (!fileName)
        throw ["Could not determine valid Haskell module name from code:\n\n", code].join("");
    mkdirParentSync(dirname(fileName));
    fs.writeFileSync(fileName, code);
    return fileName;
}

module.exports.run = function run(opts, cb) {
    shovel.start(opts, cb, {
        solutionOnly: function () {
            if (opts.setup) haskellWriteSync(opts.setup);
            return {name: 'runhaskell',
                args: ['-iframeworks/haskell',
                    haskellWriteSync(opts.solution, "Main.hs")]};
        },
        fullProject: function () {
            var solutionFileName = haskellWriteSync(opts.solution, "Main.hs"),
                fixtureFileName = solutionFileName == "Main.hs" ?
                    haskellWriteSync(opts.fixture) :
                    haskellWriteSync(opts.fixture, "Main.hs");

            if (opts.setup) haskellWriteSync(opts.setup);
            return {name: 'runhaskell',
                args: ['-iframeworks/haskell', fixtureFileName]};
        }
    });
};
