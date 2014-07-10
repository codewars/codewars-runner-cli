var shovel = require('../shovel'),
    config = require('../config'),
    mkdirParentSync = require('../util').mkdirParentSync,
    glob = require('glob'),
    fs = require('fs'),
    path = require('path'),
    temp = require('temp');

// Infer the name and directory for where a clojure name space should be written to
// based on its module declaration
function clojureFileName(code, defaultFileName) {
    var match = /\(ns\s+([A-Z|a-z]([a-z|A-Z|0-9|-]|\.[A-Z|a-z])*)/.exec(code);
    return match !== null ? match[1].replace(/\./g, '/').replace(/-/g, '_') + ".clj" : defaultFileName;
}

// Infer file name from a Clojure namespace,
// make parent directories if necessary,
// write code to file name,
// and output file name
// TODO: Not DRY, very similar to haskell runner code...
function clojureWriteSync(code, codeDir, defaultFileName) {
    var fileName = clojureFileName(code, defaultFileName);
    if (!(typeof fileName == 'string' || fileName instanceof String))
        throw new Error(["Could not determine valid Clojure namespace from code:\n\n", code].join(""));
    fileName = path.join(codeDir, fileName);
    if (fs.existsSync(fileName))
        throw new Error(["Could not write Clojure code to file ", fileName,
            " because file already exists:\n\n", code].join(""));
    mkdirParentSync(path.dirname(fileName));
    fs.writeFileSync(fileName, code);
    return fileName;
}

module.exports.run = function run(opts, cb) {
    temp.track();
    var clojureCodeDir = temp.mkdirSync('clojure'),
        classPath = glob.sync(path.join(process.env.HOME,'.m2','repository','**','*.jar'));
    classPath.push(clojureCodeDir);
    classPath.push('frameworks/clojure');

    shovel.start(opts, cb, {
        solutionOnly: function () {
            if (opts.setup) clojureWriteSync(opts.setup, clojureCodeDir);
            return {
                name: 'java',
	        // Don't dump 
                args: ['-XX:ErrorFile=/dev/null', '-cp', classPath.join(':'), 'clojure.main',
                    clojureWriteSync(opts.solution, clojureCodeDir, "solution.clj")]};
        },
        fullProject: function () {
            clojureWriteSync(opts.solution, clojureCodeDir);
            if (opts.setup) clojureWriteSync(opts.setup, clojureCodeDir);
            return {
                name: 'java',
                args: ['-XX:ErrorFile=/dev/null', '-cp', classPath.join(':'), 'clojure.main',
                    clojureWriteSync(
                            opts.fixture + config.snippets.clojure.runTests,
                        clojureCodeDir,
                        "test_fixture.clj")]};
        }
    });
};
