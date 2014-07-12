var shovel = require('../shovel'),
    config = require('../config'),
    codeWriteSync = require('../util').codeWriteSync,
    glob = require('glob'),
    fs = require('fs'),
    path = require('path'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
    temp.track();
    var clojureCodeDir = temp.mkdirSync('clojure'),
        classPath = glob.sync(path.join(
            process.env.HOME,'.m2','repository','**','*.jar'));
    classPath.push(clojureCodeDir);
    classPath.push('frameworks/clojure');

    shovel.start(opts, cb, {
        solutionOnly: function () {
            if (opts.setup) codeWriteSync('clojure', opts.setup, clojureCodeDir);
            return {
                name: 'java',
                args: ['-XX:ErrorFile=/dev/null', '-cp', classPath.join(':'), 'clojure.main',
                    codeWriteSync('clojure', opts.solution, clojureCodeDir, "solution.clj")]};
        },
        fullProject: function () {
            codeWriteSync('clojure', opts.solution, clojureCodeDir);
            if (opts.setup) codeWriteSync('clojure', opts.setup, clojureCodeDir);
            return {
                name: 'java',
                args: ['-XX:ErrorFile=/dev/null', '-cp', classPath.join(':'), 'clojure.main',
                    codeWriteSync(
                        'clojure',
                        opts.fixture + config.snippets.clojure.runTests,
                        clojureCodeDir,
                        "test_fixture.clj")]};
        }
    });
};
