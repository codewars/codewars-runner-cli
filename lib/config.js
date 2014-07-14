var sh = require('execSync'),
    path = require('path');

module.exports = {
    version: '0.1.12',
    testFramework: {
        defaults: {
            javascript: 'cw-2',
            coffeescript: 'cw-2',
            ruby: 'cw-2',
            python: 'cw-2'
        }
    },
    timeouts: {
        javascript: 5000,
        java: 5000,
        haskell: 5000,
        julia: 5000,
        erlang: 5000,
        clojure: 10000,
        coffeescript: 5000,
        ruby: 5000,
        python: 5000,
        csharp: 5000
    },
    moduleRegExs: {
        haskell: /module\s+([A-Z]([a-z|A-Z|0-9]|\.[A-Z])*)\W/,
        clojure: /\(ns\s+([A-Z|a-z]([a-z|A-Z|0-9|-]|\.[A-Z|a-z])*)\W/,
        julia: /module\s+([a-z|A-Z][a-z|A-Z|0-9]*)\W/,
        erlang: /-module\(([a-z|A-Z][a-z|A-Z|0-9]*)\)/
    },
    fileExtensions: {
        haskell: 'hs',
        clojure: 'clj',
        julia: 'jl',
        erlang: 'erl'
    },
    compile: {
        erlang: function (file) {
            var directory = path.dirname(file);
            sh.run([
                "ERL_CRASH_DUMP=/dev/null",
                "erlc",
                "-I", directory,
                "-o", directory,
                file
            ].join(" "));
            return file.substr(0, file.lastIndexOf(".")) + ".beam";
        }
    },
    snippets: {
        javascript: {
            requireCw2: "require('./frameworks/javascript/cw-2')\n",
            start: "Test.handleError(function(){\n",
            inlineTestFixture: {
                start: "\n(function() { var Test = global.Test, describe = global.describe, it = global.it, before = global.before, after = global.after;",
                end: '\n})();'
            },
            end: "});"
        },
        python: {
            requireCw2: ["from solution import *",
	                 "execfile('./frameworks/python/cw-2.py')"].join("\n"),
            // TODO: People can hack the unittest module and pass any test if they want, so this won't work
            requireUnittest: "import sys\nsys.path.append('./frameworks/python/')\nimport unittest\n",
            runUnittest: "import unittestwrapper\n_testresult = unittestwrapper.CwTestResult()\n_testsuite.run(_testresult)"
        },
        clojure: {
            runTests: ["(require '[clojure.test.codewars])",
                       "(clojure.test.codewars/run-tests)"].join('\n')
        }
    }
};
