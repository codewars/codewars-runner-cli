module.exports = {
    version: '0.1.9',
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
        coffeescript: 5000,
        ruby: 5000,
        python: 5000,
        csharp: 5000
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
            requireCw2: "execfile('./frameworks/python/cw-2.py')\n",
            requireUnittest: "import sys\nsys.path.append('./frameworks/python/')\nimport unittest\n",
            runUnittest: "import unittestwrapper\n_testresult = unittestwrapper.CwTestResult()\n_testsuite.run(_testresult)"
        }
    }
};
