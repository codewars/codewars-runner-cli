module.exports = {
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
        coffeescript: 5000,
        ruby: 5000,
        python: 5000
    },
    snippets: {
        javascript: {
            requireCw2: "require('./frameworks/javascript/cw-2')\n",
            inlineTestFixture: {
                start: "(function() { var Test = this.Test, describe = this.describe, it = this.it, before = this.before, after = this.after;try{\n",
                end: '}catch(ex){Test.handleError(ex)}})();'
            }
        },
        python: {
            requireCw2: "execfile('./frameworks/python/cw-2.py')\n",
            requireUnittest: "import sys\nsys.path.append('./frameworks/python/')\nimport unittest\n",
            runUnittest: "import unittestwrapper\n_testresult = unittestwrapper.CwTestResult()\n_testsuite.run(_testresult)"
        }
    }
};
