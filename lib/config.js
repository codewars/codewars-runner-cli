var uniqueId = ('_' + Math.random()).replace(/[^_\d]/g, '');

module.exports = {
    version: '1.0.5',
    testFramework: {
        defaults: {
            javascript: 'cw-2',
            coffeescript: 'cw-2',
            ruby: 'cw-2',
            python: 'cw-2'
        }
    },
    timeouts: {
        default: 6000,
        clojure: 10000,
        java: 10000
    },
    moduleRegExs: {
        haskell: /module\s+([A-Z]([a-z|A-Z|0-9]|\.[A-Z])*)\W/,
        julia: /module\s+([a-z|A-Z][a-z|A-Z|0-9]*)\W/,
        erlang: /-module\(([a-z|A-Z][a-z|A-Z|0-9|_]*)\)/,
        elixir: /defmodule\s+([a-z|A-Z][.a-z|A-Z|0-9|_]*)\s+do/,
        scala: /(?:object|class)\s+([A-Z][a-z|A-Z|0-9|_]*)/,
        swift: /\n*\/\/\s*([a-z|A-Z|0-9|_|-]+)\.swift\s*\n/,
        objc: /\n*\/\/\s*([a-z|A-Z|0-9|_|-]+)\.m\s*\n/,
        objcHeader: /\n*\/\/\s*([a-z|A-Z|0-9|_|-]+)\.h\s*\n/
    },
    fileExtensions: {
        haskell: 'hs',
        julia: 'jl',
        erlang: 'erl',
        elixir: 'ex',
        scala: 'scala',
	      swift: 'swift',
	      objc: 'm',
	      objcHeader: 'h'
    },
    snippets: {
        python: {
            addPythonFramework: [
                "try: from imp import reload",
                "except: pass",
                "reload(__import__('sys')).path.append('./frameworks/python')"
            ].join('\n'),
            requireCw2: "test = Test = reload(__import__('cw-2'))",
            requireUnittest: "unittest = reload(__import__('unittest'))",
            runUnittest: [
                "unittest",
                ".TestLoader()",
                ".loadTestsFromTestCase(Test)",
                ".run(reload(__import__('unittestwrapper')).CwTestResult())"
            ].join('')
        },
        php: {
            bootstrap: [
                "require_once('frameworks/php/vendor/autoload.php');"
            ].join('\n'),
            beforeUserScript : [
                "$" + uniqueId + " = count(get_defined_vars());" // http://stackoverflow.com/a/13629024/1640765
            ].join('\n'),
            afterUserScript: [
                "$_" + uniqueId + " = array_slice(get_defined_vars(), $" + uniqueId + " + 1);",
            ].join('\n'),
            classBegin: [
                "class KataTest extends CodeWars\\TestCase {",
                "public function testSolution() {",
                "extract($this->userScriptVariables);"
            ].join('\n'),
            classEnd : [
                "}",
                "}"
            ].join('\n'),
            execute: [
                "$suite = new PHPUnit_Framework_TestSuite();",
                "$test = new KataTest('testSolution', ['userScriptVariables' => $_" + uniqueId + "]);",
                "$suite->addTest($test);",
                "PHPUnit_TextUI_TestRunner::run($suite);"
            ].join('\n')
        }
    }
};

module.exports.snippets.python3 = module.exports.snippets.python;
