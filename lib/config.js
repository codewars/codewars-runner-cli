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
    images: {
        ruby: ['ruby'],
        node: ['javascript', 'coffeescript', 'typescript'],
        python: ['python'],
        dotnet: ['csharp', 'fsharp'],
        jvm: ['java', 'groovy', 'clojure', 'scala'],
        func: ['haskell', 'ocaml', 'racket', 'lisp'],
        systems: ['c', 'cpp', 'nasm', 'gas', 'bash', 'arm'],
        alt: ['r', 'rust', 'erlang', 'elixir', 'go', 'julia', 'lua', 'php', 'perl']
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
        scala: /(?:object|class)\s+([A-Z][a-z|A-Z|0-9|_]*)/
    },
    fileExtensions: {
        haskell: 'hs',
        julia: 'jl',
        erlang: 'erl',
        scala: 'scala'
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
                "$definedVarsBeforeUserScript = count(get_defined_vars());" // http://stackoverflow.com/a/13629024/1640765
            ].join('\n'),
            afterUserScript: [
                "$definedVarsFromUserScript = array_slice(get_defined_vars(), $definedVarsBeforeUserScript + 1);",
            ].join('\n'),
            classBegin: [
                "class KataTest extends CodeWars\\TestCase {",
                "public function testSolution() {",
                "extract($this->userScriptVariables);" // Make vars from user script available in test method
            ].join('\n'),
            classEnd : [
                "}",
                "}"
            ].join('\n'),
            execute: [
                "$suite = new PHPUnit_Framework_TestSuite();",
                "$test = new KataTest('testSolution', ['user_vars' => $definedVarsFromUserScript]);",
                "$suite->addTest($test);",
                "$result = PHPUnit_TextUI_TestRunner::run($suite);"
            ].join('\n')
        }
    }
};

module.exports.snippets.python3 = module.exports.snippets.python;
