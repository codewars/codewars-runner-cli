var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp');


var cw2 = `
class CW2Test {
    func expect(_ passed:Bool, _ message:String? = nil) {
        if passed {
            print("<PASSED::>Test Passed")
        } else {
            if message != nil {
                print("<FAILED::>\\(message!)")
            } else {
                print("<FAILED::>Value is not what was expected")
            }
        }
    }

    func expect_throws<T>(_ fn: @autoclosure () throws -> T, _ message: String? = nil) {
        var passed = false
        do {
            try _ = fn()
        } catch {
            passed = true
        }
        expect(passed, message)
    }

    func assert_equals<T: Equatable>(_ actual: ArraySlice<T>, _ expected: ArraySlice<T>, _ message: String? = nil) {
        let equalsMsg = "\\(actual) should equal \\(expected)"
        let messageResult = message == nil ? equalsMsg : message! + ": " + equalsMsg
        expect(actual == expected, messageResult)
    }

    func assert_equals<T: Equatable>(_ actual: Array<T>, _ expected: Array<T>, _ message: String? = nil) {
        let equalsMsg = "\\(actual) should equal \\(expected)"
        let messageResult = message == nil ? equalsMsg : message! + ": " + equalsMsg
        expect(actual == expected, messageResult)
    }

    func assert_equals<T: Equatable>(_ actual: T, _ expected: T, _ message: String? = nil) {
        let equalsMsg = "\\(actual) should equal \\(expected)"
        let messageResult = message == nil ? equalsMsg : message! + ": " + equalsMsg
        expect(actual == expected, messageResult)
    }

    func describe(_ message: String) {
        print("<DESCRIBE::>\\(message)")
    }

    func it(_ message: String) {
        print("<IT::>\\(message)")
    }
}

var Test = CW2Test()
var test = Test
`

var xctest = `
import XCTest
`

module.exports.run = function run(opts, cb) {
    temp.track();
    var dir = temp.mkdirSync('swift');
    shovel.start(opts, cb, {
        solutionOnly: function (run) {
            var code = [opts.setup, opts.solution].join("\n");
            var file = util.codeWriteSync('swift', code, dir, 'solution.swift')
            run({name: 'swift', 'args': [file]});
        },
        testIntegration: function (run) {
            var code;
            switch (opts.testFramework) {
                case 'cw-2':
                    code = [
                        opts.setup,
                        opts.solution,
                        cw2,
                        opts.fixture
                    ].join('\n');
                    break;
                case 'xctest':
                    code = [
                        opts.setup,
                        opts.solution,
                        xctest,
                        opts.fixture
                    ].join('\n');
                    break;
                default:
                    throw new Error('test framework is not supported');
            }
            var file = util.codeWriteSync('swift', code, dir, 'solution.swift')
            run({name: 'swift', 'args': [file]});
        },
        sanitizeStdErr: function (err) {
            // get rid of some of the noisy content. We remove line numbers since
            // they don't match up to what the user sees and will only confuse them.
            return err.replace(/(.*)(error:)(\n|.)*/g, '$1$2')
        }
    });
};
