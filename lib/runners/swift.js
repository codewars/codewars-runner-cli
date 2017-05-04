var shovel = require('../shovel'),
    util = require('../util');


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
`;

var xctest = 'import XCTest';

module.exports.run = function run(opts, cb) {
  var dir = '/home/codewarrior',
      fixtureOffset = 0; // tracks any code we add to the fixture, used later for line number offsets

  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      var code = [opts.setup, opts.solution].join("\n");
      var file = util.codeWriteSync('swift', code, dir, 'solution.swift');
      runCode({name: 'swift', 'args': [file]});
    },
    testIntegration: function(runCode) {
      var code;

      if (opts.setup) {
        opts.setup = opts.setup.trim();
      }

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
          code = [opts.solution];
          if (opts.setup) {
            code.unshift(opts.setup);
          }

          if (opts.fixture) {
          // if the xctest import statement was provided, then auto include it
            if (opts.fixture.indexOf(xctest) === -1) {
              code.push(xctest);
              fixtureOffset = 1;
            }

            code.push(opts.fixture);
          }

          code = code.join('\n');
          break;

        default:
          throw new Error('test framework is not supported');
      }
      var file = util.codeWriteSync('swift', code, dir, 'solution.swift');
      runCode({name: 'swift', 'args': [file]});
    },
    sanitizeStdErr: function(err) {
      var setup = opts.setup ? (opts.setup || '').split('\n').length : 0,
          code = opts.solution.split('\n').length;

      // We need to change some of the line reporting to make it more useful
      return err.replace(/solution.swift:(\d*):(\d*):/g, function(text, line, ch) {
        try {
          // convert line to an integer and make it zero based
          line = parseInt(line);

          if (setup > 0 && line <= setup)
            return `${text}(setup)`;

          else if (setup > 0 && line <= setup + code)
            return `${text}(solution:${line - setup}:${ch})`;

          else if (line > setup + code)
            return `${text}(fixture:${line - setup - code - fixtureOffset}:${ch})`;

          return text;
        }
        catch (ex) {
          return text;
        }
      });
    }
  });
};
