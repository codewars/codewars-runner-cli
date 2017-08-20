"use strict";

const shovel = require('../shovel');

module.exports.run = function run(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      var stdin;
      if (opts.setup) {
        stdin = opts.setup + "\n" + opts.solution;
      }
      else {
        stdin = opts.solution;
      }
      runCode({
        name: 'utop',
        args: [
          '-require', 'core',
          '-require', 'batteries',
          '-require', 'oUnit',
          '-stdin'
        ],
        stdin: stdin
      });
    },
    testIntegration: function(runCode) {
      var runFixtureUsingOUnit = [
        '#load "str.cma";;',
        'module TestRunner = struct',
        '    open OUnit',
        '    let cw_print_endline s = s |> Str.global_replace (Str.regexp_string "\\n") "<:LF:>" |> print_endline;;',
        '    let cw_print_test_event = function',
        '        | EStart (name::rest) -> print_endline ("\\n<IT::>" ^ OUnit.string_of_node name)',
        '        | EResult result -> ',
        '            begin match result with ',
        '            | RSuccess _ -> print_endline ("\\n<PASSED::>Test passed")',
        '            | RFailure (_, err) -> print_endline ("\\n<FAILED::>" ^ err)',
        '            | RError (_, err) -> print_endline ("\\n<ERROR::>" ^ err)',
        '            | _ -> ()',
        '            end',
        '        | _ -> ()',
        '    ',
        '    let run_test = function',
        '        | TestLabel (name, suite) -> begin',
        '            print_endline ("\\n<DESCRIBE::>" ^ name);',
        '            perform_test cw_print_test_event suite',
        '        end',
        '        | suite -> perform_test cw_print_test_event suite',
        '    ',
        'end',
        'let _ = ',
        '    List.map TestRunner.run_test Tests.suite |> ignore',
      ].join("\n");
      var stdin = [
        opts.setup ? opts.setup : "",
        opts.solution,
        opts.fixture,
        runFixtureUsingOUnit
      ].join("\n");
      runCode({
        name: 'utop',
        args: [
          '-require', 'core',
          '-require', 'batteries',
          '-require', 'oUnit',
          '-stdin'
        ],
        stdin: stdin
      });
    }
  });
};
