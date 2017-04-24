var shovel = require('../shovel');

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
        name: 'bash',
        args: ['/home/codewarrior/run_ocaml.sh', 'utop', '-require', 'core', '-require', 'batteries', '-require', 'oUnit', '-stdin'],
        stdin: stdin
      });
    },
    testIntegration: function(runCode) {
      var runFixtureUsingOUnit = [
        '#load "str.cma";;',
        'module TestRunner = struct',
        '	open OUnit',
        '	let cw_print_endline s = s |> Str.global_replace (Str.regexp_string "\n") "<:LF:>" |> print_endline;;',
        '	let cw_print_test_event = function',
        '	    | EStart (name::rest) -> print_endline ("<IT::>" ^ OUnit.string_of_node name)',
        '	    | EResult result -> ',
        '	        begin match result with ',
        '	        | RSuccess _ -> print_endline ("<PASSED::>Test passed")',
        '	        | RFailure (_, err) -> print_endline ("<FAILED::>" ^ err)',
        '	        | RError (_, err) -> print_endline ("<ERROR::>" ^ err)',
        '	        | _ -> ()',
        '	        end',
        '	    | _ -> ()',
        '	',
        '	let run_test = function',
        '	    | TestLabel (name, suite) -> begin',
        '	        print_endline ("<DESCRIBE::>" ^ name);',
        '	        perform_test cw_print_test_event suite',
        '	    end',
        '	    | suite -> perform_test cw_print_test_event suite',
        '	',
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
        name: 'bash',
        args: ['/home/codewarrior/run_ocaml.sh', 'utop', '-require', 'core', '-require', 'batteries', '-require', 'oUnit', '-stdin'],
        stdin: stdin
      });
    }
  });
};
