var shovel = require('../shovel'),
    util = require('../util'),
    path = require("path"),
    fsharpFrameworksPath = path.resolve(__dirname, '..', '..', 'frameworks', 'fsharp'),
    csharpFrameworksPath = path.resolve(__dirname, '..', '..', 'frameworks', 'csharp'),
    fuchuPath = path.resolve(fsharpFrameworksPath, 'Fuchu', 'lib', 'Fuchu'),

  // Fuchu references exception classes from these libs
    nunitAssemblies = ['nunit.core.dll', 'nunit.core.interfaces.dll', 'nunit.util.dll', 'nunit.framework.dll', 'Newtonsoft.Json.dll', 'Qualified.dll'],
    nunitPath = path.resolve(csharpFrameworksPath, 'nunit', 'bin'),
    gallioPath = path.resolve(fsharpFrameworksPath, 'Gallio', 'lib', 'NET40'),
    gallioAssembly = path.resolve(gallioPath, 'Gallio.dll'),
    xUnitPath = path.resolve(fsharpFrameworksPath, 'xunit', 'lib', 'net20'),
    xUnitAssembly = path.resolve(xUnitPath, 'Xunit.dll');


module.exports.run = function(opts, cb) {
  shovel.start(opts, cb, {
    solutionOnly: function(runCode) {
      var fileContents;
      if (opts.setup) {
        fileContents = opts.setup + "\n" + opts.solution;
      }
      else {
        fileContents = opts.solution;
      }
      var file = util.codeWriteSync('fsharp', fileContents, opts.dir, 'solution.fsx');
      runCode({name: 'fsharpi', 'args': [file]});
    },
    testIntegration: function(runCode) {
      var runFixtureUsingFuchu =
`let printCompletedIn (timeSpan: System.TimeSpan) = printfn "<COMPLETEDIN::>%f" timeSpan.TotalMilliseconds in
let codeWarsTestPrinter: Fuchu.Impl.TestPrinters = {
    BeforeRun = (fun name -> printfn "<IT::>%s" name)
    Passed = (fun name time ->
        printfn "<PASSED::>%s" (name.Replace("\n", "<:LF:>"))
        printCompletedIn time
    )
    Ignored = (fun _ _ -> ignore())
    Failed = (fun name err time ->
        printfn "<FAILED::>%s" (err.Replace("\n", "<:LF:>"))
        printCompletedIn time
    )
    Exception = (fun name err time ->
        printfn "<ERROR::>%s" (err.StackTrace.Replace("\n", "<:LF:>"))
        printCompletedIn time
    )
}

let _ =
    Fuchu.Impl.eval codeWarsTestPrinter Seq.map Tests.suite;;`;

      const code = [
        `System.IO.File.Delete("${path.resolve(opts.dir, 'program.fsx')}")`,
        opts.setup ? opts.setup : "",
        opts.solution,
        opts.fixture,
        runFixtureUsingFuchu
      ].join("\n");

      const codeFile = util.codeWriteSync('fsharp', code, opts.dir, 'program.fsx');

      const args = [
        '--reference:' + fuchuPath,
        '--lib:' + gallioPath,
        '--reference:' + gallioAssembly,
        '--lib:' + xUnitPath,
        '--reference:' + xUnitAssembly,
        '--lib:' + nunitPath
      ].concat(
        nunitAssemblies.map(function(dll) {
          return "--reference:" + path.resolve(nunitPath, dll);
        })
      );

      args.push(codeFile);
      runCode({name: 'fsharpi', 'args': args});
    }
  });
};

