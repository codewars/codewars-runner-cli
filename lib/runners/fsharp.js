var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp'),
    path = require("path"),
    fsharpFrameworksPath = path.resolve(__dirname, '..', '..', 'frameworks', 'fsharp'),
    fuchuPath = path.resolve(fsharpFrameworksPath, 'Fuchu', 'lib', 'Fuchu'),

    // Fuchu references exception classes from these libs
    nunitAssemblies = ['NUnit.Core.dll', 'NUnit.Core.Interfaces.dll', 'NUnit.Util.dll', 'NUnit.Framework.dll', 'Newtonsoft.Json.dll', 'EntityFramework.dll', 'Qualified.dll'],
    nunitPath = path.resolve(fsharpFrameworksPath, 'nunit'),
    gallioPath = path.resolve(fsharpFrameworksPath, 'Gallio', 'lib', 'NET40'),
    gallioAssembly = path.resolve(gallioPath, 'Gallio.dll'),
    xUnitPath = path.resolve(fsharpFrameworksPath, 'xunit', 'lib', 'net20');
    xUnitAssembly = path.resolve(xUnitPath, 'Xunit.dll');


module.exports.run = function(opts, cb)
{
    temp.track();
    var dir = temp.mkdirSync('fsharp');

    shovel.start(opts, cb, {
        solutionOnly: function (run)
        {
            var file = util.codeWriteSync('fsharp', opts.solution, dir, 'solution.fsx');
            run({name: 'fsharpi', 'args': [file]});
        },
        testIntegration: function (run)
        {
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
    Fuchu.Impl.eval codeWarsTestPrinter Seq.map Tests.suite;;`
            var stdin = [
                opts.solution,
                opts.fixture,
                runFixtureUsingFuchu
            ].join("\n");
            var args = [
                '--quiet',
                '--reference:' + fuchuPath,
                '--lib:' + gallioPath,
                '--reference:' + gallioAssembly,
                '--lib:' + xUnitPath,
                '--reference:' + xUnitAssembly,
                '--lib:' + nunitPath
            ].concat(
                nunitAssemblies.map(function(dll){ return "--reference:" + path.resolve(nunitPath,  dll); })
            );
            run({
                name: 'fsharpi',
                args: args,                
                stdin: stdin
            });
        }
    });
};






