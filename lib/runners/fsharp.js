var shovel = require('../shovel'),
    util = require('../util'),
    config = require('../config'),
    temp = require('temp'),
    path = require("path"),
    fuchuPath = path.resolve(__dirname, '..', '..', 'frameworks', 'fsharp', 'Fuchu', 'lib', 'Fuchu');

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
            var runFixtureUsingFuchu = [
                'let codeWarsTestPrinter: Fuchu.Impl.TestPrinters = {',
                '    BeforeRun = (fun name -> printfn "<IT::>%s" name)',
                '    Passed = (fun name time -> printfn "<PASSED::>%s" name)',
                '    Ignored = (fun _ _ -> ignore())',
                '    Failed = (fun name err time -> printfn "<FAILED::>%s" err)',
                '    Exception = (fun name err time -> printfn "<ERROR::>%s" (err.StackTrace.Replace("\n", "<:LF:>")))',
                '}',
                '',
                'let _ = ',
                '    Fuchu.Impl.eval codeWarsTestPrinter Seq.map Tests.suite;;'
            ].join("\n");
            var stdin = [
                opts.solution,
                opts.fixture,
                runFixtureUsingFuchu
            ].join("\n");
            run({
                name: 'fsharpi',
                args: ['--quiet', '--reference:' + fuchuPath],
                stdin: stdin
            });
        }
    });
};






