var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
    var objcDir = '/tmp/objc';
    util.mkdirParentSync(objcDir);
    temp.track();
    var dir = temp.mkdirSync({dir: objcDir});
	objcSandbox = path.resolve('frameworks','objc','objc.sb');

    shovel.start(opts, cb, {
        solutionOnly: function () {
            return function (run) {
                var code = (opts.setup || '') + opts.solution, // TODO: This is dumb
                    solutionFile = util.codeWriteSync('objc', code, dir, 'solution.m'),
                    command = 'clang -lobjc -framework Foundation -o solution ' + solutionFile;
                util.exec(command, {'cwd': dir}, function () { 
			run({'name': 'sandbox-exec', 
			     'args': ['-f', objcSandbox, './solution'],
			     'options': {'cwd': dir}});});
            };
        },
    });
};
