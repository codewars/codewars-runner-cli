var shovel = require('../shovel'),
    util = require('../util'),
    path = require('path'),
    temp = require('temp');

module.exports.run = function run(opts, cb) {
    var swiftDir = '/tmp/swift';
    util.mkdirParentSync(swiftDir);
    temp.track();
    var dir = temp.mkdirSync({dir: swiftDir});
	swiftSandbox = path.resolve('frameworks','swift','swift.sb');

    shovel.start(opts, cb, {
        solutionOnly: function () {
            return function (run) {
                var code = (opts.setup || '') + opts.solution, // TODO: This is dumb
                    solutionFile = util.codeWriteSync('swift', code, dir, 'solution.swift'),
                    command = 'xcrun -sdk macosx swiftc -emit-executable ' + solutionFile;
                util.exec(command, {'cwd': dir}, function () { 
			run({'name': 'sandbox-exec', 
			     'args': ['-f', swiftSandbox, './solution'],
			     'options': {'cwd': dir}});});
            };
        },
    });
};
