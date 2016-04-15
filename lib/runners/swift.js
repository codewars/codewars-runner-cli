var shovel = require('../shovel'),
    util = require('../util'),
    temp = require('temp'),
    path = require('path');

module.exports.run = function run(opts, cb) {
    // Sandbox expects files to have this directory as an ancestor
    var swiftDir = '/tmp/swift';
    util.mkdirParentSync(swiftDir);
    temp.track();
    var dir = temp.mkdirSync({dir: swiftDir}),
	      swiftSandbox = path.resolve('frameworks','osx','swift.sb');

    shovel.start(opts, cb, {
        solutionOnly: function (run) {
            var solutionFile = util.codeWriteSync(null, opts.code, dir, 'main.swift'),
                command = 'xcrun -sdk macosx swiftc -emit-executable ' + solutionFile;
            if ("setup" in opts)
                command = command + ' ' + util.codeWriteSync('swift', opts.setup, dir, 'setup.swift');
            util.exec(command, {'cwd': dir}, function () {
              run({'name': 'sandbox-exec',
                   'args': ['-f', swiftSandbox, './main'],
                   'options': {'cwd': dir}});
            });
        },
        fullProject: function (run) {
            require('./xcode').fullProject(opts, run);
        }
    });
};
