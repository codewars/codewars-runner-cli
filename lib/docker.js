var exec = require('child_process').exec,
    util = require('./util'),
    Docker = require('dockerode'),
    docker = new Docker({socketPath: '/var/run/docker.sock'} ),
    Streams = require('memory-streams');

// TODO: switch to using Docker remote API in order to prevent cmd injection attacks.
//       current implementation is just to get the ball rolling.
module.exports.run = function run(image, cmd, args, cb) {
    args.format = args.format || 'json';
    args = util.parseBodyToArray(args);
    args.unshift(cmd);

    var writer = new Streams.WritableStream();
    docker.run(image, args, writer, function(error, data) {
        cb(error, writer.toString());
    });
};

// runs using the cli instead of remote api.
module.exports.runCmd = function runCmd(image, cmd, args, cb) {
    args = util.parseBodyToArgs(args);
    var cmd = 'docker run --rm  ' + image + ' ' + cmd + ' --format json ' + args;
    exec(cmd, {timeout: 10000}, function(error, stdout, stderr) {
        cb(error, stdout || stderr);
    });
};