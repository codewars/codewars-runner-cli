var exec = require('child_process').exec,
    util = require('./util'),
    config = require('./config'),
    Docker = require('dockerode'),
    Streams = require('memory-streams'),
    docker = new Docker({socketPath: '/var/run/docker.sock'} );


module.exports.run = function run(image, cmd, args, cb) {
    args.format = args.format || 'json';
    args = util.parseBodyToArray(args);
    args.unshift(cmd);

    var stderr = new Streams.WritableStream(),
        stdout = new Streams.WritableStream();
    docker.run(image, args, [stdout, stderr], {Tty:false}, function(error, data) {
        cb(error, data, stdout.toString(), stderr.toString());
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

module.exports.build = function build(file, image, cb) {
    image = image || 'codewars/cli-runner';
    image += ':' + config.version;

    file = file || '.';
    var cmd = "docker build -t " + image + " " + file;
    exec(cmd, cb);
};

module.exports.push = function build(image, cb) {
    image = image || 'codewars/cli-runner';
    image += ':' + config.version;

    var cmd = "docker push " + image;
    exec(cmd, cb);
};