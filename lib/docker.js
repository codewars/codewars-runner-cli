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
    docker.run(taggedImage(image), args, [stdout, stderr], {Tty:false}, function(error, data) {
        cb(error, data, stdout.toString(), stderr.toString());
    });
};

// runs using the cli instead of remote api.
module.exports.runCmd = function runCmd(image, cmd, args, cb) {
    args = util.parseBodyToArgs(args);
    var cmd = 'docker run --rm  ' + taggedImage(image) + ' ' + cmd + ' --format json ' + args;
    exec(cmd, {timeout: 10000}, function(error, stdout, stderr) {
        cb(error, stdout || stderr);
    });
};

// simply utility for building images with their tag automatically set to the configured version
module.exports.build = function build(file, image, cb) {
    file = file || '.';
    var cmd = "docker build -t " + taggedImage(image) + " " + file;
    exec(cmd, cb);
};

// simply utility for pushing images with their tag automatically set to the configured version
module.exports.push = function build(image, cb) {
    exec("docker push " + taggedImage(image), cb);
};

// returns the image tagged with the current version. If no image is provided then the default
// will be used.
function taggedImage(image) {
    image = image || 'codewars/cli-runner';
    return image + ':' + config.version;
}
module.exports.taggedImage = taggedImage;