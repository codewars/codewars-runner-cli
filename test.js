var exec = require('child_process').exec,
    docker = require('./lib/docker'),
    clc = require('cli-color');

// i.e. docker run -i --entrypoint mocha codewars/cli-runner:0.1.22 --recursive -t 5000
exec("docker run -i --entrypoint mocha " + docker.taggedImage() + " --recursive -t 5000", function(err, stdout, stderr)
{
    console.log(stdout);
    console.log(clc.red(stderr));
});

