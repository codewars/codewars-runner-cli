var exec = require('child_process').exec,
    docker = require('./lib/docker');

exec("docker run --entrypoint mocha " + docker.taggedImage() + " test/*", function(err, stdout, stderr)
{
    console.log(stdout || stderr);
});

