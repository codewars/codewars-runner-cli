//! Simple utility for pulling latest docker image with version tagged, returns JSON containing details

var docker = require('./lib/docker'),
    config = require('./lib/config'),
    opts = require("nomnom")
        .options({
            version: {
                abbr: 'v',
                flag: true,
                help: 'Print version and exit',
                callback: function () {
                    return config.version
                }
            }
        })
        .help('This utility will rebuild the docker image for the latest version')
        .parse();

var results = {version: config.version, runners: {}};

docker.imageChain(opts._, function(name, image, next)
{
    console.log("Pulling " + image);
    docker.pull(image, function(err, stdout, stderr) {
        results.runners[name] = {
            output: stdout || stderr,
            image: image
        }
        next();
    });
}).run(function()
{
    console.log(JSON.stringify(results));
});


