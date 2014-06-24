//! Simple utility for pulling latest docker image with version tagged, returns JSON containing details

var docker = require('./lib/docker'),
    config = require('./lib/config'),
    opts = require("nomnom")
        .options({
            image: {
                abbr: 'i',
                help: 'The image to pull. No tag needed, it will automatically be appended. If empty the default will be used.'
            },
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

docker.pull(opts.image, function(err, stdout, stderr) {
    console.log(JSON.stringify({
        docker: stdout || stderr,
        version: config.version,
        image: docker.taggedImage()
    }));
});

