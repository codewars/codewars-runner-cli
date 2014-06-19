//! Simple utility for pushing latest docker image with version tagged

var docker = require('./lib/docker'),
    opts = require("nomnom")
        .options({
            image: {
                abbr: 'i',
                help: 'The image to push. No tag needed, it will automatically be appended. If empty the default will be used.'
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

console.log('Pushing image, this may take a while...');
docker.push(opts.image, function(code) {
    console.log('Process finished');
});

